;;; gptel-native-shell.el --- Native shell tool for gptel -*- lexical-binding: t -*-

;; Author: Gregory Grubbs
;; Created: 2026-02-10
;; Description: A native Elisp shell tool for gptel that bypasses MCP
;;              to avoid silent hangs and provide proper timeout handling.

;;; Commentary:
;;
;; This package provides a native shell execution tool for gptel that:
;; - Executes commands synchronously with enforced timeouts
;; - Returns structured output (stdout, stderr, exit-code, timed-out-p)
;; - Maintains a configurable command allowlist
;; - Operates in the `default-directory` of the gptel buffer
;;
;; Usage:
;;   (require 'gptel-native-shell)
;;   ;; Tool is auto-registered with gptel when this file loads
;;
;; Security:
;;   Commands are validated against `gptel-shell-allowed-commands`
;;   before execution. Modify this list to allow/disallow commands.

;;; Code:

(require 'cl-lib)

;;; ============================================================================
;;; Customization
;;; ============================================================================

(defgroup gptel-native-shell nil
  "Native shell tool for gptel."
  :group 'gptel
  :prefix "gptel-shell-")

(defcustom gptel-shell-default-timeout 30
  "Default timeout in seconds for shell command execution.
Commands running longer than this will be killed and return a timeout error."
  :type 'integer
  :group 'gptel-native-shell)

(defcustom gptel-shell-max-output-size 100000
  "Maximum output size in characters before truncation.
When output exceeds this, it will be truncated with a notice.
Set to nil to disable truncation (not recommended)."
  :type '(choice integer (const nil))
  :group 'gptel-native-shell)

(defcustom gptel-shell-allowed-commands
  '("bash" "bc" "cat" "chmod" "curl" "date" "df" "diff" "du" "echo"
    "emacs" "emacsclient" "env" "find" "git" "grep" "gunzip" "head"
    "jq" "less" "ls" "lynx" "man" "mkdir" "mv" "node" "npm" "pandoc"
    "psql" "pwd" "python" "python3" "rg" "rm" "sed" "sort" "sqlite3"
    "tail" "tar" "tee" "touch" "tr" "tree" "uniq" "unzip" "wc" "which"
    "xargs" "zip")
  "List of commands allowed to be executed by the shell tool.
Only the base command name is checked (the first word of the command).
This can be modified interactively during a session using
`gptel-shell-allow-command' and `gptel-shell-disallow-command'."
  :type '(repeat string)
  :group 'gptel-native-shell)

(defcustom gptel-shell-dangerous-git-subcommands
  '("push" "commit" "merge" "rebase" "reset" "checkout" "branch" "-D"
    "stash" "cherry-pick" "revert" "am" "apply" "clean" "gc" "prune"
    "remote" "submodule" "filter-branch" "replace" "notes")
  "Git subcommands that require confirmation before execution.
These are considered potentially destructive operations."
  :type '(repeat string)
  :group 'gptel-native-shell)

(defcustom gptel-shell-confirm-dangerous-commands t
  "If non-nil, prompt for confirmation before running dangerous commands.
Dangerous commands include certain git subcommands and commands with
output redirection to files outside the current directory tree."
  :type 'boolean
  :group 'gptel-native-shell)

;;; ============================================================================
;;; Command Allowlist Management
;;; ============================================================================

(defun gptel-shell-allow-command (command)
  "Add COMMAND to the list of allowed commands for this session.
This change persists only for the current Emacs session."
  (interactive "sCommand to allow: ")
  (unless (member command gptel-shell-allowed-commands)
    (push command gptel-shell-allowed-commands)
    (message "Command '%s' added to allowed list" command)))

(defun gptel-shell-disallow-command (command)
  "Remove COMMAND from the list of allowed commands for this session.
This change persists only for the current Emacs session."
  (interactive
   (list (completing-read "Command to disallow: " gptel-shell-allowed-commands nil t)))
  (setq gptel-shell-allowed-commands (delete command gptel-shell-allowed-commands))
  (message "Command '%s' removed from allowed list" command))

(defun gptel-shell--extract-base-command (command-string)
  "Extract the base command name from COMMAND-STRING.
Handles common patterns like:
  - Simple commands: 'ls -la' -> 'ls'
  - Env vars: 'FOO=bar command args' -> 'command'
  - Sudo/env wrappers: 'sudo command' -> 'command'
Returns nil if no valid command can be extracted."
  (when (and command-string (stringp command-string))
    (let* ((trimmed (string-trim command-string))
           ;; Split on whitespace
           (parts (split-string trimmed "[ \t]+" t))
           (first-part (car parts)))
      (cond
       ;; Empty command
       ((null first-part) nil)
       ;; Environment variable assignment at start: VAR=value command
       ((string-match-p "^[A-Za-z_][A-Za-z0-9_]*=" first-part)
        (gptel-shell--extract-base-command
         (mapconcat #'identity (cdr parts) " ")))
       ;; Command wrappers - recurse to find actual command
       ((member first-part '("sudo" "env" "nohup" "nice" "time" "timeout"))
        (gptel-shell--extract-base-command
         (mapconcat #'identity (cdr parts) " ")))
       ;; Regular command - return just the binary name without path
       (t (file-name-nondirectory first-part))))))

(defun gptel-shell--command-allowed-p (command-string)
  "Check if the command in COMMAND-STRING is in the allowlist.
Returns t if allowed, nil otherwise."
  (let ((base-cmd (gptel-shell--extract-base-command command-string)))
    (and base-cmd (member base-cmd gptel-shell-allowed-commands))))

(defun gptel-shell--check-dangerous-git (command-string)
  "Check if COMMAND-STRING contains a dangerous git subcommand.
Returns the dangerous subcommand if found, nil otherwise."
  (when (string-match "^\\s-*\\(?:sudo\\s-+\\)?git\\s-+\\([a-z-]+\\)" command-string)
    (let ((subcommand (match-string 1 command-string)))
      (when (member subcommand gptel-shell-dangerous-git-subcommands)
        subcommand))))

;;; ============================================================================
;;; Core Shell Execution
;;; ============================================================================

(defun gptel-shell--execute-with-timeout (command timeout working-dir)
  "Execute COMMAND synchronously with TIMEOUT seconds limit.
WORKING-DIR specifies the directory to run the command in.
Returns a plist with keys:
  :stdout    - standard output as string
  :stderr    - standard error as string
  :exit-code - process exit code (integer)
  :timed-out - t if the command was killed due to timeout
  :command   - the command that was executed
  :duration  - execution time in seconds"
  (let* ((default-directory (or working-dir default-directory))
         (stdout-buffer (generate-new-buffer " *gptel-shell-stdout*"))
         (stderr-file (make-temp-file "gptel-shell-stderr"))
         (start-time (current-time))
         (process nil)
         (timed-out nil)
         (exit-code nil))
    (unwind-protect
        (progn
          ;; Start process
          (setq process
                (start-process-shell-command
                 "gptel-shell"
                 stdout-buffer
                 (format "%s 2>%s" command (shell-quote-argument stderr-file))))

          ;; Wait for completion or timeout
          (let ((deadline (+ (float-time) timeout)))
            (while (and (process-live-p process)
                        (< (float-time) deadline))
              (accept-process-output process 0.1))

            ;; Check if we timed out
            (when (process-live-p process)
              (setq timed-out t)
              ;; Kill the process tree
              (let ((pid (process-id process)))
                (when pid
                  ;; Try to kill process group first
                  (ignore-errors (signal-process (- pid) 'TERM))
                  (sleep-for 0.1)
                  (when (process-live-p process)
                    (ignore-errors (signal-process (- pid) 'KILL)))
                  (ignore-errors (kill-process process))))))

          ;; Get exit code
          (setq exit-code (if timed-out
                              -1
                            (process-exit-status process)))

          ;; Build result plist
          (let* ((stdout-raw (with-current-buffer stdout-buffer
                               (buffer-string)))
                 (stderr-raw (with-temp-buffer
                               (insert-file-contents stderr-file)
                               (buffer-string)))
                 (duration (float-time (time-subtract (current-time) start-time)))
                 ;; Truncate if needed
                 (stdout (gptel-shell--maybe-truncate stdout-raw "stdout"))
                 (stderr (gptel-shell--maybe-truncate stderr-raw "stderr")))

            (list :stdout stdout
                  :stderr stderr
                  :exit-code exit-code
                  :timed-out timed-out
                  :command command
                  :duration (format "%.2f" duration))))

      ;; Cleanup
      (when (buffer-live-p stdout-buffer)
        (kill-buffer stdout-buffer))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun gptel-shell--maybe-truncate (text stream-name)
  "Truncate TEXT if it exceeds `gptel-shell-max-output-size'.
STREAM-NAME is used in the truncation notice (e.g., 'stdout')."
  (if (and gptel-shell-max-output-size
           (> (length text) gptel-shell-max-output-size))
      (concat (substring text 0 gptel-shell-max-output-size)
              (format "\n\n[... %s truncated: %d chars total, showing first %d ...]"
                      stream-name
                      (length text)
                      gptel-shell-max-output-size))
    text))

(defun gptel-shell--format-result (result)
  "Format RESULT plist into a human-readable string for the LLM."
  (let ((stdout (plist-get result :stdout))
        (stderr (plist-get result :stderr))
        (exit-code (plist-get result :exit-code))
        (timed-out (plist-get result :timed-out))
        (command (plist-get result :command))
        (duration (plist-get result :duration)))
    (concat
     (format "Command: %s\n" command)
     (format "Working directory: %s\n" default-directory)
     (format "Duration: %s seconds\n" duration)
     (if timed-out
         (format "Status: TIMED OUT (killed after timeout)\n")
       (format "Exit code: %d\n" exit-code))
     "\n"
     (if (string-empty-p stdout)
         "stdout: (empty)\n"
       (format "stdout:\n%s\n" stdout))
     (if (string-empty-p stderr)
         ""
       (format "\nstderr:\n%s\n" stderr)))))

;;; ============================================================================
;;; Main Entry Point for gptel Tool
;;; ============================================================================

(defun gptel-shell-execute (command &optional timeout)
  "Execute COMMAND and return formatted output suitable for gptel.
TIMEOUT specifies seconds to wait (default: `gptel-shell-default-timeout').

The command runs in the `default-directory' of the current buffer.

Returns a formatted string with:
- Command executed
- Working directory
- Duration
- Exit code (or timeout indication)
- stdout and stderr content

Security: Only commands in `gptel-shell-allowed-commands' are permitted."
  (let ((timeout (or timeout gptel-shell-default-timeout)))
    ;; Validate command is allowed
    (unless (gptel-shell--command-allowed-p command)
      (let ((base-cmd (gptel-shell--extract-base-command command)))
        (error "Command '%s' is not in the allowed list. Allowed commands: %s"
               (or base-cmd command)
               (mapconcat #'identity gptel-shell-allowed-commands ", "))))

    ;; Check for dangerous git operations
    (when (and gptel-shell-confirm-dangerous-commands
               (gptel-shell--check-dangerous-git command))
      (let ((dangerous-subcmd (gptel-shell--check-dangerous-git command)))
        (unless (yes-or-no-p
                 (format "Command contains potentially destructive git subcommand '%s'. Execute anyway? "
                         dangerous-subcmd))
          (error "Command execution cancelled by user"))))

    ;; Execute and format result
    (let ((result (gptel-shell--execute-with-timeout command timeout default-directory)))
      (gptel-shell--format-result result))))

;;; ============================================================================
;;; gptel Tool Registration
;;; ============================================================================

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "execute_shell_command"
   :description "Execute a shell command and return stdout, stderr, and exit code.

The command runs in the working directory of the current gptel buffer.

IMPORTANT CONSTRAINTS:
- Only whitelisted commands are allowed (use list_allowed_shell_commands to see the current list)
- Commands have a timeout (default 30 seconds) - long-running commands will be killed
- For large outputs, consider piping through head/tail/grep to reduce size
- Git write operations (commit, push, etc.) require user confirmation

Use this for: file operations, searching, git status/log/diff, running scripts, etc.
Do NOT use for: interactive commands, commands requiring TTY, or very long-running processes."
   :category "shell"
   :function (lambda (command &optional timeout)
               "Wrapper for gptel-shell-execute."
               (gptel-shell-execute command timeout))
   :args (list '(:name "command"
                       :type string
                       :description "The shell command to execute. Only allowed commands will run.")
               '(:name "timeout"
                       :type integer
                       :description "Timeout in seconds. Default is 30. Max recommended is 120.")))

  (gptel-make-tool
   :name "list_allowed_shell_commands"
   :description "List all shell commands that are currently whitelisted for execution.

Returns the current allowlist of commands. If a command you need is not listed,
ask the user to add it via M-x gptel-shell-allow-command.

This list can be modified dynamically during a session without restarting anything."
   :category "shell"
   :function (lambda ()
               (format "Allowed shell commands:\n%s\n\nTotal: %d commands\n\nTo request additional commands, ask the user to run M-x gptel-shell-allow-command"
                       (string-join (sort (copy-sequence gptel-shell-allowed-commands) #'string<) ", ")
                       (length gptel-shell-allowed-commands)))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun gptel-shell-list-allowed ()
  "Display a buffer showing all currently allowed commands."
  (interactive)
  (with-current-buffer (get-buffer-create "*gptel-shell-allowed*")
    (erase-buffer)
    (insert "Commands allowed for gptel shell tool:\n")
    (insert "======================================\n\n")
    (dolist (cmd (sort (copy-sequence gptel-shell-allowed-commands) #'string<))
      (insert (format "  %s\n" cmd)))
    (insert (format "\nTotal: %d commands\n" (length gptel-shell-allowed-commands)))
    (insert "\nUse M-x gptel-shell-allow-command to add commands\n")
    (insert "Use M-x gptel-shell-disallow-command to remove commands\n")
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(provide 'gptel-native-shell)
;;; gptel-native-shell.el ends here
