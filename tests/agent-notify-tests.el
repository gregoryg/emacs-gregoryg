;;; agent-notify-tests.el --- Tests for agent-notify in Desktop.org  -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs the `agent-notify' bash script exactly as Desktop.org would
;; tangle it, against fake notify-send, gdbus, curl and xprintidle
;; commands.  Each fake appends one line per call to a log, and the fake
;; curl also logs the config file it was handed, which is where the
;; webhook id must be.
;;
;; Sandboxed: state, config and the fakes live in a temporary directory,
;; XDG_RUNTIME_DIR and XDG_CONFIG_HOME point there, and the fakes come
;; first on PATH, so no notification is shown and nothing reaches Home
;; Assistant.  jq, sed and awk are the real ones.

;;; Code:

(require 'ert)
(require 'gjg-config-test)

(defconst agent-notify-test--fakes
  '(("notify-send" . "echo \"notify-send $*\" >>\"$FAKE_LOG\"; echo 42")
    ("gdbus" . "echo \"gdbus $*\" >>\"$FAKE_LOG\"")
    ("xprintidle" . "echo \"${FAKE_IDLE_MS:-0}\"")
    ("curl" . "echo \"curl $*\" >>\"$FAKE_LOG\"
prev=
for arg in \"$@\"; do
  [ \"$prev\" = --config ] && sed 's/^/config /' \"$arg\" >>\"$FAKE_LOG\"
  prev=$arg
done"))
  "Fake commands as (NAME . SH-BODY); each logs its call to $FAKE_LOG.")

(defvar agent-notify-test--dir nil "The temporary directory of the running test.")

(defun agent-notify-test--script ()
  "Return the bash code Desktop.org tangles to ~/bin/agent-notify."
  (let ((buffer (find-file-noselect
                 (expand-file-name "Desktop.org" gjg-config-test-repo))))
    (unwind-protect
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (gjg-config-test--narrow-to-custom-id "agent-notify" "Desktop.org")
            (let ((blocks (cdr (assoc (expand-file-name "~/bin/agent-notify")
                                      (org-babel-tangle-collect-blocks "\\`bash\\'")))))
              (unless blocks (error "Desktop.org: no agent-notify block"))
              (mapconcat (lambda (block) (nth 5 (cdr block))) blocks "\n"))))
      (kill-buffer buffer))))

(defmacro agent-notify-test--with-sandbox (&rest body)
  "Run BODY with a fresh sandbox in `agent-notify-test--dir'."
  (declare (indent 0))
  `(let ((agent-notify-test--dir (make-temp-file "agent-notify-test-" t)))
     (unwind-protect
         (let ((bin (expand-file-name "bin" agent-notify-test--dir)))
           (make-directory bin)
           (make-directory (expand-file-name "run" agent-notify-test--dir))
           (make-directory (expand-file-name "config/agent-notify" agent-notify-test--dir) t)
           (with-temp-file (expand-file-name "agent-notify" bin)
             (insert "#!/usr/bin/env bash\n" (agent-notify-test--script)))
           (pcase-dolist (`(,name . ,body) agent-notify-test--fakes)
             (with-temp-file (expand-file-name name bin)
               (insert "#!/bin/sh\n" body "\n")))
           (dolist (file (directory-files bin t "\\`[^.]"))
             (set-file-modes file #o755))
           ,@body)
       (delete-directory agent-notify-test--dir t))))

(defun agent-notify-test--environment (&optional env)
  "Return the sandbox `process-environment', with ENV strings first.
The webhook id is test-hook-id unless ENV overrides it."
  (let ((dir agent-notify-test--dir))
    (append env
            (list (concat "PATH=" (expand-file-name "bin" dir) ":" (getenv "PATH"))
                  (concat "XDG_RUNTIME_DIR=" (expand-file-name "run" dir))
                  (concat "XDG_CONFIG_HOME=" (expand-file-name "config" dir))
                  (concat "FAKE_LOG=" (expand-file-name "calls" dir))
                  "RANONA_NOTIFY_WEBHOOK_ID=test-hook-id")
            (seq-remove (lambda (var)
                          (string-match-p "\\`\\(AGENT_NOTIFY\\|RANONA_NOTIFY\\)" var))
                        process-environment))))

(defun agent-notify-test--run (args &optional stdin env)
  "Run agent-notify with ARGS and STDIN; return (EXIT . OUTPUT).
ENV is a list of extra NAME=VALUE strings."
  (let ((process-environment (agent-notify-test--environment env))
        (script (expand-file-name "bin/agent-notify" agent-notify-test--dir)))
    (with-temp-buffer
      (let ((exit (if stdin
                      (progn (insert stdin)
                             ;; DELETE is t: the output replaces the input.
                             (apply #'call-process-region (point-min) (point-max)
                                    script t '(t nil) nil args))
                    (apply #'call-process script nil '(t nil) nil args))))
        (cons exit (buffer-string))))))

(defun agent-notify-test--calls ()
  "Return the fake-command log lines, oldest first, and empty the log."
  (let ((file (expand-file-name "calls" agent-notify-test--dir)))
    (prog1 (and (file-exists-p file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (split-string (buffer-string) "\n" t)))
      (when (file-exists-p file) (delete-file file)))))

(defun agent-notify-test--hook (&rest fields)
  "Return a Claude hook payload with FIELDS, a plist of keywords and values."
  (json-encode
   (cl-loop for (key value) on fields by #'cddr
            collect (cons (substring (symbol-name key) 1) value))))

(ert-deftest agent-notify-usage ()
  "Help succeeds; bad arguments exit 2 and touch nothing."
  (agent-notify-test--with-sandbox
    (should (= 0 (car (agent-notify-test--run '("--help")))))
    (dolist (args '(() ("Claude" "ready") ("claude" "bogus")
                    ("claude" "ready" "--cwd") ("claude" "ready" "--what" "x")))
      (should (= 2 (car (agent-notify-test--run args)))))
    (should-not (agent-notify-test--calls))))

(ert-deftest agent-notify-approval-then-ready ()
  "Approval notifies desktop and phone; ready clears the phone and replaces the desktop one."
  (agent-notify-test--with-sandbox
    (should (equal '(0 . "")
                   (agent-notify-test--run
                    '("claude" "approval" "--session" "AB-12-cd-99xx"
                      "--cwd" "/x/proj" "--message" "Allow \"Bash\"?"))))
    (let ((calls (agent-notify-test--calls)))
      (should (equal (nth 0 calls)
                     "notify-send --app-name Claude: proj --urgency critical --expire-time 0 --print-id -- Claude: proj Allow \"Bash\"?"))
      ;; The webhook id is in curl's config, never in its arguments.
      (should (string-prefix-p "curl " (nth 1 calls)))
      (should-not (string-match-p "test-hook-id" (nth 1 calls)))
      (should (string-match-p (regexp-quote "\"key\":\"agent_claude_ab12cd99\"") (nth 1 calls)))
      (should (string-match-p (regexp-quote "\"recipients\":[\"greg\"]") (nth 1 calls)))
      (should (string-match-p (regexp-quote "\"persistent\":false") (nth 1 calls)))
      (should (equal (nth 2 calls)
                     "config url = \"http://172.16.17.7:8123/api/webhook/test-hook-id\"")))
    (should (equal '(0 . "") (agent-notify-test--run
                              '("claude" "ready" "--session" "AB-12-cd-99xx" "--cwd" "/x/proj"))))
    (let ((calls (agent-notify-test--calls)))
      (should (string-match-p (regexp-quote "{\"operation\":\"clear\",\"key\":\"agent_claude_ab12cd99\"}")
                              (nth 0 calls)))
      (should (equal (car (last calls))
                     "notify-send --app-name Claude: proj --urgency normal --expire-time 0 --print-id --replace-id 42 -- Claude: proj Ready for input")))
    ;; Nothing is left to clear.
    (agent-notify-test--run '("claude" "ready" "--session" "AB-12-cd-99xx" "--cwd" "/x/proj"))
    (should-not (seq-some (lambda (line) (string-prefix-p "curl" line))
                          (agent-notify-test--calls)))))

(ert-deftest agent-notify-clear-closes-only-what-it-sent ()
  "Clear closes this session's notifications once, and leaves other sessions alone."
  (agent-notify-test--with-sandbox
    (agent-notify-test--run '("pi" "approval" "--session" "one"))
    (agent-notify-test--run '("pi" "approval" "--session" "two"))
    (agent-notify-test--calls)
    (agent-notify-test--run '("pi" "clear" "--session" "one"))
    (let ((calls (agent-notify-test--calls)))
      (should (string-match-p "\"key\":\"agent_pi_one\"" (nth 0 calls)))
      (should (string-match-p "config url" (nth 1 calls)))
      (should (string-match-p "CloseNotification uint32 42" (nth 2 calls)))
      (should (= 3 (length calls))))
    (agent-notify-test--run '("pi" "clear" "--session" "one"))
    (should-not (agent-notify-test--calls))))

(ert-deftest agent-notify-claude-hooks ()
  "Claude hook payloads map to events; others are ignored."
  (agent-notify-test--with-sandbox
    (cl-flet ((hook (&rest fields)
                (agent-notify-test--run '("claude" "--hook")
                                        (apply #'agent-notify-test--hook fields))))
      ;; Ignored: idle reminders, subagents, ordinary tools, unknown and broken payloads.
      (hook :hook_event_name "Notification" :notification_type "idle_prompt" :session_id "s1")
      (hook :hook_event_name "Stop" :agent_id "a1" :session_id "s1")
      (hook :hook_event_name "PreToolUse" :tool_name "Read" :session_id "s1")
      (hook :hook_event_name "PostToolUse" :tool_name "Read" :session_id "s1")
      (hook :hook_event_name "SessionStart" :session_id "s1")
      (should (equal '(0 . "") (agent-notify-test--run '("claude" "--hook") "not json")))
      (should-not (agent-notify-test--calls))
      (hook :hook_event_name "Notification" :notification_type "permission_prompt"
            :message "Claude needs your permission to use Bash"
            :session_id "0f3e9a1b-2222" :cwd "/home/gregj/emacs-gregoryg")
      (let ((calls (agent-notify-test--calls)))
        (should (string-match-p "--urgency critical .* Claude: emacs-gregoryg Claude needs your permission to use Bash\\'"
                                (nth 0 calls)))
        (should (string-match-p "\"key\":\"agent_claude_0f3e9a1b\"" (nth 1 calls))))
      ;; Approving the tool clears the phone and the desktop.
      (hook :hook_event_name "PostToolUse" :tool_name "Bash" :session_id "0f3e9a1b-2222")
      (let ((calls (agent-notify-test--calls)))
        (should (string-match-p "\"operation\":\"clear\"" (nth 0 calls)))
        (should (string-match-p "CloseNotification" (nth 2 calls))))
      (hook :hook_event_name "PreToolUse" :tool_name "ExitPlanMode" :session_id "s2" :cwd "/p")
      (should (string-match-p "Claude: p Claude wants a plan approved\\'" (car (agent-notify-test--calls))))
      (hook :hook_event_name "Stop" :session_id "s2" :cwd "/p"
            :last_assistant_message "\n\n   Done: tests pass.  \nMore detail")
      (should (string-suffix-p "Claude: p Done: tests pass."
                               (car (last (agent-notify-test--calls))))))))

(ert-deftest agent-notify-claude-print-mode-is-silent ()
  "Hooks under a claude -p run notify nothing; under an interactive claude they do."
  (agent-notify-test--with-sandbox
    (let ((claude (expand-file-name "claude" agent-notify-test--dir))
          (payload (agent-notify-test--hook :hook_event_name "Stop" :session_id "s1")))
      ;; A process named claude whose arguments include -p, like a print run.
      (copy-file "/bin/sh" claude)
      (dolist (case '(("-p" . nil) ("--print" . nil) ("--resume" . t)))
        ;; "; true" keeps sh from exec'ing the script, so claude stays its parent.
        (let ((process-environment (agent-notify-test--environment)))
          (with-temp-buffer
            (insert payload)
            (call-process-region (point-min) (point-max) claude nil nil nil
                                 "-c" "agent-notify claude --hook; true" (car case))))
        (if (cdr case)
            (should (agent-notify-test--calls))
          (should-not (agent-notify-test--calls)))))))

(ert-deftest agent-notify-config-file ()
  "The config file supplies settings, the environment wins, and nothing in it runs."
  (agent-notify-test--with-sandbox
    (with-temp-file (expand-file-name "config/agent-notify/env" agent-notify-test--dir)
      (insert "# comment\n\n"
              "RANONA_NOTIFY_URL=\"http://ha.test/api/webhook\"\n"
              "AGENT_NOTIFY_RECIPIENTS='greg, rozi'\n"
              "RANONA_NOTIFY_WEBHOOK_ID=from-file\n"
              "PATH=/nonexistent\n"
              "touch /tmp/agent-notify-test-pwned\n"))
    (agent-notify-test--run '("claude" "approval"))
    (let ((calls (agent-notify-test--calls)))
      (should (string-match-p (regexp-quote "\"recipients\":[\"greg\",\"rozi\"]") (nth 1 calls)))
      ;; RANONA_NOTIFY_WEBHOOK_ID from the environment wins over the file.
      (should (equal (nth 2 calls) "config url = \"http://ha.test/api/webhook/test-hook-id\"")))
    (should-not (file-exists-p "/tmp/agent-notify-test-pwned"))
    ;; Without any webhook id, the desktop still works and the phone is skipped.
    (agent-notify-test--run '("claude" "approval" "--session" "x")
                            nil '("RANONA_NOTIFY_WEBHOOK_ID="))
    (should (equal 1 (length (seq-filter (lambda (line) (string-prefix-p "config" line))
                                         (agent-notify-test--calls)))))))

(ert-deftest agent-notify-phone-idle-threshold ()
  "With a threshold, the phone is used only when X has been idle long enough."
  (agent-notify-test--with-sandbox
    (let ((env '("AGENT_NOTIFY_PHONE_IDLE_SECONDS=300")))
      (agent-notify-test--run '("claude" "approval") nil (cons "FAKE_IDLE_MS=5000" env))
      (let ((calls (agent-notify-test--calls)))
        (should (= 1 (length calls)))
        (should (string-prefix-p "notify-send" (car calls))))
      (agent-notify-test--run '("claude" "approval") nil (cons "FAKE_IDLE_MS=600000" env))
      (should (seq-some (lambda (line) (string-prefix-p "curl" line))
                        (agent-notify-test--calls))))))

(defun agent-notify-test--desktop-body ()
  "Return the body of the last notify-send call, whose title is Claude, and empty the log."
  (let ((line (car (last (seq-filter (lambda (line) (string-prefix-p "notify-send" line))
                                     (agent-notify-test--calls))))))
    (and line (substring line (+ (string-search " -- Claude " line) (length " -- Claude "))))))

(ert-deftest agent-notify-long-messages ()
  "Long messages are cut near 400 characters at a word boundary, with an ellipsis."
  (agent-notify-test--with-sandbox
    (let ((words (mapconcat #'identity (make-list 120 "lorem") " ")))
      (agent-notify-test--run '("claude" "--hook")
                              (agent-notify-test--hook
                               :hook_event_name "Stop" :session_id "s1"
                               :last_assistant_message (concat words "\nsecond line")))
      (let ((text (agent-notify-test--desktop-body)))
        (should (string-suffix-p " lorem…" text))
        (should (<= 390 (length text) 400))))
    ;; One unbroken word is cut mid-word rather than to nothing.
    (agent-notify-test--run (list "claude" "approval" "--message" (make-string 600 ?x)))
    (should (equal (agent-notify-test--desktop-body) (concat (make-string 399 ?x) "…")))
    ;; Short messages are untouched, multibyte text included.
    (agent-notify-test--run '("claude" "approval" "--message" "¿Permitir «Bash»?"))
    (should (equal (agent-notify-test--desktop-body) "¿Permitir «Bash»?"))))

(ert-deftest agent-notify-dry-run ()
  "Dry run prints what it would deliver and calls nothing."
  (agent-notify-test--with-sandbox
    (let ((output (cdr (agent-notify-test--run '("claude" "approval" "--message" "hi")
                                               nil '("AGENT_NOTIFY_DRY_RUN=1")))))
      (should (string-match-p "\\`desktop critical: Claude | hi\nphone {" output))
      (should-not (agent-notify-test--calls)))))

;;; agent-notify-tests.el ends here
