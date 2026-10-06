;;; gjg-sql-tests.el --- Tests for the SQL section of README.org  -*- lexical-binding: t; -*-

;;; Commentary:

;; Loads the `sql' subtree of README.org -- the wallet picker, the
;; PGPASSWORD workaround, password and wallet-cache clearing, saved
;; history -- and drives `gjg/sql-connect-wallet' against fake psql and
;; mysql clients and a throwaway plain-text wallet.  A fake client prints
;; each argument as "ARG ...", then AUTH-OK if the password s3cret
;; reached it (in PGPASSWORD or --password=) or AUTH-FAIL if not, then
;; idles until the test ends.
;;
;; Sandboxed: every prompt is answered by the test, any other prompt
;; fails it, and SQL history goes to the temporary directory.  Each test
;; also checks that the real history file was not touched.

;;; Code:

(require 'ert)
(require 'gjg-config-test)
(require 'sql)

(gjg-config-test-load "README.org" "sql")

(defvar my-sql-input-ring-file)
(declare-function gjg/sql-connect-wallet "ext:README.org" (&optional buf-name))
(declare-function gjg/sql-wallet-candidates "ext:README.org" ())
(declare-function gjg/sql-wallet-forget-cache "ext:README.org" ())

(defconst gjg-sql-test--real-history (default-value 'my-sql-input-ring-file)
  "The history file the config uses, which tests must leave alone.")

(defconst gjg-sql-test--client
  "#!/bin/sh
pw=$PGPASSWORD
for arg in \"$@\"; do
  case $arg in
    --password=*) pw=${arg#--password=} ;;
    *) echo \"ARG $arg\" ;;
  esac
done
if [ \"$pw\" = s3cret ]; then echo AUTH-OK; else echo AUTH-FAIL; fi
printf 'db=> '
exec sleep 60
"
  "A fake SQL client: reports its arguments and whether the password arrived.")

(defconst gjg-sql-test--sales
  "machine db.example.com/sales login alice password s3cret port 5433 product postgres name sales"
  "A complete wallet entry: name, product, server/database and port.")

(defvar gjg-sql-test--dir nil "The temporary directory of the running test.")
(defvar gjg-sql-test--answers nil
  "Alist of (PROMPT-REGEXP . ANSWER) for `completing-read'.
ANSWER is the string to return, or `quit' to quit at that prompt.")
(defvar gjg-sql-test--prompts nil "Prompts seen so far, most recent first.")
(defvar gjg-sql-test--annotations nil
  "Alist of (LABEL . ANNOTATION) the picker showed last.")

(defun gjg-sql-test--completing-read (prompt collection &rest _)
  "Answer PROMPT from `gjg-sql-test--answers', noting the picker's COLLECTION."
  (push prompt gjg-sql-test--prompts)
  (when-let* ((annotate (plist-get completion-extra-properties :annotation-function)))
    (setq gjg-sql-test--annotations
          (mapcar (lambda (label) (cons label (funcall annotate label)))
                  (all-completions "" collection))))
  (pcase (cdr (seq-find (lambda (answer) (string-match-p (car answer) prompt))
                        gjg-sql-test--answers))
    ('nil (error "Unexpected prompt: %s" prompt))
    ('quit (signal 'quit nil))
    (answer answer)))

(defun gjg-sql-test--unexpected (name value)
  "Return a stand-in for NAME that records its prompt and returns VALUE."
  (lambda (prompt &rest _)
    (push (format "UNEXPECTED %s: %s" name prompt) gjg-sql-test--prompts)
    value))

(defun gjg-sql-test--write-client (name)
  "Write the fake client as NAME in the test directory and return its path."
  (let ((file (expand-file-name name gjg-sql-test--dir)))
    (with-temp-file file
      (insert gjg-sql-test--client))
    (set-file-modes file #o700)
    file))

(defun gjg-sql-test--history-mtime ()
  "Return the modification time of the real history file, or nil."
  (file-attribute-modification-time (file-attributes gjg-sql-test--real-history)))

(defun gjg-sql-test--cleanup ()
  "Stop the test's SQL sessions and remove its files and cached wallet."
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer
            (or (derived-mode-p 'sql-interactive-mode) (derived-mode-p 'sql-mode)))
      (when-let* ((process (get-buffer-process buffer)))
        (delete-process process))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer))))
  (auth-source-forget-all-cached)
  (delete-directory gjg-sql-test--dir t))

(defmacro gjg-sql-test-with-wallet (lines &rest body)
  "Run BODY against a wallet holding LINES, with fake clients and prompts.
SQL defaults that `sql-connect' sets globally are restored afterwards.
The test fails if anything prompts that `gjg-sql-test--answers' does
not answer, or if the real history file changes."
  (declare (indent 1) (debug t))
  `(let* ((gjg-sql-test--dir (make-temp-file "gjg-sql-test" t))
          (history-before (gjg-sql-test--history-mtime))
          (sql-password-wallet (list (expand-file-name "sql-wallet" gjg-sql-test--dir)))
          (sql-postgres-program (gjg-sql-test--write-client "psql"))
          (sql-mysql-program (gjg-sql-test--write-client "mysql"))
          (my-sql-input-ring-file (expand-file-name "sqlhistory" gjg-sql-test--dir))
          (gjg-sql-test--answers nil)
          (gjg-sql-test--prompts nil)
          (gjg-sql-test--annotations nil)
          (sql-product sql-product)
          (sql-user sql-user)
          (sql-server sql-server)
          (sql-database sql-database)
          (sql-port sql-port)
          (sql-password sql-password)
          (sql-connection sql-connection)
          (sql-buffer sql-buffer))
     (with-temp-file (car sql-password-wallet)
       (insert (string-join ,lines "\n") "\n"))
     (auth-source-forget-all-cached)
     (unwind-protect
         (cl-letf (((symbol-function 'completing-read) #'gjg-sql-test--completing-read)
                   ((symbol-function 'read-passwd) (gjg-sql-test--unexpected 'read-passwd "x"))
                   ((symbol-function 'read-string) (gjg-sql-test--unexpected 'read-string "x"))
                   ((symbol-function 'read-from-minibuffer)
                    (gjg-sql-test--unexpected 'read-from-minibuffer "x"))
                   ((symbol-function 'yes-or-no-p) (gjg-sql-test--unexpected 'yes-or-no-p nil))
                   ((symbol-function 'y-or-n-p) (gjg-sql-test--unexpected 'y-or-n-p nil)))
           ,@body
           (should-not (seq-filter (lambda (prompt) (string-prefix-p "UNEXPECTED" prompt))
                                   gjg-sql-test--prompts)))
       (gjg-sql-test--cleanup))
     (should (equal history-before (gjg-sql-test--history-mtime)))))

(defun gjg-sql-test-connect (label &rest answers)
  "Choose LABEL in the picker and return the SQLi buffer once it has logged in.
ANSWERS are extra (PROMPT-REGEXP . ANSWER) pairs for other prompts."
  (setq gjg-sql-test--answers (append answers `(("\\`Wallet connection" . ,label))))
  (let* ((result (gjg/sql-connect-wallet))
         ;; A live session is displayed, so its window comes back.
         (buffer (if (windowp result) (window-buffer result) result)))
    (with-timeout (5 (error "No login report from the fake client in %s" buffer))
      (while (not (gjg-sql-test--output-matches buffer "^AUTH-"))
        (accept-process-output (get-buffer-process buffer) 0.1)))
    buffer))

(defun gjg-sql-test--output-matches (buffer regexp)
  "Return non-nil if the output in BUFFER matches REGEXP."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward regexp nil t))))

(defun gjg-sql-test--args (buffer)
  "Return the arguments the fake client in BUFFER was started with."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (args)
        (while (re-search-forward "^ARG \\(.*\\)$" nil t)
          (push (match-string-no-properties 1) args))
        (nreverse args)))))

(defun gjg-sql-test--logged-in-p (buffer)
  "Return non-nil if the password reached the client in BUFFER."
  (gjg-sql-test--output-matches buffer "^AUTH-OK$"))

(defun gjg-sql-test--wallet-cached-p ()
  "Return non-nil if auth-source holds the test wallet in its cache.
Matched by the test directory, independently of the code under test."
  (seq-some (lambda (entry)
              (and (stringp (car entry))
                   (string-prefix-p gjg-sql-test--dir (car entry))))
            auth-source-netrc-cache))

(defun gjg-sql-test--sqli-buffers ()
  "Return the live SQLi buffers."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer (derived-mode-p 'sql-interactive-mode)))
              (buffer-list)))

;;; Reading the wallet

(ert-deftest gjg-sql-test-candidates ()
  "Labels come from name or user@server/database:port, never with secrets."
  (gjg-sql-test-with-wallet
      (list gjg-sql-test--sales
            "# machine commented login nobody password s3cret"
            "machine maria login carol password s3cret product mysql"
            "machine bare login erin password s3cret"
            "machine dup login frank password s3cret port 5432 product postgres"
            "machine dup login frank password s3cret port 5432 product mysql")
    (let ((candidates (gjg/sql-wallet-candidates)))
      (should (equal (mapcar #'car candidates)
                     '("sales" "carol@maria" "erin@bare"
                       "frank@dup:5432" "frank@dup:5432 <2>")))
      (should (equal (cdr (assoc "sales" candidates))
                     '(:name "sales" :product "postgres" :user "alice"
                       :server "db.example.com" :database "sales" :port 5433)))
      (should-not (string-search "s3cret" (prin1-to-string candidates)))
      ;; Reading the wallet caches it; the cleanup helper drops it.
      (should (gjg-sql-test--wallet-cached-p))
      (gjg/sql-wallet-forget-cache)
      (should-not (gjg-sql-test--wallet-cached-p)))))

(ert-deftest gjg-sql-test-empty-wallet ()
  (gjg-sql-test-with-wallet '("")
    (should-error (gjg-sql-test-connect "anything") :type 'user-error)))

(ert-deftest gjg-sql-test-netrc-parse-error ()
  "A second value after machine breaks the whole wallet, loudly."
  (gjg-sql-test-with-wallet
      '("machine h1 h2 login bob password s3cret"
        "machine ok login zed password s3cret")
    (let ((err (should-error (gjg-sql-test-connect "anything"))))
      (should (string-match-p "Unexpected .machine. token" (cadr err))))
    (should-not (gjg-sql-test--wallet-cached-p))))

;;; Connecting

(ert-deftest gjg-sql-test-connect-named-postgres ()
  "A complete entry logs in with no prompt but the picker."
  (gjg-sql-test-with-wallet (list gjg-sql-test--sales)
    (let ((buffer (gjg-sql-test-connect "sales")))
      (should (equal (buffer-name buffer) "*SQL: <sales>*"))
      (should (equal (gjg-sql-test--args buffer)
                     '("-p" "5433" "-U" "alice" "-h" "db.example.com"
                       "-P" "pager=off" "sales")))
      (should (gjg-sql-test--logged-in-p buffer))
      (should (equal gjg-sql-test--prompts '("Wallet connection: ")))
      (should (equal gjg-sql-test--annotations '(("sales" . "  postgres"))))
      (should (equal (default-value 'sql-password) ""))
      (should-not (gjg-sql-test--wallet-cached-p)))))

(ert-deftest gjg-sql-test-history-is-sandboxed ()
  (gjg-sql-test-with-wallet (list gjg-sql-test--sales)
    (let ((buffer (gjg-sql-test-connect "sales")))
      (should (string-prefix-p gjg-sql-test--dir
                               (buffer-local-value 'comint-input-ring-file-name buffer))))))

(ert-deftest gjg-sql-test-reuse-live-session ()
  "Choosing a connection that is already running shows that session."
  (gjg-sql-test-with-wallet (list gjg-sql-test--sales)
    (let ((first (gjg-sql-test-connect "sales"))
          (second (gjg-sql-test-connect "sales")))
      (should (eq first second))
      (should (= 1 (length (gjg-sql-test--sqli-buffers)))))))

(ert-deftest gjg-sql-test-no-port ()
  "Without a port in the entry, psql gets none, despite the global 5432."
  (gjg-sql-test-with-wallet '("machine bare login erin password s3cret product postgres")
    (should (= 5432 (default-value 'sql-port)))
    (let ((buffer (gjg-sql-test-connect "erin@bare")))
      (should (equal (gjg-sql-test--args buffer)
                     '("-U" "erin" "-h" "bare" "-P" "pager=off")))
      (should (gjg-sql-test--logged-in-p buffer)))))

(ert-deftest gjg-sql-test-mysql ()
  (gjg-sql-test-with-wallet '("machine maria login carol password s3cret product mysql")
    (let ((buffer (gjg-sql-test-connect "carol@maria")))
      (should (eq (buffer-local-value 'sql-product buffer) 'mysql))
      (should (member "--user=carol" (gjg-sql-test--args buffer)))
      (should (member "--host=maria" (gjg-sql-test--args buffer)))
      (should (gjg-sql-test--logged-in-p buffer)))))

;;; Products

(ert-deftest gjg-sql-test-product-prompt ()
  "An entry without a product asks for one and says how to skip asking."
  (gjg-sql-test-with-wallet '("machine bare login erin password s3cret")
    (let ((buffer (gjg-sql-test-connect "erin@bare" '("\\`Product for" . "postgres"))))
      (should (equal gjg-sql-test--annotations '(("erin@bare" . "  (no product)"))))
      (should (string-match-p
               "\\`Product for erin@bare (add .product. to sql-wallet to skip this)"
               (car gjg-sql-test--prompts)))
      (should (eq (buffer-local-value 'sql-product buffer) 'postgres))
      (should (gjg-sql-test--logged-in-p buffer)))))

(ert-deftest gjg-sql-test-unknown-product ()
  (gjg-sql-test-with-wallet '("machine nope login dave password s3cret product oracle-ish")
    (let ((err (should-error (gjg-sql-test-connect "dave@nope") :type 'user-error)))
      (should (string-match-p "Unknown SQL product .oracle-ish. in sql-wallet entry dave@nope"
                              (cadr err))))
    (should-not (gjg-sql-test--wallet-cached-p))))

(ert-deftest gjg-sql-test-duplicate-labels ()
  "Entries that would share a label stay apart, each with its own product."
  (gjg-sql-test-with-wallet
      '("machine dup login frank password s3cret product postgres"
        "machine dup login frank password s3cret product mysql")
    (let ((buffer (gjg-sql-test-connect "frank@dup <2>")))
      (should (eq (buffer-local-value 'sql-product buffer) 'mysql))
      (should (gjg-sql-test--logged-in-p buffer)))))

;;; Where it starts from

(ert-deftest gjg-sql-test-from-sqli-buffer ()
  "The session you start from doesn't leak its settings into the new one."
  (gjg-sql-test-with-wallet
      (list gjg-sql-test--sales
            "machine maria login carol password s3cret product mysql")
    (let* ((postgres (gjg-sql-test-connect "sales"))
           (mysql (with-current-buffer postgres
                    (gjg-sql-test-connect "carol@maria"))))
      (should-not (eq postgres mysql))
      (should (eq (buffer-local-value 'sql-product mysql) 'mysql))
      (should (member "--user=carol" (gjg-sql-test--args mysql)))
      (should (gjg-sql-test--logged-in-p mysql)))))

(ert-deftest gjg-sql-test-from-sql-mode-buffer ()
  "A sql-mode buffer's own product doesn't win, and the buffer is linked."
  (gjg-sql-test-with-wallet (list gjg-sql-test--sales)
    (let* ((source (generate-new-buffer "query.sql"))
           (buffer (with-current-buffer source
                     (sql-mode)
                     ;; As a file-local "-*- sql-product: mysql -*-" sets it.
                     ;; (`sql-set-product' sets the global value instead.)
                     (setq-local sql-product 'mysql)
                     (gjg-sql-test-connect "sales"))))
      (should (eq (buffer-local-value 'sql-product buffer) 'postgres))
      (should (gjg-sql-test--logged-in-p buffer))
      (should (equal (buffer-local-value 'sql-buffer source) (buffer-name buffer))))))

;;; Leaving early

(ert-deftest gjg-sql-test-quit-at-picker ()
  "Quitting the picker still drops the decrypted wallet from the cache."
  (gjg-sql-test-with-wallet (list gjg-sql-test--sales)
    (should (eq 'quit
                (condition-case nil
                    (progn (gjg-sql-test-connect 'quit) 'returned)
                  (quit 'quit))))
    (should-not (gjg-sql-test--wallet-cached-p))
    (should-not (gjg-sql-test--sqli-buffers))))

;;; gjg-sql-tests.el ends here
