;;; google-tts.el --- Speak a region with Google TTS  -*- lexical-binding: t; -*-
;; 2026-08 

(require 'subr-x)
(require 'url-util)

(defgroup my-google-tts nil
  "Speak text using Google Translate's text-to-speech endpoint."
  :group 'multimedia)

(defcustom my-google-tts-default-language "pt-PT"
  "Default language code offered by `my-google-tts'."
  :type 'string
  :group 'my-google-tts)

(defcustom my-google-tts-curl-program "/usr/bin/curl"
  "Curl executable used by `my-google-tts'."
  :type 'file
  :group 'my-google-tts)

(defcustom my-google-tts-file-program "/usr/bin/file"
  "File-identification executable used by `my-google-tts'."
  :type 'file
  :group 'my-google-tts)

(defcustom my-google-tts-player-program "/usr/bin/mplayer"
  "Audio player used by `my-google-tts'."
  :type 'file
  :group 'my-google-tts)

(defun my-google-tts--language-p (language)
  "Return non-nil when LANGUAGE looks like \"pt\" or \"pt-PT\"."
  (and (stringp language)
       (let ((case-fold-search nil))
         (string-match-p
          "\\`\\(?:[a-z][a-z]\\|[a-z][a-z]-[A-Z][A-Z]\\)\\'"
          language))))

(defun my-google-tts--read-language ()
  "Read and validate a Google TTS language code."
  (let ((language
         (read-string (format "Language (%s): " my-google-tts-default-language)
                      nil nil my-google-tts-default-language)))
    (unless (my-google-tts--language-p language)
      (user-error "Language must look like `pt' or `pt-PT'"))
    language))

(defun my-google-tts--check-program (program)
  "Signal a user error unless PROGRAM is executable."
  (unless (and (file-exists-p program) (file-executable-p program))
    (user-error "Program is not executable: %s" program)))

(defun my-google-tts--url (text language)
  "Build a Google TTS URL for TEXT in LANGUAGE."
  (concat "https://translate.google.com/translate_tts"
          "?ie=UTF-8"
          "&q=" (url-hexify-string text)
          "&tl=" (url-hexify-string language)
          "&total=1&idx=0"
          "&textlen=" (number-to-string (length text))
          "&client=gtx&prev=input"))

(defun my-google-tts--download (url output-file)
  "Download URL to OUTPUT-FILE, signaling an error if curl fails."
  (let ((error-file (make-temp-file "google-tts-curl-"))
        status)
    (unwind-protect
        (progn
          (setq status
                (call-process my-google-tts-curl-program nil
                              (list nil error-file) nil
                              "--silent" "--show-error" "--location" "--fail"
                              "--connect-timeout" "5" "--max-time" "30"
                              "--output" output-file url))
          (unless (and (integerp status) (zerop status))
            (let ((detail
                   (with-temp-buffer
                     (insert-file-contents error-file)
                     (string-trim (buffer-string)))))
              (error "curl failed%s"
                     (if (string-empty-p detail)
                         (format " (status %s)" status)
                       (format " (status %s): %s" status detail))))))
      (ignore-errors (delete-file error-file)))))

(defun my-google-tts--mime-type (file)
  "Return the MIME type reported for FILE by `/usr/bin/file'."
  (with-temp-buffer
    (let ((status (call-process my-google-tts-file-program nil t nil
                                "--brief" "--mime-type" "--" file)))
      (unless (and (integerp status) (zerop status))
        (error "file(1) failed with status %s" status))
      (string-trim (buffer-string)))))

;;;###autoload
(defun my-google-tts (text language)
  "Speak TEXT with Google TTS using LANGUAGE.

Interactively, use the active region as TEXT.  If there is no active
region, prompt for TEXT.  In either case, prompt for LANGUAGE, which
must look like `pt' or `pt-PT'."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties
              (region-beginning) (region-end))
           (read-string "Text to speak: "))
         (my-google-tts--read-language)))
  (unless (and (stringp text) (not (string-empty-p (string-trim text))))
    (user-error "Text must not be empty"))
  (unless (my-google-tts--language-p language)
    (user-error "Language must look like `pt' or `pt-PT'"))
  (mapc #'my-google-tts--check-program
        (list my-google-tts-curl-program
              my-google-tts-file-program
              my-google-tts-player-program))
  (let ((audio-file (make-temp-file "google-tts-" nil ".mp3")))
    (unwind-protect
        (progn
          (message "Downloading speech audio...")
          (my-google-tts--download
           (my-google-tts--url text language) audio-file)
          (let ((mime-type (my-google-tts--mime-type audio-file)))
            (unless (string= mime-type "audio/mpeg")
              (error "Downloaded data is not MP3 audio (file reports %s)"
                     mime-type)))
          (message "Playing speech audio...")
          (let ((status (call-process my-google-tts-player-program nil nil nil
                                      "-really-quiet" audio-file)))
            (unless (and (integerp status) (zerop status))
              (error "Audio player failed with status %s" status))))
      (ignore-errors (delete-file audio-file)))))

(provide 'google-tts)
;;; google-tts.el ends here
