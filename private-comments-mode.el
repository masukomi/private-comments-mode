;;; private-comments-mode.el --- minor mode for masukomi/private_comments  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2022 The Authors

;; Authors: Richard Chiang <richard@commandlinesystems.com>
;;                masukomi <masukomi@masukomi.org>
;; Version: 0.1
;; Keywords: tools
;; URL: https://github.com/commercial-emacs/private-comments-mode
;; Package-Requires: ((emacs "26.1") (anaphora "1.0.4"))

;;; Commentary:

;; Display Private Comments as overlays.

;;; Code:

(require 'url-http)
(require 'vc-git)

(defvar url-http-end-of-headers)

;;;###autoload
(defgroup private-comments nil
  "Minor mode for Private Comments."
  :group 'tools
  :prefix "private-comments-")

(defvar private-comments-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (define-key map (kbd "C-c C-r") #'private-comments-record)
    (define-key map (kbd "C-c C-d") #'private-comments-delete)
    (easy-menu-define private-comments-menu map "Private Comments Mode Menu"
      `("Private-Comments"
        :help "Private Comments"
        ["Record" private-comments-record
         :help "Record Comment"]
        ["Delete" private-comments-delete
         :help "Delete Comment"]))
    map)
  "Private comments mode key map.")

(defcustom private-comments-face 'highlight
  "Face for annotations."
  :type 'face
  :group 'private-comments)

(defcustom private-comments-executable-args ""
  "Command line arguments to Private Comments server."
  :group 'private-comments
  :type 'string)

(defcustom private-comments-executable "private_comments"
  "Private Comments server executable."
  :group 'private-comments
  :type 'string
  :set (lambda (symbol value)
         (set-default symbol value)
         (unless (executable-find value)
           (display-warning 'private-comments
                            (format "'%s' not found in PATH" value)))))

(defcustom private-comments-localhost "0.0.0.0"
  "Some users keep their browser in a separate domain.
Do not set this to \"localhost\" as a numeric IP is required
for the oauth handshake."
  :group 'private-comments
  :type 'string)

(defcustom private-comments-url nil
  "Url of so-called mini API server.
Can be specified as host:port, e.g, 0.0.0.0:5749, or just the
numerical port, e.g., 5749, which assumes
`private-comments-localhost', or nil which assumes port 5749."
  :group 'private-comments
  :type 'string
  :set-after '(private-comments-localhost)
  :set (lambda (symbol value)
         (set-default symbol value)
         (cond ((null value)
                (set-default symbol (format "http://%s:%d"
                                            private-comments-localhost
                                            5749)))
               ((or (integerp value)
                    (and (stringp value)
                         (equal value (number-to-string (string-to-number value)))))
                (set-default symbol (format "http://%s:%s"
                                            private-comments-localhost value)))
               (t (set-default symbol value)))
         (let* ((attempt (symbol-value symbol))
                (reattempt (format "http://%s" attempt)))
           (unless (url-host (url-generic-parse-url attempt))
             (when (url-host (url-generic-parse-url reattempt))
               (set-default symbol reattempt))))))

(defun private-comments-ensure-server ()
  (interactive)
  (cl-flet ((ping (repeats)
              (cl-loop
               with parsed-url = (url-generic-parse-url private-comments-url)
               repeat repeats
               when (condition-case nil
                        (prog1 t
                          (delete-process
                           (make-network-process :name "test-port"
                                                 :noquery t
                                                 :host (url-host parsed-url)
                                                 :service (url-port parsed-url)
                                                 :buffer nil
                                                 :stop t)))
                      (file-error nil))
               return t ;; skip finally
               do (accept-process-output nil 0.3))))
    (unless (ping 1)
      (private-comments--run-server))
    (unless (ping 5)
      (error "private-comments-apply: could not start server"))))

(defconst private-comments-server-process-name "PCM Server")
(defconst private-comments-server-buffer-name
  (format "*%s*" private-comments-server-process-name))

(defun private-comments-kill-server ()
  (interactive)
  (when-let ((buffer (get-buffer private-comments-server-buffer-name))
             (proc (get-buffer-process buffer)))
    (delete-process proc)))

(defun private-comments--run-server ()
  (let* ((buf (with-current-buffer
                  (get-buffer-create private-comments-server-buffer-name)
                (setq buffer-read-only t)
                (current-buffer)))
         (proc (apply #'start-process
                      private-comments-server-process-name
                      buf
                      private-comments-executable
                      (split-string private-comments-executable-args))))
    (set-process-query-on-exit-flag proc nil)
    (when (process-live-p proc)
      (message "private-comments-mode: '%s' started in %s"
               (file-name-nondirectory private-comments-executable)
               (process-buffer proc)))
    proc))

(defmacro private-comments--request (url callback)
  (declare (indent defun))
  `(url-retrieve ,url ,callback nil t))

(defun private-comments-mod-callback (ov is-after-change &rest _)
  (when is-after-change
    (unless (equal (overlay-get ov 'pcm-line-string)
                   (save-excursion
                     (goto-char (overlay-start ov))
                     (thing-at-point 'line t))))))

(defun private-comments-record-callback (buffer &rest _args)
  "Current buffer is url-http's retrieval (starts with ' *http').
BUFFER is the edit buffer from which url-retrieve was issued."
  (unwind-protect
      (progn
        (goto-char (1+ url-http-end-of-headers))
        (let ((comments (plist-get
                         (json-parse-buffer :object-type 'plist
                                            :array-type 'list
                                            :null-object json-null
                                            :false-object json-false)
                         :comments)))
          (ignore comments)
          ;; rebuild the world for now
          (with-current-buffer buffer
            (private-comments-apply))))
    (kill-buffer)))

(defun private-comments-apply-callback (buffer blame-data &rest _args)
  "Current buffer is url-http's retrieval (starts with ' *http').
BUFFER is the edit buffer from which url-retrieve was issued."
  (unwind-protect
      (progn
        (goto-char (1+ url-http-end-of-headers))
        (let ((comments (plist-get
                         (json-parse-buffer :object-type 'plist
                                            :array-type 'list
                                            :null-object json-null
                                            :false-object json-false)
                         :comments)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (save-excursion
                (save-restriction
                  (widen)
                  (dolist (ov (cl-remove-if-not
                               (lambda (ov)
                                 (overlay-get ov 'pcm-commit))
                               (overlays-in (point-min) (point-max))))
                    (delete-overlay ov))
                  (dolist (comment comments)
                    (goto-char (point-min))
                    (forward-line (1- (plist-get comment :line_number)))
                    ;; BLAME should be nil if Not Committed Yet
                    (when-let ((blame (aref blame-data
                                            (plist-get comment :line_number)))
                               (indent (current-indentation))
                               (rendered (with-temp-buffer
                                           (insert (make-string indent ? ))
                                           (save-excursion
                                             (insert (string-trim
                                                      (plist-get comment :comment))
                                                     "\n"))
                                           (fill-paragraph)
                                           (propertize (buffer-string)
                                                       'face private-comments-face)))
                               (ov (make-overlay (point) (point-at-eol) nil t nil)))
                      ;; overlay marker-following avoids having to
                      ;; blame again
                      (overlay-put ov 'pcm-line-string
                                   (plist-get blame :line-string))
                      (overlay-put ov 'pcm-commit
                                   (plist-get blame :commit))
                      (overlay-put ov 'before-string rendered)
                      (overlay-put ov 'modification-hooks
                                   (list 'private-comments-mod-callback))))))))))
    (kill-buffer)))

(defun private-comments-relative-name (base-name)
  (with-temp-buffer
    (vc-git-command t 0 base-name "ls-files" "-z" "--full-name" "--")
    (buffer-substring-no-properties
     (point-min) (max (point-min) (1- (point-max))))))

(defvar private-comments-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-k" 'private-comments-edit-abort)
    (define-key map "\C-c\C-c" 'private-comments-edit-done)
    map))

(define-minor-mode private-comments-edit-mode
  "Poor man's org-src-mode minor mode.

\\{private-comments-edit-mode-map}
"
  :lighter " PCM Edit"
  (setq-local header-line-format
              (substitute-command-keys
               "Edit, then exit with \\[private-comments-edit-done] \
or abort with \\[private-comments-edit-abort]")))

(defun private-comments-edit-abort ()
  (interactive)
  (kill-buffer))

(defun private-comments-edit-done ()
  (interactive)
  (when (bound-and-true-p private-comments--edit-callback-1)
    (funcall private-comments--edit-callback-1 (buffer-string)))
  (kill-buffer))

(defun private-comments--edit-callback-4 (buffer* line-number* commit* comment)
  (private-comments-ensure-server)
  (if (not (buffer-live-p buffer*))
      (error "private-comments--edit-callback-4: Buffer '%s' dead."
             (buffer-name buffer*))
    (with-current-buffer buffer*
      (let* ((default-directory (directory-file-name
                                 (file-name-directory (buffer-file-name))))
             (base-name (file-name-nondirectory (buffer-file-name)))
             (relative-name (private-comments-relative-name base-name))
             (url-request-method 'POST)
             (url-request-data
              (json-encode-alist
               `((project_name_hash . ,(secure-hash
                                        'sha256
                                        (file-name-nondirectory
                                         (directory-file-name
                                          (vc-git-root default-directory)))))
                 (file_path_hash . ,(secure-hash 'sha256 relative-name))
                 (treeish . ,commit*)
                 (line_number . ,line-number*)
                 (comment . ,comment))))
             (query (format "%s/v1/comments"
                            (directory-file-name private-comments-url))))
        (url-retrieve
         query
         (apply-partially
          #'private-comments-record-callback
          buffer*)
         nil t)))))

(defun private-comments-edit (callback)
  "Like `org-edit-special'."
  (interactive)
  (let ((restore-window-config (current-window-configuration))
        (buffer (switch-to-buffer-other-window (generate-new-buffer "PCM Edit"))))
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook
                (apply-partially #'set-window-configuration restore-window-config)
                nil t)
      (private-comments-edit-mode)
      (setq-local private-comments--edit-callback-1 callback))))

(defun private-comments-blame-data (base-name)
  (with-temp-buffer
    (vc-git-command t 0 base-name "blame")
    (goto-char (point-max))
    (while (progn (beginning-of-line)
                  (and (not (bobp))
                       (not (looking-at "\\S-"))))
      (forward-line -1))
    (cl-loop with data
             with ws = "[ \\f\\t\\n\\r\\v]"
             until (not (looking-at "\\S-"))
             ;; so-called boundary commit marker
             when (looking-at (regexp-quote "^"))
             do (forward-char)
             end
             for commit = (string-trim
                           (buffer-substring-no-properties
                            (point)
                            (re-search-forward ws)))
             for line-number = (progn
                                 (re-search-forward
                                  (concat "\\([0-9]+\\))" ws))
                                 (string-to-number (match-string 1)))
             for line-string = (buffer-substring-no-properties
                                (point)
                                (line-end-position))
             unless data
             do (setq data (make-vector (1+ line-number) nil))
             end
             do (aset data line-number
                      (list :line-string line-string
                            :commit commit))
             do (beginning-of-line)
             until (bobp)
             do (forward-line -1)
             finally return data)))

(defun private-comments-delete ())

(defun private-comments-record ()
  (interactive)
  (let* ((default-directory (directory-file-name
                             (file-name-directory (buffer-file-name))))
         (base-name (file-name-nondirectory (buffer-file-name)))
         (blame-data (private-comments-blame-data base-name))
         ;; line-number and blame-data are one-indexed
         (line-number (line-number-at-pos))
         (blame (when (<= line-number (length blame-data))
                  (aref blame-data line-number)))
         (commit (plist-get blame :commit)))
    (if commit
        (private-comments-edit
         (apply-partially #'private-comments--edit-callback-4
                          (current-buffer)
                          line-number
                          commit))
      (error "Line %s is uncommitted." line-number))))

(defun private-comments-apply ()
  (interactive)
  (private-comments-ensure-server)
  (let* ((default-directory (directory-file-name
                             (file-name-directory (buffer-file-name))))
         (base-name (file-name-nondirectory (buffer-file-name)))
         (relative-name (private-comments-relative-name base-name))
         (blame-data (private-comments-blame-data base-name))
         (blame-commits
          (cl-delete-duplicates
           (cl-remove-if
            (lambda (x) (or (null x) (string-match-p "^0+$" x)))
            (seq-map (lambda (x) (plist-get x :commit)) blame-data))
           :test #'equal))
         (query (format "%s/v1/comments?%s"
                        (directory-file-name private-comments-url)
                        (url-build-query-string
                         `((project_name_hash ,(secure-hash
                                                'sha256
                                                (file-name-nondirectory
                                                 (directory-file-name
                                                  (vc-git-root default-directory)))))
                           (file_path_hash ,(secure-hash 'sha256 relative-name))
                           (treeishes ,(mapconcat #'identity blame-commits ",")))))))
    (url-retrieve
     query
     (apply-partially
      #'private-comments-apply-callback
      (current-buffer) blame-data)
     nil t)))

(provide 'private-comments-mode)

;;; private-comments-mode.el ends here
