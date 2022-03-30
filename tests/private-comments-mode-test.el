;;; private-comments-mode-test.el --- Test utilities for private-comments-mode  -*- lexical-binding: t; coding: utf-8 -*-

(require 'private-comments-mode)

(custom-set-variables
 '(auto-revert-verbose nil)
 '(auto-revert-stop-on-user-input nil)
 '(network-security-level (quote low))
 '(private-comments-url "2702"))

(setenv "PRIVATE_COMMENTS_DIR"
        (concat
         (file-name-as-directory
          (expand-file-name "tests" (vc-git-root default-directory)))
         "config/private_comments"))

(setenv "PRIVATE_COMMENTS_PORT"
        (number-to-string (url-port (url-generic-parse-url private-comments-url))))

(when (getenv "CI")
  (with-temp-buffer
    (apply #'call-process "git" nil t
           (split-string "config --global user.email foo@example.com"))
    (apply #'call-process "git" nil t
           (split-string "config --global user.name his_fooness"))
    (apply #'call-process "git" nil t
           (split-string "config --list"))
    (message "%s" (buffer-string))))

(defun pcm-test-wait-for (predicate &optional predargs ms interval continue)
  "Wait until PREDICATE function returns non-`nil'.
  PREDARGS is argument list for the PREDICATE function.
  MS is milliseconds to wait.  INTERVAL is polling interval in milliseconds."
  (let* ((int (if interval interval (if ms (max 300 (/ ms 10)) 300)))
         (count (max 1 (if ms (truncate (/ ms int)) 25))))
    (unless (or (cl-loop repeat count
                         when (apply predicate predargs)
                         return t
                         do (sleep-for 0 int))
                continue)
      (error "Timeout: %s" predicate))))

(provide 'private-comments-mode-test)
