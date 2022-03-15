;;; private-comments-mode-test.el --- Test utilities for private-comments-mode  -*- lexical-binding: t; coding: utf-8 -*-

(custom-set-variables
 '(auto-revert-verbose nil)
 '(auto-revert-stop-on-user-input nil)
 '(network-security-level (quote low)))

(require 'private-comments-mode)

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
