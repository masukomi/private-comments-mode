;;; env.el --- env.el -*- lexical-binding: t; coding: utf-8 -*-
(custom-set-variables
 )

(add-to-list 'load-path (expand-file-name "lisp" (vc-git-root default-directory)))
(add-to-list 'load-path (expand-file-name "test" (vc-git-root default-directory)))

(require 'private-comments-mode-test)

(defvar test-directories nil)

(defun cleanup ()
  (dolist (dir test-directories)
    (delete-directory dir t)))

(Setup
  )

(Before
 )

(After
 )

(Teardown
 (cleanup)
 )

(Fail
 (if noninteractive
     (with-demoted-errors "demote: %s"
       (Teardown))
   (backtrace)
   (keyboard-quit))) ;; useful to prevent emacs from quitting
