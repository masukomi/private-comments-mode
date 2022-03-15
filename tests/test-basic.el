(require 'edmacro)
(require 'private-comments-mode-test)

(defsubst pcm-test-press (string)
  (execute-kbd-macro (edmacro-parse-keys string)))

(ert-deftest pcm-test-record ()
  (find-file "private-comments-mode.el")
  (cl-letf (((symbol-function 'y-or-n-p)
             (lambda (&rest _args) t)))
    (private-comments-mode)
    (pcm-test-press "C-c C-r"))
  (should t))
