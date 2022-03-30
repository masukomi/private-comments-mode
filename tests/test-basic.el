(require 'edmacro)
(require 'private-comments-mode-test)
(require 'log-edit)

(ert-deftest pcm-equal-signs ()
  (let* ((repo (expand-file-name "tests/pcm-equal-signs"
                                 (vc-git-root default-directory)))
         (default-directory (progn
                              (delete-directory repo t)
                              (mkdir repo)
                              (file-name-as-directory repo)))
         (file-name (expand-file-name "test-repo.c"))
         (buffer (find-file-noselect file-name)))
    (message "directory is %S" default-directory)
    (vc-git-create-repo)
    (with-current-buffer buffer
      (save-excursion (insert "#include <stdio.h>" "\n"))
      (save-buffer))
    (vc-git-register (list file-name))
    (vc-git-checkin (list file-name) "Initial commit")
    ;; Basic record
    (with-current-buffer buffer
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _args) t)))
        (private-comments-mode))
      (call-interactively #'private-comments-record))
    (with-current-buffer "PCM Edit"
      (insert "=====")
      (call-interactively #'private-comments-edit-done))
    (with-current-buffer buffer
      (goto-char (point-min))
      (pcm-test-wait-for
       (lambda ()
         (when-let ((ov (car (overlays-at (point)))))
           (equal "=====\n"
                  (substring-no-properties
                   (overlay-get ov 'before-string)))))
       nil 1500))
    (let (kill-buffer-query-functions)
      (kill-buffer buffer)
      (kill-buffer private-comments-server-buffer-name))))
