(require 'edmacro)
(require 'private-comments-mode-test)
(require 'log-edit)

(ert-deftest pcm-test-record ()
  (skip-unless (executable-find private-comments-executable))
  (skip-unless (executable-find "git"))
  (let* ((repo (expand-file-name "tests/pcm-test-record"
                                 (vc-git-root default-directory)))
         (default-directory (progn
                              (delete-directory repo t)
                              (mkdir repo)
                              (file-name-as-directory repo)))
         (file-name (expand-file-name "test-repo.c"))
         (buffer (find-file-noselect file-name))
         (source (list "#include <stdio.h>" "\n"
                       "    /* indented four spaces */" "\n")))
    (vc-git-create-repo)
    (with-current-buffer buffer
      (save-excursion (apply #'insert source))
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
      (insert "line 1" "\n" "line2")
      (call-interactively #'private-comments-edit-done))
    (with-current-buffer buffer
      (goto-char (point-min))
      (pcm-test-wait-for
       (lambda ()
         (when-let ((ov (car (overlays-at (point)))))
           (equal "line 1\nline2\n"
                  (substring-no-properties
                   (overlay-get ov 'before-string)))))
       nil 1500))
    ;; Apply uncommitted change
    (with-current-buffer buffer
      (goto-char (point-min))
      (save-excursion (insert "foobar"))
      (save-buffer)
      (should-error (call-interactively #'private-comments-record))
      (erase-buffer)
      ;; Restore committed version
      (save-excursion (apply #'insert source))
      (save-buffer)
      (call-interactively #'private-comments-record))
    ;; Update record
    (with-current-buffer "PCM Edit"
      (erase-buffer)
      (insert "line 3" "\n" "line4")
      (call-interactively #'private-comments-edit-done))
    (with-current-buffer buffer
      (pcm-test-wait-for
       (lambda ()
         (when-let ((ov (car (overlays-at (point)))))
           (equal "line 3\nline4\n"
                  (substring-no-properties
                   (overlay-get ov 'before-string)))))
       nil 1500))
    ;; Delete record
    (with-current-buffer buffer
      (goto-char (point-min))
      (call-interactively #'private-comments-delete)
      (pcm-test-wait-for
       (lambda ()
         (not (overlays-at (point))))))
    ;; Basic record with indent
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line 1)
      (call-interactively #'private-comments-record))
    (with-current-buffer "PCM Edit"
      (insert "line 1" "\n" "line2")
      (call-interactively #'private-comments-edit-done))
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line 1)
      (pcm-test-wait-for
       (lambda ()
         (when-let ((ov (car (overlays-at (point)))))
           (equal "    line 1\n    line2\n"
                  (substring-no-properties
                   (overlay-get ov 'before-string)))))
       nil 1500)
      (call-interactively #'private-comments-record))
    (with-current-buffer "PCM Edit"
      (pcm-test-wait-for
       (lambda ()
         (equal "line 1\nline2" (buffer-string)))
       nil 1500)
      (call-interactively #'private-comments-edit-abort))
    (let (kill-buffer-query-functions)
      (kill-buffer buffer)
      (kill-buffer private-comments-server-buffer-name))))

(ert-deftest pcm-equal-signs ()
  (skip-unless (executable-find private-comments-executable))
  (skip-unless (executable-find "git"))
  (let* ((repo (expand-file-name "tests/pcm-equal-signs"
                                 (vc-git-root default-directory)))
         (default-directory (progn
                              (delete-directory repo t)
                              (mkdir repo)
                              (file-name-as-directory repo)))
         (file-name (expand-file-name "test-repo.c"))
         (buffer (find-file-noselect file-name)))
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
