;;; private-comments-mode.el --- minor mode for masukomi/private_comments  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2022 masukomi@masukomi.org

;; Authors: Richard Chiang <richard@commandlinesystems.com>
;;                masukomi <masukomi@masukomi.org>
;; Version: 0.1
;; Keywords: tools
;; URL: https://github.com/commercial-emacs/private-comments-mode
;; Package-Requires: ((emacs "26.1") (anaphora "1.0.4"))

;;; Commentary:

;; Display Private Comments as overlays.

;;; Code:

;;;###autoload
(defgroup private-comments nil
  "Minor mode for Private Comments."
  :group 'tools
  :prefix "private-comments:")

(defvar pcm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (define-key map (kbd "C-c C-r") #'pcm:record)
    (define-key map (kbd "C-c C-d") #'pcm:delete)
    (define-key map (kbd "C-c C-j") #'pcm:jump)
    (easy-menu-define private-comments-menu map "Private-Comments Mode menu"
      `("Private-Comments"
        :help "Private-Comments-specific Features"
        ["Record" pcm:record
         :help "Record Private Comment"]
        ["Jump" pcm:jump
         :help "Jump to Private Comment"]
        ["Delete" pcm:delete
         :help "Delete Private Comment"]))
    map)
  "Private comments mode key map.")

(provide 'private-comments-mode)

;;; private-comments-mode.el ends here
