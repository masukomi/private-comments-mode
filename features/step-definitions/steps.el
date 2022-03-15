(When "I wait for messages to hack \\(.*\\)$"
  (lambda (message)
    (pcm-test-wait-for
     (lambda ()
       (let* ((raw (symbol-value (intern message)))
              (upto-command-key
               (replace-regexp-in-string
                (regexp-quote "\\\"") "\""
                (substring raw 0 (cl-search "\\[" raw)))))
         ;; `substitute-command-keys' seems to depend on window sizing
         ;; which is compromised by the fmakunbound of `split-window-sensibly'
         (cl-some (lambda (x) (string-match-p (format "^%s" upto-command-key) x))
                  (mapcar #'string-trim ecukes-message-log))))
     nil 2000 300)))

(When "I wait for messages to say \\(.*\\)$"
  (lambda (message)
    (pcm-test-wait-for
     (lambda ()
       (let ((message (replace-regexp-in-string
                       (regexp-quote "\\\"") "\""
                       (substitute-command-keys
                        (symbol-value (intern message))))))
         (member message (mapcar #'string-trim ecukes-message-log))))
     nil 2000 300)))

(When "I wait for messages to say \"\\(.*\\)\"$"
  (lambda (message)
    (pcm-test-wait-for
     (lambda ()
       (let* ((message (replace-regex-pin-string
                        (regexp-quote "\\\"") "\""
                        (substitute-command-keys message))))
         (member message (mapcar #'string-trim ecukes-message-log))))
     nil 2000 300)))

(When "^eval \"\\(.*\\)\"$"
  (lambda (command)
    (eval (car (read-from-string command)))))

(When "^I dump current buffer$"
  (lambda ()
    (message "%s" (buffer-string))))
