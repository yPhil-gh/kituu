
(defmacro mail-modeline-shell-command (cmd callback)
  "Run CMD asynchronously in a buffer"
  `(let* ((buf (generate-new-buffer "mail-modeline"))
          (p (start-process-shell-command ,cmd buf ,cmd)))
     (set-process-sentinel
      p
      (lambda (process event)
        (with-current-buffer (process-buffer process)

          (when (eq (process-status  process) 'exit)
            (let ((inhibit-read-only t)
                  (err (process-exit-status process)))
              (if (zerop err)
		  (funcall, callback)
                  ;; (message "God exists")
                (error "(mail-modeline) error: %d" err)))))))))

(defun callback ()
  (message "i'm the callback")
  (let* ((header-str (read-output-buffer (current-buffer))))

    ;; (unless (null mail-modeline-unread-entries)
    ;;   (run-hooks 'mail-modeline-new-mails-hook))
    ;; (mail-modeline-make-unread-string)
    (force-mode-line-update)
    (kill-buffer)))


;; copied from twittering-mode
(defun read-output-buffer (buffer)
  "Read and parse BUFFER"
  (with-current-buffer buffer
    (message "I'm %s" buffer)
    ;; (let ((mystring
    ;; 	   '(buffer-substring (point-min) (point-max))))
    ;;   mystring)
    (message "And my string is %s" (buffer-substring (point-min) (point-max)))))

(defun mail-modeline-check ()
  "Check unread mail now."
  (interactive)
  (mail-modeline-shell-command (format "php /var/www/html/test/index.php") 'callback))


(let ((words '("fight" "foo" "for" "food!")))
  words)
;; ==> ("bar" "fight" "foo" "for" "food!")
