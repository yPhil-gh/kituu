
(defmacro mail-modeline-shell-command (cmd)
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
                  (message "God exists")
                (error "(mail-modeline) error: %d" err)))))))))

(defun mail-modeline-check ()
  "Check unread gmail now."
  (interactive)
  (mail-modeline-shell-command (format "php /var/www/html/test/index.php")))
