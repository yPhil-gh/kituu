
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

(defun count-words-in-list ()
  "Return the number of words and symbols in a defun."
  (beginning-of-defun)
  (let ((count 0)
	(end (save-excursion (end-of-defun) (point))))
    (while
	(and (< (point) end)
	     (re-search-forward
	      "\\(\\w\\|\\s_\\)+[^ \t\n]*[ \t\n]*"
	      end t))
      (setq count (1+ count)))
    count))

;; copied from twittering-mode
(defun read-output-buffer (buffer)
  "Read and parse BUFFER"
  (with-current-buffer buffer
    (message "I'm %s" buffer)

     (goto-char (point-min))
     (while (re-search-forward "^\"" nil t)
       (setq lengths-list
             (cons (count-words-in-defun) lengths-list)))

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
