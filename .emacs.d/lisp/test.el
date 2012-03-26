(defvar mail-bugger-unread-entries nil)

(defconst mail-modeline-logo-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
            "/* XPM */
static char * gmail_xpm[] = {
\"16 16 8 1\",
\" 	c None\",
\".	c #DA3838\",
\"+	c #E95A5A\",
\"@	c #F28181\",
\"#	c #F9A7A7\",
\"$	c #FFB6B6\",
\"%	c #FFE2E2\",
\"&	c #FFFFFF\",
\"                \",
\"                \",
\"                \",
\"...@########@...\",
\"....$&&&&&&$....\",
\"..@+.$&&&&$.+@..\",
\"..&@+.$&&$.+@&..\",
\"..&&@+.$$.+@&&..\",
\"..&&$@+..+@$&&..\",
\"..&%#$@++@$#%&..\",
\"..%#%&&@@&&%#%..\",
\"..#%&&&&&&&&%#..\",
\"..%&&&&&&&&&&%..\",
\"..############..\",
\"                \",
\"                \"};
"))
  "Image for gmail logo.")

(defconst mail-modeline-logo
  (if mail-modeline-logo-image
      (apply 'propertize " " `(display ,mail-modeline-logo-image))
    "G"))

(defvar mail-modeline-timer nil)

(defcustom mail-modeline-timer-interval 300
  "Interval(in seconds) for checking gmail."
)

;;;###autoload
(defun mail-modeline-start ()
  (interactive)

  (add-to-list 'global-mode-string
               '(:eval (mail-modeline-make-unread-string)) t)

  (setq mail-modeline-timer
        (run-with-timer 0
                        mail-modeline-timer-interval
                        'mail-modeline-check)))


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
  (message "There are %d mails, 1st subject: %s, second : %s" (list-length lines) first-subject second-subject)
  ;; (message "i'm the callback")
  (let* ((header-str (read-output-buffer (current-buffer))))

    (setq mail-modeline-unread-entries lines)
    (mail-modeline-make-unread-string)

    (force-mode-line-update)
    (kill-buffer)))


(defun mail-modeline-make-unread-string ()
  (if (null mail-modeline-unread-entries)
      ""
    (let ((s (format "(%d) " (length lines)))
          (map (make-sparse-keymap))
          (url "https://mail.google.com"))
      (define-key map (vector 'mode-line 'mouse-2)
        `(lambda (e)
           (interactive "e")
           (browse-url ,url)
           (setq mail-modeline-unread-entries nil)))
      (add-text-properties 0 (length s)
                           `(local-map ,map mouse-face mode-line-highlight
                                       uri ,url help-echo
                                       ,(concat
                                         (mail-modeline-make-preview-string)
                                         "\nmouse-2: View mail"))
                           s)
      (concat " " mail-modeline-logo s))))


(defun mail-modeline-make-preview-string ()
  (mapconcat
   (lambda (entry)
     (let ((s (format "%s - %s - %s"
		      (list-length lines) first-subject second-subject
		      )))
       s))
   mail-modeline-unread-entries
   "\n"))


(defun buffer-to-list-of-lists (buf)
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (let ((lines '()))
        (while (not (eobp))
          (push (split-string
                 (buffer-substring (point) (point-at-eol)) "|")
                lines)
          (beginning-of-line 2))
	lines))))

;; copied from twittering-mode
(defun read-output-buffer (buffer)
  "Read and parse BUFFER"

    (setq lines (buffer-to-list-of-lists buffer))
    (setq first-subject (car (car lines)))
    (setq second-subject (car (car (cdr lines))))

    ;; (message "And my string is %s" (buffer-substring (point-min) (point-max)))
    ;; (message "There are %d mails, 1st subject: %s, second : %s" (list-length lines) first-subject second-subject)
)


(defun mail-modeline-check ()
  "Check unread mail now."
  (interactive)
  (mail-modeline-shell-command (format "php /var/www/html/test/index.php") 'callback))

(defun parse-quote-buffer(b)
  "Parse the buffer for quotes"
  (goto-line 1)
  (re-search-forward "^\n")
  (beginning-of-line)
  (let ((res nil))
    (while (> (point-max) (point))
      (setf res (cons  (split-string (thing-at-point 'line) ",") res))
      (forward-line 1))
    (reverse res)))







(defun count-words-in-defun ()
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

;;; Interactive version.
(defun count-words-defun ()
  "Number of words and symbols in a function definition."
  (interactive)
  (message
   "Counting words and symbols in function definition ... ")
  (let ((count (count-words-in-defun)))
    (cond
     ((zerop count)
      (message
       "The definition does NOT have any words or symbols."))
     ((= 1 count)
      (message
       "The definition has 1 word or symbol."))
     (t
      (message
       "The definition has %d words or symbols." count)))))

(let ((words '("fight" "foo" "for" "food!")))
  words)
;; ==> ("bar" "fight" "foo" "for" "food!")
(message "%s loaded" (or load-file-name buffer-file-name))
