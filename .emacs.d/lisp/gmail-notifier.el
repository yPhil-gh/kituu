;;; gmail-notifier.el --- Notify unread gmail on mode line

;; Copyright (C) 2010  William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Keywords: mail
;; Version: 0.2.1
;; Url: http://github.com/xwl/xwl-elisp/blob/master/gmail-notifier.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Show unread gmail count on mode line, it looks like this: G(2).
;; `G' could be the gmail logo if your emacs supports image.
;; To setup:
;;   (require 'gmail-notifier)
;;   (setq gmail-notifier-username "william.xwl"
;;         gmail-notifier-password "******")
;;   (gmail-notifier-start)
;;
;; You may also store account and password in `~/.authinfo.gpg'.

;;; Code:

(setq gnus-user-date "10:00")
(require 'xml)
;; (require 'gnus-util)
(eval-when-compile (require 'cl))
(require 'auth-source)

(defgroup gmail-notifier nil
  "Gmail notifier."
  :prefix "gmail-notifier-"
  :group 'mail)

(defcustom gmail-notifier-username nil
  "Gmail username."
  :type 'string
  :group 'gmail-notifier)

(defcustom gmail-notifier-password nil
  "Gmail password."
  :type 'string
  :group 'gmail-notifier)

(defcustom gmail-notifier-curl-command "curl"
  "curl command line including additional options."
  :type 'string
  :group 'gmail-notifier)

(defcustom gmail-notifier-new-mails-hook nil
  "Hooks to run when new mails arrive."
  :type 'list
  :group 'gmail-notifier)

(defcustom gmail-notifier-timer-interval 300
  "Interval(in seconds) for checking gmail."
  :type 'number
  :group 'gmail-notifier)

(defcustom gmail-notifier-host "imap.gmail.com"
  "Gmail host."
  :type 'string
  :group 'gmail-notifier)

(defcustom gmail-notifier-protocol "587"
  "Protocol name or number, as string."
  :type 'string
  :group 'gmail-notifier)

;; Entry format:
;; '(((author "AUTHOR") (title "TITLE") (summary "SUMMARY") (link "LINK") (date "DATE"))...)
(defvar gmail-notifier-unread-entries nil)

(defconst gmail-notifier-logo-image
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

(defconst gmail-notifier-logo
  (if gmail-notifier-logo-image
      (apply 'propertize " " `(display ,gmail-notifier-logo-image))
    "G"))

(defvar gmail-notifier-timer nil)

;;;###autoload
(defun gmail-notifier-start ()
  (interactive)
  (unless gmail-notifier-username
    (setq gmail-notifier-username
          (or (auth-source-user-or-password
               "login"  gmail-notifier-host gmail-notifier-protocol)
              (read-string "Gmail username: "))))

  (unless gmail-notifier-password
    (setq gmail-notifier-password
          (or (auth-source-user-or-password
               "password" gmail-notifier-host gmail-notifier-protocol)
              (read-passwd "Gmail password: "))))

  (add-to-list 'global-mode-string
               '(:eval (gmail-notifier-make-unread-string)) t)

  (setq gmail-notifier-timer
        (run-with-timer 0
                        gmail-notifier-timer-interval
                        'gmail-notifier-check)))

(defun gmail-notifier-stop ()
  (interactive)
  (cancel-timer gmail-notifier-timer)
  (setq gmail-notifier-timer nil)
  (setq global-mode-string
	(remove '(:eval (gmail-notifier-make-unread-string))
		global-mode-string)))

(defun gmail-notifier-check ()
  "Check unread gmail now."
  (interactive)
  (gmail-notifier-shell-command-asynchronously-with-callback
   (format "%s --include -s --user \"%s:%s\" https://mail.google.com/mail/feed/atom"
           gmail-notifier-curl-command
           gmail-notifier-username
           gmail-notifier-password)
   'gmail-notifier-callback))

(defun gmail-notifier-callback ()
  (let* ((header-str (gmail-notifier-get-response-header (current-buffer)))
         (header-info (and header-str
                           (gmail-notifier-make-header-info-alist header-str)))
         (status-line (cdr (assq 'status-line header-info)))
         (status-code (cdr (assq 'status-code header-info))))
    (unless (string= status-code "200")
      (error "(gmail-notifier): %s" status-line))
    (setq gmail-notifier-unread-entries
          (mapcar
           (lambda (entry)
             `((author ,(or (caddr (assoc 'name (assoc 'author entry))) ""))
               (title ,(or (caddr (assoc 'title entry)) ""))
               (summary ,(or (caddr (assoc 'summary entry)) ""))
               (link ,(cdr (assoc 'href (cadr (assoc 'link entry)))))
               (date ,(caddr (assoc 'issued entry)))))
           (remove-if-not
            (lambda (tag)
              (and (consp tag) (eq (car tag) 'entry)))
            (car (xml-parse-region (point-min) (point-max))))))
    (unless (null gmail-notifier-unread-entries)
      (run-hooks 'gmail-notifier-new-mails-hook))
    (gmail-notifier-make-unread-string)
    (force-mode-line-update)
    (kill-buffer)))

(defun gmail-notifier-make-unread-string ()
  (if (null gmail-notifier-unread-entries)
      ""
    (let ((s (format "(%d) " (length gmail-notifier-unread-entries)))
          (map (make-sparse-keymap))
          (url "https://mail.google.com"))
      (define-key map (vector 'mode-line 'mouse-2)
        `(lambda (e)
           (interactive "e")
           (browse-url ,url)
           (setq gmail-notifier-unread-entries nil)))
      (add-text-properties 0 (length s)
                           `(local-map ,map mouse-face mode-line-highlight
                                       uri ,url help-echo
                                       ,(concat
                                         (gmail-notifier-make-preview-string)
                                         "\nmouse-2: View mail"))
                           s)
      (concat " " gmail-notifier-logo s))))

(defmacro gmail-notifier-shell-command-asynchronously-with-callback (cmd
                                                                     callback)
  "Run CMD asynchronously and apply CALLBACK in the output buffer.
Note: you are suggested to kill process buffer at the end of CALLBACK. "
  `(let* ((buf (generate-new-buffer (concat "*" (replace-regexp-in-string " .*" "" ,cmd) "*")))
          (p (start-process-shell-command ,cmd buf ,cmd)))
     (set-process-sentinel
      p
      (lambda (process event)
        (with-current-buffer (process-buffer process)
          (when (eq (process-status  process) 'exit)
            (let ((inhibit-read-only t)
                  (err (process-exit-status process)))
              (if (zerop err)
                  (funcall ,callback)
                (error "(gmail-notifier) curl failed: %d" err)))))))))

(defun gmail-notifier-make-preview-string ()
  (mapconcat
   (lambda (entry)
     (let ((s (format "%s - %s - %s"
                      (cadr (assoc 'author entry))
                      (let ((title (cadr (assoc 'title entry))))
                        (substring title 0 (min (length title) 20)))
                      (let ((summary (cadr (assoc 'summary entry))))
                        (substring summary 0 (min (length summary) 20)))
		      )))
       ;; (add-text-properties 0 (length s)
       ;;                      `(mouse-face mode-line-highlight
       ;;                                   uri ,(cadr (assoc 'link entry)))
       ;;                      s)
       s))
   gmail-notifier-unread-entries
   "\n"))

;; copied from twittering-mode
(defun gmail-notifier-get-response-header (buffer)
  "Extract HTTP response header from HTTP response.
BUFFER may be a buffer or the name of an existing buffer which contains the HTTP response."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp "\r?\n\r?\n" nil t)
	  (buffer-substring (point-min) (match-end 0))
	nil))))

;; copied from twittering-mode
(defun gmail-notifier-make-header-info-alist (header-str)
  "Make HTTP header alist from HEADER-STR.
The alist consists of pairs of field-name and field-value, such as
'((\"Content-Type\" . \"application/xml\; charset=utf-8\")
  (\"Content-Length\" . \"2075\"))."
  (let* ((lines (split-string header-str "\r?\n"))
         (status-line (car lines))
         (header-lines (cdr lines)))
    (when (string-match
	   "^\\(HTTP/1\.[01]\\) \\([0-9][0-9][0-9]\\) \\(.*\\)$"
	   status-line)
      (append `((status-line . ,status-line)
		(http-version . ,(match-string 1 status-line))
		(status-code . ,(match-string 2 status-line))
		(reason-phrase . ,(match-string 3 status-line)))
	      (remove nil
		      (mapcar
		       (lambda (line)
			 (when (string-match "^\\([^: ]*\\): *\\(.*\\)$" line)
			   (cons (match-string 1 line) (match-string 2 line))))
		       header-lines))))))


(provide 'gmail-notifier)
;;; gmail-notifier.el ends here
