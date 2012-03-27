;;; mail-bugger.el --- Notify of unread mail on mode line &DBus

;; Copyright (C) 2012 Phil CM

;; Author: Phil CM <philippe.coatmeur@gmail.com>
;; Keywords: mail wanderlust gnus mutt pine
;; Version: 0.0.1
;; Url: http://github.com/xaccrocheur/mail-bugger.el

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

;; Show unread mail count on mode line, it looks like this: G(2).
;; `G' could be the gmail logo if your emacs supports image.
;; To setup:
;;   (require 'mail-bugger)
;;   (mail-bugger-start)
;;
;; Store your login and password in `~/.authinfo.gpg'.

;;; Code:

(require 'auth-source)

(defgroup mail-bugger nil
  "Mail notifier."
  :prefix "mail-bugger-"
  :group 'mail)

(defcustom mail-bugger-shell-script-command "php ~/scripts/unread.php"
  "Full command line."
  :type 'string
  :group 'mail-bugger)

(defcustom mail-bugger-host "imap.gmail.com"
  "Mail host.
Example : imap.gmail.com"
  :type 'string
  :group 'mail-bugger)

(defcustom mail-bugger-protocol "993/imap/ssl"
  "Protocol name or number, as string.
Example : 993/imap/ssl"
  :type 'string
  :group 'mail-bugger)

(defcustom mail-bugger-imap-box "INBOX"
  "Name of the imap folder on the server.
Example : INBOX"
  :type 'string
  :group 'mail-bugger)

(defcustom mail-bugger-username nil
  "Mail username.
DONT't put your user name & password in here, put them in ~/authinfo.gpg like this :
machine <host> login <login> port <port> password <password>
"
  :type 'string
  :group 'mail-bugger)

(defcustom mail-bugger-password nil
  "Mail password."
  :type 'string
  :group 'mail-bugger)

(defcustom mail-bugger-new-mails-hook nil
  "Hooks to run when new mails arrive."
  :type 'list
  :group 'mail-bugger)

(defcustom mail-bugger-timer-interval 300
  "Interval(in seconds) for mail check."
  :type 'number
  :group 'mail-bugger)

(defvar mail-bugger-unread-entries nil)

(defconst mail-bugger-icon
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
            "/* XPM */
static char * perso_xpm[] = {
\"16 16 9 1\",
\" 	c None\",
\".	c #070C06\",
\"+	c #3F1826\",
\"@	c #90044E\",
\"#	c #153914\",
\"$	c #C00068\",
\"%	c #3A3225\",
\"&	c #236621\",
\"*	c #2F982E\",
\"      #&&&#     \",
\"   #&******&#   \",
\"  #**********#  \",
\" #************# \",
\" &************& \",
\" **************.\",
\".&+#&******&#...\",
\" #$$@%****%@$$+.\",
\" #@$$$+**%$$$@. \",
\" .%$$$@#*@$$$+. \",
\"  #%@$$+&$$@+.  \",
\"   #&%+.&+%&#   \",
\"    &******&    \",
\"     #****&.    \",
\"      #**#.     \",
\"       ..       \"};
"))
  "My mail icon.")

(defconst mail-bugger-logo
  (if mail-bugger-icon
      (apply 'propertize " " `(display ,mail-bugger-icon))
    "G"))

(defvar mail-bugger-timer nil)

;;;###autoload
(defun mail-bugger-start ()
  (interactive)
  (add-to-list 'global-mode-string
               '(:eval (mail-bugger-make-unread-string)) t)
  (setq mail-bugger-timer
        (run-with-timer 0
                        mail-bugger-timer-interval
                        'mail-bugger-check)))

(defmacro mail-bugger-shell-command (cmd callback)
  "Run CMD asynchronously in a buffer"
  `(let* ((buf (generate-new-buffer "mail-bugger"))
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
                (error "(mail-bugger) error: %d" err)))))))))

(defun mail-bugger-shell-command-callback ()
  (let* ((header-str (read-output-buffer (current-buffer))))
    (setq mail-bugger-unread-entries lines)
    (unless (null mail-bugger-unread-entries)
      (run-hooks 'mail-bugger-new-mails-hook))
    (mail-bugger-make-unread-string)
    (force-mode-line-update)
    (kill-buffer)))

(defun mail-bugger-make-unread-string ()
  (if (null mail-bugger-unread-entries)
      ""
    (let ((s (format ":%d]" (length lines)))
          (map (make-sparse-keymap))
          (url "https://mail.google.com"))
      (define-key map (vector 'mode-line 'mouse-2)
        `(lambda (e)
           (interactive "e")
           (browse-url ,url)
           (setq mail-bugger-unread-entries nil)))
      (add-text-properties 0 (length s)
                           `(local-map ,map mouse-face mode-line-highlight
                                       uri ,url help-echo
                                       ,(concat
                                         (mail-bugger-make-preview-string)
                                         "\n\nmouse-2: View mail"))
                           s)
      (concat " [" mail-bugger-logo s))))



(defun mail-modeline-make-preview-string ()
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
   mail-modeline-unread-entries
   "\n"))


(defun mail-bugger-make-preview-string ()

  (mapconcat
   (lambda (x)
     (let
	 ((s
	    (format "%s\n%s\n--------------\n%s\n"
		    (car (nthcdr 1 x))
		    (nthcdr 2 x)
		    (car x))
	    ))
       s))
   mail-bugger-unread-entries
   "\n"))

(format "%s" (substring "(Mon, 26 Mar 2012 12:55:42 +0000)" 1 26))

  ;; (mapconcat
  ;;  (setq d
  ;; 	 (mapcar
  ;; 	  (lambda (x)
  ;; 	    (format "From: %s\nSubject: %s\nDate: %s\n\n" (car (nthcdr 1 x)) (car x) (nthcdr 2 x)) )
  ;; 	  mylist))
  ;;  mail-bugger-unread-entries
  ;;  "\n\n")



  ;; (mapconcat
  ;;  (lambda (entry)
  ;;    (let ((s (format "%s\n%s\n----------------\n%s"
  ;; 		      mail-bugger-mail-from mail-bugger-mail-date mail-bugger-mail-subject
  ;; 		      )))
  ;;      s))
  ;;  mail-bugger-unread-entries
  ;;  "\n\n")



(defun buffer-to-list-of-lists (buf)
  "make & return a list (of lists) LINES from lines in a buffer BUF"
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

(defun read-output-buffer (buffer)
  "Read and parse BUFFER"
    (setq lines (buffer-to-list-of-lists buffer))
    (setq mail-bugger-number-of-unread-mails (list-length lines))
    (setq mail-bugger-mail-subject (car (car lines)))
    (setq mail-bugger-mail-date (nthcdr 2 (car (cdr lines))))
    (setq mail-bugger-mail-from (car (nthcdr 1 (car (cdr lines)))))
)

(defun mail-bugger-check ()
  "Check unread mail now."
  (interactive)
  (mail-bugger-shell-command
   (format "%s %s %s %s %s %s"
           mail-bugger-shell-script-command
	   mail-bugger-host
	   mail-bugger-protocol
	   mail-bugger-imap-box

	  (auth-source-user-or-password
               "login"  mail-bugger-host mail-bugger-protocol)

	  (auth-source-user-or-password
               "password"  mail-bugger-host mail-bugger-protocol))

   'mail-bugger-shell-command-callback))

(message "%s loaded" (or load-file-name buffer-file-name))

(provide 'mail-bugger)
;;; mail-bugger.el ends here

;; (loop for i on
;; '(("1st" "Adamweb <contact@adamweb.net>" "Mon, 26 Mar 2012 12:55:18 +0000")
;;  ("2nd" "Adamweb <contact@adamweb.net>" "Mon, 26 Mar 2012 12:55:42 +0000")) do (return (car (car (cdr i)))))


;; (setq mylist '(("1st" "Adamweb <contact@adamweb.net>" "Mon, 26 Mar 2012 12:55:18 +0000")
;;  ("2nd" "Adamweb <contact@adamweb.net>" "Mon, 26 Mar 2012 12:55:42 +0000")))

;; (mapcar '((format "%s" car) mylist)
;; (mapcar '(nthcdr 1) mylist)

;; (mapcar 'car (nthcdr 1 mylist))

;; (setq c '(23.0 47.8 52.1 35.6))

;; (setq d (mapcar
;; 	 '(lambda (x)
;; 	    (format "From: %s\nSubject: %s\nDate: %s\n\n" (car (nthcdr 1 x)) (car x) (nthcdr 2 x)) )
;; 	 lines))

;; (setq d (mapcar
;; 	 '(lambda (x)
;; 	    (format "Subject: %s" x))
;; 	 mylist))

;; This function will convert all the angles in the list c to radians and store them in variable d.
