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
(require 'dbus)

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

(defcustom mail-bugger-icon
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :file "~/.emacs.d/img/perso.xpm"
	    :ascent center))
  "Icon for the first account."
  :group 'mail-bugger)

(defconst mail-bugger-logo
  (if mail-bugger-icon
      (apply 'propertize " " `(display ,mail-bugger-icon))
    "G"))

(defvar mail-bugger-timer nil)

;;;###autoload
(defun mail-bugger-desktop-notification (summary body timeout sound icon)
  "call notification-daemon method METHOD with ARGS over dbus"
  (dbus-call-method
    :session                        ; use the session (not system) bus
    "org.freedesktop.Notifications" ; service name
    "/org/freedesktop/Notifications"   ; path name
    "org.freedesktop.Notifications" "Notify" ; Method
    "GNU Emacs"
    0
    ;; "/usr/share/icons/gnuitar.png"
    icon
    summary
    body
    '(:array)
    '(:array :signature "{sv}")
    ':int32 timeout)

  (when sound (shell-command
                (concat "mplayer -really-quiet " sound " 2> /dev/null")))
)

;; (px-send-desktop-notification "Test" "Plip" 2000 "/usr/share/sounds/KDE-Im-New-Mail.ogg" "/usr/share/icons/Revenge/128x128/apps/emacs.png")

(defun mail-bugger-start ()
"Init"
  (interactive)
  (add-to-list 'global-mode-string
               '(:eval (mail-bugger-mode-line)) t)
  (setq mail-bugger-timer
        (run-with-timer 0
                        mail-bugger-timer-interval
                        'mail-bugger-check)))

(defmacro mail-bugger-shell-command (cmd callback)
  "Run CMD asynchronously in a buffer"
  `(let* ((buf (generate-new-buffer "*mail-bugger*"))
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
  "Now we're talking"
  (let* ((header-str (read-output-buffer (current-buffer))))
    (setq mail-bugger-unread-entries lines)
    (unless (null mail-bugger-unread-entries)
      (run-hooks 'mail-bugger-new-mails-hook))
    (mail-bugger-mode-line)

    (store-last-5-read-mails)

    (force-mode-line-update)
    (kill-buffer)))

(defun mail-bugger-mode-line ()
  (if (null mail-bugger-unread-entries)
      ""
    (let ((s (format "%d " (length lines)))
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
                                         (mail-bugger-tooltip)
                                         "\n\nmouse-2: View mail"))
                           s)
      (concat " " mail-bugger-logo ":" s))))

(setq mail-bugger-advertised-mails '())

(defun store-last-5-read-mails ()
  (mapcar
   (lambda (x)
     (let
	 ((s
	   (progn
	     (if
		 (not (member x mail-bugger-advertised-mails))
		 (progn (message "Message number %s advertised!" (car (nthcdr 3 x)))
			(add-to-list 'mail-bugger-advertised-mails x))))
	   ))
       s)
     )
   mail-bugger-unread-entries))

;; (defun store-last-5-read-mails ()
;;   (mapcar
;;    (lambda (x)
;;      (let
;; 	 ((s
;; 	   (progn
;; 	     (add-to-list 'mail-bugger-advertised-mails x)
;; 	     (if
;; 		 (member x 'mail-bugger-advertised-mails)
;; 		 (progn (message "%s advertised!" (car (nthcdr 3 x)))
;; 			(delete 'x mail-bugger-advertised-mails))))
;; 	   ))
;;        s)
;;      )
;;    mail-bugger-unread-entries))

  ;; (add-to-list 'mail-bugger-advertised-mails (car (nthcdr 3 x)))

;; (setq mylist '("plip" "plop"))

;; (if (member "plip" mylist)
;;     (message "yeap!")
;;   (message "nope!"))

;; (if (not (member 5 '(1 2 3)))
;; (message "nope!"))

(defun mail-bugger-tooltip ()
  "loop through the mail headers and build the hover tooltip"
  (mapconcat
   (lambda (x)
     (let
	 ((s
	   (format "%s\n%s -%s- \n--------------\n%s\n"
		   (car (nthcdr 1 x))
		   (nthcdr 2 x)
		   (car (nthcdr 3 x))
		   (car x)
		   )))
       s)
     )
   mail-bugger-unread-entries
   "\n")
  )

(defun mail-bugger-buffer-to-list (buf)
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
  (setq lines (mail-bugger-buffer-to-list buffer))
  ;; (setq mail-bugger-unadvertised-mails (mail-bugger-buffer-to-list buffer))
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


;; (car (nthcdr 1 lines))
;; (nthcdr 2 lines)
;; (car (nthcdr 3 lines))
;; (car lines)


;; (setq list3 '())

;; (add-to-list 'list3 (+ 1 1))
