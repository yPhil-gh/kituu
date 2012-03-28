;;; mail-bugger.el --- Notify of unread mails on mode line & DBus

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

;; Show unread mails count on mode line (and details in tooltip on
;; mouse over) and a desktop notification for new mails.
;; To enable it, put this in your .emacs :

;; (require 'mail-bugger)
;; (mail-bugger-init)

;; and enter your details at first init

;;; Code:

(require 'auth-source)
(require 'dbus)

(defgroup mail-bugger nil
  "Mail notifier."
  :prefix "mail-bugger-"
  :group 'mail)

;; (defgroup mail-bugger-accounts nil
;;   "Mail notifier."
;;   :prefix "mail-bugger-accounts"
;;   :group 'mail-bugger)

(defvar mail-bugger-shell-script-command "php ~/scripts/unread.php"
  "Full command line. Can't touch that.")

(defcustom mail-bugger-host "imap.gmail.com"
  "Mail host.
Example : imap.gmail.com"
  :type 'string
  :group 'mail-bugger)

(defcustom mail-bugger-protocol "993/imap/ssl"
  "Port number and (optional) protocol path.
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
Put your user name & password in ~/authinfo.gpg like this :
machine <host> login <login> port <port> password <password>"
  :type 'string
  :group 'mail-bugger)

(defcustom mail-bugger-password nil
  "Mail password.
Put your user name & password in ~/authinfo.gpg like this :
machine <host> login <login> port <port> password <password>"
  :type 'string
  :group 'mail-bugger)

(defcustom mail-bugger-new-mails-hook nil
  "Hooks to run on new mail arrival."
  :type 'list
  :group 'mail-bugger)

(defcustom mail-bugger-timer-interval 300
  "Interval(in seconds) for mail check."
  :type 'number
  :group 'mail-bugger)

(defvar mail-bugger-unseen-mails nil)

(defcustom mail-bugger-new-mail-sound "/usr/share/sounds/KDE-Im-New-Mail.ogg"
  "Sound for new mail notification.
Any format works."
  :type 'string
  :group 'mail-bugger)

(defcustom mail-bugger-new-mail-icon "/usr/share/icons/Revenge/128x128/apps/emacs.png"
  "Icon for new mail notification.
PNG works."
  :type 'string
  :group 'mail-bugger)

(defcustom mail-bugger-icon
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :file "~/.emacs.d/img/perso.xpm"
	    :ascent center))
  "Icon for the first account.
Must be an XPM (use Gimp)."
  :group 'mail-bugger)

(defconst mail-bugger-logo
  (if mail-bugger-icon
      (apply 'propertize " " `(display ,mail-bugger-icon))
    "G"))

(defvar mail-bugger-timer nil)

(defvar mail-bugger-advertised-mails '())
;; (setq mail-bugger-advertised-mails '())

;;;###autoload
(defun mail-bugger-init ()
"Init"
  (interactive)
  (add-to-list 'global-mode-string
               '(:eval (mail-bugger-mode-line)) t)
  (setq mail-bugger-timer
        (run-with-timer 0
                        mail-bugger-timer-interval
                        'mail-bugger-check)))

(defmacro mail-bugger-shell-command (cmd callback account)
  "Run CMD asynchronously, then run CALLBACK"
  `(let* ((buf (generate-new-buffer  (concat "*mail-bugger-" ,account "*")))
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
                (error "mail-bugger (%s) error: %d" account err)))))))))

(defun mail-bugger-shell-command-callback ()
  "Now we're talking"
  (let* ((header-str (mail-bugger-read-output-buffer (current-buffer))))
    (setq mail-bugger-unseen-mails lines)
    (unless (null mail-bugger-unseen-mails)
      (run-hooks 'mail-bugger-new-mails-hook))
    (mail-bugger-mode-line)
    (mail-bugger-desktop-notify)
    (force-mode-line-update)))

(defun mail-bugger-desktop-notification (summary body timeout)
  "Call notification-daemon method with ARGS over dbus"
  (dbus-call-method-non-blocking
   :session                                 ; use the session (not system) bus
   "org.freedesktop.Notifications"          ; service name
   "/org/freedesktop/Notifications"         ; path name
   "org.freedesktop.Notifications" "Notify" ; Method
   "GNU Emacs"				    ; Application
   0					    ; Timeout
   mail-bugger-new-mail-icon
   summary
   body
   '(:array)
   '(:array :signature "{sv}")
   ':int32 timeout)
  (if mail-bugger-new-mail-sound
      (shell-command
       (concat "mplayer -really-quiet " mail-bugger-new-mail-sound " 2> /dev/null"))))

(defun mail-bugger-mode-line ()
  (if (null mail-bugger-unseen-mails)
      ""
    (let ((s (format "%d " (length lines)))
          (map (make-sparse-keymap))
          (url "https://mail.google.com"))
      (define-key map (vector 'mode-line 'mouse-2)
        `(lambda (e)
           (interactive "e")
           (browse-url ,url)
           ;; (setq mail-bugger-unseen-mails nil)
))
      (add-text-properties 0 (length s)
                           `(local-map ,map mouse-face mode-line-highlight
                                       uri ,url help-echo
                                       ,(concat
                                         (mail-bugger-tooltip)
                                         "\n\nmouse-2: View mail"))
                           s)
      (concat " " mail-bugger-logo ":" s))))

(defun mail-bugger-desktop-notify ()
  (mapcar
   (lambda (x)
       (if (not (member x mail-bugger-advertised-mails))
	   (progn
	     (mail-bugger-desktop-notification
	      "<h3 style='color:red;'>New mail!</h3>"
	      (format "<h4>%s</h4><h5>%s</h5><hr>%s"
		      (car (nthcdr 1 x))
		      (nthcdr 2 x)
		      (car x))
	      1)
	     (add-to-list 'mail-bugger-advertised-mails x))))
   mail-bugger-unseen-mails))

(defun mail-bugger-wordwrap (s N)
  "Hard wrap string S on 2 lines to N chars"
  (if (< N (length s))
      (concat (subseq s 0 N) "\n" (subseq s N (+ N N)) "...")
    s))

(defun mail-bugger-format-time (s)
  "Clean Time string S"
  (subseq (car s) 0 -6))

(defun mail-bugger-tooltip ()
  "Loop through the mail headers and build the hover tooltip"
  (mapconcat
   (lambda (x)
     (let
	 ((tooltip-string
	   (format "%s\n%s \n--------------\n%s\n"
		   (car (nthcdr 1 x))
		   ;; (nthcdr 2 x)
		   (mail-bugger-format-time (nthcdr 2 x))
		   (mail-bugger-wordwrap (car x) 35)
		   )))
       tooltip-string)
     )
   mail-bugger-unseen-mails
   "\n"))

(defun mail-bugger-buffer-to-list (buf)
  "Make & return a list (of lists) LINES from lines in a buffer BUF"
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

(defun mail-bugger-read-output-buffer (buffer)
  "Read and parse BUFFER"
  (setq lines (mail-bugger-buffer-to-list buffer)))

(defun mail-bugger-check ()
  "Check unread mail."
  (interactive)
  (mail-bugger-shell-command
   (format "%s %s %s %s %s %s"
           mail-bugger-shell-script-command
	   mail-bugger-host
	   mail-bugger-protocol
	   mail-bugger-imap-box
	   (auth-source-user-or-password "login" mail-bugger-host mail-bugger-protocol)
	   (auth-source-user-or-password "password" mail-bugger-host mail-bugger-protocol))
   'mail-bugger-shell-command-callback mail-bugger-host))

(message "%s loaded" (or load-file-name buffer-file-name))


(provide 'mail-bugger)
;;; mail-bugger.el ends here
