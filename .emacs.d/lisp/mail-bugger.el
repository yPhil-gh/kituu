;;; mail-bugger.el --- Notify of unread mails on mode line & on DBus

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
;; Fill in your ~/authinfo according to the book
;; and use M-x customize-group "mail-bugger" RET

;;; Code:

(require 'auth-source)
(require 'dbus)

(defgroup mail-bugger nil
  "Universal mail notifier."
  :prefix "mail-bugger-"
  :group 'mail)

(defcustom mail-bugger-launch-client-command "px-go-mail"
  "Mail client command.
Example : wl"
  :type 'string
  :group 'mail-bugger)

(defun mail-bugger-launch-client ()
  (if (string-equal mail-bugger-launch-client-command "gnus")
      (gnus)
    (if (string-equal mail-bugger-launch-client-command "wl")
	(wl))
    (px-go-mail)))

(defgroup mail-bugger-account-one nil
  "Details for account one."
  :prefix "mail-bugger-accounts"
  :group 'mail-bugger)

(defgroup mail-bugger-account-two nil
  "Details for account two."
  :prefix "mail-bugger-accounts"
  :group 'mail-bugger)

(defcustom mail-bugger-host-one "imap.gmail.com"
  "Mail host.
Example : imap.gmail.com"
  :type 'string
  :group 'mail-bugger-account-one)

(defcustom mail-bugger-protocol-one "993/imap/ssl"
  "Port number and (optional) protocol path.
Example : 993/imap/ssl"
  :type 'string
  :group 'mail-bugger-account-one)

(defcustom mail-bugger-imap-box-one "INBOX"
  "Name of the imap folder on the server.
Example : INBOX"
  :type 'string
  :group 'mail-bugger-account-one)

(defcustom mail-bugger-host-two "mail.gandi.net"
  "Mail host.
Example : imap.gmail.com"
  :type 'string
  :group 'mail-bugger-account-two)

(defcustom mail-bugger-protocol-two "143"
  "Port number and (optional) protocol path.
Example : 993/imap/ssl"
  :type 'string
  :group 'mail-bugger-account-two)

(defcustom mail-bugger-imap-box-two "INBOX"
  "Name of the imap folder on the server.
Example : INBOX"
  :type 'string
  :group 'mail-bugger-account-two)

;; (defcustom mail-bugger-username nil
;;   "Mail username.
;; Put your user name & password in ~/authinfo.gpg like this :
;; machine <host> login <login> port <port> password <password>"
;;   :type 'string
;;   :group 'mail-bugger)

;; (defcustom mail-bugger-password nil
;;   "Mail password.
;; Put your user name & password in ~/authinfo.gpg like this :
;; machine <host> login <login> port <port> password <password>"
;;   :type 'string
;;   :group 'mail-bugger)

(defcustom mail-bugger-new-mail-sound "/usr/share/sounds/KDE-Im-New-Mail.ogg"
  "Sound for new mail notification.
Any format works."
  :type 'string
  :group 'mail-bugger)

(defcustom mail-bugger-new-mail-icon-one "/usr/share/icons/oxygen/128x128/actions/configure.png"
  "Icon for new mail notification.
PNG works."
  :type 'string
  :group 'mail-bugger-account-one)

(defcustom mail-bugger-new-mail-icon-two "/usr/share/icons/Revenge/128x128/apps/emacs.png"
  "Icon for new mail notification.
PNG works."
  :type 'string
  :group 'mail-bugger-account-two)

(defcustom mail-bugger-icon-one
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :file "~/.emacs.d/img/perso.xpm"
	    :ascent center))
  "Icon for the first account.
Must be an XPM (use Gimp)."
  :group 'mail-bugger-account-one)

(defcustom mail-bugger-icon-two
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :file "~/.emacs.d/img/adamweb.xpm"
	    :ascent center))
  "Icon for the second account.
Must be an XPM (use Gimp)."
  :group 'mail-bugger-account-two)

(defconst mail-bugger-logo-one
  (if mail-bugger-icon-one
      (apply 'propertize " " `(display ,mail-bugger-icon-one))
    "G"))

(defconst mail-bugger-logo-two
  (if mail-bugger-icon-two
      (apply 'propertize " " `(display ,mail-bugger-icon-two))
    "G"))

(defvar mail-bugger-unseen-mails nil)
(defvar mail-bugger-advertised-mails-one '())
(defvar mail-bugger-advertised-mails-two '())

(defvar mail-bugger-shell-script-command "~/scripts/unread.php"
  "Full command line. Can't touch that.")

(defcustom mail-bugger-timer-interval 150
  "Interval(in seconds) for mail check."
  :type 'number
  :group 'mail-bugger)

;;;###autoload
(defun mail-bugger-init ()
  "Init"
  (interactive)
  (add-to-list 'global-mode-string
               '(:eval (mail-bugger-mode-line)))
  (run-with-timer 0
		  mail-bugger-timer-interval
		  'mail-bugger-check-all))

(defun mail-bugger-check-all ()
  "Check unread mail now."
  (interactive)
  (if (get-buffer "*mail-bugger-mail.gandi.net*")
      (kill-buffer "*mail-bugger-mail.gandi.net*"))
  (if (get-buffer "*mail-bugger-imap.gmail.com*")
      (kill-buffer "*mail-bugger-imap.gmail.com*"))
  (mail-bugger-check mail-bugger-host-one mail-bugger-protocol-one mail-bugger-imap-box-one)
  (mail-bugger-check mail-bugger-host-two mail-bugger-protocol-two mail-bugger-imap-box-two))

(defun mail-bugger-check (host protocol box)
  "Check unread mail."
  ;; (message "%s %s %s" host protocol box)
  (mail-bugger-shell-command
   (format "%s %s %s %s %s %s"
           mail-bugger-shell-script-command
	   host
	   protocol
	   box
	   ;; mail-bugger-protocol
	   ;; mail-bugger-imap-box
	   (auth-source-user-or-password "login" host protocol)
	   (auth-source-user-or-password "password" host protocol))
   'mail-bugger-shell-command-callback host))

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
                (error "mail-bugger (%s) error: %d", account err)))))))))

(defun mail-bugger-shell-command-callback ()
  "Construct the unread mails lists"
  (setq mail-bugger-unseen-mails-one (mail-bugger-buffer-to-list "*mail-bugger-imap.gmail.com*"))
  (setq mail-bugger-unseen-mails-two (mail-bugger-buffer-to-list "*mail-bugger-mail.gandi.net*"))
  (mail-bugger-mode-line)
  ;; (mail-bugger-desktop-notify mail-bugger-new-mail-icon-one)
  ;; (mail-bugger-desktop-notify mail-bugger-new-mail-icon-two)
  (mail-bugger-desktop-notify-one)
  (mail-bugger-desktop-notify-two)
  (force-mode-line-update))




(defun mail-bugger-own-little-imap-client (maillist)
  (interactive)
  (princ
   (mapconcat
   (lambda (x)
     (let
	 ((tooltip-string
	   (format "%s\n%s \n--------------\n%s\n"
		   (car (nthcdr 1 x))
		   ;; (nthcdr 2 x)
		   (mail-bugger-format-time (nthcdr 2 x))
		   (mail-bugger-wordwrap (car x) 50)
		   ;; (car x)
		   )))
       tooltip-string)
     )
   maillist
   "\n")
   (generate-new-buffer "MBOLIC"))
  (switch-to-buffer "MBOLIC"))

(defun mail-bugger-mode-line ()
  "Construct an emacs modeline object"
(concat
  (if (null mail-bugger-unseen-mails-one)
      (concat " " mail-bugger-logo-one)
    (let ((s
	   (format "%d" (length mail-bugger-unseen-mails-one)))
          (map (make-sparse-keymap))
          (url (concat "http://" mail-bugger-host-one)))

      (define-key map (vector 'mode-line 'mouse-1)
        `(lambda (e)
           (interactive "e")
	   (mail-bugger-launch-client)))

      (define-key map (vector 'mode-line 'mouse-2)
        `(lambda (e)
           (interactive "e")
           (browse-url ,url)))

      (define-key map (vector 'mode-line 'mouse-3)
        `(lambda (e)
           (interactive "e")
	   (mail-bugger-own-little-imap-client mail-bugger-unseen-mails-one)))

      (add-text-properties 0 (length s)
                           `(local-map,
			     map mouse-face mode-line-highlight
			     uri, url help-echo,
			     (concat
			      (mail-bugger-tooltip-one)
			      (format "
\n--------------\nmouse-1: View mail in wl
\nmouse-2: View mail on %s
\nmouse-3: View mail on MBOLIC" mail-bugger-launch-client-command mail-bugger-host-one)))
                           s)
      (concat mail-bugger-logo-one ":" s)))
" "
  (if (null mail-bugger-unseen-mails-two)
      (concat " " mail-bugger-logo-two)
    (let ((s
	   (format "%d" (length mail-bugger-unseen-mails-two)))
          (map (make-sparse-keymap))
          (url (concat "http://" mail-bugger-host-two)))

      (define-key map (vector 'mode-line 'mouse-1)
        `(lambda (e)
           (interactive "e")
	   (mail-bugger-launch-client)))

      (define-key map (vector 'mode-line 'mouse-2)
        `(lambda (e)
           (interactive "e")
           (browse-url ,url)))

      (define-key map (vector 'mode-line 'mouse-3)
        `(lambda (e)
           (interactive "e")
	   (mail-bugger-own-little-imap-client mail-bugger-unseen-mails-two)))

      (add-text-properties 0 (length s)
                           `(local-map,
			     map mouse-face mode-line-highlight
			     uri, url help-echo,
			     (concat
			      (mail-bugger-tooltip-two)
			      (format "
\n--------------\nmouse-1: View mail in wl
\nmouse-2: View mail on %s
\nmouse-3: View mail on MBOLIC" mail-bugger-launch-client-command mail-bugger-host-two)))
                           s)
      (concat mail-bugger-logo-two ":" s)))))

(defun mail-bugger-tooltip-one ()
  "Loop through the mail headers and build the hover tooltip"
  (mapconcat
   (lambda (x)
     (let
	 ((tooltip-string
	   (format "%s\n%s \n--------------\n%s\n"
		   (car (nthcdr 1 x))
		   ;; (nthcdr 2 x)
		   (mail-bugger-format-time (nthcdr 2 x))
		   (mail-bugger-wordwrap (car x) 50)
		   ;; (car x)
		   )))
       tooltip-string)
     )
   mail-bugger-unseen-mails-one
   "\n"))

(defun mail-bugger-tooltip-two ()
  "Loop through the mail headers and build the hover tooltip"
  (mapconcat
   (lambda (x)
     (let
	 ((tooltip-string
	   (format "%s\n%s \n--------------\n%s\n"
		   (car (nthcdr 1 x))
		   ;; (nthcdr 2 x)
		   (mail-bugger-format-time (nthcdr 2 x))
		   (mail-bugger-wordwrap (car x) 50)
		   ;; (car x)
		   )))
       tooltip-string)
     )
   mail-bugger-unseen-mails-two
   "\n"))

(defun mail-bugger-desktop-notify-one ()
  (mapcar
   (lambda (x)
       (if (not (member x mail-bugger-advertised-mails-one))
	   (progn
	     (mail-bugger-desktop-notification
	      "<h3 style='color:palegreen;'>New mail!</h3>"
	      (format "<h4>%s</h4><h5>%s</h5><hr>%s"
		      (car (nthcdr 1 x))
		      (nthcdr 2 x)
		      (car x))
	      1 mail-bugger-new-mail-icon-one)
	     (add-to-list 'mail-bugger-advertised-mails-one x))))
   mail-bugger-unseen-mails-one))

(defun mail-bugger-desktop-notify-two ()
  (mapcar
   (lambda (z)
       (if (not (member z mail-bugger-advertised-mails-two))
	   (progn
	     (mail-bugger-desktop-notification
	      "<h3 style='color:red;'>New mail!</h3>"
	      (format "<h4>%s</h4><h5>%s</h5><hr>%s"
		      (car (nthcdr 1 z))
		      (nthcdr 2 z)
		      (car z))
	      1 mail-bugger-new-mail-icon-two)
	     (add-to-list 'mail-bugger-advertised-mails-two z))))
   mail-bugger-unseen-mails-two))

(defun mail-bugger-desktop-notification (summary body timeout icon)
  "Call notification-daemon method with ARGS over dbus"
  ;; (if mail-bugger-new-mail-sound
  ;;     (shell-command
  ;;      (concat "mplayer -really-quiet " mail-bugger-new-mail-sound " 2> /dev/null")))
  (dbus-call-method-non-blocking
   :session                                 ; use the session (not system) bus
   "org.freedesktop.Notifications"          ; service name
   "/org/freedesktop/Notifications"         ; path name
   "org.freedesktop.Notifications" "Notify" ; Method
   "GNU Emacs"			       	    ; Application
   0					    ; Timeout
   icon
   summary
   body
   '(:array)
   '(:array :signature "{sv}")
   ':int32 timeout))

;; Utilities

(defun mail-bugger-buffer-to-list (buf)
  "Make & return a list (of lists) LINES from lines in a buffer BUF"
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (let ((lines '()))
        (while (not (eobp))
          (push (split-string
                 (buffer-substring (point) (point-at-eol)) "\|_\|")
                lines)
          (beginning-of-line 2))
	lines))))

(defun mail-bugger-wordwrap (s N)
  "Hard wrap string S on 2 lines to N chars"
  (if (< N (length s))
      (concat (subseq s 0 N) "\n" (subseq s N) "...")
    s))

(defun mail-bugger-format-time (s)
  "Clean Time string S"
  (subseq (car s) 0 -6))

(defun Rx ()
(interactive)
(setq mail-bugger-advertised-mails-one '())
(setq mail-bugger-advertised-mails-two '()))

(message "%s loaded" (or load-file-name buffer-file-name))
(provide 'mail-bugger)
