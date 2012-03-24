;; You should set this variable if you use multiple e-mail addresses.
;; (setq wl-user-mail-address-list (quote ("philippe.coatmeur@gmail.com" "contact@adamweb.net")))
;; (define-key wl-summary-mode-map px-toggle-mail-key 'px-no-mail)
;; (define-key wl-folder-mode-map px-toggle-mail-key 'px-no-mail)
;; (setq wl-from "Phil CM <philippe.coatmeur@gmail.net>")

;; Vars! ______________________________________________________________________

(setq wl-stay-folder-window t)
(setq

 ;; Offline and synchronization
 wl-plugged t
 elmo-imap4-use-modified-utf7 t
 elmo-imap4-use-cache t
 elmo-nntp-use-cache t
 elmo-pop3-use-cache t
 wl-ask-range nil
 wl-auto-flush-queue t

 elmo-message-fetch-confirm t
 elmo-message-fetch-threshold 250000
 elmo-network-session-idle-timeout 60

;; Threading
 wl-thread-insert-opened t
 wl-thread-indent-level 1

 wl-thread-have-younger-brother-str "├►"
 wl-thread-youngest-child-str       "╰►"
 wl-thread-vertical-str             "│ "
 wl-thread-horizontal-str           "──"
 wl-thread-space-str                "  "

;; Visual
 wl-summary-always-sticky-folder-list t
 wl-folder-summary-line-format-alist
      '(("^%" . "%T%P%M/%D(%W)%h:%m %t[%17(%c %f%) ] %s")
        ("^-" . "%Y/%M/%D (%W) %[%17(%f %c%)%]%t %s"))

      wl-message-window-size '(4 . 6)
      wl-auto-select-first t

)

(setq
 wl-message-ignored-field-list '("^.*:")
 wl-message-visible-field-list
 '("^\\(To\\|Cc\\):"
   "^Subject:"
   "^\\(From\\|Reply-To\\):"
   "^Organization:"
   "^Message-Id:"
   "^\\(Posted\\|Date\\):"
   "^[xX]-[Ff]ace:"
   )
 wl-message-sort-field-list
 '("^From"

   "^Organization:"
   "^X-Attribution:"
   "^Subject"
   "^Date"
   "^To"
   "^Cc")

 elmo-nntp-default-server "news.eternal-september.org"
 elmo-nntp-default-user "PhilippeCM"
)
(setq wl-nntp-posting-server elmo-nntp-default-server)

(require 'bbdb-wl)
(bbdb-wl-setup)

;; i don't want to store addresses from my mailing folders
(setq
  bbdb-wl-folder-regexp    ;; get addresses only from these folders
  "sent")    ;;

;; Keys! ______________________________________________________________________

(define-key wl-draft-mode-map (kbd "<tab>") 'bbdb-complete-name)
(define-key wl-draft-mode-map (kbd "<C-return>") 'wl-draft-send-and-exit)

;;select correct email address when we _start_ writing a draft.
(add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)

;;is run when wl-draft-send-and-exit or wl-draft-send is invoked:
;;(NOTE: "M-: wl-draft-parent-folder" => %INBOX:myname/clear@imap.gmail.com:993)
(setq wl-draft-config-alist
      '(
        ((string-match "adamweb.net" wl-draft-parent-folder)
         (template . "work"))
        ((string-match "gmail.com" wl-draft-parent-folder)
         (template . "gmail")
         )))

;; xprmt
;; IMAP, gmail:
(setq
 elmo-imap4-default-server "imap.gmail.com"
 elmo-imap4-default-user "philippe.coatmeur@gmail.com"
 elmo-imap4-default-authenticate-type 'clear
 elmo-imap4-default-port '993
 elmo-imap4-default-stream-type 'ssl

 ;;for non ascii-characters in folder-names
 elmo-imap4-use-modified-utf7 t)

;; SMTP
(setq
 wl-smtp-connection-type 'starttls
 wl-smtp-posting-port 587
 wl-smtp-authenticate-type "plain"
 wl-smtp-posting-user "philippe.coatmeur"
 wl-smtp-posting-server "smtp.gmail.com"
 wl-local-domain "gmail.com"
 wl-message-id-domain "smtp.gmail.com")

;;choose template with C-c C-j
(setq wl-template-alist
      '(("gmail"
         (wl-from . "Philippe M. Coatmeur <philippe.coatmeur@gmail.com>")
         ("From" . wl-from)
         (wl-smtp-posting-user . "philippe.coatmeur")
         (wl-smtp-posting-server . "smtp.gmail.com")
         (wl-smtp-authenticate-type ."plain")
         (wl-smtp-connection-type . 'starttls)
         (wl-smtp-posting-port . 587)
         (wl-local-domain . "gmail.com")
         (wl-message-id-domain . "smtp.gmail.com"))

        ("work"
         (wl-from . "Adamweb <contact@adamweb.net>")
         ("From" . wl-from)
         (wl-smtp-posting-user . "contact@adamweb.net")
         (wl-smtp-posting-server . "mail.gandi.net")
         (wl-smtp-authenticate-type ."plain")
         (wl-smtp-connection-type . 'starttls)
         (wl-smtp-posting-port . 587)
         (wl-local-domain . "mail.gandi.net")
         (wl-message-id-domain . "mail.gandi.net"))))

;; open unread group folder after checking.
(add-hook 'wl-folder-check-entity-hook
	  '(lambda ()
	     (wl-folder-open-unread-folder entity)
	     ))

;; notify mail arrival
(setq wl-biff-check-folder-list '(".Inbox"))
;; Set mail-icon to be shown universally in the modeline.
;; (setq global-mode-string
;;       (cons
;;        '(wl-modeline-biff-status
;;          wl-modeline-biff-state-on
;;          wl-modeline-biff-state-off)
;;        global-mode-string))


;; Use different signature files based on From: address
(setq signature-file-alist
      `((("From" . "myname@company.com") . ,(expand-file-name "~/.contact@adamweb.net.sig"))
	(("From" . "myname@gmail.com") . ,(expand-file-name "~/.philippe.coatmeur@gmail.com.sig"))))

;;Cycle through templates with arrow keys
(define-key wl-template-mode-map (kbd "<C-i>") 'wl-template-next)
(define-key wl-template-mode-map (kbd "<C-o>") 'wl-template-prev)

(setq
 wl-biff-use-idle-timer t
 wl-biff-check-interval 5)

(setq wl-biff-notify-hook '(message "plop! New mail"))

(setq display-time-mail-function
'(lambda () wl-modeline-biff-status))

;;default folder name auto completion:
(setq wl-default-spec "%")

;; mark sent messages (folder carbon copy) as read.
;; (setq wl-fcc-force-as-read    t)

;;Only save draft when I tell it to! (C-x C-s or C-c C-s):
;;(arg: seconds of idle time untill auto-save).
(setq wl-auto-save-drafts-interval nil)

;; This kinda works

;; ;; IMAP
;; (setq elmo-imap4-default-server "imap.gmail.com")
;; (setq elmo-imap4-default-user "philippe.coatmeur@gmail.com")
;; (setq elmo-imap4-default-authenticate-type 'clear)
;; (setq elmo-imap4-default-port '993)
;; (setq elmo-imap4-default-stream-type 'ssl)

;; ;; (setq elmo-imap4-use-modified-utf7 t)

;; ;; SMTP
;; (setq wl-smtp-connection-type 'starttls)
;; (setq wl-smtp-posting-port 587)
;; (setq wl-smtp-authenticate-type "plain")
;; (setq wl-smtp-posting-user "philippe.coatmeur")
;; (setq wl-smtp-posting-server "smtp.gmail.com")
;; (setq wl-local-domain "gmail.com")

;; (setq wl-default-folder "%inbox")
;; (setq wl-default-spec "%")
;; (setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
;; (setq wl-trash-folder "%[Gmail]/Trash")

;; (setq wl-folder-check-async t)

;; (autoload 'wl-user-agent-compose "wl-draft" nil t)
;; (if (boundp 'mail-user-agent)
;;     (setq mail-user-agent 'wl-user-agent))
;; (if (fboundp 'define-mail-user-agent)
;;     (define-mail-user-agent
;;       'wl-user-agent
;;       'wl-user-agent-compose
;;       'wl-draft-send
;;       'wl-draft-kill
;;       'mail-send-hook))



;; %n  message number
;; %T  temporary mark
;; %P  persistent mark
;; %Y  year
;; %M  month
;; %D  day
;; %W  day of week
;; %h  hour
;; %m  minute
;; %t  branch of the thread
;; %[  [ (< for re-connected child)
;; %]  ] (> for re-connected child)
;; %f  sender
;; %s  subject
;; %S  size
;; %c  +number-of-children: (display only for opened thread)
;; %C  [+number-of-children] (display only for opened thread)
;; %#  mailing list information (`(' ML-name [ ` ' ML-number ] `)')
;; %l  number in the mailing list
;; %@ `@' only if the first MIME part is multipart/mixed
;; %~  ` ' only if previous column is empty

;; (setq wl-auto-save-drafts-interval nil) And/or change you draft folder
;; to local: (setq wl-draft-folder "+draft").

;; > Also, when I check the inbox after some idle time, WL will become
;; > dead ! It seems the problem of IMAP.

;; Set elmo-network-session-idle-timeout to some reasonable value (60
;; seconds, for example)

;; offline at startup.
;(setq wl-plugged nil)
;; change plug status by server or port at startup.
;(add-hook 'wl-make-plugged-hook
;	  '(lambda ()
;	     ;; Add or Change plug status for SERVER and PORT.
;	     (elmo-set-plugged plugged(t/nil) server port)
;	     ;; When omit port, SEVERS all port was changes.
;	     ;; (Can't add plug status without PORT)
;	     (elmo-set-plugged plugged(t/nil) server)
;	     ))

;; ;; How messages with disposal mark ("d") are to be handled.
;; ;; remove = instant removal (same as "D"), thrash = move to wl-trash-folder
;; ;; string = move to string.
;; (setq wl-dispose-folder-alist
;;       '(
;;         ;;("^-" . remove) ;;default    ;;I don't know what this is?
;;         ;;("^@" . remove) ;;default    ;;I don't know what this is?
;;         ("\.\*company\\.com" . "%INBOX.Trash:\"myname@company.com\"/clear@mail.company.com")
;;         ("\.\*gmail\\.com" . "%[Gmail]/Trash:myname/clear@imap.gmail.com:993!")
;;         ))

;;Name of top-folder, default "Desktop".
;; (setq wl-folder-desktop-name "plop")

;; (setq
;;  wl-from "My Name <myname@gmail.com>"

;;  ;;all system folders (draft, trash, spam, etc) are placed in the
;;  ;;[Gmail]-folder, except inbox. "%" means it's an IMAP-folder
;;  wl-default-folder "%inbox"
;;  wl-draft-folder   "%[Gmail]/Drafts"
;;  wl-trash-folder   "%[Gmail]/Trash"
;;  wl-fcc            "%[Gmail]/Sent Mail"

;;  ;; mark sent messages as read (sent messages get sent back to you and
;;  ;; placed in the folder specified by wl-fcc)
;;  wl-fcc-force-as-read    t

;;  ;;for when auto-compleating foldernames
;;  wl-default-spec "%")
