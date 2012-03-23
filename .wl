;; You should set this variable if you use multiple e-mail addresses.
;; (setq wl-user-mail-address-list (quote ("philippe.coatmeur@gmail.com" "contact@adamweb.net")))
;; (define-key wl-summary-mode-map px-toggle-mail-key 'px-no-mail)
;; (define-key wl-folder-mode-map px-toggle-mail-key 'px-no-mail)

(setq wl-stay-folder-window t)
(setq
 wl-summary-always-sticky-folder-list t
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
 )

;; Change format of thread view
;; (setq wl-thread-indent-level 2)
(setq

 ;; wl-thread-insert-opened t
 ;; wl-thread-indent-level 1
 ;; wl-thread-have-younger-brother-str "y"
 ;; wl-thread-youngest-child-str       "z"
 ;; wl-thread-vertical-str             "|"
 ;; wl-thread-horizontal-str           "x"
 ;; wl-thread-space-str                "w"

 wl-thread-have-younger-brother-str "◯ "
 wl-thread-youngest-child-str       "╰►"
 wl-thread-vertical-str             "│ "
 wl-thread-horizontal-str           ""
 wl-thread-space-str                "  "

 ;; wl-thread-insert-opened t
 ;; wl-thread-indent-level 2
 ;; wl-thread-have-younger-brother-str "●"
 ;; wl-thread-youngest-child-str       "◎"
 ;; wl-thread-vertical-str             "|"
 ;; wl-thread-horizontal-str           "◯"
 ;; wl-thread-space-str                "►"

)

(setq wl-message-window-size '(4 . 6))
(setq wl-auto-select-first t)

(message "plop loaded")
 ;; gnus-sum-thread-tree-indent          "  "
 ;; gnus-sum-thread-tree-root            "● "
 ;; gnus-sum-thread-tree-false-root      "◎ "
 ;; gnus-sum-thread-tree-single-indent   "◯ "
 ;; gnus-sum-thread-tree-leaf-with-other "├─► "
 ;; gnus-sum-thread-tree-vertical        "│ "
 ;; gnus-sum-thread-tree-single-leaf     "╰─► "


;; (setq elmo-enable-disconnected-operation t)

;; Store draft message in queue folder if message is sent in unplugged status.
(setq wl-draft-enable-queuing t)
;; when plug status is changed from unplugged to plugged,
;; queued message is flushed automatically.
(setq wl-auto-flush-queue t)

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

;; open unread group folder after checking.
(add-hook 'wl-folder-check-entity-hook
	  '(lambda ()
	     (wl-folder-open-unread-folder entity)
	     ))

;; notify mail arrival
;(setq wl-biff-check-folder-list '("%inbox"))
;(setq wl-biff-notify-hook '(ding))

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
         (wl-message-id-domain . "smtp.gmail.com")


         (wl-fcc . "%[Gmail]/Sent:philippe.coatmeur/clear@imap.gmail.com:993!")
         (wl-draft-folder . "%[Gmail]/Draft:philippe.coatmeur/clear@imap.gmail.com:993!")
         )
        ("work"
         (wl-from . "Adamweb <contact@adamweb.net>")
         ("From" . wl-from)
         (wl-smtp-posting-user . "contact@adamweb.net")
         (wl-smtp-posting-server . "mail.gandi.net")

         (wl-smtp-authenticate-type ."plain")
         (wl-smtp-connection-type . 'starttls)
         (wl-smtp-posting-port . 587)

         (wl-local-domain . "mail.gandi.net")

         (wl-fcc . "%INBOX.Sent:\"contact@adamweb.net\"/clear@mail.company.com")
         (wl-draft-folder . "%INBOX.Drafts:\"contact@adamweb.net\"/clear@mail.gandi.net"))
        ))

;; Use different signature files based on From: address
(setq signature-file-alist
      `((("From" . "myname@company.com") . ,(expand-file-name "~/.contact@adamweb.net.sig"))
	(("From" . "myname@gmail.com") . ,(expand-file-name "~/.philippe.coatmeur@gmail.com.sig"))))

;;Cycle through templates with arrow keys
(define-key wl-template-mode-map (kbd "<right>") 'wl-template-next)
(define-key wl-template-mode-map (kbd "<left>") 'wl-template-prev)

;;default folder name auto completion:
(setq wl-default-spec "%")

;; mark sent messages (folder carbon copy) as read.
(setq wl-fcc-force-as-read    t)

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
