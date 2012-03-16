;; ==========================================================================
;; Time-stamp: <.gnus.el - Fri 16-Mar-2012 01:08:05>
;; ===========================================================================
;; Remember to install gnutls!!
(load "starttls")
;; (load-library "smtpmail")
(gnus-demon-init)
(gnus-demon-add-handler 'chk-all 5 nil) ; 5 minutes
;; (gnus-demon-add-rescan)

(require 'olimap)
(require 'gnus-notify-)
(require 'nnir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BBDB:
;; - see `http://www.mail-archive.com/info-gnus-english@gnu.org/msg00624.html'
(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-hooks)

(if (fboundp 'modify-coding-system-alist)
    (modify-coding-system-alist 'file "bbdb\\'" 'utf-8))

(bbdb-initialize 'gnus 'message)

;; Set `bbdb-north-american-phone-numbers-p' to `nil' if you want to
;; enter phone numbers that aren't the same syntax as those in North
;; America.
;;
;; (Info-goto-node "(bbdb)Customization Parameters")
(setq bbdb-north-american-phone-numbers-p nil)

;; The address inserted is normally of the form `User Name
;; <email-address>'; however, if `User Name' has an address of the form
;; `<user.name@somedomain>', only the `<email-address>' portion is
;; inserted. This can be overridden by setting
;; `bbdb-dwim-net-address-allow-redundancy' to `t'.
;;
;; (Info-goto-node "(bbdb)Mail Sending Interfaces")
(setq bbdb-dwim-net-address-allow-redundancy t)

;; If the variable `bbdb-completion-display-record' is `t' then when you
;; successfully complete an address with `M-TAB', the corresponding
;; record will be appended to the `*BBDB*' buffer.  The buffer will not
;; be displayed if it is not already visible, but the record will be
;; displayed there.
;;
;; (Info-goto-node "(bbdb)Mail Sending Interfaces")
(setq bbdb-completion-display-record t)

;; Whether bbdb mode should be "electric" like `electric-buffer-list'.
;; Basically this means that when you type space after `M-x bbdb', your
;; window configuration will be restored to what it was before you
;; invoked the db list.  (The `bbdb-mode' commands still work as well.)
;;
;; (Info-goto-node "(bbdb)Customization Parameters")
(setq bbdb-electric-p t)

;; If the variable `bbdb/mail-auto-create-p' is set to the symbol
;; `bbdb-ignore-most-messages-hook', then the variable
;; `bbdb-ignore-most-messages-alist' will determine which messages should
;; have records automatically created for them.
;;
;; (Info-goto-node "(bbdb)Predefined Hooks")
;; (setq bbdb/mail-auto-create-p 'bbdb-ignore-most-messages-hook
;;       bbdb-ignore-most-messages-alist
;;       '(("To" . "@sugarshark\\.\\(com\\|de\\)")))
(setq bbdb/mail-auto-create-p nil
      bbdb/news-auto-create-p nil)

;; If `bbdb-notice-auto-save-file' is `t', then the BBDB will notice when
;; its auto-save file is newer than the file is was read from, and will
;; offer to revert.
;;
;; (Info-goto-node "(bbdb)Customization Parameters")
(setq bbdb-notice-auto-save-file t)

;; If `bbdb-offer-save' is `t', then certain actions will cause the BBDB
;; to ask you whether you wish to save the database.  If `nil', then the
;; offer to save will never be made.  If not t and not `nil', then any
;; time it would ask you, it will just save it without asking.
;;
;; (Info-goto-node "(bbdb)Customization Parameters")
(setq bbdb-offer-save 'auto)

;; A regular expression identifying the addresses that belong to you.
;; If a message from an address matching this is seen, the BBDB record for
;; the To: line will be shown instead of the one for the From: line.
;;
;; (Info-goto-node "(bbdb)Customization Parameters")
(setq bbdb-user-mail-names message-alternative-emails)

;; If the variable `bbdb-change-hook' is set to the symbol
;; `bbdb-timestamp-hook', then every record in the database will have a
;; field named `timestamp', which will always contain the date and time
;; at which this record was created or last modified.
;;
;; If the variable `bbdb-create-hook' is set to the symbol
;; `bbdb-creation-date-hook', then every record in the database will have
;; a field named `creation-date', which will contain the date and time at
;; which this record was added to the database.
;;
;; (Info-goto-node "(bbdb)Predefined Hooks")
(add-hook 'bbdb-change-hook 'bbdb-timestamp-hook)
(add-hook 'bbdb-create-hook 'bbdb-creation-date-hook)

;; If the variable `bbdb-notice-hook' is set to the symbol
;; `bbdb-auto-notes-hook', then the variable `bbdb-auto-notes-alist' may
;; be used to automatically add text to the notes fields of the records
;; corresponding to certain messages.
;;
;; Explanation for x-face:
;;
;; (The calls to `list' and `concat' are just for readability, it could
;; easily be a constant.)  The tricky bit here is that it strips out the
;; newlines and whitespace used for header continuation, which are not
;; actually a part of the face data.  So though the mail message may have
;; the face data on multiple lines, the entry in the `*BBDB*' will be just
;; one line.
;;
;; (Info-goto-node "(bbdb)Predefined Hooks")
(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
(setq bbdb-auto-notes-alist
      (list
       ;;: This grabs the "Organization" header & updates the company slot.
       '("Organization" (".*" company 0))
       '("Newsgroups"   ("[^,]+" newsgroups 0))
       '("Subject"      (".*" last-subj 0 t))
       '("User-Agent"   (".*" mailer 0 t))
       '("X-Mailer"     (".*" mailer 0 t))
       '("X-Newsreader" (".*" mailer 0 t))
       '("X-URL"        (".*" "www" 0))
       '("X-ICQ"        (".*" "icq" 0))
       '("X-Face"       (".+" face 0 'replace))
       '("Face"         (".+" cface 0 'replace))
       '("X-PGP-Fingerprint"        (".*" "pgp-fingerprint" 0))
       '("X-PGP-Public-Key-ID"      (".*" "pgp-key" 0))
       '("X-Pgp-Fingerprint"        (".*" "pgp-fingerprint" 0))
       '("X-Pgp-Public-Key-Id"      (".*" "pgp-key" 0))
       '("X-Gpg-Fingerprint"        (".*" "pgp-fingerprint" 0))
       '("X-Gpg-Public-Key-Id"      (".*" "pgp-key" 0))
       '("X-Gpg-Key-ID"             (".*" "pgp-key" 0))
       ))

;; You can control what the separator is by putting a `field-separator'
;; property on the symbol naming the field.  For example, to make text
;; automatically added to a field named `newsgroups' be separated by
;; commas, you could do
;;
;;     (put 'newsgroups 'field-separator "; ")
;;
;; `bbdb-notes-default-separator' is used (by `bbdb-annotate-notes') for
;; notes which do not have `field-separator' property set.  E.g., if you
;; want URLs to be separated by newlines, you can put
;;
;;     (put 'www 'field-separator "\n") into your .emacs.
(setq bbdb-notes-default-separator "\n")

;; If `bbdb-always-add-addresses' is `t', then whenever the Insidious Big
;; Brother Database notices a new email address corresponding to a person
;; who is in the database, it will add it to the database.  If this is
;; `nil', then whenever a new network address is noticed
;; for a person in the database, you will be asked whether to add the
;; address.  If this is the symbol `never' (really if it is not `t' and
;; not `nil') then new network addresses will never be automatically
;; added.
;;
;; If `bbdb-new-nets-always-primary' is `t', then when the Insidious Big
;; Brother Database adds a new address to a record, it will always add it
;; to the front of the list of addresses, making it the primary address.
;; If this is `nil', then you will be asked.  If this is the symbol
;; `never' (really if it is not `t' and not `nil') then new network
;; addresses will always be added to the end of the list.
;;
;; (Info-goto-node "(bbdb)Customization Parameters")
(setq bbdb-always-add-addresses nil
      bbdb-new-nets-always-primary nil)

;; If `bbdb-quiet-about-name-mismatches' is `nil', then BBDB will prompt
;; you when it notices a name change, that is, when the "real name" in a
;; message doesn't correspond to a record already in the database with
;; the same network address.  As in, "John Smith <jqs@frob.com>" versus
;; "John Q. Smith <jqs@frob.com>".  If this is `t', then you will not be
;; asked if you want to change it (and it will not be changed.)  If a
;; number then it is the number of seconds to sit-for while displaying
;; the name mismatch.
;;
;; (Info-goto-node "(bbdb)Customization Parameters")
(setq bbdb-quiet-about-name-mismatches nil)

;; display-layout
;;
;; Currently there are three different layout types, which are
;; `one-line', `multi-line' and `full-multi-line'. You can use `t' and
;; `T' to toggle the display-layout.
;;
;;
;; My current setting (`bbdb-display-layout-alist'):
;;
;; ((one-line   (order     . (phones mail-alias net notes))
;;              (name-end  . 24)
;;              (toggle    . t))
;;  (multi-line (omit      . (creation-date timestamp))
;;              (toggle    . t))
;;  (full-multi-line))
;;
;; (describe-variable 'bbdb-display-layout-alist)
(setq bbdb-display-layout 'full-multi-line
      bbdb-pop-up-display-layout 'full-multi-line)

;; simplify
;; TMDA address: ray-dated-989958350.021c23@zonix.de -> ray@zonix.de
;;
;; ToDo:
;; dated address:                  ray-2000@zonix.de -> ray@zonix.de
;; newsgroup address:    ray+gnu.emacs.gnus@zonix.de -> ray@zonix.de
;;
;; `bbdb-canonicalize-net-hook' is NOT a hook. This will possibly be
;; changed in a future relase of bbdb but it's not yet done!
;; (setq bbdb-canonicalize-net-hook
;;       '(lambda (addr)
;; 	(cond
;; 	  ((string-match
;; 	    "\\([^0-9]+\\)\\(-dated-[^@]+\\|-[0-9]+\\|+[^@]+.[^@]+\\)\\(@.*\\)"
;; 	    addr)
;; 	   (concat (substring addr (match-beginning 1) (match-end 1))
;; 		   (substring addr (match-beginning 3) (match-end 3))))
;; 	  (t addr))))


;; Autoloads:
(autoload 'bbdb/send-hook "moy-bbdb"
  "Function to be added to `message-send-hook' to notice records when sending
  messages" t)
(add-hook 'message-send-hook 'bbdb/send-hook)
(autoload 'bbdb-define-all-aliases "bbdb-com"
  "Hook mail alias feature of BBDB into message-mode." t)

;; Address keymap for entering bbdb:
(defvar address-keymap (make-sparse-keymap "Adresses")
  "Keymap used to globally access address related functions")
(define-key mode-specific-map [?a] address-keymap)
(define-key address-keymap    [?s] 'bbdb)
(define-key address-keymap    [?a] 'bbdb-display)
(define-key address-keymap    [?c] 'bbdb-create)

;; Insinuation:
(bbdb-insinuate-message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'gnus-message-setup-hook 'bbdb-define-all-aliases)
(setq bbdb-notice-hook '(bbdb-auto-notes-hook))

;; (moy)
(defadvice bbdb-complete-name (after bbdb-complete-name-default-domain activate)
  (let ((completed ad-return-value))
    (if (null completed)
	(expand-abbrev))))

;; End bbdb


(defun chk-all ()
  "Let gnus read the msgs fetched by offlineimap"
  (interactive)
  (message "Chkng...")
  (olimap-run)
  )

(defun full-frame-iswitchb ()
  "*full frame"
  (interactive)
  (defadvice iswitchb (after iswitchb activate)
    (delete-other-windows)))

(add-hook 'gnus-group-mode-hook 'full-frame-iswitchb)

;; ;; From and to fields
;; (defvar my-email-addresses
;;   '("contact@adamweb.net"
;;     "philippe.coatmeur@gmail.com"))

;; (let ((addr my-email-addresses))
;;   (setq-default
;;    user-mail-address (car addr)
;;    message-alternative-emails (regexp-opt (cdr addr) 'words)
;;    message-dont-reply-to-names (regexp-opt addr 'words)
;;    gnus-ignored-from-addresses message-dont-reply-to-names))

;; ;; The cycling functionality
;; (add-hook 'message-mode-hook 'my-message-mode-hook)

;; (defun my-message-mode-hook ()
;;   (define-key message-mode-map (kbd "<f9>")
;;     'my-message-toggle-from))

;; (defun my-message-toggle-from ()
;;   (interactive)
;;   (require 'mail-extr)
;;   (let* ((current (nth 1 (mail-extract-address-components
;; 			  (message-fetch-field "From"))))
;; 	 (next (or (nth 1 (member current my-email-addresses))
;; 		   (nth 0 my-email-addresses))))

;;     (when (and current next)
;;       (save-excursion
;; 	(save-restriction
;; 	  (message-narrow-to-head)
;; 	  (save-match-data
;; 	    (when (re-search-forward "^From: " nil t)
;; 	      (message-narrow-to-field)
;; 	      (delete-region (point-min) (point-max))
;; 	      (insert "From: " user-full-name " <" next ">\n"))))))))

(setq gnus-ignored-from-addresses "philippe\\.coatmeur@gmail.com\\|adamweb\\.net")

;; (setq gnus-ignored-from-addresses
;;       "schua@ateneo.edu\\|sacha@free.net.ph\\|sachachua.com\\|sachac@ca.ibm.com")

(setq gnus-visual t)
(setq message-from-style 'angles)

;; Topics
(setq gnus-topic-indent-level 0)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; (gnus-group-list-groups)

(setq gnus-select-group-hook
      '(lambda ()
	 (if (this-buffer-is-visible "*Group*")
	     (progn
	       (sit-for 0.5)
	       (gnus-group-list-groups)))))

(setq gnus-article-prepare-hook
      '(lambda ()
	 (if (this-buffer-is-visible "*Group*")
	     (gnus-group-list-groups))))

(setq gnus-select-article-hook
      '(lambda ()
	 (if (this-buffer-is-visible "*Group*")
	     (progn
	       (message "hookin")
	       (tabbar-mode -1)
	       (sit-for 0.1)
	       (gnus-group-get-new-news)))))

(defadvice gnus-topic-select-group (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))

;; (setq gnus-permanently-visible-groups ".*")
;; (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

;; (setq gnus-topic-topology '(("Perso" visible) (("Adamweb" visible nil nil)) (("News" visible nil nil))))


;; (if (string-match "z" "z") "●" "◯")

;; (nth 0 (buffer-list (car (visible-frame-list))))

(defun gnus-user-format-function-j (dummy)
  ;; (if (string-match "z" "z") "●" "◯")
  (format "(%s) (%s)" (nth 0 (buffer-list (car (visible-frame-list)))) (nth 1 (buffer-list (car (visible-frame-list)))))
  ;; (substring headers 0 5)
  ;; (setq plop (nth 0 (buffer-list (car (visible-frame-list)))))

 ;;  (let ((to (gnus-extra-header 'To headers)))
;;     (if (string-match "z" to)
;; 	(setq plop "plop")
;; ))
)

(setq gnus-topic-line-format "%i%u&topic-line; %v\n")

;; this corresponds to a topic line format of "%n %A"
(defun gnus-user-format-function-topic-line (dummy)
  (let ((topic-face (if (zerop total-number-of-articles)
			'my-tushi-face
		      'my-tushi-face)))
    (propertize
     (format "%s %d" name total-number-of-articles)
     'face topic-face)))

(setq
 ;; gnus-topic-line-format "%{%n%}\n"
 gnus-group-line-format "%(%* %G %-12y%)\n"
 gnus-summary-line-format (concat
 "%(%* %0{%U%R%z%}"
 "%3{│%}" "%1{%10&user-date;%}" "%3{│%}" ;; date
 "%4{%-20,20f%}"               ;; name
 "%3{│%}"
 "%1{%B%}"
 "%~(max-right 55)~(pad-right 55)s%)\n"))

 ;; gnus-group-line-format "%(%M %G %B %)\n"
 ;; gnus-group-line-format " %M%S%p%P%5y:%B%(%g%)\n"
 ;; This one
 ;; gnus-group-line-format " %(%G:%N %M%)\n"
 ;; gnus-group-line-format " %G %N %B\n"
 ;; gnus-group-line-format "%P|%B|%M%o%S%L[%6t|%3i]%6y :%(%~(pad-right 65)g%):%6,6~(cut 2)d\n"

;; Test
(defun gnus-user-format-function-t (dummy)
  (format "%d" 05)
  )

;; Works
;; (setq gnus-group-line-format " %(%G:%N %M%)\n")

;; (defun gnus-user-format-function-j ()
;; (message "plop"))

;; (defvar *jao-mails*
;;         "jao@foo\\.org\\|jao@baz\\.com\\|jao@grogle\\.com")

;; (defun gnus-user-format-function-j (headers)
;;   (let ((to (gnus-extra-header 'To headers)))
;;     (if (string-match *jao-mails* to)
;;         (if (string-match "," to) "~" "»")
;;         (if (or (string-match *jao-mails*
;;                               (gnus-extra-header 'Cc headers))
;;                 (string-match *jao-mails*
;;                               (gnus-extra-header 'BCc headers)))
;;             "~"
;;             " "))))

;; (defun gnus-user-format-function-x (header)
;; (setq plop header)
;; )

;; (setq gnus-topic-line-format "%([%{%n%} %A]%)\n")

(setq nntp-authinfo-file "~/.authinfo.pgp")

;; (setq gnus-select-method
;;       '(nntp "news.sunsite.dk")
;;       )

;; (setq gnus-select-method
;;       '(nntp "news.eternal-september.org")
;;       )

(setq gnus-select-method '(nnnil ""))

;; ;; Offline
(setq gnus-secondary-select-methods
      '(
	(nntp "news"
	      (nntp-address "news.eternal-september.org")
	      ;; (nntp-xref-number-is-evil t)
	      )

	(nnmaildir "gmail-pcm"
		   (directory "~/.mail/perso/")
		   (directory-files nnheader-directory-files-safe)
		   (get-new-mail nil)
		   )
	(nnmaildir "adamweb"
		   ;; (remove-prefix "INBOX.")
		   (directory "~/.mail/adamweb/")
		   (directory-files nnheader-directory-files-safe)
		   (get-new-mail nil)
		   )
	)
      )

;; Online
;; (setq gnus-secondary-select-methods
;;       '(
;; 	;; (nntp "news"
;; 	;;       (nntp-address "news.sunsite.dk")
;; 	;;       )
;; 	(nnimap "gmail-pcm"
;; 		(nnimap-address "imap.gmail.com")
;; 		;; (nnimap-authinfo-file "~/.authinfo.gpg")
;; 		(nnimap-stream ssl)
;; 		(nnir-search-engine imap)
;; 		)
;; 	(nnimap "adamweb"
;; 	       ;; (remove-prefix "INBOX.")
;; 	       (nnimap-stream ssl)
;; 	       (nnimap-address "mail.gandi.net")
;; 	       ;; (nnimap-authinfo-file "~/.authinfo.gpg")
;; 	       (nnimap-authenticator cram-md5)
;; 	       (nnimap-server-port 993)
;; 	       (nnir-search-engine imap)
;; 	       )
;;        )
;;    )


;; This for setting the "from" field depending on the group we're on
(setq gnus-parameters
      '(
	("news"
         (gnus-group-line-format
          "(% %c %)\n")
	 (modeline-notify . t)
	 (visible . t)
	 (display . 100)
	 (posting-style
	  (address "philippe.coatmeur@gmail.com")
	  (signature "Philippe M. Coatmeur
+212(0)6 10 64 73 72")
	  (name "Philippe M. Coatmeur")
	  ))

	("INBOX"
	 ;; (gnus-use-adaptive-scoring nil)
	 ;; (gnus-use-scoring nil)
	 ;; (visible . t)
         (gnus-use-scoring nil)
	 (display . all)
	 (visible . t)
	 (modeline-notify . t))

	("gmail"
	 ;; (display . all)
	 ;; (modeline-notify . t)
	 (posting-style
	  (address "philippe.coatmeur@gmail.com")
	  (signature "Philippe M. Coatmeur
+212(0)6 10 64 73 72")
	  (name "Philippe M. Coatmeur")
	  ))

	("adamweb"
	 ;; (display . all)
	 (posting-style
	  (address "contact@adamweb.net")
	  (signature "http://adamweb.net - Full-service web agency
+212(0)6 10 64 73 72 - +212(0)5 37 78 55 46")
	  (name "Adamweb")
	  ;; (body "\n\n\n Sivaram A\n -- \n")
	  ;; (eval (setq message-sendmail-extra-arguments '("-a" "neo")))
	  ))

	))

;; Vars
(setq
 mm-discouraged-alternatives '("text/html" "text/richtext")
 gnus-fetch-old-headers t
 gnus-large-newsgroup nil
 ;; user-mail-address "philippe.coatmeur@gmail.com"
 ;; user-full-name "Philippe M. Coatmeur"
 gnus-always-force-window-configuration t
 gnus-read-active-file nil
 mm-inline-large-images t
 gnus-always-read-dribble-file nil
 gnus-show-threads t
 gnus-use-cross-reference nil
 gnus-nov-is-evil nil
 gnus-check-new-newsgroups nil
 gnus-check-bogus-newsgroups nil
 gnus-no-groups-message "No news is good news"
 gnus-save-newsrc-file t
 gnus-agent-go-online t
 gnus-agent-queue-mail nil
 message-signature t
 ;; gnus-treat-body-boundary nil
 )
(add-to-list 'mm-attachment-override-types "image/.*")
;; (when (fboundp 'turn-on-gnus-mailing-list-mode)
;;   (add-hook 'gnus-summary-mode-hook 'turn-on-gnus-mailing-list-mode))

;; Hooks & Keys

(defun gnus-hook-px ()
  "A nice gnus session"
  ;; (menu-bar-mode)
  (scroll-bar-mode -1)
  (tabbar-mode -1))

(setq gnus-Select-group-hook
      '(lambda ()
         ;; First of all, sort by date.
         (gnus-sort-headers
          '(lambda (a b)
             (gnus-date-lessp (gnus-header-date a)
                              (gnus-header-date b))))
	 (scroll-bar-mode -1)
	 (tabbar-mode -1)
	 ))

(defun alert-me ()
  (setq inbox "plop")
  (el-get-notify (format "New mail in %s" inbox)
		 "Click on the mailbox icon to open it"))

(defun skipit ()
  (other-window 1))

(eval-after-load "gnus-group"
  '(progn
     (define-key gnus-summary-mode-map [(meta up)] '(lambda() (interactive) (scroll-other-window -1)))
     (define-key gnus-summary-mode-map [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))
     (define-key gnus-group-mode-map (kbd "²") 'other-window)
     (define-key gnus-summary-mode-map (kbd "<backtab>") (lambda ()
							   (interactive)
							   (other-window 2)))
     (define-key gnus-summary-mode-map (kbd "\177") 'gnus-summary-delete-article)
     (define-key gnus-summary-mode-map [delete] 'gnus-summary-delete-article)))

(define-key gnus-summary-mode-map "-" 'gnus-summary-hide-thread)
(define-key gnus-summary-mode-map "+" 'gnus-summary-show-thread)

(add-hook 'message-mode-hook 'turn-on-auto-fill)
(add-hook 'message-mode-hook 'gnus-hook-px)
(add-hook 'summary-mode-hook 'gnus-hook-px)
(add-hook 'gnus-group-mode-hook 'gnus-hook-px)

(add-hook 'nnmaildir-read-incoming-hook 'alert-me)

(add-hook 'gnus-started-hook 'gnus-mst-show-groups-with-new-messages)

;; Summary format stuff
(setq
 gnus-user-date-format-alist
 '(((gnus-seconds-today) . "     %k:%M")		;dans la journée = 14:39
   ((+ 86400 (gnus-seconds-today)) . "Yest %k:%M") ;hier = hier 14:39
   ((+ 604800 (gnus-seconds-today)) . "%a %k:%M") ;dans la semaine = sam 14:39
   ((gnus-seconds-month) . "%a %d")	       ;ce mois  = sam 28
   ((gnus-seconds-year) . "%d %b")		       ;durant l'année = mai 28
   (t . "%d %b '%y"))			       ;le reste = mai 28 '05

 ;; gnus-thread-indent-level 2	;threads indentation

 ;; gnus-article-sort-functions '((not gnus-article-sort-by-date)) ;sorting
 ;; gnus-thread-sort-functions  '((not gnus-thread-sort-by-date))

 gnus-summary-same-subject ""
 gnus-permanently-visible-groups "^.*"

 gnus-use-trees nil		;no thread tree buffers
 gnus-generate-tree-function 'gnus-generate-horizontal-tree
 gnus-tree-minimize-window t
 gnus-thread-hide-subtree t	;auto collapse

 gnus-sum-thread-tree-indent          "  "
 gnus-sum-thread-tree-root            "● "
 gnus-sum-thread-tree-false-root      "◎ "
 gnus-sum-thread-tree-single-indent   "◯ "
 gnus-sum-thread-tree-leaf-with-other "├─► "
 gnus-sum-thread-tree-vertical        "│ "
 gnus-sum-thread-tree-single-leaf     "╰─► "

 ;; gnus-summary-line-format "%U%R%z%12&user-date; %(%[%-30,30f%]%) %B %s\n"

;; gnus-summary-line-format
;; (concat "%(%U%R %~(pad-right 2)t%* %12&user-date; %B%~(max-right 30)~(pad-right 30)n  "
;; 	 "%~(max-right 50)~(pad-right 10)s%)\n")


;; gnus-summary-line-format
;; (concat "%(%U%R %~(pad-right 2)t%* %12&user-date; %B│%~(max-right 15)~(pad-right 15)n  "
;; 	 "%~(max-right 69)~(pad-right 69)s%)\n")


)

(setq gnus-visible-headers
      '("^From:" "^Subject:" "^To:" "^Cc:" "^Resent-To:" "^Message-ID:"
        "^Date:" "^X-Sent:" "^Newsgroups:" "^Organization:" "Followup-To:"
        "Reply-To:" "^X-Newsreader:" "^X-Mailer:"
        "X-Mail-Count" "User-Agent"
        "X-Spam-Status" "^X-Spam-Level:" "Archived-At"))


;; Image handling
(condition-case nil
    (progn (require ‘w3m nil t)
	   (setq mm-text-html-renderer ‘w3m
		 mm-inline-text-html-with-images t
		 mm-w3m-safe-url-regexp nil
		 mm-inline-large-images nil))
  (error nil))

;; Functions
(defun jidanni-gnus-summary-first-unseen-or-last-subject ()
  "Place the point on the subject line of the first unseen article.
If all article have been seen, on the subject line of the last article."
  (interactive)
  (prog1
      (unless
	  (when (gnus-summary-first-subject nil nil t)
	    (gnus-summary-show-thread)
	    (gnus-summary-first-subject nil nil t))
	(goto-char (point-max))
	(forward-line -1))
    (gnus-summary-position-point)))
(setq gnus-auto-select-subject 'jidanni-gnus-summary-first-unseen-or-last-subject)

(defun Chk-mail-px ()
  "Sync IMAP, Get new mails, update modeline"
  (interactive)
  ;; (gnus-group-get-new-news)
  (gnus-summary-rescan-group 'all)
  ;; (gnus-mst-show-groups-with-new-messages)
  )

;; tells gnus to get new mail and also display all old mail
(define-key gnus-summary-mode-map (kbd "s-m") 'Chk-mail-px)


;; Set the SMTP host depending on the "To:" field of the mail we're replying to
(defun send-this-biatch-px ()
  "Sets the \"from\" field depending on the \"To:\" field of the mail we're replying to"
  ;; (interactive)
  (let (fromfield)
    (setq fromfield (save-excursion
		      (save-restriction
			(message-narrow-to-headers)
			(message-fetch-field "from"))))
    (if (string-match "\\`philippe" fromfield)
	(progn (setq message-send-mail-function 'smtpmail-send-it
		     smtpmail-smtp-server "smtp.gmail.com"
		     smtpmail-default-smtp-server "smtp.gmail.com"
		     smtpmail-smtp-service 587
		     smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
	       (message "yowa, %s" fromfield)
	       (message-send-and-exit))

      (progn (setq message-send-mail-function 'smtpmail-send-it
		   smtpmail-smtp-server "mail.gandi.net"
		   smtpmail-default-smtp-server "mail.gandi.net"
		   smtpmail-smtp-service 587
		   smtpmail-starttls-credentials '(("mail.gandi.net" 587 nil nil)))
	     (message "yowa, %s" fromfield)
	     (message-send-and-exit)))))

(defun send-mail-px (bool)
  "Send mail by C-RETURN"
  (interactive
   (list
    (y-or-n-p "Send this mail? ")))
  (if bool (send-this-biatch-px)))

(global-set-key (kbd "<C-return>") 'send-mail-px)


;; SMTP configs.
(require 'smtpmail)

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil
      smtpmail-debug-info t
      smtpmail-debug-verb t)

(setq starttls-use-gnutls t
      starttls-gnutls-program "gnutls-cli"
      starttls-extra-arguments '("--insecure"))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "imap.gmail.com"
      smtpmail-default-smtp-server "imap.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-starttls-credentials '(("imap.gmail.com" 587 nil nil)))

;; (gnus-add-configuration
;;  '(group
;;    (horizontal 1.0
;;                (vertical 22 (group 1.0 point))
;;                (vertical 1.0 (summary 1.0)))))

(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 22 (group 1.0))
               (vertical 1.0 (summary 1.0 point)))))

(gnus-add-configuration
 '(article
   (horizontal 1.0
               (vertical 22 (group 1.0))
               (vertical 1.0
                         (summary 0.35 point)
                         (article 1.0)))))

(gnus-add-configuration
 '(reply
   (horizontal 1.0
               (vertical 22 (group 1.0))
               (vertical 1.0
                         (summary 0.35)
                         (reply 1.0 point)))))

(gnus-add-configuration
 '(reply-yank
   (horizontal 1.0
               (vertical 22 (group 1.0))
               (vertical 1.0
                         (summary 0.35)
                         (reply-yank 1.0 point)))))

(gnus-add-configuration
 '(forward
   (horizontal 1.0
               (vertical 22 (group 1.0))
               (vertical 1.0
                         (summary 0.35)
                         (forward 1.0 point)))))


(message "%s loaded" (buffer-file-name))
