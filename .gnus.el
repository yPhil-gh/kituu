;; ==========================================================================
;; Time-stamp: <.gnus.el - Sun 11-Mar-2012 18:57:54>
;; ===========================================================================
;; Remember to install gnutls!!
(load "starttls")
;; (load-library "smtpmail")
(gnus-demon-init)
;; (gnus-demon-add-handler 'chk-all 5 nil) ; One minute
;; (gnus-demon-add-rescan)

(defun chk-all ()
(message "checking...")
;; (start-offlineimap)
(gnus-demon-scan-news)
)

(require 'olimap)
(require 'nnir)

(setq gnus-visual t)

;; Topics
(setq gnus-topic-indent-level 0)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;; (setq gnus-permanently-visible-groups ".*")
;; (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

;; (setq gnus-topic-topology '(("Perso" visible) (("Adamweb" visible nil nil)) (("News" visible nil nil))))

(setq
 ;; gnus-group-line-format "%(%M %G %B %)\n"
 ;; gnus-group-line-format " %M%S%p%P%5y:%B%(%g%)\n"
 ;; This one
 ;; gnus-group-line-format " %(%G:%N %M%)\n"
 ;; gnus-group-line-format " %G %N %B\n"
 ;; gnus-group-line-format "%P|%B|%M%o%S%L[%6t|%3i]%6y :%(%~(pad-right 65)g%):%6,6~(cut 2)d\n"
)

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

;; Test
(defun gnus-user-format-function-t (dummy)
  (format "%d" 05)
)

;; Works
(setq gnus-group-line-format " %(%G:%N %M%)\n")

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
	      (nntp-address "news.sunsite.dk")
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

	("nnmaildir\\+gmail"
	 ;; (display . all)
	 ;; (modeline-notify . t)
	 (posting-style
	  (address "philippe.coatmeur@gmail.com")
	  (signature "Philippe M. Coatmeur
+212(0)6 10 64 73 72")
	  (name "Philippe M. Coatmeur")
	  ))

	("nnmaildir\\+adamweb"
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
 gnus-large-newsgroup 'nil
 user-mail-address "philippe.coatmeur@gmail.com"
 user-full-name "Philippe M. Coatmeur"
 gnus-always-force-window-configuration t
 ;; gnus-read-active-file nil
 mm-inline-large-images t
 gnus-always-read-dribble-file t
 gnus-show-threads t
 gnus-use-cross-reference nil
 gnus-nov-is-evil nil
 gnus-check-new-newsgroups nil
 gnus-check-bogus-newsgroups nil
 gnus-no-groups-message "No news is terrrible news"
 ;; gnus-group-line-format  "%M%5y:%B%(%G%)\n"
 ;; gnus-group-line-format  " %y:%B%(%G%)\n"
 ;; gnus-group-line-format  "%B%(%G%)\n"
 gnus-save-newsrc-file t
 gnus-agent-go-online t
 gnus-agent-queue-mail nil
 message-signature t
 ;; gnus-treat-body-boundary nil
 )

;; Hooks & Keys

(defun gnus-hook-px ()
  "A nice gnus session"
  ;; (menu-bar-mode)
  (scroll-bar-mode -1)
  (tabbar-mode -1))

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

 gnus-summary-line-format (concat
 			   "%( %0{%U%R%z%}"
 			   "%3{│%}" "%1{%10&user-date;%}" "%3{│%}" ;; date
 			   "%4{%-20,20f%}"               ;; name
 			   "%3{│%}"
 			   "%1{%B%}"
			   "%~(max-right 69)~(pad-right 69)s%)\n"
			   )

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

;; (setq gnus-summary-line-format
;;       (concat
;;        "%( %0{%U%R%z%}"
;;        "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
;;        "  "
;;        "%4{%-20,20f%}"               ;; name
;;        "  "
;;        "%3{│%}"
;;        " "
;;        "%1{%B%}"
;;        "%s %)\n"))

(defun Chk-mail-px ()
  "Sync IMAP, Get new mails, update modeline"
  (interactive)
  ;; (gnus-group-get-new-news)
  (gnus-summary-rescan-group 'all)
  ;; (gnus-mst-show-groups-with-new-messages)
  )

;; tells gnus to get new mail and also display all old mail
(define-key gnus-summary-mode-map (kbd "s-m") 'Chk-mail-px)


;; This for setting the SMTP host depending on the "To:" field of the mail we're replying to
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
(require 'cl)
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

;; Window configuration - http://gnus.org/manual/gnus_289.html
(gnus-add-configuration
 '(article
   (horizontal 1.0
               (vertical 24 (group 1.0))
               (vertical 1.0
                         (summary 0.35 point)
                         (article 1.0)))))

(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 24 (group 1.0))
               (vertical 1.0 (summary 1.0 point)))))

;; Enable mailinglist support
;; (when (fboundp 'turn-on-gnus-mailing-list-mode)
;;   (add-hook 'gnus-summary-mode-hook 'turn-on-gnus-mailing-list-mode))
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
;; Inline images?
;; (setq mm-attachment-override-types '("image/.*"))

;; Or, like this:
(add-to-list 'mm-attachment-override-types "image/.*")

;; Garbage
;; Jumps point to ":". %( %) will be be highlighted on mouse-over.
;; %U = mark (eg "R")
;; %R = secondary mark. Indicates if replied to, cached, or saved.
;; %z = zcore. "+"/"-" if above/below default score.
;; %i = score as a number
;; %3i    => minimum of 3 characters, right-justify
;; %-3i   => minimum of 3 characters, left-justify
;; %3,6i  => minimum of 3 characters, maximum of 6, right-justify
;; %-3,6i => minimum of 3 characters, maximum of 6, left-justify
;; %t = articles in thread
;; %I = start of thread indentation
;; %B = complex threading.
;; %s = subject if the article is the root of the thread
;; %n = name
;; %L = number of lines
;; %D = date
;; %n{ %} = gnus-face-n
;; %&user-date; - see `gnus-user-date-format-alist'.

;; ;; This HAS to come AFTER the previous rules
;; (setq gnus-parameters
;;  '(("INBOX"
;;     ;; (gnus-use-adaptive-scoring nil)
;;     ;; (gnus-use-scoring nil)
;;     ;; (visible . t)
;;     (display . all)
;;     (modeline-notify . t)
;;     )))

;; nnmaildir+adamweb:INBOX
