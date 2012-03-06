;; ==========================================================================
;; Time-stamp: <.gnus.el - Tue 06-Mar-2012 18:17:41>
;; ===========================================================================
;; Remember to install gnutls!!
(load "starttls")
(load-library "smtpmail")
(gnus-demon-add-handler 'gnus-demon-scan-news 1 t) ; this does a call to gnus-group-get-new-news

(add-hook 'message-mode-hook 'turn-on-auto-fill)

;; Image handling
(condition-case nil
    (progn (require ‘w3m nil t)
	   (setq mm-text-html-renderer ‘w3m
		 mm-inline-text-html-with-images t
		 mm-w3m-safe-url-regexp nil
		 mm-inline-large-images nil))
  (error nil))

;; Go to the bottom
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

(setq
 ;; The threading is not based on subject
 gnus-thread-ignore-subject t
 ;; when the root is lost
 gnus-summary-make-false-root `adopt
 ;; How to save articles
 gnus-default-article-saver `gnus-summary-save-in-file

 gnus-unbuttonized-mime-types nil
 gnus-use-full-window nil
 mm-inline-large-images t
 gnus-always-read-dribble-file t
 ;; gnus-show-threads nil
 ;; gnus-use-cross-reference nil
 gnus-nov-is-evil nil
 gnus-check-new-newsgroups nil
 gnus-check-bogus-newsgroups nil
 gnus-no-groups-message "No news is terrrible news"
 ;; This does not seem to work
 gnus-asynchronous t
 gnus-use-cache t
 ;; visible . t
 ;; display . all
 ;; gnus-group-line-format  "%M%S%p%P%5y:%B%(%g%)\n"
 gnus-group-line-format  "%M%5y:%B%(%G%)\n"
 ;; gnus-group-line-format "%M:%G\n"
 gnus-save-killed-list nil
 ;; gnus-check-new-newsgroups nil
 ;; gnus-save-newsrc-file nil
 gnus-agent-go-online t
 gnus-agent-queue-mail nil
 ;; imap-store-password t
 ;; gnus-use-dribble-file nil
 message-signature t
 message-signature-file "~/.signature"
 ;; gnus-treat-body-boundary nil
 gnus-inhibit-mime-unbuttonizing t
 gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed")
)

(setq gnus-visible-headers
      '("^From:" "^Subject:" "^To:" "^Cc:" "^Resent-To:" "^Message-ID:"
        "^Date:" "^X-Sent:" "^Newsgroups:" "^Organization:" "Followup-To:"
        "Reply-To:" "^X-Newsreader:" "^X-Mailer:"
        "X-Mail-Count" "User-Agent"
        "X-Spam-Status" "^X-Spam-Level:" "Archived-At"))

(setq gnus-summary-line-format
      (concat
       "%( %0{%U%R%z%}"
       "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s %)\n"))

;; gnus-summary-line-format "%U%R%z %12&user-date; %(%[%-30,30f%]%) %B %s\n"

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

;; My C-RETURN send mail
(defun xx-send-mail (bool)
  (interactive
   (list
    (y-or-n-p "Send this mail? ")))
  (if bool (message-send-and-exit)))

(global-set-key (kbd "<C-return>") 'xx-send-mail)

(require 'nnir)

(setq
 gnus-parameters
 '(("imap.*"
    (gnus-use-adaptive-scoring nil)
    (gnus-use-scoring nil)
    (display . 200)
    (visible . t)
    (display . all)
    )))

(setq gnus-select-method
      '(nntp "news.eternal-september.org"))


;; Online
;; (setq gnus-secondary-select-methods
;;       '(
;; 	(nnimap "gmail-pcm"
;; 	       (nnimap-address "imap.gmail.com")
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

;; Offline
(setq gnus-secondary-select-methods
      '(
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

;; Splitting & "from" setup
(setq nnimap-split-inbox "INBOX") ;; (1)
;; (setq nnimap-split-predicate "UNDELETED") ;; (2)
(setq nnimap-split-rule
      '(
	("adamweb" "^To:.*contact@adamweb.net")
	("P.Coatmeur" "^To:.*philippe.coatmeur@gmail.com")
        ))

(setq gnus-posting-styles
      '(((header "to" "contact@adamweb.net")
         (address "contact@adamweb.net")
	 ("X-SMTP-Server" "mail.gandi.net")
	 )
	((header "to" "philippe.coatmeur@gmail.com")
	 ("X-SMTP-Server" "smtp.gmail.com")
         (address "philippe.coatmeur@gmail.com")
	 )))

;; Default smtpmail.el configurations.
(require 'cl)
(require 'smtpmail)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil
      ;; user-full-name "Philippe M. Coatmeur"
      smtpmail-debug-info t
      smtpmail-debug-verb t)

(defun set-smtp (mech server port user password)
  "Set related SMTP variables for supplied parameters."
  (setq smtpmail-smtp-server server
	smtpmail-smtp-service port
	smtpmail-auth-credentials (list (list server port user password))
	smtpmail-auth-supported (list mech)
	smtpmail-starttls-credentials nil)
  (message "Setting SMTP server to `%s:%s' for user `%s'."
	   server port user))

(defun set-smtp-ssl (server port user password  &optional key cert)
  "Set related SMTP and SSL variables for supplied parameters."
  (setq starttls-use-gnutls t
	starttls-gnutls-program "gnutls-cli"
	starttls-extra-arguments nil
	smtpmail-smtp-server server
	smtpmail-smtp-service port
	smtpmail-auth-credentials (list (list server port user password))
	smtpmail-starttls-credentials (list (list server port key cert)))
  (message
   "Setting SMTP server to `%s:%s' for user `%s'. (SSL enabled.)"
   server port user))

(defun change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (loop with from = (save-restriction
			(message-narrow-to-headers)
			(message-fetch-field "from"))
	  for (auth-mech address . auth-spec) in smtp-accounts
	  when (string-match address from)
	  do (cond
	      ((memq auth-mech '(cram-md5 plain login))
	       (return (apply 'set-smtp (cons auth-mech auth-spec))))
	      ((eql auth-mech 'ssl)
	       (return (apply 'set-smtp-ssl auth-spec)))
	      (t (error "Unrecognized SMTP auth. mechanism: `%s'." auth-mech)))
	  finally (error "Cannot infer SMTP information."))))

;; In order to trigger CHANGE-SMTP before every SMTPMAIL-VIA-SMTP call, we introduce an advice as follows.
(defadvice smtpmail-via-smtp
  (before smtpmail-via-smtp-ad-change-smtp (recipient smtpmail-text-buffer))
  "Call `change-smtp' before every `smtpmail-via-smtp'."
  (with-current-buffer smtpmail-text-buffer (change-smtp)))

(ad-activate 'smtpmail-via-smtp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window configuration.
;; see `http://www-verimag.imag.fr/~moy/emacs/.gnus.el'
(if (eq window-system 'x)
    (progn
      (gnus-add-configuration
       '(summary
      	 (horizontal 1.0
      		     (vertical 0.25 (group 1.0))
      		     (summary 1.0))))

      (gnus-add-configuration
       '(article
      	 (horizontal 1.0
      		     (vertical 0.25 (group 1.0))
      		     (vertical 1.0 (summary 0.25) (article 1.0)))))

      (gnus-add-configuration
       '(reply-yank
      	 (horizontal 1.0
      		     (vertical 0.25 (group 1.0))
      		     (vertical 1.0 (summary 0.25) (message 1.0 point)))))

      (gnus-add-configuration
       '(reply
	 (vertical 1.0
		   (article 0.3)
		   (message 1.0 point)))))

  (gnus-add-configuration
   '(article
     (vertical 1.0
	       (summary 0.3 point)
	       (article 1.0)))))

;; affichage de la date en relatif
(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "     %k:%M")		;dans la journée = 14:39
	((+ 86400 (gnus-seconds-today)) . "Yest %k:%M") ;hier = hier 14:39
	((+ 604800 (gnus-seconds-today)) . "%a %k:%M") ;dans la semaine = sam 14:39
	((gnus-seconds-month) . "%a %d")	       ;ce mois  = sam 28
	((gnus-seconds-year) . "%d %b")		       ;durant l'année = mai 28
	(t . "%d %b '%y"))			       ;le reste = mai 28 '05

      gnus-thread-indent-level 2	;threads indentation

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
      gnus-sum-thread-tree-single-leaf     "╰─► ")

(define-key gnus-summary-mode-map "-" 'gnus-summary-hide-thread)
(define-key gnus-summary-mode-map "+" 'gnus-summary-show-thread)

;; Enable mailinglist support
;; (when (fboundp 'turn-on-gnus-mailing-list-mode)
;;   (add-hook 'gnus-summary-mode-hook 'turn-on-gnus-mailing-list-mode))
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
;; Inline images?
;; (setq mm-attachment-override-types '("image/.*"))

;; Or, like this:
(add-to-list 'mm-attachment-override-types "image/.*")
