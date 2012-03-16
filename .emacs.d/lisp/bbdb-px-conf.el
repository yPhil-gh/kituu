;;; bbdb-px-conf.el --- My bbdb config

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

(message "%s loaded" (buffer-file-name))
(provide 'bbdb-px-conf)
