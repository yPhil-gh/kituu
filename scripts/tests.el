(setq test-string "plop")

(defface my-tushi-face
  '((t
     :family "Metal"
     ))
  "Face used for topics."
  :group 'faces)

(set-face-attribute 'my-tushi-face nil :family "Metal")
(set-face-attribute 'my-tushi-face nil :foreground "goldenrod" :weight 'ultra-bold)

(insert (propertize
	(format "%s" test-string)
	'face 'my-tushi-face))

(insert (propertize "Gah! I'm green!" 'face '(:inherits nil :weight 'ultra-bold :foreground "#00ff00" :background "#005000")))

(defcustom foo '(("account1"
                  (key1 "value1")
                  (key2 "value2")
                  (key3 "value3")
                  (key4 "value4")
                  (key5 "value5")))
  "The inimitable foo."
  :type '(repeat (list
                  (string :tag "Account")
                  (list (const key1) (string :tag "Key1 Value"))
                  (list (const key2) (string :tag "Key2 Value"))
                  (list (const key3) (string :tag "Key3 Value"))
                  (list (const key4) (string :tag "Key4 Value"))
                  (list (const key5) (string :tag "Key5 Value")))))


(defcustom imapua-imap-accounts-tests
		'(("account1"
					(login "value4")
					(key5 "value5")))
  "The inimitable foo."
  :type '(repeat (list
                  (string :tag "Account")
                  ( (const login) (string :tag "Login Value"))
                  ( (const key5) (string :tag "Key5 Value")))))

(defcustom person-data '(("brian"  50 t)
																									("dorith" 55 nil)
																									("ken"    52 t))
		"Alist of basic info about people.
          Each element has the form (NAME AGE MALE-FLAG)."
		:type '(alist :value-type (group integer boolean)))

(setq login "plop")

(list (const login) (string :tag "Login Value"))

(defcustom imapua-imap-accounts
		'(("account1"
					(host "value1")
					(type "value2")
					(port "value3")
					(login "value4")
					(key5 "value5")))
  "The inimitable foo."
  :type '(repeat (list
                  (string :tag "Account")
                  (list (const host) (string :tag "Host Name"))
                  (list (const type) (string :tag "Connection type"))
                  (list (const port) (string :tag "Port Number"))
                  (list (const login) (string :tag "Login"))
                  (list (const key5) (string :tag "Key5 Value")))))


(assert (setq imapua-connection (imap-open "imap.gmxs.com" 993 'ssl)) nil "the imap connection could not be opened")
(imap-authenticate imapua-username imapua-password imapua-connection)

(defun auth-credentials ()
		(let*
						((machine (read-from-minibuffer "IMAP server host name: "))
							(port (read-from-minibuffer "IMAP server port: "))

							;; (assert (setq imapua-connection (imap-open machine port 'ssl)) nil "the imap connection could not be opened")

							(auth-source-creation-prompts
								'((user  . "IMAP user at %h: ")
										(secret . "IMAP password for %u@%h: ")))
							(found (nth 0 (auth-source-search :max 1
																																									:host machine
																																									;; :port port
																																									;; :require '(:user :secret)
																																									:require '(:user :secret )
																																									:create t))))
				(if found
								(progn
										(list (plist-get found :machine)
																(let ((secret (plist-get found :secret)))
																		(if (functionp secret)
																						(funcall secret)
																				secret))
																(message "and the secret is %s" (car secret))
																(plist-get found :save-function))
										)
						(message "not found")
						nil))
		)

(progn
		(funcall (nth 2 (auth-credentials)))
		(find-file "~/.authinfo")
		(write-file "~/.authinfo.gpg" nil)
		(delete-file "~/.authinfo")
		(kill-buffer ".authinfo.gpg")
)


(setq imapua-connection (imap-open imapua-host imapua-port 'ssl))
(imap-authenticate imapua-username imapua-password imapua-connection)
