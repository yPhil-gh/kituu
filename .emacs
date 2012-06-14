;;; See https://github.com/xaccrocheur/kituu/
;; Keep it under 1k lines ;p

;; Init! ______________________________________________________________________

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; GNU
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/bbdb/")
;; (autoload 'wl "wl" "Wanderlust" t)
;; (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
;; (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; (if (< emacs-major-version 24)
;;     (progn
;;       (load "~/.emacs.d/lisp/nxhtml/autostart.el")
;;       (tabkey2-mode t))
;;   (progn
;;     (require 'php-mode nil t)
;;     (autoload 'php-mode "php-mode" "Major mode for editing php code." t)
;;     ;; (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;;     ;; (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode)
;; ))


;; Experiments
;;----------------------------------------
;;automatic font-lock-mode [TUCKERM Jan2002]
;;----------------------------------------

;; network-speed configuration:
;; (add-to-list 'load-path "/path/to/network-speed.el")

;; (require 'network-speed)
;; (network-speed-start)

;; (setq redisplay-dont-pause t)
;; (setq delete-by-moving-to-trash t)
;; (setq list-colors-sort 'hsv )

(defun notify-send (title message icon)
  (start-process "notify" " notify"
		 libnotify-program "--expire-time=5000"
  "--urgency=low" (concat "--icon=" icon) title message))

;; ;; Message alert hooks
;; (define-jabber-alert echo "Show a message in the echo area"
;;   (lambda (msg)
;;     (unless (minibuffer-prompt)
;;       (message "%s" msg))))

(add-hook 'find-file-hooks 'turn-on-font-lock)

(set-face-attribute 'default nil
:font "Monospace"
:height 125
;; :font "Inconsolata"
;; :width 'normal
:weight 'normal
;; :slant 'reverse-italic
;; :weight 'bold
;; :width 'wide
)

(message "we are at line %s" (line-number-at-pos))

;; (has ital)
;; Liberation Mono-11
;; Liberation Sans-11
;; (no ital)
;; Haramain-13
;; Inconsolata-12

;; (set-default-font
;;  "-*-lucida-*-*-*-*-*-*-*-*-*-*-*-*")

;; (set-face-font "-b&h-lucida-*-i-*-*-11-*-*-*-*-*-*-*")

;; (set-face-attribute 'default nil :family "Inconsolata" :height 140)
;;----------------------------------------
;;default to text-mode with auto-fill at column 75 [TUCKERM Feb2002]
;;----------------------------------------
(setq default-major-mode 'text-mode
  text-mode-hook 'turn-on-auto-fill
  fill-column 75)

(put 'overwrite-mode 'disabled t)

(global-set-key "\C-xE" 'apply-macro-to-region-lines)


  (defun date (&optional insert)
    "Display the current date and time.
  With a prefix arg, INSERT it into the buffer."
    (interactive "P")
    (funcall (if insert 'insert 'message)
             (format-time-string "%a, %d %b %Y %T %Z"
    (current-time))))

  ;; (defconst animate-n-steps 3)
  ;; (defun emacs-reloaded ()
  ;;   (animate-string (concat ";; Initialization successful, welcome to "
  ;; 			  (substring (emacs-version) 0 16)
  ;; 			  ".")
  ;; 		  0 0)
  ;;   (newline-and-indent)  (newline-and-indent))

;; (emacs-reloaded)

  ;; (add-hook 'after-init-hook 'emacs-reloaded)

;; End XPs

;;----------------------------------------
;; Tramp settings
;;----------------------------------------
;; (add-to-list 'load-path "/usr/share/emacs21/site-lisp/tramp")
;; (require 'tramp)
;; (setq tramp-default-method "ssh")
;; ;(setq shell-prompt-pattern ".*\}")
;; (setq shell-prompt-pattern "^[\[].*[\]]*")
;; ;(setq tramp-rcp-args "-C")
;; (setq tramp-ssh-args "-C")
;; (setq tramp-auto-save-directory "~/.emacs_backups")

(setq tabbar-ruler-global-tabbar 't) ; If you want tabbar
;; (setq tabbar-ruler-global-ruler 't) ; if you want a global ruler
;; (setq tabbar-ruler-popup-menu 't) ; If you want a popup menu.
;; (setq tabbar-ruler-popup-toolbar 't) ; If you want a popup toolbar


;; (if (>= emacs-major-version 23)
;;     (set-frame-font "Monospace-12"))

(setq auto-mode-alist (cons '(".php" . php-mode) auto-mode-alist))

(eval-and-compile
(require 'tabbar nil t)
(require 'tabbar-ruler)
;; (require 'powerline)
(require 'mail-bug nil t)
(require 'bbdb nil t)
;; (require 'tabkey2 nil t)
(require 'undo-tree)
(require 'marker-visit)
(require 'cl)
)

(mail-bug-init)

;; Required by my iswitchb hack
(require 'edmacro)

(defvar iswitchb-mode-map)
(defvar iswitchb-buffer-ignore)
(defvar show-paren-delay)
(defvar recentf-max-saved-items)
(defvar recentf-max-menu-items)
(defvar ispell-dictionary)
(defvar yas/trigger-key)
(defvar desktop-path)
(defvar desktop-dirname)
(defvar desktop-base-file-name)

;; (defvar el-get-dir)
;; (defvar el-get-sources)
;; (defvar my-packages)

(defvar px-newName)

(defvar display-time-string)
(defvar gnus-mode-non-string-length)
(defvar ediff-window-setup-function)
(defvar ediff-split-window-function)
(defvar tabbar-buffer-groups-function)

;; (defvar gnus-bury-window-configuration)
;; (defvar gnus-bury-window-configuration)
(defvar px-minibuffer-history)
(defvar savehist-file)

;; Server! ____________________________________________________________________

(server-start)
(defun px-raise-and-focus ()
  (when window-system
    (raise-frame)
    (x-focus-frame (selected-frame))
    (set-mouse-pixel-position (selected-frame) 4 4)
    (delete-other-windows)
    ))
(add-hook 'server-switch-hook 'px-raise-and-focus)


;; Funcs! _________________________________________________________________

(defun px-undo-kill-buffer (arg)
  "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
	(buffer-files-list
	 (delq nil (mapcar (lambda (buf)
			     (when (buffer-file-name buf)
			       (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
	     (delq buf-file recently-killed-list)))
     buffer-files-list)
    (find-file
     (if arg (nth arg recently-killed-list)
       (car recently-killed-list)))))

(defun px-go-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun px-kill-now ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun this-buffer-is-visible (buffer)
  "Test if BUFFER is actually on screen"
  (if (get-buffer buffer)
      (if (get-buffer-window-list buffer)
	  t
	nil)))

(defun byte-compile-user-init-file-px ()
  (let ((byte-compile-warnings '(unresolved)))
    ;; in case compilation fails, don't leave the old .elc around:
    (when (file-exists-p (concat user-init-file ".elc"))
      (delete-file (concat user-init-file ".elc")))
    (byte-compile-file user-init-file)
    (message "%s compiled" user-init-file)
    ))

(defun my-emacs-lisp-mode-hook ()
  (when (string-match "\\.emacs" (buffer-name))
    (add-hook 'after-save-hook 'byte-compile-user-init-file-px t t)))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; (defun compile-init-file-px ()
;;   (let ((byte-compile-warnings '(unresolved)))
;;     (byte-compile-file user-init-file)
;;     (message "%s saved and compiled." user-init-file)))

;; (defun my-emacs-lisp-mode-hook ()
;;   ;; (if (string-equal buffer-file-name user-init-file)
;;   (if (search ".emacs" (buffer-name))
;;       (message "compiling %s" buffer-file-name)
;;       (progn (add-hook 'after-save-hook 'compile-init-file-px t t)
;;     	     )))
;; (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)


(defun make-backup-dir-px (dirname)
  "create backup dir"
  (interactive)
    (if (not (file-exists-p dirname))
        (make-directory dirname t)))
(make-backup-dir-px "~/.bkp/")

(defun bkp-px ()
  "Write the current buffer to a new file - silently - and append the date+time to the filename, retaining extention"
  (interactive)
  (setq px-newName
	(concat
	 (file-name-sans-extension buffer-file-name) "-"
	 (format-time-string  "%Y-%m-%d") "."
	 (format-time-string "%Hh%M") "."
	 (file-name-extension buffer-file-name))
	)
  (write-region (point-min) (point-max) px-newName)
  (message "backuped %s" px-newName)
  )

(defun px-query-replace-in-open-buffers (arg1 arg2)
  "query-replace in open files"
  (interactive "sRegexp:\nsReplace with:")
  (mapcar
   (lambda (x)
     (find-file x)
     (save-excursion
       (goto-char (point-min))
       ;; (beginning-of-buffer)
       (query-replace-regexp arg1 arg2)))
   (delq
    nil
    (mapcar
     (lambda (x)
       (buffer-file-name x))
     (buffer-list)))))

(defun px-fullscreen ()
  "Maximize the current frame (to full screen)"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

(defun px-google-that-bitch (start end)
  "Google selected string"
  (interactive "r")
  (let ((q (buffer-substring-no-properties start end)))
    (browse-url (concat "http://www.google.com/search?&q="
			(url-hexify-string q)))))

(defun select-text-in-quote-px ()
  "Select text between the nearest left and right delimiters."
  (interactive)
  (let (b1 b2)
    (skip-chars-backward "^<>([{“「『‹«（〈《〔【〖⦗〘⦅〚⦃\"")
    (setq b1 (point))
    (skip-chars-forward "^<>)]}”」』›»）〉》〕】〗⦘〙⦆〛⦄\"")
    (setq b2 (point))
    (set-mark b1)))

(global-set-key (kbd "s-SPC") 'select-text-in-quote-px)

(defun insert-bracket-pair (leftBracket rightBracket)
  "Insert a matching bracket and place the cursor between them."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end) ) )
  (insert leftBracket rightBracket)
  (backward-char 1) )

(defun insert-pair-paren () (interactive) (insert-bracket-pair "(" ")") )
(defun insert-pair-brace () (interactive) (insert-bracket-pair "{" "}") )
(defun insert-pair-bracket () (interactive) (insert-bracket-pair "[" "]") )
(defun insert-pair-single-angle () (interactive) (insert-bracket-pair "<" ">") )
(defun insert-pair-single-straight-quote () (interactive) (insert-bracket-pair "'" "'") )
(defun insert-pair-dbquotes () (interactive) (insert-bracket-pair "\"" "\"") )
;; Thanks Xah Lee, my favorite usenet freak, for those last two

(defun px-frigo ()
  (interactive)
  "Copy the current region, paste it in frigo.txt with a time tag, and save this file"
  (unless (use-region-p) (error "No region selected"))
  (let ((bn (file-name-nondirectory (buffer-file-name))))
    (copy-region-as-kill (region-beginning) (region-end))
    (with-current-buffer (find-file-noselect "~/.bkp/Frigo.txt")
      (goto-char (point-max))
      (insert "\n")
      (insert "######################################################################\n")
      (insert "\n"
              (format-time-string "%Y %b %d %H:%M:%S" (current-time))
              " (from "
              bn
              ")\n\n")
      (yank)
      (save-buffer)
      (message "Region refrigerated!"))))

;; (the lock file is ~/.bkp/.emacs.desktop.lock)
(defun px-saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

(defun Session-restore-px ()
  "Restore a saved emacs session."
  (interactive)
  (if (px-saved-session)
      (progn
	;; (delete-file (concat desktop-dirname "/.emacs.desktop.lock"))
	(desktop-read)
	(recenter-top-bottom 15))
    (message "No desktop (session) file found.")))

(defun px-session-save ()
  "Save an emacs session."
  (interactive)
  (if (px-saved-session)
      (if (y-or-n-p "Save session? ")
	  (desktop-save-in-desktop-dir)
	(message "Session not saved."))
  (desktop-save-in-desktop-dir)))

(defun my-test (&optional arg)
  (interactive (list (if current-prefix-arg
                         (read-from-minibuffer "MyPrompt: ")
                       nil)))
  (if arg
      (message arg)
    (message "NO ARG")))

(defun px-session-save-named (px-session-named-name)
  "Prompt the user for a session name."
  (interactive "MSession name: ")
  (message "So what do I do with this: %s ?" px-session-named-name)
  (desktop-save (concat desktop-dirname "/" px-session-named-name
			".session") t)
)

;; (defun px-session-save-named ()
;;   "Save a named emacs session."
;;   (interactive)
;;   (if (px-saved-session)
;;       (if (y-or-n-p "Save session? ")
;; 	  (desktop-save-in-desktop-dir)
;; 	(message "Session not saved."))
;;   (desktop-save-in-desktop-dir)))

;; This will only work for one session
(add-hook 'after-init-hook
	  '(lambda ()
	     (if (px-saved-session)
		 (if (y-or-n-p "Restore session? ")
		     (Session-restore-px)))))

(add-hook 'kill-emacs-hook
	  '(lambda ()
	     (px-session-save)))

;; Modes! ______________________________________________________________________
;; (display-time-mode t)
(tabbar-mode t)
(show-paren-mode t)
(menu-bar-mode -1)
(global-linum-mode t)
;; (global-smart-tab-mode 1)
;; (smart-tab-mode t)
(global-font-lock-mode t)
(tool-bar-mode 0)
(set-scroll-bar-mode `right)
(delete-selection-mode t)
(auto-fill-mode t)
(recentf-mode 1)
;; (mouse-avoidance-mode 'cat-and-mouse)
(iswitchb-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq suggest-key-bindings 1) ; wait 5 seconds

;; This is harsh but just, shortens the war and saves lives.
(setq resize-mini-windows nil)

;; Modal setting (if this mode then this setting)
(add-hook 'custom-mode-hook 'linum-mode -1)

(defun iswitchb-local-keys ()
  (mapc (lambda (K)
	  (let* ((key (car K)) (fun (cdr K)))
	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	'(("<right>" . iswitchb-next-match)
	  ("<left>"  . iswitchb-prev-match)
	  ("<up>"    . ignore             )
	  ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

(setq-default cursor-type 'bar)

(setq c-default-style "bsd"
      c-basic-offset 4)

;; Vars! ______________________________________________________________________

(setq
 vc-make-backup-files t
 iswitchb-buffer-ignore '("^ " "*.")
 ;; scroll-preserve-screen-position t
 ;; scroll-up-aggressively 0.1
 ;; scroll-down-aggressively 0.5
 scroll-conservatively 200
 scroll-margin 3
 recenter-redisplay nil
 inhibit-startup-screen t
 inhibit-startup-echo-area-message t
 ;; user-mail-address "philippe.coatmeur@gmail.com"
 ;; user-full-name "Philippe M. Coatmeur"
 recentf-max-saved-items 120
 recentf-max-menu-items 60
 x-select-enable-clipboard t
 enable-recursive-minibuffers t
 show-paren-delay 0
 ;; ediff-setup-windows-plain t
 ;; tramp-terminal-type dumb
 ispell-dictionary "francais"
 completion-auto-help nil
)

(set-face-foreground 'show-paren-mismatch-face "red")
(set-face-attribute 'show-paren-mismatch-face nil
                    :weight 'bold)

(setq yas/trigger-key (kbd "TAB"))
;; (global-set-key (kbd "C-²") 'yas/expand-from-trigger-key)

;; Desktop
(setq desktop-path '("~/.bkp/"))
(setq desktop-dirname "~/.bkp/")
(setq desktop-base-file-name "emacs-desktop")

;; Ediff
(setq ediff-window-setup-function (quote ediff-setup-windows-plain))
(setq ediff-split-window-function 'split-window-horizontally)


;; Window title (with edited status + remote indication)
(setq frame-title-format
      '("" invocation-name " %@ "(:eval (if (buffer-file-name)
					    (abbreviate-file-name (buffer-file-name))
					  "%b")) " [%*]"))

;; Hooks! _____________________________________________________________________

(add-hook 'wl-summary-mode-hook 'hl-line-mode)
(add-hook 'recentf-dialog-mode-hook 'hl-line-mode)
(add-hook 'perl-mode-hook 'cperl-mode)

(add-hook 'cperl-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c h") 'cperl-perldoc)))

(defun text-mode-hook-px ()
(tabbar-mode t)
(menu-bar-mode -1))

(defun gnus-mode-hook-px ()
(tabbar-mode -1)
;; (menu-bar-mode -1)
)

;; FIXME
(defun info-mode-hook-px ()
  (tabbar-mode t)
  ;; (menu-bar-mode -1)
  )

(add-hook 'text-mode-hook 'text-mode-hook-px)
(add-hook 'gnus-before-startup-hook 'gnus-mode-hook-px)
(add-hook 'gnus-exit-gnus-hook 'text-mode-hook-px)
(add-hook 'lisp-mode-hook 'info-mode-hook-px)

(add-hook 'flyspell-mode-hook 'flyspell-prog-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Keys! ______________________________________________________________________

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-<escape>") 'keyboard-quit)
(global-set-key (kbd "²") 'hippie-expand)

(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
point."
  (interactive "*P")
      (if (and
	   (or (bobp) (= ?w (char-syntax (char-before))))
	   (or (eobp) (not (= ?w (char-syntax (char-after))))))
	  (dabbrev-expand arg)
	(indent-according-to-mode)))

(defun my-tab-fix ()
  (local-set-key [tab] 'indent-or-expand))

(add-hook 'c-mode-hook          'my-tab-fix)
(add-hook 'sh-mode-hook         'my-tab-fix)
(add-hook 'emacs-lisp-mode-hook 'my-tab-fix)

(define-key global-map [(meta up)] '(lambda() (interactive) (scroll-other-window -1)))
(define-key global-map [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))

(define-key global-map [f1] 'delete-other-windows)
(define-key global-map [S-f1] 'px-help-emacs)
(define-key global-map [f2] 'other-window)
(define-key global-map [M-f2] 'swap-buffers-in-windows)
(define-key global-map [f3] 'isearch-forward)
(define-key global-map [f4] 'split-window-horizontally)
(define-key global-map [f5] 'iswitchb-buffer) ;new way
(define-key global-map [f7] 'flyspell-buffer)
(define-key global-map [M-f7] 'flyspell-mode)
(define-key global-map [f10] 'toggle-truncate-lines)
(define-key global-map [f11] 'px-fullscreen)

(global-set-key (kbd "C-c C-g") 'goto-line)
(global-set-key (kbd "s-g") 'px-google-that-bitch)
(global-set-key (kbd "s-r") 'replace-string)
(global-set-key (kbd "s-²") (kbd "C-x b <return>")) ; Keyboard macro!
(global-set-key (kbd "<C-kp-add>") 'kmacro-end-and-call-macro) ; Keyboard macro!
(global-set-key (kbd "s-t") 'sgml-tag)
(global-set-key (kbd "C-s-t") 'sgml-close-tag)
(global-set-key (kbd "s-b") 'px-kill-now)
(global-set-key (kbd "s-p") 'php-mode)
(global-set-key (kbd "s-h") 'html-mode)
(global-set-key (kbd "s-j") 'js-mode)
(global-set-key (kbd "s-o") 'org-mode)
(global-set-key (kbd "s-d") 'wl-draft-mode)
(global-set-key (kbd "s-m") 'kmacro-end-and-call-macro)
(global-set-key (kbd "<s-left>") 'marker-visit-prev)
(global-set-key (kbd "<s-right>") 'marker-visit-next)

;; (define-key global-map [s-k] 'kill-buffer)

(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-d") nil)	; I kept deleting stuff
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-S-o") 'desktop-read)
(global-set-key (kbd "C-S-<mouse-1>") 'flyspell-correct-word)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
(global-set-key (kbd "<C-next>") 'forward-page)
(global-set-key (kbd "<C-prior>") 'backward-page)
(global-set-key (kbd "C-<tab>") 'tabbar-forward)
(global-set-key (kbd "<C-S-iso-lefttab>") 'tabbar-backward)
(global-set-key (kbd "C-\"") 'insert-pair-dbquotes)		;""
(global-set-key (kbd "C-)") 'insert-pair-paren)			;()
(global-set-key (kbd "C-=") 'insert-pair-brace)			;{}
(global-set-key (kbd "C-'") 'insert-pair-brace)			;{}
(global-set-key (kbd "C-(") 'insert-pair-bracket)		;[]
(global-set-key (kbd "C-<") 'insert-pair-single-angle)		;<>

(global-set-key (kbd "M-s") 'save-buffer) ; Meta+s saves !! (see C-h b for all bindings, and C-h k + keystroke(s) for help)
(global-set-key (kbd "M-DEL") 'kill-word)
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-o") 'recentf-open-files)
(global-set-key (kbd "M-d") 'px-toggle-comments)

;; px Minor mode
;; (defvar px-keys-minor-mode-map (make-keymap) "px-keys-minor-mode keymap.")
;; (define-minor-mode px-keys-minor-mode
;;   "A minor mode so that my key settings override annoying major modes."
;;   t " px" 'px-keys-minor-mode-map)
;; (px-keys-minor-mode 1)
;; (define-key px-keys-minor-mode-map (kbd "²") 'hippie-expand) ; Hippie-expand everywhere

;; (defun tabbar-buffer-groups ()
;;   "Return the list of group names the current buffer belongs to.
;; This function is a custom function for tabbar-mode's tabbar-buffer-groups.
;; This function groups all buffers into 3 groups:
;; Those Dired, those user buffer, and those emacs buffer.
;; Emacs buffer are those starting with “*”."
;;   (list
;;    (cond
;;     ((string-equal "*" (substring (buffer-name) 0 1))
;;      "Emacs Buffer"
;;      )
;;     ((eq major-mode 'dired-mode)
;;      "Dired"
;;      )
;;     (t
;;      "User Buffer"
;;      )
;;     )))

;; (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;; (if (not (search "*Group*" (buffer-name)))
;;      (setq px-no-gnus-window-configuration (current-window-configuration))
;;   (set-window-configuration px-no-gnus-window-configuration))

;; Save the minibuffer history
(setq px-minibuffer-history (concat user-emacs-directory "px-minibuffer-history"))
(setq savehist-file px-minibuffer-history)
(when (functionp 'savehist-mode) (savehist-mode 1))

;; (cdr (car backup-directory-alist))

(defun Kill-boring-buffers-px (regexp &optional internal-too)
  "Kill buffers whose name matches REGEXP.
The optional second argument indicates whether to kill internal buffers too."
  ;; (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))

(Kill-boring-buffers-px "*Completions*\\|*Compile\-Log*\\|*.*trace\\|*Help*\\|*RE-Builder*\\|Customize\\|\\.newsrc-dribble\\|*olimap*\\|.*el\\.gz")

;; ;; Kill & copy lines
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, COPY a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, KILL a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Killed line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun px-toggle-comments () ;; function definition
  "If region is set, [un]comments it. Otherwise [un]comments current line."
  (interactive) ;; apparently I need this line so it can move the cursor
  ;; (save-excursion ;; don't mess with mark
  (if (eq mark-active nil) ;; no region active
      (progn (beginning-of-line 1)
	     (set-mark (point))
	     (forward-line)
	     (comment-dwim nil))
    (comment-dwim nil)) ;; [un]comment
  (deactivate-mark)) ;; don't mess with selection

;; Help! ______________________________________________________________________

(defun px-help-emacs ()
  (interactive)
  (princ "* My EMACS cheat cheet

** notes
's' (super) on a PC keyboard, is the 'windows logo' key

*** EMACSEN
Copy to register A                  C-x r s A
Paste from register A               C-x r g A
Clone emacs (!?)                    C-x 5 2
Set bookmark at point               C-x r m RET
Close HTML tag                      sgml-close-tag
Switch to *Messages* buffer         C-h e
capitalize-word                     M-c
kill-paragraph                      C-S-del

*** EDIFF
Next / previous diff                n / p
Copy a diff into b / opposite       a / b
Save a / b buffer                   wa / wb

*** GNUS
Sort summary by author/date         C-c C-s C-a/d
Search selected imap folder         G G
Mark thread read                    T k

*** PHP-MODE
C-c C-f                             Search PHP manual for point.
C-c RET / C-c C-m                   Browse PHP manual in a Web browser.

*** MISC EDITING
M-c		                    capitalize-word		Capitalize the first letter of the current word.
M-u		                    upcase-word		Make the word all uppercase.
M-l		                    downcase-word		Make the word all lowercase.
C-x C-l		                    downcase-region		Make the region all lowercase.
C-x C-u		                    uppercase-region	Make the region all uppercase.

*** RECTANGLES
C-x r k/c                           Kill/clear rectangle
C-x r y                             yank-rectangle (upper left corner at point)
C-x r t string <RET>                Replace rectangle contents with string on each line (string-rectangle).

*** WANDERLUST
T                                   Toggle Threading
d                                   Dispose MSG (mark)
D                                   Delete MSG (mark)
rx                                  Execute marks


*** MACROS
C-x (		                    start-kbd-macro		Start a new macro definition.
C-x )		                    end-kbd-macro		End the current macro definition.
C-x e		                    call-last-kbd-macro	Execute the last defined macro.
M-(number) C-x e	            call-last-kbd-maco	Do that last macro (number times).
C-u C-x (	                    stat-kbd-macro		Execute last macro and add to it.
		                    name-last-kbd-macro	Name the last macro before saving it.
		                    insert-last-keyboard-macro	Insert the macro you made into a file.
		                    load-file			Load a file with macros in it.
C-x q		                    kbd-macro-query		Insert a query into a keyboard macro.
M-C-c		                    exit-recursive-edit		Get the hell out of a recursive edit.

*** THIS VERY EMACS CONFIG
Save buffer                         M-s
Kill current buffer                 s-b
Undo                                C-z
Open file                           C-o
Open recent file                    M-o
Close other window (frame)          F1
Switch to other window (frame)      F2
Split horizontally                  F3
Split vertically                    F4
Switch to buffer                    F5
Spell-check buffer                  F7
Word-wrap toggle                    F10
enclose region in <tag> (sgml-tag)  s-t RET tag [ args... ]
select 'this' or <that> (enclosed)  s-SPC

*** VERSION CONTROL
C-x v v                                               vc-next-action
perform the next logical control operation on file
C-x v i                                               vc-register
add a new file to version control

C-x v +                                               vc-update
Get latest changes from version control
C-x v ~                                               vc-version-other-window
look at other revisions
C-x v =                                               vc-diff
diff with other revisions
C-x v u                                               vc-revert-buffer
undo checkout
C-x v c                                               vc-cancel-version
delete latest rev (look at an old rev and re-check it)

C-x v d                                               vc-directory
show all files which are not up to date
C-x v g                                               vc-annotate
show when each line in a tracked file was added and by whom
C-x v s                                               vc-create-snapshot
tag all the files with a symbolic name
C-x v r                                               vc-retrieve-snapshot
undo checkouts and return to a snapshot with a symbolic name

C-x v l                                               vc-print-log
show log (not in ChangeLog format)
C-x v a                                               vc-update-change-log
update ChangeLog

C-x v m     vc-merge
C-x v h     vc-insert-headers

M-x                                                   vc-resolve-conflicts
ediff-merge session on a file with conflict markers

*** OTHER
git reflog                                            view log
git reset --hard HEAD@{7}                             revert HEAD to 7

"
         (generate-new-buffer "px-help-emacs"))
  (switch-to-buffer "px-help-emacs")
  (org-mode)
  ;; (show-all) ; ?
  )
(put 'upcase-region 'disabled nil)


;; ;; Bbdb! ____________________________________________________________________

(setq bbdb-file "~/.emacs.d/bbdb") ;; keep ~/ clean; set before loading
(bbdb-initialize)

(setq
    bbdb-offer-save 1 ;; 1 means save-without-asking

    ;; bbdb-use-pop-up t ;; allow popups for addresses
    ;; bbdb-electric-p t ;; be disposable with SPC
    ;; bbdb-popup-target-lines 1 ;; very small
    bbdb-dwim-net-address-allow-redundancy t ;; always use full name
    bbdb-quiet-about-name-mismatches 2 ;; show name-mismatches 2 secs
    bbdb-always-add-address t ;; add new addresses to existing...
                                             ;; ...contacts automatically
    bbdb-canonicalize-redundant-nets-p t ;; x@foo.bar.cx => x@bar.cx
    bbdb-completion-type nil ;; complete on anything
    bbdb-complete-name-allow-cycling t ;; cycle through matches
                                             ;; this only works partially
    bbbd-message-caching-enabled t ;; be fast
    bbdb-use-alternate-names t ;; use AKA
    bbdb-elided-display t ;; single-line addresses

    ;; auto-create addresses from mail
    bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
    bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
    ;; NOTE: there can be only one entry per header (such as To, From)
    ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

    '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))


;; ;; Toggling email! _____________________________________________________________

"Key used to switch to mail and back"
(defvar px-toggle-mail-key [(meta f1)])

"Mail client, use wl or gnus"
(fset 'px-mail-client 'wl)
(defvar px-no-mail-window-configuration (current-window-configuration))
(defvar px-mail-window-configuration t)

(defun px-prefs (arg)
  "toggle pref bits"
  (tabbar-mode arg)
  (scroll-bar-mode arg)
  (global-linum-mode arg)
  (fringe-mode arg))

(defun px-reset-prefs nil
  "reset my fucking prefs"
  (interactive)
  (px-prefs 0))

(defun px-exit-mail nil
  "called after switch back from mail"
  (set-window-configuration px-no-mail-window-configuration)
  (px-prefs t))

(defun px-go-mail nil
  "switch to mail client or launch it"
  (interactive)
  (if (or (get-buffer "Folder")		; Wanderlust
	  (get-buffer "*Group*"))	; Gnus
      (progn
	(setq px-no-mail-window-configuration (current-window-configuration))
	(px-prefs 0)
	(set-window-configuration px-mail-window-configuration))
    (progn
      (setq px-no-mail-window-configuration (current-window-configuration))
      (px-prefs 0)
      (px-mail-client))))

(defun px-no-mail nil
  "switch back from mail"
  (interactive)
  (setq px-mail-window-configuration (current-window-configuration))
  (set-window-configuration px-no-mail-window-configuration)
  (px-prefs t))

(eval-after-load "wl-folder"
  '(define-key wl-folder-mode-map px-toggle-mail-key 'px-no-mail))

(eval-after-load "wl-summary"
  '(progn
     (define-key wl-summary-mode-map [f3] 'wl-summary-pick)
     (define-key wl-summary-mode-map px-toggle-mail-key 'px-no-mail)))

(eval-after-load "wl-draft"
  '(define-key wl-draft-mode-map px-toggle-mail-key 'px-no-mail))

(eval-after-load "gnus"
  '(progn
     (define-key gnus-summary-mode-map px-toggle-mail-key 'px-no-mail)
     (define-key gnus-group-mode-map px-toggle-mail-key 'px-no-mail)
     (define-key gnus-article-mode-map px-toggle-mail-key 'px-no-mail)))

(eval-after-load "message"
  '(define-key message-mode-map px-toggle-mail-key 'px-no-mail))

(add-hook
 'mime-view-mode-hook
 '(lambda ()
    (local-set-key px-toggle-mail-key 'px-no-mail)))

(define-key global-map px-toggle-mail-key 'px-go-mail)

;; This is sufficient apart from exiting
(add-hook 'wl-mail-setup-hook 'px-reset-prefs)
(add-hook 'wl-draft-send-hook 'px-reset-prefs)
(add-hook 'wl-mail-send-pre-hook 'px-reset-prefs)
(add-hook 'wl-news-send-pre-hook 'px-reset-prefs)
(add-hook 'wl-message-buffer-created-hook 'px-reset-prefs)
(add-hook 'wl-message-redisplay-hook 'px-reset-prefs)
(add-hook 'wl-message-exit-hook 'px-reset-prefs)
(add-hook 'wl-summary-exit-pre-hook 'px-reset-prefs)
(add-hook 'wl-summary-exit-hook 'px-reset-prefs)

;; Faces ______________________________________________________________________

(set-face-attribute 'tabbar-default nil
		    :inherit nil
		    :height 110
		    :weight 'normal
		    :width 'normal
		    :slant 'normal
		    :underline nil
		    :strike-through nil
		    :stipple nil
		    ;; :background "gray80"
		    :background nil
		    :foreground "black"
		    :box nil
		    :family "Vera Sans Mono Bold Oblique")

(set-face-attribute 'tabbar-separator nil
                    :background "gray40"
                    :foreground nil
                    :height 1.0)

(set-face-attribute 'tabbar-selected nil
		    :background "#2e3436"
		    :foreground "red"
		    :inherit 'tabbar-default
		    :box '(:line-width 1 :color "#2e3436" :style nil))

(set-face-attribute 'tabbar-unselected nil
		    :inherit 'tabbar-default
		    :background "gray50"
		    ;; :background "red"
		    :box '(:line-width 1 :color "gray50" :style nil))

;; (set-face-attribute 'tabbar-highlight nil
;; 		    :foreground "white"
;; 		    :underline nil)

;; (set-face-attribute 'tabbar-button nil
;; 		    :inherit 'tabbar-default
;; 		    :box nil)


(if (window-system)
    (progn
      (set-face-attribute 'default nil :background "#2e3436" :foreground "#eeeeec")
      (set-face-attribute 'cursor nil :background "#fce94f" :foreground "#2e3436")
      (set-face-attribute 'highlight nil :background "dark red" :foreground
    "#fce94f")
      (set-face-attribute 'font-lock-builtin-face nil :foreground "#ad7fa8")
      (set-face-attribute 'font-lock-comment-face nil :slant 'oblique :foreground "#73d216")
      (set-face-attribute 'font-lock-constant-face nil :foreground "#e6a8df")
      (set-face-attribute 'font-lock-function-name-face nil :foreground "#fce84f")
      (set-face-attribute 'font-lock-keyword-face nil :foreground "#8cc4ff")
      (set-face-attribute 'font-lock-string-face nil :foreground "#e9b96e")
      (set-face-attribute 'font-lock-type-face nil :foreground "#a5ff4d")
      (set-face-attribute 'font-lock-variable-name-face nil :foreground "#fcaf3e")
      (set-face-attribute 'font-lock-warning-face nil :foreground "#ef2929")
      (set-face-attribute 'fringe nil :background "#2c2c2c")
      (set-face-attribute 'header-line nil :background "#555753" :foreground "#ffffff")
      (set-face-attribute 'isearch nil :background "#ce5c00" :foreground "#ffffff")
      (set-face-attribute 'lazy-highlight nil :background "#8f5902")
      (set-face-attribute 'link nil :foreground "#729fcf" :underline t)
      (set-face-attribute 'link-visited nil :foreground "#3465a4" :underline t)
      (set-face-attribute 'minibuffer-prompt nil :foreground "#fce94f")
      (set-face-attribute 'mode-line nil :background "gray10" :foreground "#eeeeee")
      (set-face-attribute 'mode-line-inactive nil :background "#555753" :foreground "#ffffff")
      (set-face-attribute 'mode-line-highlight nil :inverse-video t)
      (set-face-attribute 'region nil :background "#555753"))
  (set-face-attribute 'default nil :background "black" :foreground
    "white")
  (set-face-attribute 'mode-line nil :background "blue" :foreground "yellow"))

;; Custom ______________________________________________________________________

;; (setq tabbar-separator '(1)) ;; set tabbar-separator size to 1 pixel

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(cperl-array-face ((t (:foreground "#fcaf3e" :weight bold))))
;;  '(cperl-hash-face ((t (:foreground "#fcaf3e" :slant italic :weight bold))))
;;  '(font-lock-comment-face ((t (:slant oblique :weight light))))
;;  '(highlight ((t (:background "dark red"))))
;;  '(mode-line ((t (:background "gray10" :foreground "#eeeeee"))))
;;  '(mode-line-highlight ((t (:inverse-video t))))
;;  '(mumamo-background-chunk-major ((t (:background "gray15"))))
;;  '(mumamo-background-chunk-submode1 ((t (:background "gray16"))))
;;  '(mumamo-region ((t nil)))
;;  '(undo-tree-visualizer-default-face ((t (:foreground "gray"))))
;;  '(wl-highlight-folder-few-face ((t (:foreground "gainsboro" :weight bold))))
;;  '(wl-highlight-folder-many-face ((t (:foreground "AntiqueWhite3" :weight extra-bold))))
;;  '(wl-highlight-folder-path-face ((t (:background "dark red" :foreground "white" :weight bold))))
;;  '(wl-highlight-folder-unread-face ((t (:foreground "light gray" :weight bold))))
;;  '(wl-highlight-folder-zero-face ((t (:foreground "AntiqueWhite2"))))
;;  '(wl-highlight-summary-answered-face ((t (:foreground "khaki"))))
;;  '(wl-highlight-summary-displaying-face ((t (:background "dark red" :foreground "yellow" :weight bold))))
;;  '(wl-highlight-summary-new-face ((t (:foreground "white" :weight ultra-bold))))
;;  '(wl-highlight-summary-thread-top-face ((t (:foreground "gray")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.bkp/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.bkp/"))))
 '(bbdb-use-pop-up nil)
 '(canlock-password "cf5f7a7261c5832898abfc7ea08ba333a36ed78c")
 '(epa-popup-info-window nil)
 '(global-undo-tree-mode t)
 '(inhibit-startup-echo-area-message (user-login-name))
 '(recentf-save-file "~/.bkp/recentf")
 '(send-mail-function (quote mailclient-send-it))
 '(tabbar-ruler-excluded-buffers (quote ("*Messages*" "*scratch*" "\\*.\\*")))
 '(undo-tree-auto-save-history t)
 '(undo-tree-visualizer-relative-timestamps t)
 '(undo-tree-visualizer-timestamps t)
 '(web-vcs-default-download-directory (quote site-lisp-dir))
 '(wl-draft-add-in-reply-to nil)
 '(wl-draft-buffer-style (quote keep))
 '(wl-draft-reply-buffer-style (quote keep))
 '(wl-message-mode-line-format "")
 '(wl-message-truncate-lines t)
 '(wl-prefetch-threshold 300000)
 '(wl-subscribed-mailing-list (quote ("wl@lists.airs.net")))
 '(wl-summary-default-view (quote sequence))
 '(wl-summary-exit-next-move nil)
 '(wl-summary-mode-line-format "")
 '(wl-summary-recenter nil)
 '(wl-summary-width 150))

;; Garbage ______________________________________________________________________

;; (setq yas/root-directory "~/.emacs.d/el-get/yasnippet/snippets")
;; (add-hook 'php-mode-hook 'yas/global-mode)
(message "%s loaded" (or load-file-name buffer-file-name))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
