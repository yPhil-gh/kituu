;; ===========================================================================
;; Time-stamp: <.emacs - Fri 23-Mar-2012 20:03:32>
;; ===========================================================================
;; See https://github.com/xaccrocheur/kituu/

;; Init! ______________________________________________________________________

;; ;; (setq user-emacs-directory "~/.emacs/")
;; ;; (eval-when-compile
;;   (let ((default-directory "~/.emacs.d/lisp/"))
;;     (normal-top-level-add-subdirs-to-load-path))
;; ;;(require 'px-org-conf)
;; ;; )
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; (eval-and-compile
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (add-to-list 'load-path "~/.emacs.d/lisp/tabbar/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/bbdb/")
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; (load "~/.emacs.d/lisp/nxhtml/autostart.el")

(require 'cl)
(require 'tabbar)
;; wtf?
;; (require 'smart-tab)
(require 'dbus)


;; ;; DBus! ____________________________________________________________________

(defun px-send-desktop-notification (summary body timeout)
  "call notification-daemon method METHOD with ARGS over dbus"
  (dbus-call-method
    :session                        ; use the session (not system) bus
    "org.freedesktop.Notifications" ; service name
    "/org/freedesktop/Notifications"   ; path name
    "org.freedesktop.Notifications" "Notify" ; Method
    "emacs"
    0
    ""
    summary
    body
    '(:array)
    '(:array :signature "{sv}")
    ':int32 timeout))

;; (defun pw/compile-notify (buffer message)
  ;; (send-desktop-notification "emacs compile" message 0))

;; (setq compilation-finish-function 'pw/compile-notify)

;; (px-send-desktop-notification "test" "plip" 0)


(defvar iswitchb-mode-map)
(defvar iswitchb-buffer-ignore)
(defvar show-paren-delay)
(defvar time-stamp-active)
(defvar time-stamp-warn-inactive)
(defvar time-stamp-format)
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


;; ;; El-get! ____________________________________________________________________

;; (setq el-get-dir (concat user-emacs-directory "el-get/"))
;; (add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
;; (unless (require 'el-get nil t)
;;   (url-retrieve
;;    "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
;;    (lambda (s)
;;      ;; (end-of-buffer)
;;      (goto-char (point-max))
;;      (eval-print-last-sexp))))

;; ;; REMEMBER to put your el-get installed packages here, if you want to use this .emacs on another machine
;; (setq my-packages
;;       (append
;;        '(linum-off php-mode-improved haml-mode cperl-mode tail)
;;        (mapcar 'el-get-source-name el-get-sources)))

;; ;; (declare-function tabbar-mode "tabbar.el")
;; (declare-function el-get "el-get.el")
;; (el-get 'sync my-packages)


;; Server! ____________________________________________________________________

(server-start)
(defun ff/raise-frame-and-give-focus ()
  (when window-system
    (raise-frame)
    (x-focus-frame (selected-frame))
    (set-mouse-pixel-position (selected-frame) 4 4)
    ))
(add-hook 'server-switch-hook 'ff/raise-frame-and-give-focus)


;; Functions! _________________________________________________________________

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


(if (string-match "\\.emacs" (buffer-name))
(message "plop"))

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


(defun switch-buffer-px ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "s-²") 'switch-buffer-px)

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

(defun query-replace-regexp-in-open-buffers-px (arg1 arg2)
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

(defun Fullscreen-px ()
  "Maximize the current frame (to full screen)"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

(defun Google-that-bitch-px (start end)
  "Google selected string"
  (interactive "r")
  (let ((q (buffer-substring-no-properties start end)))
    (browse-url (concat "http://www.google.com/search?&q="
			(url-hexify-string q)))))

;; (autoload 'zap-up-to-char "misc"
;;   "Kill up to, but not including ARGth occurrence of CHAR.
;;   \(fn arg char)"
;;   'interactive)

(defun swap-buffers-in-windows-px ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
	 (other (next-window))
	 (this-buffer (window-buffer this))
	 (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    )
  )

(defun select-text-in-quote-px ()
  "Select text between the nearest left and right delimiters.
Delimiters are paired characters: For practical purposes, it also
includes double straight quote , but not curly single quote
matching pairs, because that is often used as apostrophy. It also
consider both left and right angle brackets <> as either
beginning or ending pair, so that it is easy to get content
inside html tags."
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

(defun frigo-kill-px () (interactive)
  "Cut the current region, paste it in frigo.txt with a time tag, and save this file"
  (unless (use-region-p) (error "No region selected"))
  (let ((bn (file-name-nondirectory (buffer-file-name))))
    (kill-region (region-beginning) (region-end))
    (with-current-buffer (find-file-noselect "~/.frigo.txt")
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
      (message "Region refrigerated!")
      )
    )
  )

(defun frigo-px () (interactive)
  "Copy the current region, paste it in frigo.txt with a time tag, and save this file"
  (unless (use-region-p) (error "No region selected"))
  (let ((bn (file-name-nondirectory (buffer-file-name))))
    (copy-region-as-kill (region-beginning) (region-end))
    (with-current-buffer (find-file-noselect "~/.frigo.txt")
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
      (message "Region refrigerated!")
      )
    )
  )


(defun px-saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

(defun Session-restore-px ()
  "Restore a saved emacs session."
  (interactive)
  (if (px-saved-session)
      (progn (desktop-read)
	     (recenter-top-bottom 15))
    (message "No desktop (session) file found.")))

(defun Session-save-px ()
  "Save an emacs session."
  (interactive)
  (if (px-saved-session)
      (if (y-or-n-p "Save session? ")
	  (desktop-save-in-desktop-dir)
	(message "Session not saved."))
  (desktop-save-in-desktop-dir)))

;; This will only work for one session
(add-hook 'after-init-hook
	  '(lambda ()
	     (if (px-saved-session)
		 (if (y-or-n-p "Restore session? ")
		     (Session-restore-px)))))

(add-hook 'kill-emacs-hook
	  '(lambda ()
	     (Session-save-px)))

;; ;; Save desktop + Prevent pussy questions
;; (defadvice save-buffers-kill-emacs (around no-y-or-n activate)
;;   (flet ((yes-or-no-p (&rest args) t)
;;          (y-or-n-p (&rest args) t))
;;     ad-do-it))

;; Modes! ______________________________________________________________________

(tabbar-mode t)
;; (set-fringe-mode '(1 . 1))
(show-paren-mode t)
(menu-bar-mode -1)
;; (global-linum-mode 1)
;; (global-undo-tree-mode 1)
;; (global-smart-tab-mode 1)
;; (smart-tab-mode t)
(global-font-lock-mode t)
(tool-bar-mode 0)
(set-scroll-bar-mode `right)
(delete-selection-mode t)
(auto-fill-mode t)
;; (setq-default fill-column 99999)
;; (setq fill-column 99999)
;; (setq set-mark-command-repeat-pop t)
;; (setq word-wrap t)
(recentf-mode 1)
(mouse-avoidance-mode 'cat-and-mouse)
(iswitchb-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

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

;; Time-stamp
(setq time-stamp-active t
      time-stamp-warn-inactive t
      time-stamp-format "%f - %3a %02d-%3b-%:y %02H:%02M:%02S")


;; Hooks! _____________________________________________________________________

(add-hook 'perl-mode-hook 'cperl-mode)

(add-hook 'cperl-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-h f") 'cperl-perldoc)))

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

(add-hook 'write-file-hooks 'time-stamp)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'display-time-hook
          (lambda () (setq gnus-mode-non-string-length
                           (+ 21
                              (if line-number-mode 5 0)
                              (if column-number-mode 4 0)
                              (length display-time-string)))))
;; (add-hook 'php-mode-hook 'yas/minor-mode)
;; (add-hook 'html-mode-hook 'yas/minor-mode)
;; (setq yas/extra-mode-hooks '(php-html-helper-mode))
;; (setq yas/extra-mode-hooks '(css-mode))


;; ;; remove desktop after it's been read
;; (add-hook 'desktop-after-read-hook
;; 	  '(lambda ()
;; 	     ;; desktop-remove clears desktop-dirname
;; 	     (setq desktop-dirname-tmp desktop-dirname)
;; 	     (desktop-remove)
;; 	     (setq desktop-dirname desktop-dirname-tmp)))

;; Keys! ______________________________________________________________________
(define-key global-map [(meta up)] '(lambda() (interactive) (scroll-other-window -1)))
(define-key global-map [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))

(define-key global-map [f1] 'delete-other-windows)
(define-key global-map [S-f1] 'px-help-emacs)
;; (define-key global-map [M-f1] 'delete-window)
(define-key global-map [M-f2] 'swap-buffers-in-windows)
(define-key global-map [f2] 'other-window)
(define-key global-map [f11] 'Fullscreen-px)

(define-key global-map [s-kp-0] 'zzzap)

(defun zzzap ()
(interactive)
(other-window 1))

(define-key global-map [f3] 'isearch-forward)

(define-key global-map [f4] 'split-window-horizontally)
(define-key global-map [f5] 'iswitchb-buffer) ;new way
(define-key global-map [f7] 'flyspell-buffer)
(define-key global-map [M-f7] 'flyspell-mode)
(define-key global-map [f10] 'toggle-truncate-lines)

(global-set-key (kbd "s-g") 'goto-line)
(global-set-key (kbd "C-r") 'replace-string)
(global-set-key (kbd "C-f") 'isearch-forward)
;; (global-set-key (kbd "C-d") 'isearch-forward) ; I kept deleting stuff
(global-set-key (kbd "C-S-s") 'isearch-backward)
(global-set-key (kbd "C-S-f") 'isearch-backward)
(global-set-key (kbd "C-c b") (kbd "C-x b <return>"))
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-S-o") 'desktop-read)
;; (global-set-key (kbd "C-S-<delete>") 'kill-paragraph)
(global-set-key (kbd "C-S-<mouse-1>") 'flyspell-correct-word)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "<C-next>") 'forward-page)
(global-set-key (kbd "<C-prior>") 'backward-page)

(global-set-key (kbd "M-s") 'save-buffer) ; Meta+s saves !! (see C-h b for all bindings, and C-h k + keystroke(s) for help)
;; (global-set-key "C-b" 'match-paren) ; Match brace
(global-set-key (kbd "M-DEL") 'kill-word)
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-o") 'recentf-open-files)
(global-set-key (kbd "M-d") 'comment-out)
(global-set-key (kbd "C-<tab>") 'tabbar-forward)
(global-set-key (kbd "<C-S-iso-lefttab>") 'tabbar-backward)
(global-set-key (kbd "C-\"") 'insert-pair-dbquotes)		;""
(global-set-key (kbd "C-)") 'insert-pair-paren)			;()
(global-set-key (kbd "C-=") 'insert-pair-brace)			;{}
(global-set-key (kbd "C-'") 'insert-pair-brace)			;{}
(global-set-key (kbd "C-(") 'insert-pair-bracket)		;[]
(global-set-key (kbd "C-<") 'insert-pair-single-angle)		;<>
;; (global-set-key (kbd "C-'") 'insert-pair-single-straight-quote) ;''
(global-set-key (kbd "s-t") 'sgml-tag)

(global-set-key (kbd "C-ù") 'forward-sexp)
(global-set-key (kbd "C-%") 'backward-sexp)

(global-set-key (kbd "M-²") 'hippie-expand)

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups.
This function groups all buffers into 3 groups:
Those Dired, those user buffer, and those emacs buffer.
Emacs buffer are those starting with “*”."
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs Buffer"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    (t
     "User Buffer"
     )
    )))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

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

;; Comment-Out
(defun comment-out () ;; function definition
  "If region is set, [un]comments it. Otherwise [un]comments current line." ;; description for help file
  (interactive) ;; apparently I need this line so it can move the cursor
  ;; (save-excursion ;; don't mess with mark
  (if (eq mark-active nil) ;; if the variable "mark-active" is nil, there's no region active
      (progn (beginning-of-line 1) ;; go to beginning of current line
	     (set-mark (point)) ;; set the mark there
	     (forward-line)
	     (comment-dwim nil)) ;; go down one line
    (comment-dwim nil)) ;; [un]comment
  (deactivate-mark)) ;; don't mess with selection


;; Help ______________________________________________________________________

(defun Help-px ()
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
C-c C-m
C-c RET                             Browse PHP manual in a Web browser.
C-c C-f                             Search PHP manual using identifier
at point.

*** THIS VERY EMACS CONFIG
Save buffer                         M-s
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
"
         (generate-new-buffer "px-help-emacs"))
  (switch-to-buffer "px-help-emacs")
  (org-mode)
  ;; (show-all) ; ?
  )
(put 'upcase-region 'disabled nil)


;; ;; Toggling email! _________________________________________________________________

"Key used to switch to mail and back"
(setq px-toggle-mail-key [(meta f1)])

"Mail client"
(setq mail-client "wl")

(defun px-mail-client (mail-client)
  (if (string-equal mail-client "gnus")
      (gnus)
    (wl)))

(defun px-prefs (arg)
  "toggle pref bits"
  (tabbar-mode arg)
  (scroll-bar-mode arg)
  (linum-mode arg))

(defun px-exit-mail nil
  "called after exiting mail"
  (set-window-configuration px-no-mail-window-configuration)
  (px-prefs -1))

(defun px-go-mail nil
  "switch to mail or launch it"
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
      (px-mail-client mail-client))))

(defun px-no-mail nil
  "switch back from mail"
  (interactive)
  (setq px-mail-window-configuration (current-window-configuration))
  (set-window-configuration px-no-mail-window-configuration)
  (px-prefs t))

(eval-after-load "wl-folder"
  '(define-key wl-folder-mode-map px-toggle-mail-key 'px-no-mail))

(eval-after-load "wl-summary"
  '(define-key wl-summary-mode-map px-toggle-mail-key 'px-no-mail))

(eval-after-load "wl-draft"
  '(define-key wl-draft-mode-map px-toggle-mail-key 'px-no-mail))

(eval-after-load "gnus"
  '(progn
     (define-key gnus-summary-mode-map px-toggle-mail-key 'px-no-mail)
     (define-key gnus-group-mode-map px-toggle-mail-key 'px-no-mail)
     (define-key gnus-article-mode-map px-toggle-mail-key 'px-no-mail)))

(eval-after-load "message"
  '(define-key message-mode-map px-toggle-mail-key 'px-no-mail))

(define-key global-map px-toggle-mail-key 'px-go-mail)

(defun Reset-prefs nil
  "reset my fucking prefs"
  (interactive)
  (px-prefs 0))

(add-hook 'wl-mail-setup-hook 'Reset-prefs)
;; (add-hook 'wl-summary-toggle-disp-off-hook 'Reset-prefs)
;; (add-hook 'wl-summary-toggle-disp-on-hook 'Reset-prefs)
;; (add-hook 'wl-summary-toggle-disp-off-hook 'Reset-prefs)
;; (add-hook 'wl-summary-toggle-disp-folder-on-hook 'Reset-prefs)
;; (add-hook 'wl-summary-toggle-disp-folder-off-hook 'Reset-prefs)
;; (add-hook 'wl-summary-toggle-disp-folder-message-resumed-hook 'Reset-prefs)
;; (add-hook 'wl-summary-mode-hook 'Reset-prefs)
;; (add-hook 'wl-summary-prepared-pre-hook 'Reset-prefs)
;; (add-hook 'wl-summary-prepared-hook 'Reset-prefs)
;; (add-hook 'wl-summary-sync-updated-hook 'Reset-prefs)
;; (add-hook 'wl-summary-unread-message-hook 'Reset-prefs)
;; (add-hook 'wl-summary-edit-addresses-hook 'Reset-prefs)
;; (add-hook 'wl-summary-buffer-message-saved-hook 'Reset-prefs)

;; (add-hook 'bbdb-wl-get-update-record-hook 'Reset-prefs)
;; (add-hook 'elmo-archive-load-hook 'Reset-prefs)
;; (add-hook 'elmo-nntp-opened-hook 'Reset-prefs)
;; (add-hook 'elmo-pipe-drained-hook 'Reset-prefs)
;; (add-hook 'elmo-msg-appended-hook 'Reset-prefs)
;; (add-hook 'elmo-msg-deleted-hook 'Reset-prefs)
;; (add-hook 'elmo-nntp-post-pre-hook 'Reset-prefs)
;; (add-hook 'wl-ps-preprint-hook 'Reset-prefs)
;; (add-hook 'wl-ps-print-hook 'Reset-prefs)
;; (add-hook 'wl-folder-mode-hook 'Reset-prefs)
;; (add-hook 'wl-summary-toggle-disp-on-hook 'Reset-prefs)
;; (add-hook 'wl-summary-toggle-disp-off-hook 'Reset-prefs)
;; (add-hook 'wl-summary-toggle-disp-folder-on-hook 'Reset-prefs)
;; (add-hook 'wl-summary-toggle-disp-folder-off-hook 'Reset-prefs)
;; (add-hook 'wl-summary-toggle-disp-folder-message-resumed-hook 'Reset-prefs)
;; (add-hook 'wl-summary-mode-hook 'Reset-prefs)
;; (add-hook 'wl-summary-prepared-pre-hook 'Reset-prefs)
;; (add-hook 'wl-summary-prepared-hook 'Reset-prefs)
;; (add-hook 'wl-summary-sync-updated-hook 'Reset-prefs)
;; (add-hook 'wl-summary-unread-message-hook 'Reset-prefs)
;; (add-hook 'wl-summary-edit-addresses-hook 'Reset-prefs)
;; (add-hook 'wl-summary-buffer-message-saved-hook 'Reset-prefs)
;; (add-hook 'wl-summary-buffer-mark-saved-hook 'Reset-prefs)
;; (add-hook 'wl-init-hook 'Reset-prefs)
;; (add-hook 'wl-hook 'Reset-prefs)
;; (add-hook 'wl-draft-reply-hook 'Reset-prefs)
;; (add-hook 'wl-draft-forward-hook 'Reset-prefs)
;; (add-hook 'wl-draft-kill-pre-hook 'Reset-prefs)
;; (add-hook 'wl-summary-reply-hook 'Reset-prefs)
;; (add-hook 'wl-summary-forward-hook 'Reset-prefs)
;; (add-hook 'wl-summary-resend-hook 'Reset-prefs)
;; (add-hook 'wl-mail-setup-hook 'Reset-prefs)
;; (add-hook 'wl-draft-reedit-hook 'Reset-prefs)

(add-hook 'wl-draft-send-hook 'Reset-prefs)
(add-hook 'wl-mail-send-pre-hook 'Reset-prefs)
(add-hook 'wl-news-send-pre-hook 'Reset-prefs)
(add-hook 'wl-message-buffer-created-hook 'Reset-prefs)
(add-hook 'wl-message-redisplay-hook 'Reset-prefs)
(add-hook 'wl-message-exit-hook 'Reset-prefs)
(add-hook 'wl-summary-exit-pre-hook 'Reset-prefs)
(add-hook 'wl-summary-exit-hook 'Reset-prefs)

;; (add-hook 'wl-highlight-headers-hook 'Reset-prefs)
;; (add-hook 'wl-highlight-message-hook 'Reset-prefs)
;; (add-hook 'wl-save-hook 'Reset-prefs)
;; (add-hook 'wl-exit-hook 'Reset-prefs)
;; (add-hook 'wl-folder-suspend-hook 'Reset-prefs)
;; (add-hook 'wl-biff-notify-hook 'Reset-prefs)
;; (add-hook 'wl-biff-unnotify-hook 'Reset-prefs)
;; (add-hook 'wl-auto-check-folder-pre-hook 'Reset-prefs)
;; (add-hook 'wl-auto-check-folder-hook 'Reset-prefs)
;; (add-hook 'wl-folder-check-entity-pre-hook 'Reset-prefs)
;; (add-hook 'wl-folder-check-entity-hook 'Reset-prefs)
;; (add-hook 'wl-draft-config-exec-hook 'Reset-prefs)
;; (add-hook 'wl-summary-expire-pre-hook 'Reset-prefs)
;; (add-hook 'wl-summary-expire-hook 'Reset-prefs)
;; (add-hook 'wl-summary-archive-pre-hook 'Reset-prefs)
;; (add-hook 'wl-summary-archive-hook 'Reset-prefs)
;; (add-hook 'wl-summary-line-inserted-hook 'Reset-prefs)
;; (add-hook 'wl-summary-insert-headers-hook 'Reset-prefs)
;; (add-hook 'wl-message-display-internal-hook 'Reset-prefs)
;; (add-hook 'wl-thread-update-children-number-hook 'Reset-prefs)
;; (add-hook 'wl-folder-update-access-group-hook 'Reset-prefs)
;; (add-hook 'wl-draft-cited-hook 'Reset-prefs)
;; (add-hook 'wl-draft-insert-x-face-field-hook 'Reset-prefs)
;; (add-hook 'wl-template-mode-hook 'Reset-prefs)
;; (add-hook 'wl-score-mode-hook 'Reset-prefs)
;; (add-hook 'wl-make-plugged-hook 'Reset-prefs)
;; (add-hook 'wl-plugged-exit-hook 'Reset-prefs)
;; (add-hook 'wl-plugged-hook 'Reset-prefs)
;; (add-hook 'wl-unplugged-hook 'Reset-prefs)

;; Faces ______________________________________________________________________

(if (eq window-system 'x)
    (progn
      (set-face-attribute 'default nil :background "#2e3436" :foreground "#eeeeec")
      (set-face-attribute 'cursor nil :background "#fce94f" :foreground "#2e3436")
      (set-face-attribute 'font-lock-builtin-face nil :foreground "#ad7fa8")
      (set-face-attribute 'font-lock-comment-face nil :foreground "#73d216")
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
      (set-face-attribute 'mode-line nil :background "#777777" :foreground "#000000")
      (set-face-attribute 'mode-line-inactive nil :background "#555753" :foreground "#ffffff")
      (set-face-attribute 'region nil :background "#555753")
)
  (set-face-attribute 'default nil :background "black" :foreground "white"))

(if (>= emacs-major-version 23)
(set-frame-font "Monospace-12"))

(set-face-attribute 'tabbar-default nil
		    :inherit nil
		    :height 110
		    :weight 'normal
		    :width 'normal
		    :slant 'normal
		    :underline nil
		    :strike-through nil
		    :stipple nil
		    :background "gray80"
		    :foreground "black"
		    :box nil
		    ;; :family "Lucida Grande"
		    )

(set-face-attribute 'tabbar-selected nil
		    :background "#2e3436"
		    :foreground "red"
		    :inherit 'tabbar-default
		    :box '(:line-width 3 :color "#2e3436" :style nil))

(set-face-attribute 'tabbar-unselected nil
		    :inherit 'tabbar-default
		    :background "gray50"
		    :box '(:line-width 3 :color "grey50" :style nil))

(set-face-attribute 'tabbar-highlight nil
		    :foreground "white"
		    :underline nil)

(set-face-attribute 'tabbar-button nil
		    :inherit 'tabbar-default
		    :box nil)

;; Custom ______________________________________________________________________

;; (setq tabbar-separator '(1)) ;; set tabbar-separator size to 1 pixel
(message "%s loaded" (buffer-file-name))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:foreground "#fcaf3e" :weight bold))))
 '(cperl-hash-face ((t (:foreground "#fcaf3e" :slant italic :weight bold))))
 '(wl-highlight-folder-path-face ((t (:background "dark red" :foreground "white" :weight bold))))
 '(wl-highlight-summary-displaying-face ((t (:background "dark red" :foreground "white" :weight bold)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.bkp/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.bkp/"))))
 '(bbdb-use-pop-up (quote (quote horiz)))
 '(canlock-password "cf5f7a7261c5832898abfc7ea08ba333a36ed78c")
 '(display-time-use-mail-icon t)
 '(gnus-group-highlight (quote (((this-buffer-is-visible (concat "*Summary " group "*")) . gnus-summary-selected) ((and mailp (= unread 0) (eq level 1)) . gnus-group-mail-1-empty) ((and mailp (eq level 1)) . gnus-group-mail-1) ((and mailp (= unread 0) (eq level 2)) . gnus-group-mail-2-empty) ((and mailp (eq level 2)) . gnus-group-mail-2) ((and mailp (= unread 0) (eq level 3)) . gnus-group-mail-3-empty) ((and mailp (eq level 3)) . gnus-group-mail-3) ((and mailp (= unread 0)) . gnus-group-mail-low-empty) ((and mailp) . gnus-group-mail-low) ((and (= unread 0) (eq level 1)) . gnus-group-news-1-empty) ((and (eq level 1)) . gnus-group-news-1) ((and (= unread 0) (eq level 2)) . gnus-group-news-2-empty) ((and (eq level 2)) . gnus-group-news-2) ((and (= unread 0) (eq level 3)) . gnus-group-news-3-empty) ((and (eq level 3)) . gnus-group-news-3) ((and (= unread 0) (eq level 4)) . gnus-group-news-4-empty) ((and (eq level 4)) . gnus-group-news-4) ((and (= unread 0) (eq level 5)) . gnus-group-news-5-empty) ((and (eq level 5)) . gnus-group-news-5) ((and (= unread 0) (eq level 6)) . gnus-group-news-6-empty) ((and (eq level 6)) . gnus-group-news-6) ((and (= unread 0)) . gnus-group-news-low-empty) (t . gnus-group-news-low))))
 '(gnus-read-active-file nil)
 '(inhibit-startup-echo-area-message (user-login-name))
 '(recentf-save-file "~/.bkp/recentf")
 '(web-vcs-default-download-directory (quote site-lisp-dir))
 '(wl-fldmgr-folders-indent "\" \"")
 '(wl-folder-window-width 25)
 '(wl-subscribed-mailing-list (quote ("wl@lists.airs.net"))))


;; Garbage ______________________________________________________________________

;; (setq yas/root-directory "~/.emacs.d/el-get/yasnippet/snippets")
;; (add-hook 'php-mode-hook 'yas/global-mode)
