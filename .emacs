;; ==========================================================================
;; Time-stamp: <.emacs - Mon 12-Mar-2012 12:41:32>
;; ===========================================================================
  ;; (kill-buffer "*scratch*")
;; See https://github.com/xaccrocheur/kituu/

;; Init
;; (setq user-emacs-directory "~/.lisp/")
;; (eval-when-compile
  ;; (let ((default-directory "~/.emacs.d/px-lisp/"))
  ;;   (normal-top-level-add-subdirs-to-load-path))
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (add-to-list 'load-path "~/.emacs.d/lisp/tabbar/")
  ;; )

;; (when (file-exists-p "~/.emacs.d/lisp/px.el")
;;   (require 'px))
;; Encryption
;; (require 'epa-file)
;; (epa-file-enable)

(require 'tabbar)

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

(defvar el-get-dir)
(defvar el-get-sources)
(defvar my-packages)
(defvar my-packages)

(defvar newName)
(defvar newName)

(defvar display-time-string)
(defvar gnus-mode-non-string-length)
(defvar ediff-window-setup-function)
(defvar ediff-split-window-function)
(defvar tabbar-buffer-groups-function)

(defvar gnus-bury-window-configuration)
(defvar gnus-bury-window-configuration)
(defvar minibuffer_history)
(defvar minibuffer_history)
(defvar savehist-file)


;; El-get
(setq el-get-dir (concat user-emacs-directory "el-get/"))
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     ;; (end-of-buffer)
     (goto-char (point-max))
     (eval-print-last-sexp))))

;; REMEMBER to put your el-get installed packages here, if you want to use this .emacs on another machine
(setq my-packages
      (append
       '(linum-off smart-tab php-mode-improved haml-mode tail)
       (mapcar 'el-get-source-name el-get-sources)))

;; (declare-function tabbar-mode "tabbar.el")
(declare-function el-get "el-get.el")
(el-get 'sync my-packages)

;; Server
(server-start)
(defun ff/raise-frame-and-give-focus ()
  (when window-system
    (raise-frame)
    (x-focus-frame (selected-frame))
    (set-mouse-pixel-position (selected-frame) 4 4)
    ))
(add-hook 'server-switch-hook 'ff/raise-frame-and-give-focus)


;; Functions

;; (defun compile-init-file-px ()
;;   (let ((byte-compile-warnings '(unresolved)))
;;     (byte-compile-file user-init-file)
;;     (message "%s saved and compiled." user-init-file)))

;; (defun my-emacs-lisp-mode-hook ()
;;   ;; (if (string-equal buffer-file-name user-init-file)
;;   (if (search ".emacs" buffer-file-name)
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
  (setq newName
	(concat
	 (file-name-sans-extension buffer-file-name) "-"
	 (format-time-string  "%Y-%m-%d") "."
	 (format-time-string "%Hh%M") "."
	 (file-name-extension buffer-file-name))
	)
  (write-region (point-min) (point-max) newName)
  (message "backuped %s" newName)
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

(defun fullscreen-px ()
  "Maximize the current frame (to full screen)"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

(defun google-that-bitch-px (start end)
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

(defun kill-other-buffers-px ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
	(delq (current-buffer)
	      (remove-if-not 'buffer-file-name (buffer-list)))))

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


;; Modes
;; (set-fringe-mode '(1 . 1))
(tabbar-mode t)
(show-paren-mode t)
(menu-bar-mode -1)
(global-linum-mode 1)
;; (global-undo-tree-mode 1)
(global-smart-tab-mode 1)
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
;; Vars
(setq
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

;; use only one desktop
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


;; Hooks
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

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

(defun session-restore-px ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop file found.")))

(defun session-save-px ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop file? ")
	  (desktop-save-in-desktop-dir)
	(message "Session not saved."))
  (desktop-save-in-desktop-dir)))

;; This will only work for one session
(add-hook 'after-init-hook
	  '(lambda ()
	     (if (saved-session)
		 (if (y-or-n-p "Restore session? ")
		     (session-restore-px)))))


;; Keys !!
(define-key global-map [(meta up)] '(lambda() (interactive) (scroll-other-window -1)))
(define-key global-map [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))

(define-key global-map [f1] 'delete-other-windows)
(define-key global-map [S-f1] 'px-help-emacs)
;; (define-key global-map [M-f1] 'delete-window)
(define-key global-map [M-f2] 'swap-buffers-in-windows)
(define-key global-map [f2] 'other-window)

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

;; (global-set-key [C-] 'tabbar-forward)
;; (global-set-key [M-s-left] 'tabbar-backward)

;; ;; (global-set-key (kbd "C-<tab>") 'tabbar-forward)
;; (defun xsteve-gnus-px ()
;;   "Invoke gnus"
;;   (interactive)
;;   (let ((bufname (buffer-name)))
;;     (if (or
;;          (string-equal "*Group*" bufname)
;;          (string-equal "*BBDB*" bufname)
;;          (string-match "\*Summary" bufname)
;;          (string-match "\*Article" bufname))
;;         (progn
;;           (xsteve-bury-gnus)
;; 	  (tabbar-mode t)
;; 	  (scroll-bar-mode t)
;; 	  ;; (menu-bar-mode -1)
;; 	  )
;;       ;; unbury
;;       (if (get-buffer "*Group*")
;;           (progn (xsteve-unbury-gnus)
;; 		 (tabbar-mode -1)
;; 		 (scroll-bar-mode -1)
;; 		 ;; (menu-bar-mode)
;; 		 )
;;         (gnus)))))

;; (defun xsteve-unbury-gnus ()
;;   "Restore gnus in its previous state"
;;   (interactive)
;;   (when (and (boundp 'gnus-bury-window-configuration) gnus-bury-window-configuration)
;;     (set-window-configuration gnus-bury-window-configuration)))

;; (defun xsteve-bury-gnus ()
;;   "Bury gnus and restore previous buffer"
;;   (interactive)
;;   (setq gnus-bury-window-configuration nil)
;;   (let ((buf nil)
;;         (bufname nil))
;;     (dolist (buf (buffer-list))
;;       (setq bufname (buffer-name buf))
;;       (when (or
;;              (string-equal "*Group*" bufname)
;;              (string-equal "*BBDB*" bufname)
;;              (string-match "\*Summary" bufname)
;;              (string-match "\*Article" bufname))
;;         (unless gnus-bury-window-configuration
;;           (setq gnus-bury-window-configuration (current-window-configuration)))
;;         (delete-other-windows)
;;         (if (eq (current-buffer) buf)
;;             (bury-buffer)
;;           (bury-buffer buf))))))

;; (global-set-key [(meta f1)] 'xsteve-gnus-px)
(defun xsteve-gnus ()
  (interactive)
  (let ((bufname (buffer-name)))
    (if (or
         (string-equal "*Group*" bufname)
         (string-equal "*BBDB*" bufname)
         (string-match "\*Summary" bufname)
         (string-match "\*Article" bufname))
        (progn
          (xsteve-bury-gnus)
	  (message "back to %s" backbuffer)
	  (switch-to-buffer backbuffer)
	  (tabbar-mode t))
      ;unbury
      (if (get-buffer "*Group*")
          (progn
	    (setq backbuffer (buffer-name))
	    (message "my name is %s" backbuffer)
	    (xsteve-unbury-gnus)
		 (tabbar-mode -1))
        (gnus-unplugged)))))

(defun xsteve-unbury-gnus ()
  (interactive)
  (when (and (boundp 'gnus-bury-window-configuration) gnus-bury-window-configuration)
    (set-window-configuration gnus-bury-window-configuration)))

(defun xsteve-bury-gnus ()
  (interactive)
  (setq gnus-bury-window-configuration nil)
  (let ((buf nil)
        (bufname nil))
    (dolist (buf (buffer-list))
      (setq bufname (buffer-name buf))
      (when (or
             (string-equal "*Group*" bufname)
             (string-equal "*BBDB*" bufname)
             (string-match "\*Summary" bufname)
             (string-match "\*Article" bufname))
        (unless gnus-bury-window-configuration
          (setq gnus-bury-window-configuration (current-window-configuration)))
        (delete-other-windows)
        (if (eq (current-buffer) buf)
            (bury-buffer)
          (bury-buffer buf))))))

(global-set-key [(meta f1)] 'xsteve-gnus)

;;(message "%s" bufname)
;; (current-buffer)
;; (switch-to-buffer "olimap.el")
;; Save the minibuffer history
(setq minibuffer_history (concat user-emacs-directory "minibuffer_history"))
(setq savehist-file minibuffer_history)
(when (functionp 'savehist-mode) (savehist-mode 1))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.bkp/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.bkp/"))))
 '(canlock-password "cf5f7a7261c5832898abfc7ea08ba333a36ed78c")
 '(display-time-use-mail-icon t)
 '(gnus-read-active-file nil)
 '(inhibit-startup-echo-area-message (user-login-name))
 '(recentf-save-file "~/.bkp/recentf"))


;; Help
(defun help-px ()
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


(message "Everything is UP, %s" user-login-name)


;; Garbage ;;

;; (setq yas/root-directory "~/.emacs.d/el-get/yasnippet/snippets")
;; (add-hook 'php-mode-hook 'yas/global-mode)

;; Buffer name (file path) and status in window title
;; (setq frame-title-format
;;   '("emacs%@" (:eval (system-name)) ": " (:eval (if (buffer-file-name)
;;                 (abbreviate-file-name (buffer-file-name))
;;                   "%b")) " [%*]"))

;; Interactive function skeleton
;; (defun foo (str bool)
;;   (interactive
;;    (list (read-string "Some text: ")
;; 	 (y-or-n-p "Do the thing? ")))
;;   (some-func str)
;;   (if bool (some-other-func str)))

;; (add-hook 'yas/minor-mode-on-hook
;;           '(lambda ()
;;             (define-key yas/minor-mode-map yas/trigger-key 'yas/expand)))



(if (eq window-system 'x)
(progn (set-background-color "black")
(message "we are running in x")
))


;; (if (eq window-system 'x)
;; (progn
;; (set-face-attribute 'default nil :background "#2e3436" :foreground "#eeeeec")
;; (set-face-attribute 'cursor nil :background "#fce94f" :foreground "#2e3436")
;; (set-face-attribute 'font-lock-builtin-face nil :foreground "#ad7fa8")
;; (set-face-attribute 'font-lock-comment-face nil :foreground "#73d216")
;; (set-face-attribute 'font-lock-constant-face nil :foreground "#e6a8df")
;; (set-face-attribute 'font-lock-function-name-face nil :foreground "#fce84f")
;; (set-face-attribute 'font-lock-keyword-face nil :foreground "#8cc4ff")
;; (set-face-attribute 'font-lock-string-face nil :foreground "#e9b96e")
;; (set-face-attribute 'font-lock-type-face nil :foreground "#a5ff4d")
;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#fcaf3e")
;; (set-face-attribute 'font-lock-warning-face nil :foreground "#ef2929")
;; (set-face-attribute 'fringe nil :background "#2c2c2c")
;; (set-face-attribute 'header-line nil :background "#555753" :foreground "#ffffff")
;; (set-face-attribute 'isearch nil :background "#ce5c00" :foreground "#ffffff")
;; (set-face-attribute 'lazy-highlight nil :background "#8f5902")
;; (set-face-attribute 'link nil :foreground "#729fcf" :underline t)
;; (set-face-attribute 'link-visited nil :foreground "#3465a4" :underline t)
;; (set-face-attribute 'minibuffer-prompt nil :foreground "#fce94f")
;; (set-face-attribute 'mode-line nil :background "#777777" :foreground "#000000")
;; (set-face-attribute 'mode-line-inactive nil :background "#555753" :foreground "#ffffff")
;; (set-face-attribute 'region nil :background "#555753")
;; )
;; (set-face-attribute 'default nil :background "black" :foreground "white")
;; )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#2e3436" :foreground "#eeeeec"))))
 '(cursor ((t (:background "#fce94f" :foreground "#2e3436"))))
 '(font-lock-builtin-face ((t (:foreground "#ad7fa8"))))
 '(font-lock-comment-face ((t (:foreground "#73d216"))))
 '(font-lock-constant-face ((t (:foreground "#e6a8df"))))
 '(font-lock-function-name-face ((t (:foreground "#fce84f"))))
 '(font-lock-keyword-face ((t (:foreground "#8cc4ff"))))
 '(font-lock-string-face ((t (:foreground "#e9b96e"))))
 '(font-lock-type-face ((t (:foreground "#a5ff4d"))))
 '(font-lock-variable-name-face ((t (:foreground "#fcaf3e"))))
 '(font-lock-warning-face ((t (:foreground "#ef2929"))))
 '(fringe ((t (:background "#2c2c2c"))))
 '(gnus-summary-selected ((t (:background "orange red" :foreground "black" :weight bold))))
 '(gnus-summary-selected-face ((t (:bold t))) t)
 '(header-line ((t (:background "#555753" :foreground "#ffffff"))))
 '(isearch ((t (:background "#ce5c00" :foreground "#ffffff"))))
 '(lazy-highlight ((t (:background "#8f5902"))))
 '(link ((t (:foreground "#729fcf" :underline t))))
 '(link-visited ((t (:foreground "#3465a4" :underline t))))
 '(minibuffer-prompt ((t (:foreground "#fce94f"))))
 '(mode-line ((t (:background "#777777" :foreground "#000000"))))
 '(mode-line-inactive ((t (:background "#555753" :foreground "#ffffff"))))
 '(my-face ((t (:foreground "goldenrod" :weight ultra-bold))) t)
 '(region ((t (:background "#555753")))))


;; Tabbar faces
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

;; (set-face-attribute 'tabbar-separator nil
;; 		    :background "grey50"
;;  		    :foreground "grey50"
;; 		    :height 1.0)

;; (setq tabbar-separator '(1)) ;; set tabbar-separator size to 1 pixel
(message "%s loaded" (buffer-file-name))
(switch-to-buffer (create-file-buffer "untitled.txt"))
(setq backbuffer (buffer-name))
