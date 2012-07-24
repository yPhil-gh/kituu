;;; See https://github.com/xaccrocheur/kituu/
;; Keep it under 1k lines ;p
;; Remember to remove ~/.emacs.elc if you edit ~/.emacs, break it and repair it
;; Use C-h x to read about what this .emacs can do for you (quite a bit)


;; Init! ______________________________________________________________________

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; External libs
(eval-and-compile
	(require 'tabbar nil 'noerror)				; Tabs to switch buffer
	(require 'tabbar-ruler nil 'noerror)	; Additional functions for tabbar
	(require 'undo-tree nil 'noerror)			; Visualize undo (and allow sane redo)
	(require 'cl nil 'noerror)						; Built-in : Common Lisp lib
	(require 'edmacro nil 'noerror)				; Built-in : Macro bits (Required by iswitchb)
	;; (require 'elid)
	;; (require 'mail-bug nil t)
	(require 'imapua)
	;; Required by my iswitchb hack
	)
;; (mail-bug-init)

(if
		(and
		 (file-exists-p "~/.emacs.d/lisp/nxhtml/autostart.el")
		 (< emacs-major-version 24))
    (progn
      (load "~/.emacs.d/lisp/nxhtml/autostart.el")
      ;; (tabkey2-mode t)
			)
  (progn
    (require 'php-mode nil t)
    (autoload 'php-mode "php-mode" "Major mode for editing php code." t)
    (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))))

(defvar iswitchb-mode-map)
(defvar iswitchb-buffer-ignore)
(defvar show-paren-delay)
(defvar recentf-max-saved-items)
(defvar recentf-max-menu-items)
(defvar ispell-dictionary)
(defvar desktop-path)
(defvar desktop-dirname)
(defvar desktop-base-file-name)
(defvar display-time-string)
(defvar ediff-window-setup-function)
(defvar ediff-split-window-function)
(defvar tabbar-buffer-groups-function)
(defvar px-newName)

;; Funcs! _________________________________________________________________

(defun px-match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert <key>."
  (interactive "p")
  (cond
   ((char-equal 41 (char-before)) (backward-list 1))
   ((char-equal 125 (char-before)) (backward-list 1))
   ((and
     (char-equal 123 (char-before))
     (char-equal 10 (char-after)))
    (backward-char 1) (forward-list 1))
   ((looking-at "\\s\(") (forward-list 1))
   ((looking-at "\\s\)") (backward-list 1))
   (t (self-insert-command (or arg 1)))))

(defun px-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun px-kill-buffer ()
  "Prompt when a buffer is about to be killed."
  (interactive)
	(if (and (buffer-modified-p)
					 buffer-file-name
					 (file-exists-p buffer-file-name)
					 (setq backup-file (car (find-backup-file-name buffer-file-name))))
			(let ((answer (completing-read (format "Buffer modified %s, (d)iff, (s)ave, (k)ill? " (buffer-name))
																		 '("d" "s" "k") nil t)))
				(cond
				 ((equal answer "d")
					(set-buffer-modified-p nil)
					(let ((orig-buffer (current-buffer))
								(file-to-diff (if (file-newer-than-file-p buffer-file-name backup-file)
																	buffer-file-name
																backup-file)))
						(set-buffer (get-buffer-create (format "%s last-revision" (file-name-nondirectory file-to-diff))))
						(buffer-disable-undo)
						(insert-file-contents file-to-diff nil nil nil t)
						(set-buffer-modified-p nil)
						(setq buffer-read-only t)
						(ediff-buffers (current-buffer) orig-buffer)))
				 ((equal answer "k")
					(progn
						(kill-buffer (current-buffer))
						(delete-window)))
				 (t
					(progn
						(save-buffer)
						(kill-buffer (current-buffer))
						(delete-window)
						))))
		(progn
			(kill-buffer (current-buffer))
			(delete-window))))

(defun px-byte-compile-user-init-file ()
	"byte-compile .emacs each time it is edited"
  (let ((byte-compile-warnings '(unresolved)))
    ;; in case compilation fails, don't leave the old .elc around:
    (when (file-exists-p (concat user-init-file ".elc"))
      (delete-file (concat user-init-file ".elc")))
    (byte-compile-file user-init-file)
    (message "%s compiled" user-init-file)
    ))

(defun px-emacs-lisp-mode-hook ()
  (when (string-match "\\.emacs" (buffer-name))
    (add-hook 'after-save-hook 'px-byte-compile-user-init-file t t)))

(add-hook 'emacs-lisp-mode-hook 'px-emacs-lisp-mode-hook)

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
				 (file-name-extension buffer-file-name)))
  (write-region (point-min) (point-max) px-newName)
  (message "backuped %s" px-newName))

(defun px-query-replace-in-open-buffers (arg1 arg2)
  "query-replace in all open files"
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
    (skip-chars-backward "^<>([{“「『‹«（〈《〔【〖⦃\"")
    (setq b1 (point))
    (skip-chars-forward "^<>)]}”」』›»）〉》〕】〗⦄\"")
    (setq b2 (point))
    (set-mark b1)))

(global-set-key (kbd "s-SPC") 'select-text-in-quote-px)

(defun insert-or-enclose-with-signs (leftSign rightSign)
  "Insert a matching bracket and place the cursor between them."
  (interactive)
  (if mark-active
      (progn
        (save-excursion
          (setq debut (region-beginning)
                fin (+ 1(region-end)))
					(goto-char debut)
					(insert leftSign)
					(goto-char fin)
					(insert rightSign)
					(forward-char 1)))
    (progn
      (insert leftSign rightSign)
      (backward-char 1))))

(defun insert-pair-paren () (interactive) (insert-or-enclose-with-signs "(" ")"))
(defun insert-pair-brace () (interactive) (insert-or-enclose-with-signs "{" "}"))
(defun insert-pair-bracket () (interactive) (insert-or-enclose-with-signs "[" "]"))
(defun insert-pair-single-angle () (interactive) (insert-or-enclose-with-signs "<" ">"))
(defun insert-pair-squote () (interactive) (insert-or-enclose-with-signs "'" "'"))
(defun insert-pair-dbquote () (interactive) (insert-or-enclose-with-signs "\"" "\""))

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

(defun stop-using-minibuffer ()
  "kill the minibuffer when going back to emacs using the mouse"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

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

(defun px-toggle-comments ()
  "If region is set, [un]comments it. Otherwise [un]comments current line."
  (interactive)
  (if (eq mark-active nil)
      (progn (beginning-of-line 1)
						 (set-mark (point))
						 (forward-line)
						 (comment-dwim nil))
    (comment-dwim nil))
  (deactivate-mark))

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups."
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs Buffer"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    (t
     "User Buffer"))))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)


;; Sessions! ______________________________________________________________________

(require 'desktop)

(defvar my-desktop-session-dir
  (concat (getenv "HOME") "/.emacs.d/desktop-sessions/")
  "Directory to save desktop sessions in")

(defvar my-desktop-session-name-hist nil
  "Desktop session name history")

(defun my-desktop-save (&optional name)
  "Save desktop by name."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Save session" t)))
  (when name
    (make-directory (concat my-desktop-session-dir name) t)
    (desktop-save (concat my-desktop-session-dir name) t)))

(defun my-desktop-save-and-clear ()
  "Save and clear desktop."
  (interactive)
  (call-interactively 'my-desktop-save)
  (desktop-clear)
  (setq desktop-dirname nil))

(defun my-desktop-read (&optional name)
  "Read desktop by name."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Load session")))
  (when name
    (desktop-clear)
    (desktop-read (concat my-desktop-session-dir name))))

(defun my-desktop-change (&optional name)
  "Change desktops by name."
  (interactive)
  (let ((name (my-desktop-get-current-name)))
    (when name
      (my-desktop-save name))
    (call-interactively 'my-desktop-read)))

(defun my-desktop-name ()
  "Return the current desktop name."
  (interactive)
  (let ((name (my-desktop-get-current-name)))
    (if name
        (message (concat "Desktop name: " name))
      (message "No named desktop loaded"))))

(defun my-desktop-get-current-name ()
  "Get the current desktop name."
  (when desktop-dirname
    (let ((dirname (substring desktop-dirname 0 -1)))
      (when (string= (file-name-directory dirname) my-desktop-session-dir)
        (file-name-nondirectory dirname)))))

(defun my-desktop-get-session-name (prompt &optional use-default)
  "Get a session name."
  (let* ((default (and use-default (my-desktop-get-current-name)))
         (full-prompt (concat prompt (if default
                                         (concat " (default " default "): ")
                                       ": "))))
    (completing-read full-prompt (and (file-exists-p my-desktop-session-dir)
                                      (directory-files my-desktop-session-dir))
                     nil nil nil my-desktop-session-name-hist default)))

(defun iswitchb-local-keys ()
	"easily switch buffers (F5 or C-x b)"
  (mapc (lambda (K)
					(let* ((key (car K)) (fun (cdr K)))
						(define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
				'(("<right>" . iswitchb-next-match)
					("<left>"  . iswitchb-prev-match)
					("<up>"    . ignore             )
					("<down>"  . ignore             ))))

(defun my-desktop-kill-emacs-hook ()
  "Save desktop before killing emacs."
  (when (file-exists-p (concat my-desktop-session-dir "last-session"))
    (setq desktop-file-modtime
          (nth 5 (file-attributes (desktop-full-file-name (concat my-desktop-session-dir "last-session"))))))
  (my-desktop-save "last-session"))

(add-hook 'kill-emacs-hook 'my-desktop-kill-emacs-hook)


;; Modes! ______________________________________________________________________

(set-scroll-bar-mode `right)
(auto-fill-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(put 'overwrite-mode 'disabled t)

(setq c-default-style "bsd"
      c-basic-offset 2)
;; (when (functionp 'savehist-mode) (savehist-mode 1))

;; Hooks! _____________________________________________________________________

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
(add-hook 'find-file-hooks 'turn-on-font-lock)
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Vars! ______________________________________________________________________

;; (all of this will slowly migrate to custom)
(setq-default cursor-type 'bar)

(setq
 iswitchb-buffer-ignore '("^ " "*.")
 ispell-dictionary "francais"

 ;; delete-by-moving-to-trash t
 list-colors-sort 'hsv

 default-major-mode 'text-mode
 text-mode-hook 'turn-on-auto-fill
 fill-column 75

 ediff-window-setup-function (quote ediff-setup-windows-plain)
 ediff-split-window-function 'split-window-horizontally)

;; Window title (with edited status + remote indication)
(setq frame-title-format
      '("" invocation-name " " emacs-version " %@ "(:eval (if (buffer-file-name)
																															(abbreviate-file-name (buffer-file-name))
																														"%b")) " [%*]"))


;; Keys! ______________________________________________________________________

(global-set-key (kbd "C-h x") 'px-help-emacs)

(global-set-key "ù" 'px-match-paren)

(global-set-key (kbd "²") 'dabbrev-expand)
(global-set-key (kbd "M-²") 'hippie-expand)

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

(global-set-key (kbd "C-s-g") 'goto-line)
(global-set-key (kbd "C-s-t") 'sgml-close-tag)
(global-set-key "\C-f" 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-backward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)

(global-set-key (kbd "C-ù") 'forward-sexp)
(global-set-key (kbd "C-%") 'backward-sexp)

(global-set-key (kbd "s-g") 'px-google-that-bitch) ;; zob
(global-set-key (kbd "s-r") 'replace-string)
(global-set-key (kbd "s-²") (kbd "C-x b <return>")) ; Keyboard macro! (toggle last buffer)
(global-set-key (kbd "s-t") 'sgml-tag) ;; zob
(global-set-key (kbd "s-k") 'px-kill-buffer) ;; zob
(global-set-key (kbd "s-p") 'php-mode) ;; zob
(global-set-key (kbd "s-h") 'html-mode) ;; zob
(global-set-key (kbd "s-j") 'js-mode) ;; zob
(global-set-key (kbd "s-o") 'find-file-at-point)
(global-set-key (kbd "s-d") 'wl-draft-mode)
(global-set-key (kbd "s-m") 'kmacro-end-and-call-macro)
(global-set-key (kbd "C-s-m") 'apply-macro-to-region-lines)
(global-set-key (kbd "<s-left>") (kbd "C-x C-SPC"))
(global-set-key (kbd "<s-right>") 'marker-visit-next)
(global-set-key (kbd "<s-up>") (kbd "C-u C-SPC"))

;; THIS NEXT ONE BROKE HAVOC!!
;; (global-set-key (kbd "C-d") nil)	; I kept deleting stuff
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-S-o") 'my-desktop-read)
(global-set-key (kbd "C-S-<mouse-1>") 'flyspell-correct-word)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

(global-set-key (kbd "C-<tab>") 'tabbar-forward)
(global-set-key (kbd "<C-S-iso-lefttab>") 'tabbar-backward)

(global-set-key (kbd "C-=") 'insert-pair-brace)				 ;{}
(global-set-key (kbd "C-)") 'insert-pair-paren)				 ;()
(global-set-key (kbd "C-(") 'insert-pair-bracket)			 ;[]
(global-set-key (kbd "C-<") 'insert-pair-single-angle) ;<>
(global-set-key (kbd "C-'") 'insert-pair-squote)			 ;''
(global-set-key (kbd "C-\"") 'insert-pair-dbquote)		 ;""

(global-set-key (kbd "M-s") 'save-buffer) ; Meta+s saves !! (see C-h b for all bindings, and C-h k + keystroke(s) for help)
(global-set-key (kbd "M-DEL") 'kill-word)
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-o") 'recentf-open-files)
(global-set-key (kbd "M-d") 'px-toggle-comments)


;; ;; Help! ______________________________________________________________________

(defun px-help-emacs ()
  (interactive)
  (princ "* My EMACS cheat cheet

** notes
's' (super) on a PC keyboard, is the 'windows logo' key
*bold* denotes a custom bit (eg specific to this emacs config)

*** THIS VERY EMACS CONFIG
*Open file                           C-o*
*Open recent file                    M-o*
*Open file path at point             s-o*
*Open last session (buffers)         C-S-o*

*Save buffer                         M-s*
*Kill current buffer                 s-k*
*Undo                                C-z*
*Redo                                C-S-z*
*Switch last buffer                  s-²*
*Scroll other buffer                 M-<arrow>*

*Close other window (frame)          F1*
*Switch to other window (frame)      F2*
*Split horizontally                  F3*
*Split vertically                    F4*
*Switch to buffer                    F5*
*Toggle two last buffers             s-²*
*Spell-check buffer                  F7*
*Word-wrap toggle                    F10*

*match brace                         ù*
*next brace pair                     C-ù*
*Previous brace pair                 C-S-ù*
*enclose region in <tag> (sgml-tag)  s-t RET tag [ args... ]*
*select 'this' or <that> (enclosed)  s-SPC**

*php-mode                            s-p*
*html-mode                           s-h*
*js-mode                             s-j*

*** EMACSEN
Recenter window around current line  C-l
Intelligently recenter window        C-S-l
Copy to register A                   C-x r s A
Paste from register A                C-x r g A
Set bookmark at point                C-x r m RET
Close HTML tag                       sgml-close-tag
Switch to *Messages* buffer          C-h e

*** RECTANGLES
C-x r k/c                            Kill/clear rectangle
C-x r y                              yank-rectangle (upper left corner at point)
C-x r t string <RET>                 Insert STRING on each rectangle line.

*** MISC EDITING
M-c		                               capitalize-word		Capitalize the first letter of the current word.
M-u		                               upcase-word		Make the word all uppercase.
M-l		                               downcase-word		Make the word all lowercase.
C-x C-l		                           downcase-region		Make the region all lowercase.
C-x C-u		                           uppercase-region	Make the region all uppercase.

*** MACROS
C-x (		                             start-kbd-macro		Start a new macro definition.
C-x )		                             end-kbd-macro		End the current macro definition.
C-x e		                             call-last-kbd-macro	Execute the last defined macro.
M-(number) C-x e	                   call-last-kbd-maco	Do that last macro (number times).
C-u C-x (	                           stat-kbd-macro		Execute last macro and add to it.
		                                 name-last-kbd-macro	Name the last macro before saving it.
            		                     insert-last-keyboard-macro	Insert the macro you made into a file.
		                                 load-file			Load a file with macros in it.
C-x q		                             kbd-macro-query		Insert a query into a keyboard macro.
M-C-c		                             exit-recursive-edit		Get the hell out of a recursive edit.

*** EDIFF
Next / previous diff                 n / p
Copy a diff into b / opposite        a / b
Save a / b buffer                    wa / wb

*** GNUS
Sort summary by author/date          C-c C-s C-a/d
Search selected imap folder          G G
Mark thread read                     T k

*** PHP-MODE
C-c C-f                              Search PHP manual for point.
C-c RET / C-c C-m                    Browse PHP manual in a Web browser.

*** WANDERLUST
T                                    Toggle Threading
d                                    Dispose MSG (mark)
D                                    Delete MSG (mark)
rx                                   Execute marks

*** VERSION CONTROL
C-x v v                              vc-next-action
perform the next logical control operation on file
C-x v i                              vc-register
add a new file to version control

C-x v +                              vc-update
Get latest changes from version control
C-x v ~                              vc-version-other-window
look at other revisions
C-x v =                              vc-diff
diff with other revisions
C-x v u                              vc-revert-buffer
undo checkout
C-x v c                              vc-cancel-version
delete latest rev (look at an old rev and re-check it)

C-x v d                              vc-directory
show all files which are not up to date
C-x v g                              vc-annotate
show when each line in a tracked file was added and by whom
C-x v s                              vc-create-snapshot
tag all the files with a symbolic name
C-x v r                              vc-retrieve-snapshot
undo checkouts and return to a snapshot with a symbolic name

C-x v l                              vc-print-log
show log (not in ChangeLog format)
C-x v a                              vc-update-change-log
update ChangeLog

C-x v m                              vc-merge
C-x v h                              vc-insert-headers

M-x                                  vc-resolve-conflicts
ediff-merge session on a file with conflict markers

*** OTHER
git reflog                           view log
git reset --hard HEAD@{7}            revert HEAD to 7
"
         (generate-new-buffer "px-help-emacs"))
  (switch-to-buffer "px-help-emacs")
  (org-mode)
  (goto-char (point-min))
  (org-show-subtree))


;; Custom ! ______________________________________________________________________

(if (< emacs-major-version 24)
		(set-face-attribute 'default nil :background "#2e3436" :foreground "#eeeeec"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.bkp/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.bkp/"))))
 '(bbdb-use-pop-up nil)
 '(buffer-offer-save nil)
 '(c-basic-offset (quote set-from-style))
 '(c-default-style "gnu")
 '(canlock-password "cf5f7a7261c5832898abfc7ea08ba333a36ed78c")
 '(comment-style (quote extra-line))
 '(completion-auto-help (quote lazy))
 '(cursor-in-non-selected-windows nil)
 '(custom-enabled-themes (quote (tango-dark)))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(epa-popup-info-window nil)
 '(global-font-lock-mode t)
 '(global-linum-mode t)
 '(global-undo-tree-mode t)
 '(indent-tabs-mode t)
 '(inhibit-startup-echo-area-message (user-login-name))
 '(inhibit-startup-screen t)
 '(iswitchb-mode t)
 '(menu-bar-mode nil)
 '(mumamo-margin-use (quote (left-margin 13)))
 '(recenter-redisplay nil)
 '(recentf-max-menu-items 60)
 '(recentf-max-saved-items 120)
 '(recentf-mode t)
 '(recentf-save-file "~/.bkp/recentf")
 '(require-final-newline (quote ask))
 '(savehist-mode t)
 '(scroll-conservatively 200)
 '(scroll-margin 3)
 '(send-mail-function (quote mailclient-send-it))
 '(server-mode t)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(standard-indent 2)
 '(tab-always-indent (quote complete))
 '(tab-stop-list (quote (2 4 8 16 24 32 40 48 56 64 72 80 88 96 104 112 120)))
 '(tab-width 2)
 '(tramp-default-method "ssh")
 '(undo-tree-auto-save-history t)
 '(undo-tree-enable-undo-in-region nil)
 '(undo-tree-visualizer-diff t)
 '(vc-make-backup-files t)
 '(web-vcs-default-download-directory (quote site-lisp-dir)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "unknown" :family "Monospace"))))
 '(font-lock-comment-face ((t (:foreground "#73d216" :slant italic))))
 '(minibuffer-prompt ((t (:foreground "#fce94f" :height 1.0))))
 '(mode-line ((t (:background "gray10" :foreground "white" :box nil))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#555753" :foreground "#eeeeec" :box nil :weight light))))
 '(mumamo-background-chunk-major ((t (:background "gray10"))) t)
 '(mumamo-background-chunk-submode1 ((t (:background "gray15"))) t)
 '(mumamo-background-chunk-submode2 ((t (:background "gray20"))) t)
 '(mumamo-background-chunk-submode3 ((t (:background "gray25"))) t)
 '(mumamo-background-chunk-submode4 ((t (:background "gray30"))) t)
 '(show-paren-match ((t (:background "salmon4"))))
 '(tabbar-default ((t (:inherit default))))
 '(tabbar-highlight ((t (:foreground "red" :underline nil))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "#2e3436" :foreground "yellow" :box (:line-width 3 :color "#2e3436")))))
 '(tabbar-unselected ((t (:inherit tabbar-default :background "dim gray" :box (:line-width 3 :color "dim gray"))))))
