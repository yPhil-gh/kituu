;;; See https://github.com/xaccrocheur/kituu/
;; Try & keep it under 1k lines ;p
;; Use M-x px-help-emacs to read about what this .emacs can do for you (quite a bit)

;; Init! ______________________________________________________________________

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(make-directory "~/.emacs.d/lisp/" t)
(make-directory "~/.emacs.d/backup/" t)

;; External libs
(eval-and-compile
  (require 'undo-tree nil 'noerror)   ; Visualize undo (and allow sane redo)
  (require 'cl nil 'noerror)          ; Built-in : Common Lisp lib
  (require 'edmacro nil 'noerror)     ; Built-in : Macro bits (Required by iswitchb)
  (require 'package nil 'noerror)
  (require 'ecb nil 'noerror)
  (require 'uniquify nil 'noerror))

(if (>= emacs-major-version 24)
    (progn
      ;; (require 'mail-bug nil 'noerror)
      (require 'cedet)
      (tool-bar-mode -1)))


;; Packages! ____________________________________________________________________
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (progn (message "installing %s" package)
            (package-refresh-contents)
            (package-install package))))
 '(
   tabbar
   undo-tree
   magit
   yasnippet
   paredit
   paredit-menu
   clojure-mode
   markdown-mode
   php-mode
   rainbow-mode
   ))

(autoload 'magit-status "magit" nil t)


;; JIRA! ______________________________________________________________________

(setq jiralib-url "http://jira.sbcmaroc.com")


;; ORG! ______________________________________________________________________

;; (require 'org-publish)
(setq org-publish-project-alist
      '(("mensup" :components ("org-notes" "org-static"))
        ("org-notes"
         :base-directory "~/Documents/svnmen/"
         :base-extension "org"
         :publishing-directory "~/Documents/svnmen/"
         :recursive t
         :table-of-contents nil
         :publishing-function org-html-publish-to-html)
        ("org-static"
         :base-directory "~/Documents/svnmen/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Documents/svnmen/"
         :recursive t
         :publishing-function org-publish-attachment)))


;; Server! ____________________________________________________________________

(server-start)
(defun ff/raise-frame-and-give-focus ()
  (when window-system
    (raise-frame)
    (x-focus-frame (selected-frame))
    (set-mouse-pixel-position (selected-frame) 4 4)
    ))
(add-hook 'server-switch-hook 'ff/raise-frame-and-give-focus)


(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(global-set-key (kbd "M-i") 'ido-goto-symbol)

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
(defvar px-bkp-new-name)


;; Keywords! _________________________________________________________________

;; (set-face-underline 'font-lock-warning-face "yellow")
;; (set-face-background 'font-lock-warning-face "blue")

;; (add-hook 'emacs-lisp-mode-hook
          ;; (highlight-lines-matching-regexp "\\<\\(FIXME\\|HACK\\|BUG\\|pX\\):" "hi-green-b"))

;; Funcs! _________________________________________________________________

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))
(global-set-key (kbd "<s-up>") 'move-text-up)
(global-set-key (kbd "<s-down>") 'move-text-down)

(defun px-date ()
  "Insert date"
  (interactive)
  (insert (format-time-string "%d %B %Y - %H:%M:%S - %3Nms" (current-time))))

(defun toggle-fullscreen ()
  "Real, mozilla-like full screen."
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defadvice bookmark-jump (after bookmark-jump activate)
  "Bubble last bookmark to the top of the alist"
  (progn
    (let ((latest (bookmark-get-bookmark bookmark)))
      (setq bookmark-alist (delq latest bookmark-alist))
      (add-to-list 'bookmark-alist latest))
    (recenter-top-bottom 15)))

(defun px-bookmarks-toggle-last ()
  "Jump to last bookmark"
  (interactive)
  (bookmark-jump (second bookmark-alist)))

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
  "Switch to scratch buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun px-kill-buffer ()
  "Prompt when a buffer is about to be killed.
Do the right thing and delete the window."
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
            (delete-window)))))
    (progn
      (kill-buffer)
      (if (> (length (window-list)) 1)
          (delete-window))
      (kbd "C-x b <return>"))))

(defun px-bkp ()
  "Write the current buffer to a new file - silently - and append the date+time to the filename, retaining extention
This dates from old times, before VC, I'm keeping it out of pure nostalgy."
  (interactive)
  (setq px-bkp-new-name
        (concat
         (file-name-sans-extension buffer-file-name) "-"
         (format-time-string  "%Y-%m-%d") "."
         (format-time-string "%Hh%M") "."
         (file-name-extension buffer-file-name)))
  (write-region (point-min) (point-max) px-bkp-new-name)
  (message "backuped %s" px-bkp-new-name))

(defun px-query-replace-in-open-buffers (arg1 arg2)
  "query-replace in all open files"
  (interactive "sRegexp:\nsReplace with:")
  (mapcar
   (lambda (x)
     (find-file x)
     (save-excursion
       (goto-char (point-min))
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

(defun px-websearch-that-bitch (start end)
  "Google selected string"
  (interactive "r")
  (let ((q (buffer-substring-no-properties start end)))
    (browse-url (concat "https://duckduckgo.com/?q="
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

(defun px-insert-or-enclose-with-signs (leftSign rightSign)
  "Insert a matching bracket and place the cursor between them."
  (interactive)
  (if mark-active
      (let ((st (point))
            (ed (mark)))
        (goto-char ed)
        (save-excursion
          (if (> st ed)
              (progn (message "sup st: %s ed: %s" st ed)
                     (insert leftSign)
                     (goto-char st)
                     (forward-char 1)
                     (insert rightSign))
            (progn (message "end st: %s ed: %s" st ed)
                   (insert rightSign)
                   (goto-char st)
                   (insert leftSign)
                   (goto-char (+ 1 ed)))))
        (if (> st ed)
            (goto-char (+ 2 st))
          (goto-char (+ 2 ed))))
    (progn
      (insert leftSign rightSign)
      (backward-char 1))))

;; (test string)

(defun insert-pair-paren () (interactive) (px-insert-or-enclose-with-signs "(" ")"))
(defun insert-pair-brace () (interactive) (px-insert-or-enclose-with-signs "{" "}"))
(defun insert-pair-bracket () (interactive) (px-insert-or-enclose-with-signs "[" "]"))
(defun insert-pair-single-angle () (interactive) (px-insert-or-enclose-with-signs "<" ">"))
(defun insert-pair-squote () (interactive) (px-insert-or-enclose-with-signs "'" "'"))
(defun insert-pair-dbquote () (interactive) (px-insert-or-enclose-with-signs "\"" "\""))

(defun px-frigo ()
  (interactive)
  "Copy the current region, paste it in frigo.txt with a time tag, and save this file"
  (unless (use-region-p) (error "No region selected"))
  (let ((bn (file-name-nondirectory (buffer-file-name))))
    (copy-region-as-kill (region-beginning) (region-end))
    (with-current-buffer (find-file-noselect "~/.emacs.d/backup/frigo.txt")
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

(defun px-exit-minibuffer ()
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
      (progn
        (beginning-of-line 1)
        (set-mark (point))
        (forward-line)
        (comment-dwim nil))
    (comment-dwim nil))
  (deactivate-mark))

(defun px-tabbar-buffer-groups ()
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

(defun iswitchb-local-keys ()
  "easily switch buffers (F5 or C-x b)"
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))

;; Sessions! ______________________________________________________________________

(require 'desktop)

;; px-session (is broken with undo)
;; Desktop
(setq desktop-path '("~/.emacs.d/backup/"))
(setq desktop-dirname "~/.emacs.d/backup/")
(setq desktop-base-file-name "emacs-desktop")

;; (the lock file is ~/.emacs.d/backup/.emacs.desktop.lock)
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

(defun px-session-save-named (px-session-named-name)
  "Prompt the user for a session name."
  (interactive "MSession name: ")
  (message "So what do I do with this: %s ?" px-session-named-name)
  (desktop-save (concat desktop-dirname "/" px-session-named-name
                        ".session") t)
  )


;; Modes! _____________________________________________________________________

;; (string-match "*message*" "*message*-plop")

(tabbar-mode t)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(put 'overwrite-mode 'disabled t)
(setq c-default-style "bsd"
      c-basic-offset 2)
;; (when (functionp 'savehist-mode) (savehist-mode 1))
;; (semantic-mode t)
;; (which-function-mode t)
(yas-global-mode 1)
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.list\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

(add-hook 'php-mode-hook (lambda () (setq comment-start "//"
                                        comment-end   "")))

(add-hook 'markdown-mode-hook (lambda () (turn-off-auto-fill)))

(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(remove-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; Externals! _________________________________________________________________

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; Hooks! _____________________________________________________________________

(add-hook 'haml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (define-key haml-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
(add-hook 'find-file-hooks 'turn-on-font-lock)
;; (add-hook 'mouse-leave-buffer-hook 'px-exit-minibuffer)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-to-list 'fill-nobreak-predicate 'fill-french-nobreak-p)
(setq paragraph-start "\\*\\|$"
      paragraph-separate "$")

;; Vars! ______________________________________________________________________

;; (all of this will slowly migrate to custom)
(setq-default cursor-type 'bar)

(setq

 bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
 bookmark-save-flag 1

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
      '(""
        invocation-name
        " "
        emacs-version
        " %@ "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " [%*]"))

;; Keys! ______________________________________________________________________

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(defun px-join-line ()
  (join-line 1))

;; (global-set-key (kbd "C-j") 'join-line)

(setq-default indent-tabs-mode nil)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(global-set-key (kbd "M-s-b") 'bookmark-set)
(global-set-key (kbd "s-b") 'bookmark-jump)
(define-key global-map [M-f1] 'px-bookmarks-toggle-last)
;; (define-key global-map [s-b] 'bookmark-set)

(global-set-key (kbd "C-h x") 'px-help-emacs)
(global-set-key (kbd "C-h *") 'px-scratch)

;; (global-set-key (kbd "²") 'dabbrev-expand)
(global-set-key (kbd "²") 'hippie-expand)

(define-key global-map [(meta up)] '(lambda() (interactive) (scroll-other-window -1)))
(define-key global-map [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))

(define-key global-map [f1] 'delete-other-windows)
(define-key global-map [S-f1] 'px-help-emacs)
(define-key global-map [f2] 'other-window)
(define-key global-map [M-f2] 'swap-buffers-in-windows)
(define-key global-map [f3] 'split-window-vertically)
(define-key global-map [f4] 'split-window-horizontally)
(define-key global-map [f5] 'iswitchb-buffer) ;new way
(define-key global-map [f7] 'flyspell-buffer)
(define-key global-map [M-f7] 'flyspell-mode)
(define-key global-map [f10] 'toggle-truncate-lines)
(define-key global-map [f12] 'px-fullscreen)


(global-set-key (kbd "C-s-g") 'goto-line)
(global-set-key (kbd "C-s-t") 'sgml-close-tag)
(global-set-key "\C-f" 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-backward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)

(global-set-key "ù" 'px-match-paren)
(global-set-key (kbd "C-ù") 'forward-sexp)
(global-set-key (kbd "C-%") 'backward-sexp)

(global-set-key (kbd "C-c C-g") 'px-websearch-that-bitch) ;; zob
(global-set-key (kbd "s-r") 'replace-regexp)
(global-set-key (kbd "s-²") (kbd "C-x b <return>")) ; Keyboard macro! (toggle last buffer)
(global-set-key (kbd "s-t") 'sgml-tag) ;; zob
(global-set-key (kbd "s-k") 'px-kill-buffer) ;; zob
(global-set-key (kbd "s-p") 'php-mode) ;; zob
(global-set-key (kbd "s-h") 'html-mode) ;; zob
(global-set-key (kbd "s-j") 'js-mode) ;; zob
(global-set-key (kbd "s-m") 'message-mail)
(global-set-key (kbd "s-o") 'find-file-at-point)
(global-set-key (kbd "s-d") 'px-date)
(global-set-key (kbd "s-<") 'kmacro-end-and-call-macro)
(global-set-key (kbd "C-s-m") 'apply-macro-to-region-lines)
(global-set-key (kbd "s-SPC") 'select-text-in-quote-px)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "<s-left>") (kbd "C-u C-SPC"))

(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-S-o") 'my-desktop-read)
(global-set-key (kbd "C-S-<mouse-1>") 'flyspell-correct-word)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)


(global-set-key (kbd "C-<tab>") 'tabbar-forward)
(global-set-key (kbd "<C-S-iso-lefttab>") 'tabbar-backward)

(global-set-key (kbd "C-=") 'insert-pair-brace)        ;{}
(global-set-key (kbd "C-)") 'insert-pair-paren)        ;()
(global-set-key (kbd "C-(") 'insert-pair-bracket)      ;[]
(global-set-key (kbd "C-<") 'insert-pair-single-angle) ;<>
(global-set-key (kbd "C-'") 'insert-pair-squote)       ;''
(global-set-key (kbd "C-\"") 'insert-pair-dbquote)     ;""

(global-set-key (kbd "M-s") 'save-buffer) ; Meta+s saves !! (see C-h b for all bindings, and C-h k + keystroke(s) for help)
(global-set-key (kbd "M-o") 'recentf-open-files)
(global-set-key (kbd "M-d") 'px-toggle-comments)


;; Help! ______________________________________________________________________

(defun px-help-emacs ()
  (interactive)
  (princ "* EMACS cheat cheet

** notes
- A newline is added at the EOF and all trailing spaces are removed at each
  file save
- The 'emacs buffers' like '*Scratch*' are hidden from the main - F5, C-x b -
buffer list. If you really must see them, use the usual C-x C-b. To
directly access the scratch buffer, use C-h *
- Tabs and spaces are mixed. Emacs tries to do the smart thing depending on
context
- 's' (super) on a PC keyboard, is the 'windows logo' key

** THIS VERY EMACS CONFIG
Open file                                                        C-o
Open recent file                                                 M-o
Open file path at point                                          s-o
Open last session (buffers)                                      C-S-o
Save named session (buffers)                                     s-s

Save buffer                                                      M-s
Safely kill buffer                                               s-k
Undo                                                             C-z
Redo                                                             C-S-z
Undo Tree                                                        C-x u
Scroll buffer in other window/pane                               M-<arrow>

Go back to previous position (marking current)                   s-<left>

Next buffer                                                      C-TAB
Previous buffer                                                  C-S-TAB
Toggle two last buffers                                          s-²

Close other window/pane                                          F1
Switch to other window/pane                                      F2
Split horizontally                                               F3
Split vertically                                                 F4
Switch to buffer (list)                                          F5
Spell-check buffer                                               F7
Word-wrap toggle                                                 F10

Move line up / Down                                              s-<arrow>

Match brace (() and {})                                          ù
Next brace pair                                                  C-ù
Previous brace pair                                              C-S-ù
Enclose region in <tag> (sgml-tag)                               s-t RET tag [ args... ]
Select 'this' or <that> (enclosed)                               s-SPC
Search selection in google                                       C-c g
Completion (hippie-expand : any term, dir paths, etc.)           ²

Php-mode                                                         s-p
Html-mode                                                        s-h
Js-mode                                                          s-j

** ALL EMACSEN
Uppercase                                                        M-u
Lowercase                                                        M-l
Capitalize                                                       M-c

Recenter line on screen (cycle up-down-middle)                   C-l
Go to line                                                       M-g g
Go back to previous position  (w/o marking current -?!)          C-u C-SPC
Intelligently recenter window                                    C-S-l
Copy to register A                                               C-x r s A
Paste from register A                                            C-x r g A
Set bookmark at point                                            C-x r m RET
Close HTML tag                                                   sgml-close-tag
Switch to *Messages* buffer                                      C-h e
Transpose current line with previous one                         C-x C-t

** RECTANGLES
Kill/clear rectangle                                             C-x r k/c
yank-rectangle (upper left corner at point)                      C-x r y
Insert STRING on each rectangle line.                            C-x r t string <RET>

** MISC EDITING
capitalize-word                                                  M-c
upcase-word                                                      M-u
downcase-word                                                    M-l
downcase-region                                                  C-x C-l
uppercase-region                                                 C-x C-u

** MACROS
start-kbd-macro                                                  C-x (
Start a new macro definition.
end-kbd-macro                                                    C-x )
End the current macro definition.
call-last-kbd-macro                                              C-x e
Execute the last defined macro.
call-last-kbd-maco                                               M-(number) C-x e
Do that last macro (number times).
stat-kbd-macro                                                   C-u C-x (
Execute last macro and add to it.
name-last-kbd-macro
Name the last macro before saving it.
insert-last-keyboard-macro
Insert the macro you made into a file.
load-file
Load a file with macros in it.
kbd-macro-query                                                  C-x q
Insert a query into a keyboard macro.
exit-recursive-edit                                              M-C-c
Get the hell out of a recursive edit.

** EDIFF (ediff)
Next / previous diff                                             n / p
Copy a diff into b / opposite                                    a / b
Save a / b buffer                                                wa / wb

** GNUS (gnus)
Sort summary by author/date                                      C-c C-s C-a/d
Search selected imap folder                                      G G
Mark thread read                                                 T k

** PHP-MODE (php-mode)
Search PHP manual for <point>.                                   C-c C-f
Browse PHP manual in a Web browser.                              C-c RET / C-c C-m

** VERSION CONTROL
M-x vc-resolve-conflicts
Ediff-merge session on a file with conflict markers

** OTHER
Revert HEAD to 7                                                 git reset --hard HEAD@{7}
"
         (generate-new-buffer "px-help-emacs"))
  (switch-to-buffer "px-help-emacs")
  (org-mode)
  (goto-char (point-min))
  (org-show-subtree))


;; Custom ! ______________________________________________________________________

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/backup/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backup/"))))
 '(bbdb-use-pop-up nil)
 '(bookmark-sort-flag nil)
 '(buffer-offer-save nil)
 '(c-basic-offset (quote set-from-style) t)
 '(c-default-style "gnu" t)
 '(canlock-password "ebef4a12d0fad1c648b4b829291adb16cdefb9da")
 '(comment-style (quote indent))
 '(completion-auto-help (quote lazy))
 '(cursor-in-non-selected-windows nil)
 '(custom-enabled-themes (quote (tango-dark)))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(epa-popup-info-window nil)
 '(fold-dwim-outline-style-default (quote nested))
 '(font-use-system-font t)
 '(global-font-lock-mode t)
 '(global-linum-mode t)
 '(global-undo-tree-mode t)
 '(haml-backspace-backdents-nesting nil)
 '(inhibit-startup-echo-area-message (user-login-name))
 '(inhibit-startup-screen t)
 '(iswitchb-mode t)
 '(keyboard-coding-system (quote utf-8) nil nil "nil before, now utf-8.")
 '(magit-restore-window-configuration t)
 '(mail-host-address "")
 '(mail-interactive t)
 '(mark-ring-max 8)
 '(mbug-bcc-to-sender t)
 '(mbug-host-name "")
 '(mbug-inline-images t)
 '(mbug-modal t)
 '(mbug-short-headers t)
 '(mbug-username "")
 '(message-confirm-send t)
 '(message-default-charset (quote utf-8))
 '(mm-enable-external (quote ask))
 '(mm-text-html-renderer (quote links))
 '(mumamo-margin-use (quote (left-margin 13)))
 '(org-agenda-files (quote ("~/.org/")))
 '(org-cycle-separator-lines 1)
 '(org-directory "~/.org")
 '(org-export-html-validation-link "<a href=\"http://validator.w3.org/check?uri=referer\">Validation XHTML 1.0</a>")
 '(org-html-metadata-timestamp-format "%d/%m/%Y %T")
 '(org-html-postamble t)
 '(org-html-postamble-format (quote (("en" "<p class=\"author\"><a href=\"/men/wiki\">← Accueil</a> - <a
href=\"#\">↑ Page</a> %a (%e) - %v</p>"))))
 '(org-log-done (quote time))
 '(org-support-shift-select (quote always))
 '(org-use-sub-superscripts nil)
 '(recenter-positions (quote (middle top bottom)))
 '(recenter-redisplay nil)
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude (quote ("emacs.d\\/session")))
 '(recentf-max-menu-items 60)
 '(recentf-max-saved-items 120)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/backup/recentf")
 '(require-final-newline t)
 '(savehist-mode t nil (savehist))
 '(scroll-conservatively 200)
 '(scroll-margin 3)
 '(send-mail-function (quote smtpmail-send-it))
 '(server-mode t)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(standard-indent 2)
 '(tramp-default-method "ssh")
 '(undo-limit 400000)
 '(undo-strong-limit 600000)
 '(undo-tree-auto-save-history t)
 '(undo-tree-enable-undo-in-region nil)
 '(undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/backup/"))))
 '(undo-tree-visualizer-diff t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-full-name "Philippe Coatmeur")
 '(user-mail-address "hidden")
 '(vc-make-backup-files nil)
 '(web-vcs-default-download-directory (quote site-lisp-dir)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :background "gray20" :foreground "white" :height 105))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(mode-line ((t (:background "gray10" :foreground "white" :box nil))))
 '(mode-line-buffer-id ((t (:weight bold :foreground "OrangeRed1"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "gray33" :foreground "#eeeeec" :box nil :weight light))))
 '(show-paren-match ((t (:background "OrangeRed1"))))
 '(tabbar-button ((t (:inherit tabbar-default))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default :background "OrangeRed1"))))
 '(tabbar-default ((t (:inherit default :background "dim gray" :box (:line-width 1 :color "gray35")))))
 '(tabbar-highlight ((t (:background "OrangeRed1" :foreground "white" :box (:line-width 1 :color "OrangeRed1")))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "grey20" :foreground "OrangeRed1" :box (:line-width 1 :color "grey20")))))
 '(tabbar-separator ((t (:height 0.1))))
 '(tabbar-unselected ((t (:inherit tabbar-default :background "gray35"))))
 '(which-func ((t (:foreground "OrangeRed1"))) t))

;; Ever since elpa tabbar, this has to be called late
(setq tabbar-buffer-groups-function 'px-tabbar-buffer-groups)

(defun px-laptop-mode ()
  "smaller default size"
  (interactive)
  (set-face-attribute 'default nil :height 90))

(defun px-desktop-mode ()
  "default font size"
  (interactive)
  (set-face-attribute 'default nil :height 105))
