;;; See https://github.com/xaccrocheur/kituu/
;; Keep it under 1k lines ;p

;; Init! ______________________________________________________________________

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(make-directory "~/.emacs.d/elisp/" t)
(make-directory "~/.emacs.d/backup/" t)

(let ((default-directory "~/.emacs.d/elisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; (eval-and-compile
;;   (require 'mail-bug nil 'noerror))

;; Packages! ____________________________________________________________________

(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;(add-to-list 'package-archives
  ;;           '("marmalade" . "http://marmalade-repo.org/packages/"))

(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (progn (message "installing %s" package)
            (package-refresh-contents)
            (package-install package))))
 '(less-css-mode tabbar org auto-complete undo-tree magit clojure-mode markdown-mode yasnippet paredit paredit-menu php-mode haml-mode rainbow-mode))


;; LIBS! ______________________________________________________________________

(eval-and-compile
  (require 'cl nil 'noerror)          ; Built-in : Common Lisp lib
  ;; (require 'edmacro nil 'noerror)     ; Built-in : Macro bits (Required by iswitchb)
  (require 'package nil 'noerror)
  (require 'mail-bug nil 'noerror)
  (require 'pixilang-mode nil 'noerror)
  (require 'bpm nil 'noerror)
  (require 'zeroconf nil 'noerror)
  (require 'auto-complete nil 'noerror)
  )

;; (require 'semantic/ia)
;; (require 'semantic/bovine/gcc)

;; (semantic-mode 1)

;; (defun my-semantic-hook ()
;;   (imenu-add-to-menubar "TAGS"))
;; (add-hook 'semantic-init-hooks 'my-semantic-hook)

;; (global-ede-mode 1)                      ; Enable the Project management system
;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
;(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; if you want to enable support for gnu global
;(when (cedet-gnu-global-version-check t)
; (semanticdb-enable-gnu-global-databases 'c-mode)
; (semanticdb-enable-gnu-global-databases 'c++-mode)

;; enable ctags for some languages:
 ;; Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
;(when (cedet-ectag-version-check t)
; (semantic-load-enable-primary-exuberent-ctags-support))

(zeroconf-init nil)                   ; NIL means "local"

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; JIRA! ______________________________________________________________________

;; (setq jiralib-url "http://jira.sbcmaroc.com:8080")

;; Vars!

;; Keep unreadable files in recentf
(setq recentf-keep '(file-remote-p file-readable-p))

; style I want to use in c++ mode
(c-add-style "my-style"
             '("stroustrup"
               (indent-tabs-mode . nil)                           ; use spaces rather than tabs
               (c-basic-offset . 4)                               ; indent by four spaces
               (c-offsets-alist . ((inline-open . 0)              ; custom indentation rules
                                   (brace-list-open . 0)
                                   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; (defvar iswitchb-mode-map)
;; (defvar iswitchb-buffer-ignore)
;; (defvar show-paren-delay)
;; (defvar recentf-max-saved-items)
;; (defvar recentf-max-menu-items)
;; (defvar ispell-dictionary)
;; (defvar desktop-path)
;; (defvar desktop-dirname)
;; (defvar desktop-base-file-name)
;; (defvar display-time-string)
;; (defvar ediff-window-setup-function)
;; (defvar ediff-split-window-function)
;; (defvar tabbar-buffer-groups-function)
;; (defvar px-bkp-new-name)

;; Funcs! _________________________________________________________________

(defun px-shell-command (cmd)
  "Invoke CMD in a `px-shell'"
  (interactive "sShell command: ")
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (shell (get-buffer-create "px-shell"))
        (process-send-string (get-buffer-process "px-shell") (concat "reset\n" cmd "\nexit 0 &> /dev/null\n")))
      (other-window 1))

(defun test-rx-read-file (test-file test-reg)
  "Test rx form"
  (if (get-buffer (setq test-buffer "RX"))
      (kill-buffer test-buffer))

  (with-temp-buffer
    (insert-file-contents-literally test-file)
    (goto-char (point-min))

    (while
        (re-search-forward test-reg nil t)
      (when (match-string 0)
        (let ((one (match-string 1))
              (two (match-string 2)))
          (if one
              (progn
                (pop-to-buffer test-buffer)
                (goto-char (point-max))
                (insert (format "Found (%s) and possibly (%s) in (%s)" one two test-file)))))))))

(defun test-rx (file)
  "Test rx form"
  (interactive
   (let (file)
     (setq file (read-file-name "File to search: "))
     (list file)))
  (test-rx-read-file file
                     (rx
                      "$(function"
                      (*? anything)
                      (or "'" "\"")
                      (group
                       (* (not (any "'" "\"")))
                       ".php"
                       )
                      (or "'" "\"")
                      (*? anything)
                      "})"
                      )))


(defun px-pop-to-mark-command ()
  "Go back up the mark history. Recenter if far away."
  (interactive)
  (let ((current-prefix-arg '(t))
        (start-line (line-number-at-pos))
        (maxxx-line (line-number-at-pos (point-max)))
        (view-lines (window-height)))
    (call-interactively 'set-mark-command)

    (let ((end-line (line-number-at-pos (point))))
      (if (< end-line start-line)
          (if (> (- start-line end-line) view-lines)
              (call-interactively 'recenter-top-bottom))
        (if (> (- end-line start-line) view-lines)
            (call-interactively 'recenter-top-bottom))))))

(defun px-unpop-to-mark-command ()
  "Go forward down the mark history. Recenter if far away. Do nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))

    (let
        ((current-prefix-arg '(t))
         (start-line (line-number-at-pos))
         (maxxx-line (line-number-at-pos (point-max)))
         (view-lines (window-height)))
      (call-interactively 'recenter-top-bottom)

      (let ((end-line (line-number-at-pos (point))))
        (if (< end-line start-line)
            (if (> (- start-line end-line) view-lines)
                (call-interactively 'recenter-top-bottom))
          (if (> (- end-line start-line) view-lines)
              (call-interactively 'recenter-top-bottom)))))))

(defun px-kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido. Bound to M-i.
Try it on your dad's stereo."
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

(defun px-vc-manage-current-file ()
  "VC-manage the current file.
That means save it, check the hash of the previous commit, and replace it in the file with the current one.
"
  (interactive)

  (shell-command (format "cd %s" default-directory))

  (defun s-trim-right (s)
    "Remove whitespace at the end of S."
    (if (string-match "[ \t\n\r]+\\'" s)
        (replace-match "" t t s)
      s))

  (let* (
         (commit-message (read-from-minibuffer
                          (format "Enter commit MSG (default %s): " default-directory)
                          nil nil nil nil default-directory))
         (mysearch (shell-command-to-string "git log | head -7 | tail -1  | cut -c 8-47"))
         (myreplace (shell-command-to-string "git log | head -1 | cut -c 8-47"))
         (yo (format "%s" (s-trim-right mysearch)))
         (ya (format "%s" (s-trim-right myreplace)))
         )


    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        (while (re-search-forward yo nil t)
          (replace-match ya))))

    (progn
      (message "%s|%s" yo ya)
      (message "Committing...")
      (save-buffer)
      (shell-command (format "git commit -am \"%s\" && git push origin master" commit-message))
      (shell-command "ssh simo -C 'cd opensimo/www/play/ && git pull'")
      (message "Upping in distant repo...")
      (message "... Done."))))

(require 'sgml-mode) ; need sgml-skip-tag-forward

(defun px-replace-oneshot ()
  "Use the title of the page to replace a named tag"
  (interactive)
  (goto-char 1)
  (while
      (search-forward "modify"  nil t)

    (setq p3 (point)) ; beginning of text content, after <div class="x-note">
    (backward-char)
    (sgml-skip-tag-forward 1)
    (backward-char 30)
    (setq p4 (point)) ; end of tag content, before the </div>

    (setq contenu (buffer-substring-no-properties p3 p4))

    (setq position1 (line-number-at-pos))

    (setq bulletCnt (count-matches "[a-z]" p3 p4) )

    (when (> bulletCnt 2)
      (progn
        (message (format "%d bullets" bulletCnt))
        ;; (message (format "(%s) Found at line: %d" contenu position1))
        ;; (query-replace-regexp "<h2>Contents" (concat "<h2>" contenu))
        ;; (query-replace-regexp "<h1>*.*" "")
        ))
    ;; (save-buffer (current-buffer))
    ;; (kill-buffer (current-buffer))
    ))

(defun px-cleanup ()
  "Cleanup"
  (interactive)
  (goto-char 1)
  (progn
    (query-replace-regexp "" "fi")
    (query-replace-regexp "" "ff")
    (query-replace-regexp "- " "")))

(defun px-date ()
  "Insert date"
  (interactive)
  (insert (format-time-string "%d %B %Y - %H:%M:%S - %3Nms" (current-time))))

(defun toggle-fullscreen ()
  "Real, mozilla-like full screen."
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

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
Do the right thing and delete window."
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
      ;; (message "Buffer is %s" (current-buffer))
      (kill-buffer)
      ;; (switch-to-buffer (current-buffer))
      ;; (message "Buffer is %s" (current-buffer))
      (if (> (length (window-list)) 1)
          (delete-window))
      (kbd "C-x b <return>")
      ;; (switch-to-buffer (other-buffer))
      )))

;; (defun px-byte-compile-user-init-file ()
;;      "byte-compile .emacs each time it is edited"
;;   (let ((byte-compile-warnings '(unresolved)))
;;     ;; in case compilation fails, don't leave the old .elc around:
;;     (when (file-exists-p (concat user-init-file ".elc"))
;;       (delete-file (concat user-init-file ".elc")))
;;     (byte-compile-file user-init-file)
;;     (message "%s compiled" user-init-file)
;;     ))

;; (defun px-emacs-lisp-mode-hook ()
;;   (when (string-match "\\.emacs" (buffer-name))
;;     (add-hook 'after-save-hook 'px-byte-compile-user-init-file t t)))

;; (add-hook 'emacs-lisp-mode-hook 'px-emacs-lisp-mode-hook)

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
  (interactive "sRegexp: \nsReplace with: ")
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
  "Websearch selected string
Bound to c-c g."
  (interactive "r")
  (let ((q (buffer-substring-no-properties start end)))
    (browse-url (concat "https://duckduckgo.com/?q="
                        (url-hexify-string q) "!g"))))

(defun px-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Bound to S-SPC."
  (interactive)
  (let (b1 b2)
    (skip-chars-backward "^<>([{“「『‹«（〈《〔【〖⦃\"")
    (setq b1 (point))
    (skip-chars-forward "^<>)]}”」』›»）〉》〕】〗⦄\"")
    (setq b2 (point))
    (set-mark b1)))

(global-set-key (kbd "S-SPC") 'px-select-text-in-quote)

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


(defun px-insert-end-of-command-sign ()
  "Insert a ; at the end of the current line."
  (interactive)
  (let ((st (point)))
    (save-excursion
      (move-end-of-line nil)
      (insert ";")
      (goto-char st))))

(defun px-insert-jq ()
  "Insert jquery object skel."
  (interactive)
  (insert "$(\"\")")
  (left-char 2))

(defun insert-pair-paren () (interactive) (px-insert-or-enclose-with-signs "(" ")"))
(defun insert-pair-brace () (interactive) (px-insert-or-enclose-with-signs "{" "}")
  (newline-and-indent)
  (newline-and-indent)
  (previous-line)
  (c-indent-line-or-region)
)
(defun insert-pair-bracket () (interactive) (px-insert-or-enclose-with-signs "[" "]"))
(defun insert-pair-single-angle () (interactive) (px-insert-or-enclose-with-signs "<" ">"))
(defun insert-pair-squote () (interactive) (px-insert-or-enclose-with-signs "'" "'"))
(defun insert-pair-dbquote () (interactive) (px-insert-or-enclose-with-signs "\"" "\""))

(defun px-frigo ()
  (interactive)
  "Copy the current region, paste it in frigo.txt with a time tag, and save this file.
Again, here by pure nostalgia."
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

(setq tabbar-buffer-groups-function 'px-tabbar-buffer-groups)

;; (defun iswitchb-local-keys ()
;;   "easily switch buffers (F5 or C-x b)"
;;   (mapc (lambda (K)
;;           (let* ((key (car K)) (fun (cdr K)))
;;             (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
;;         '(("<right>" . iswitchb-next-match)
;;           ("<left>"  . iswitchb-prev-match)
;;           ("<up>"    . ignore             )
;;           ("<down>"  . ignore             ))))

(defun px-laptop-mode ()
  "smaller default size"
  (interactive)
  (set-face-attribute 'default nil :height 90))

(defun px-desktop-mode ()
  "default font size"
  (interactive)
  (set-face-attribute 'default nil :height 105))

;; Sessions! ______________________________________________________________________

;; Automatically save and restore sessions
(setq desktop-dirname             "~/.emacs.d/backup/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil)
(desktop-save-mode 0)

(defun px-session-restore ()
  "Load the desktop and enable autosaving"
  (interactive)
  (let ((desktop-load-locked-desktop "ask"))
    (desktop-read)
    (desktop-save-mode 1)))

;; Modes! _____________________________________________________________________

(autoload 'ttl-mode "ttl-mode" "Major mode for OWL or Turtle files" t)
(add-hook 'ttl-mode-hook    ; Turn on font lock when in ttl mode
          'turn-on-font-lock)
(setq auto-mode-alist
      (append
       (list
        '("\\.n3" . ttl-mode)
        '("\\.ttl" . ttl-mode))
       auto-mode-alist))

;; (auto-complete-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(put 'overwrite-mode 'disabled t)
(setq c-default-style "bsd"
      c-basic-offset 2)
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.list\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(add-to-list 'auto-mode-alist '("\\.pixi\\'" . pixilang-mode))


;; Hooks! _____________________________________________________________________

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hooks 'my-find-file-check-make-large-file-read-only-hook)

(add-hook 'recentf-dialog-mode-hook 'hl-line-mode)

(add-hook 'emacs-lisp-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))

(defun my-c-mode-hook ()
  "Proper mono-line comments"
  (setq-local comment-start "//")
  (setq-local comment-padding " ")
  (setq-local comment-end "")
  (setq-local comment-style 'indent))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'php-mode-hook 'my-c-mode-hook)
(add-hook 'text-mode-hook 'turn-off-auto-fill)
(add-hook 'haml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (define-key haml-mode-map "\C-m" 'newline-and-indent)))
;; (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
(add-hook 'find-file-hooks 'turn-on-font-lock)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-to-list 'fill-nobreak-predicate 'fill-french-nobreak-p)

;; FIXME: Find out what this is
(setq paragraph-start "\\*\\|$"
      paragraph-separate "$")

(mapcar (lambda (mode)
	  (font-lock-add-keywords
           mode
           '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
             ("\\<\\(TODO\\|BUGGY\\):" 1 font-lock-warning-face prepend))))
	'(text-mode latex-mode html-mode emacs-lisp-mode php-mode texinfo-mode js-mode))

;; Externals! _________________________________________________________________

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")


;; Vars! ______________________________________________________________________

(setq dired-dwim-target t)

;; (all of this will slowly migrate to custom)
(setq-default cursor-type 'bar)

(setq

 bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
 bookmark-save-flag 1

 ;; iswitchb-buffer-ignore '("^ " "*.")
 ispell-dictionary "francais"

 ;; delete-by-moving-to-trash t

 text-mode-hook 'turn-on-auto-fill
 fill-column 75

 ediff-window-setup-function (quote ediff-setup-windows-plain)
 ediff-split-window-function 'split-window-horizontally
 )

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

(global-set-key (kbd "<f6>") 'px-shell-command) ; Keyboard macro! (open new line)

(global-set-key (kbd "<C-return>") (kbd "C-e C-j")) ; Keyboard macro! (open new line)
(global-set-key (kbd "C-c t") 'sgml-tag)
(global-set-key (kbd "C-c r") 'rgrep)
(global-set-key (kbd "C-c j") 'px-insert-jq)

(define-key global-map [(meta up)] '(lambda() (interactive) (scroll-other-window -1)))
(define-key global-map [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))

(global-set-key (kbd "C-c p") 'php-mode)

(global-set-key (kbd "C-;") 'px-insert-end-of-command-sign)

(global-set-key [(meta shift up)]  'move-line-up)

(global-set-key [(meta shift up)]  'move-line-up)
(global-set-key [(meta shift down)]  'move-line-down)

(global-set-key (kbd "M-i") 'ido-goto-symbol)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(setq-default indent-tabs-mode nil)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(global-set-key (kbd "M-s-b") 'bookmark-set)
(global-set-key (kbd "s-b") 'bookmark-jump)

(global-set-key (kbd "C-h x") 'px-help-emacs)
(global-set-key (kbd "C-h *") 'px-scratch)

(global-set-key (kbd "²") 'hippie-expand)

;; (define-key global-map [(meta up)] '(lambda() (interactive) (scroll-other-window -1)))
;; (define-key global-map [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))

(define-key global-map [f1] 'delete-other-windows)
(define-key global-map [S-f1] 'px-help-emacs)
(define-key global-map [f2] 'other-window)
(define-key global-map [M-f2] 'swap-buffers-in-windows)
(define-key global-map [f3] 'split-window-vertically)
(define-key global-map [f4] 'split-window-horizontally)
(define-key global-map [f5] 'ido-switch-buffer) ;std way
(define-key global-map [f7] 'flyspell-buffer)
(define-key global-map [M-f7] 'flyspell-mode)
(define-key global-map [M-f10] 'toggle-truncate-lines)
(define-key global-map [f12] 'px-fullscreen)

(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-backward)

(global-set-key (kbd "C-s-t") 'sgml-close-tag)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)

(global-set-key (kbd "ù") 'px-match-paren)
(global-set-key (kbd "C-ù") 'forward-sexp)
(global-set-key (kbd "C-%") 'backward-sexp)

(global-set-key (kbd "C-c C-g") 'px-websearch-that-bitch)
(global-set-key (kbd "s-r") 'replace-regexp)
(global-set-key (kbd "s-²") (kbd "C-x b <return>")) ; Keyboard macro! (toggle last buffer)
(global-set-key (kbd "s-t") 'sgml-tag)
(global-set-key (kbd "s-k") 'px-kill-buffer)
(global-set-key (kbd "s-p") 'php-mode)
(global-set-key (kbd "s-h") 'html-mode)
(global-set-key (kbd "s-j") 'js-mode)
(global-set-key (kbd "s-m") 'message-mail)
(global-set-key (kbd "s-o") 'find-file-at-point)
(global-set-key (kbd "s-d") 'px-date)
(global-set-key (kbd "<C-kp-0>") 'kmacro-end-and-call-macro)
(global-set-key (kbd "C-s-m") 'apply-macro-to-region-lines)
(global-set-key (kbd "<s-left>") 'px-pop-to-mark-command)
(global-set-key (kbd "<s-right>") 'px-unpop-to-mark-command)

(global-set-key (kbd "C-x g") 'magit-status)


(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-S-o") 'my-desktop-read)
(global-set-key (kbd "C-S-<mouse-1>") 'flyspell-correct-word)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

;; (define-key global-map [M-tab] 'tabbar-forward)
;; (global-set-key (kbd "<C-S-iso-lefttab>") 'tabbar-backward)
(global-set-key (kbd "<M-left>") 'tabbar-backward)
(global-set-key (kbd "<M-right>") 'tabbar-forward)

(global-set-key (kbd "C-=") 'insert-pair-brace)        ;{}
(global-set-key (kbd "C-)") 'insert-pair-paren)        ;()
(global-set-key (kbd "C-(") 'insert-pair-bracket)      ;[]
(global-set-key (kbd "C-<") 'insert-pair-single-angle) ;<>
(global-set-key (kbd "C-'") 'insert-pair-squote)       ;''
(global-set-key (kbd "C-\"") 'insert-pair-dbquote)     ;""

;; (global-set-key (kbd "M-DEL") 'kill-word)

(global-set-key (kbd "M-s") 'save-buffer) ; Meta+s saves !!  (and Jesus too BTW) (see C-h b for all bindings, and C-h k + keystroke(s) for help)
(global-set-key (kbd "M-o") 'recentf-open-files)
(global-set-key (kbd "M-d") 'px-toggle-comments)

;; Custom ! ______________________________________________________________________

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu t)
 '(ac-auto-start t)
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/backup/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backup/"))))
 '(bbdb-use-pop-up nil)
 '(bookmark-sort-flag nil)
 '(buffer-offer-save nil)
 '(c-basic-offset (quote set-from-style))
 '(c-default-style (quote ((c++-mode . ""))))
 '(canlock-password "ebef4a12d0fad1c648b4b829291adb16cdefb9da")
 '(comment-style (quote extra-line))
 '(completion-auto-help (quote lazy))
 '(cursor-in-non-selected-windows nil)
 '(custom-enabled-themes (quote (tango-dark)))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(diary-file "~/Ubuntu One/org/agenda.org")
 '(ecb-layout-name "left1")
 '(ecb-layout-window-sizes
   (quote
    (("Cdev-def"
      (ecb-directories-buffer-name 0.15418502202643172 . 0.23880597014925373)
      (ecb-sources-buffer-name 0.1762114537444934 . 0.23880597014925373)
      (ecb-methods-buffer-name 0.3303964757709251 . 0.19402985074626866)
      (ecb-analyse-buffer-name 0.3303964757709251 . 0.22388059701492538)
      (ecb-symboldef-buffer-name 0.3303964757709251 . 0.3283582089552239))
     ("left1"
      (ecb-directories-buffer-name 0.27312775330396477 . 0.2835820895522388)
      (ecb-sources-buffer-name 0.14977973568281938 . 0.34328358208955223)
      (ecb-history-buffer-name 0.12334801762114538 . 0.34328358208955223)
      (ecb-methods-buffer-name 0.27312775330396477 . 0.3582089552238806)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(epa-popup-info-window nil)
 '(fold-dwim-outline-style-default (quote nested))
 '(font-use-system-font t)
 '(global-auto-complete-mode t)
 '(global-font-lock-mode t)
 '(global-linum-mode t)
 '(global-undo-tree-mode t)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "compiled" "libs/bootstrap")))
 '(haml-backspace-backdents-nesting nil)
 '(holiday-other-holidays (quote islamic-holidays))
 ;; '(ido-everywhere t)
 '(ido-ignore-buffers (quote ("\\` " "*Messages*")))
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-echo-area-message (user-login-name))
 '(inhibit-startup-screen t)
 '(keyboard-coding-system (quote utf-8) nil nil "nil before, now utf-8.")
 '(mail-interactive t)
 '(mark-ring-max 8)
 '(max-lisp-eval-depth 6000)
 '(max-specpdl-size 13000)
 '(mbug-bcc-to-sender t)
 '(mbug-host-name "imap.riseup.net")
 '(mbug-inline-images t)
 '(mbug-modal t)
 '(mbug-short-headers t)
 '(mbug-username "philcm")
 '(menu-bar-mode nil)
 '(message-confirm-send t)
 '(message-default-charset (quote utf-8))
 '(mm-enable-external (quote ask))
 '(mm-text-html-renderer (quote links))
 '(mumamo-margin-use (quote (left-margin 13)))
 '(org-agenda-files (quote ("~/Ubuntu One/org/agenda.org")))
 '(org-html-postamble t)
 '(org-html-validation-link
   "<a href=\"http://validator.w3.org/check?uri=referer\">Valid HTML</a>")
 '(org-return-follows-link t)
 '(org-support-shift-select (quote always))
 '(org-use-sub-superscripts nil)
 '(read-file-name-completion-ignore-case t)
 '(recenter-positions (quote (middle top bottom)))
 '(recenter-redisplay nil)
 '(recentf-auto-cleanup (quote never))
 '(recentf-mode t)
 '(require-final-newline t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/emacs.d/.places")
 '(savehist-mode t nil (savehist))
 '(scroll-conservatively 200)
 '(scroll-margin 3)
 '(send-mail-function (quote smtpmail-send-it))
 '(server-mode t)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(standard-indent 4)
 '(tabbar-mode t nil (tabbar))
 '(tags-add-tables t)
 '(text-mode-hook nil)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(tramp-verbose 6)
 '(undo-limit 400000)
 '(undo-strong-limit 600000)
 '(undo-tree-auto-save-history t)
 '(undo-tree-enable-undo-in-region nil)
 '(undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/backup/"))))
 '(undo-tree-visualizer-diff t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(user-full-name "Philippe Coatmeur")
 '(user-mail-address "philcm@gnu.org")
 '(vc-make-backup-files nil)
 '(web-vcs-default-download-directory (quote site-lisp-dir)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "gray20" :foreground "white"))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(highlight ((t (:background "#ce5c00" :foreground "#2e3436"))))
 '(ido-first-match ((t (:inherit which-func))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :foreground "deep sky blue"))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :foreground "coral"))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :foreground "navajo white"))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :foreground "yellow green"))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :foreground "magenta3"))))
 '(mode-line ((t (:background "gray10" :foreground "white" :box nil))))
 '(mode-line-buffer-id ((t (:weight bold :foreground "OrangeRed1"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "gray33" :foreground "#eeeeec" :box nil :weight light))))
 '(show-paren-match ((t (:background "OrangeRed1"))))
 '(tabbar-button ((t (:inherit tabbar-default))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default :background "OrangeRed1"))))
 '(tabbar-default ((t (:inherit default :background "dim gray" :box (:line-width 1 :color "gray35")))))
 '(tabbar-highlight ((t (:background "OrangeRed1" :foreground "white" :box (:line-width 1 :color "OrangeRed1")))))
 '(tabbar-modified ((t (:inherit tabbar-unselected :foreground "OrangeRed1"))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "grey20" :foreground "OrangeRed1" :box (:line-width 1 :color "grey20")))))
 '(tabbar-separator ((t (:height 0.1))))
 '(tabbar-unselected ((t (:inherit tabbar-default :background "gray35"))))
 '(web-mode-html-tag-face ((t (:foreground "RosyBrown2"))) t)
 '(which-func ((t (:foreground "OrangeRed1"))) t))


;; ORG! ______________________________________________________________________

;; (require 'ox-publish)
;; (require 'ox-html)

;; (setq org-publish-project-alist
;;       '(("mensup" :components ("org-notes" "org-static"))
;;         ("org-notes"
;;          :base-directory "~/Documents/svnmen/"
;;          :base-extension "org"
;;          :publishing-directory "~/Documents/svnmen/"
;;          :recursive t
;;          :auto-postamble nil
;;          :publishing-function org-html-publish-to-html)
;;         ("org-static"
;;          :base-directory "~/Documents/svnmen/"
;;          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
;;          :publishing-directory "~/Documents/svnmen/"
;;          :recursive t
;;          :publishing-function org-publish-attachment)))

;; (setq org-capture-templates
;;       '(("t" "Todo" entry (file+headline (car org-agenda-files) "Tasks")
;;          "* TODO %?\n%i \n  DEADLINE: %^t")
;;         ("r" "Rendez-vous" entry (file+headline (car org-agenda-files) "Rendez-vous")
;;          "* RV %?\n  %i\n %^t\n %a")
;;         ("j" "Journal" entry (file+datetree (car org-agenda-files))
;;          "* %?\nEntered on %U\n  %i\n  %a")))
