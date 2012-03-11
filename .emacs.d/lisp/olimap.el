;; olimap.el
(defun olimap-check (ol-output-line)
  (if (string-match "terminate" ol-output-line)
      (progn
	(message "Sync finished on %s" (format-time-string "%Y-%m-%d at %H:%M:%S"))
	(gnus-demon-scan-news)
	)
    (when
	(string-match "copyingmessage" ol-output-line)
      (el-get-notify "New Mail!" (format "Mail received at %s on %s" (format-time-string "%H:%M:%S") (substring ol-output-line ( + (search ":Folder " ol-output-line) 8) (- (search "[acc" ol-output-line) 1))))
      ;; (message "Mail received at %s on %s" (format-time-string "%H:%M:%S") (substring ol-output-line ( + (search ":Folder " ol-output-line) 8) (- (search "[acc" ol-output-line) 1)))
      ;; (el-get-notify "New Mail!"
      ;; 		     (format "In (%s) on account (%s)"
      ;; 			     (substring ol-output-line ( + (search ":Folder " ol-output-line) 8) (- (search "[acc" ol-output-line) 1))
      ;; 			     (substring ol-output-line  (+ (search "[acc" ol-output-line) 6) (search "]" ol-output-line :from-end t))))
      )))

(add-hook 'comint-output-filter-functions
          'olimap-check)

(defconst *main* "~/scripts/offlineimap.py")
(defconst *argx* "-umachineui")

(defun olimap-run ()
  "Run an inferior offlineimap process,\
 input and output via buffer *olineimap*."
  (interactive)
  (message "Sync started on %s" (format-time-string "%Y-%m-%d at %H:%M:%S"))
  (apply 'make-comint "olineimap" *main* nil
         (list *argx*)))

(defun olimap-kill ()
  (interactive)
  (with-current-buffer "*olineimap*"
    (comint-kill-subjob)
    (kill-buffer "*olinemap*")))

(defun olimap-see ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer-other-window "*olineimap*")
  (erase-buffer)
  (other-window -1))

;; (setq ol-output-line "msg:copyingmessage:Folder [Gmail]/Sent Mail [acc: perso]446%0AGmail%0A%5BGmail%5D%2FSent+Mail%0AMaildir%5B%5BGmail%5D.Sent+Mail%5D")
;; (substring ol-output-line ( + (search ":Folder " ol-output-line) 8) (- (search "[acc" ol-output-line) 1))
;; (substring ol-output-line  (+ (search "[acc" ol-output-line) 6) (search "]" ol-output-line :from-end t))

(message "%s loaded" (buffer-file-name))
(provide 'olimap)
