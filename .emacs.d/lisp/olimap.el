;; (if (require 'el-get nil 'noerror)
;;     (progn (string-match "\\[acc:.*\\]"
;; 			 "msg:copyingmessage:Folder INBOX [acc: adamwebzoul]87%0AIMAP%0AINBOX%0AMaildir%5BINBOX%5D")
;; 	   (setq mysweetvar (match-string 0 "msg:copyingmessage:Folder INBOX [acc: adamwebzoul]87%0AIMAP%0AINBOX%0AMaildir%5BINBOX%5D"))))
;; (match-string NUM &optional STRING)

;; ;; olimap.el
;; (defun olimap-check (ol-output-line)
;;   (if (string-match "terminate" ol-output-line)
;;       (progn
;; 	(message "Sync finished on %s" (format-time-string "%Y-%m-%d at %H:%M:%S"))
;; 	(gnus-demon-scan-news))
;;     (when
;; 	(string-match "copyingmessage" ol-output-line)
;;       (if (require 'el-get nil 'noerror)
;; 	  (progn
;; 	    (el-get-notify "New Mail!"
;; 			   (format "Mail received at %s on %s (%s)"
;; 				   (format-time-string "%H:%M:%S")
;; 				   (substring ol-output-line ( + (search ":Folder " ol-output-line) 8) (- (search "[acc" ol-output-line) 1)) (progn (string-match "\\[acc:.*\\]" ol-output-line) (setq mysweetvar (match-string 0 ol-output-line)))))
;; 	    (message "test [ %s ]" (substring ol-output-line  (+ (search "[acc" ol-output-line) 6) (search "]" ol-output-line :from-end t))))
;; 	(message "Mail received at %s on %s" (format-time-string "%H:%M:%S") (substring ol-output-line ( + (search ":Folder " ol-output-line) 8) (- (search "[acc" ol-output-line) 1))))


(defun olimap-check (ol-output-line)
  (if (string-match "terminate" ol-output-line)
      (progn
	(message "Sync finished on %s" (format-time-string "%Y-%m-%d at %H:%M:%S"))
	(gnus-demon-scan-news))
    (when
	(string-match "copyingmessage" ol-output-line)
      (setq time-received (format-time-string "%H:%M:%S"))
      (setq mailbox-name
	    (substring ol-output-line ( + (search ":Folder " ol-output-line) 8)
		       (- (search "[acc" ol-output-line) 1)))
      (setq account-name
	    (progn
	      (string-match "\\[acc:.*\\]" ol-output-line)
	      (setq mysweetvar (match-string 0 ol-output-line))))
      (if
	  (require 'el-get nil 'noerror)
	  (el-get-notify "New Mail! Yiii!" (format "Mail received at %s on %s %s" time-received mailbox-name account-name))
	(message "Mail received at %s on %s %s" time-received mailbox-name account-name))


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

;; (setq ol-output-line "msg:copyingmessage:Folder INBOX [acc: adamweb]87%0AIMAP%0AINBOX%0AMaildir%5BINBOX%5D")
;; (substring ol-output-line ( + (search ":Folder " ol-output-line) 8) (- (search "[acc" ol-output-line) 1))
;; (substring ol-output-line  (+ (search "[acc" ol-output-line) 6) (search "]" ol-output-line :from-end t))

(message "%s loaded" (buffer-file-name))
(provide 'olimap)
