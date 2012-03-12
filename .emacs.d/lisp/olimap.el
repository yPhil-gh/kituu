;;; olimap.el --- Control & monitor Offlineimap

;; Copyright (C) 2011 Philippe M. Coatmeur

;; Author: Philippe M. Coatmeur <philippe.coatmeur@gmail.com>
;; Created: 12 Mars 2012
;; Version: 1.0
;; Keywords: gnus imap mail message offlineimap
;; X-URL: https://github.com/xaccrocheur/kituu/blob/master/.emacs.d/lisp/olimap.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; The idea is to know when offlineimap has finished to do his
;; business to do things, not before. And while we're at it, try to
;; figure out what he's doing. And no, no fancy color overlays, thank
;; you very much.

;; The path to offlineimap
(defconst *main* "~/scripts/offlineimap.py")

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
	(message "Mail received at %s on %s %s" time-received mailbox-name account-name)))))

(defconst *argx* "-umachineui")

(add-hook 'comint-output-filter-functions
          'olimap-check)

(defun olimap-run ()
  "Run an inferior offlineimap process,\
 input and output via buffer *olimap*."
  (interactive)
  (message "Sync started on %s" (format-time-string "%Y-%m-%d at %H:%M:%S"))
  (apply 'make-comint "ofimap" *main* nil
         (list *argx*)))

(defun olimap-kill ()
  "cleanly kill OL"
  (interactive)
  (with-current-buffer "*olimap*"
    (comint-kill-subjob)
    (kill-buffer "*olimap*")))

(defun olimap-see ()
  "Switch to OL output buffer"
  (interactive)
  (delete-other-windows)
  (switch-to-buffer-other-window "*olimap*")
  (erase-buffer)
  (other-window -1))

(message "%s loaded" (buffer-file-name))
(provide 'olimap)
