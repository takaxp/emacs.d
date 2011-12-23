;;;; Utility functions
;;;;                                       Last Update: 2011-11-23@23:47
;;;;                                       Takaaki ISHIKAWA  <takaxp@ieee.org>

(message "* --[ Loading an init file, takaxp-utility.el ] --")

(run-with-idle-timer 60 t 'sleep-after-reload)

(defvar kyoko-mad-mode t)
(defun kyoko-mad-mode-toggle ()
  (interactive)
  (setq kyoko-mad-mode (not kyoko-mad-mode))
  (cond (kyoko-mad-mode
	 (message "Kyoko mad mode: ON"))
	(t
	 (message "Kyoko mad mode: OFF"))))
;; She will be mad if you do nothing within 10 min.
(run-with-idle-timer 600 t
		     '(lambda () 
			(when kyoko-mad-mode
			  (shell-command-to-string
			   "say -v Kyoko おいおまえ，遊んでないで，仕事しろ"))))

;; Start Emacs with scratch buffer even though it call session.el/desktop.el
(add-hook 'emacs-startup-hook '(lambda () (switch-to-buffer "*scratch*")))

(defun org2dokuwiki-cp-kill-ring ()
  "Convert the current org-file to dokuwiki text, and copy it to kill-ring."
  (interactive)
  (cond (buffer-file-name
	 (kill-new
	  (shell-command-to-string
	   (concat "cat " buffer-file-name "| perl "
		   (expand-file-name "~/env/scripts/org2dokuwiki.pl"))))
	 (message "Copy %s ... done" buffer-file-name)
	 (sit-for 1.5)
	 (message ""))
	(t (message "There is NOT such a file."))))

(defun add-itemize-head ()
  "Insert \"  - \" at the head of line.
  If the cursor is already at the head of line, it is NOT returned back to the
  original position again. Otherwise, the cursor is moved to the right of the
  inserted string."
  (interactive)
  (cond ((= (point) (line-beginning-position))
	 (insert "  - "))
	(t (save-excursion
	     (move-beginning-of-line 1)
	     (insert "  - ")))))

(defun show-org-buffer (file)
  "Show an org-file on the current buffer"
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
	(switch-to-buffer buffer)
	(message "%s" file))
    (find-file (concat "~/Dropbox/org/" file))))

;;; Cite: http://flex.ee.uec.ac.jp/texi/emacs-jp/emacs-jp_12.html
;;; Cite: http://d.hatena.ne.jp/Ubuntu/20090417/1239934416
;; A simple solution is (setq confirm-kill-emacs 'y-or-n-p).
(defun confirm-save-buffers-kill-emacs (&optional arg)
  "Show yes or no when you try to kill Emacs"
  (interactive "P")
  (cond (arg (save-buffers-kill-emacs))
	(t
	 (when (yes-or-no-p "Are you sure to quit Emacs now? ")
	   (save-buffers-kill-emacs)))))

;;; Insert a date and time quickly
;;; Cite: http://www.fan.gr.jp/~ring/doc/elisp_20/elisp_38.html#SEC608
(defun insert-formatted-current-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))
(defun insert-formatted-current-time ()
  (interactive)
  (insert (format-time-string "%H:%M")))
(defun insert-formatted-signature ()
  (interactive)
  (insert (concat (format-time-string "%Y-%m-%d") "  " user-full-name
		  "  <" user-mail-address ">")))

;;; Visible-bell
;; Alternative to the default behavior of visible-bell
(defcustom echo-area-bell-string " BEEP ";
  "Message displayed in mode-line by `echo-area-bell' function."
  :group 'user)
(defcustom echo-area-bell-delay 0.1
  "Number of seconds `echo-area-bell' displays its message."
  :group 'user)
;; internal variables
(defvar echo-area-bell-cached-string nil)
(defvar echo-area-bell-propertized-string nil)
(defun echo-area-bell ()
  "Briefly display a highlighted message in the echo-area.
    The string displayed is the value of `echo-area-bell-string',
    with a red background; the background highlighting extends to the
    right margin.  The string is displayed for `echo-area-bell-delay'
    seconds.
    This function is intended to be used as a value of `ring-bell-function'."
  (unless (equal echo-area-bell-string echo-area-bell-cached-string)
    (setq echo-area-bell-propertized-string
	  (propertize
	   (concat
	    (propertize
	     "x"
	     'display
	     `(space :align-to (- right ,(+ 2 (length echo-area-bell-string)))))
	    echo-area-bell-string)
;	   'face '(:background "red")
;	   'face '(:foreground "#FFFFFF" :background "#FF4040")
;	   'face '(:foreground "#FFFFFF" :background "#C1252D")
;	   'face '(:foreground "#FFFFFF" :background "#FD8A4B")
	   'face '(:foreground "#FFFFFF" :background "#FF7D7D")
	   ))
    (setq echo-area-bell-cached-string echo-area-bell-string))
  (message echo-area-bell-propertized-string)
  (sit-for echo-area-bell-delay)
  (message ""))
(setq ring-bell-function 'echo-area-bell)
;; (setq ring-bell-function
;;       '(lambda () (message "                                          BEEP!")))


;;; Test function from GNU Emacs (O'REILLY, P.328)
(defun count-words-buffer ()
  "Count the number of words in the current buffer"
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(forward-word 1)
	(setq count (1+ count)))
      (message "buffer contains %d words." count))))

;; http://d.hatena.ne.jp/rubikitch/20111120/elispbook
(defvar my-file-ring nil)
(defun my-make-file-ring (files)
  (setq my-file-ring (copy-sequence files))
  (setf (cdr (last my-file-ring)) my-file-ring))
(my-make-file-ring '("~/Dropbox/org/next.org" "~/Dropbox/org/buffer.org"))
(defun my-open-file-ring ()
  (interactive)
  (find-file (car my-file-ring))
  (setq my-file-ring (cdr my-file-ring)))


;;; Test function for AppleScript
;;; Cite: http://sakito.jp/emacs/emacsobjectivec.html
(defun do-test-applescript ()
  (interactive)
  (do-applescript
   (format
    (concat
     "display dialog \"Hello world!\" \r"))))

(defcustom my-auto-install-batch-list-el-url nil
  "URL of a auto-install-batch-list.el"
  :type 'string
  :group 'takaxp-utility)

(defun init-auto-install ()
  "Setup auto-install.el.
1. Set my-auto-install-batch-list-el-url
2. M-x init-auto-install
3. M-x auto-install-batch hoge"
  (interactive)
  (when (and (require 'auto-install nil t)
	     my-auto-install-batch-list-el-url)
    (setq auto-install-batch-list-el-url my-auto-install-batch-list-el-url)
    (setq auto-install-directory "~/env/config/emacs/")
    (setq auto-install-wget-command "/opt/local/bin/wget")
    (auto-install-update-emacswiki-package-name t)
    ;; compatibility
    (auto-install-compatibility-setup))) ; for install-elisp users

(defun get-random-string (length)
  "Get a string contain the length digit number with random selection"
  (interactive)
  (random t)
  (cond ((> length 0)
	 (let
	     ((count length)
	      (string nil)
	      (tmp nil))
	   (while (< 0 count)
	     (setq count (1- count))
	     (setq tmp string)
	     (setq string
		   (concat tmp (number-to-string (random 10)))))
	   (message "%s" string)))
	(t "0")))

;;; Publish an xml file to show a Gantt Chart
(defcustom default-timeline-csv-file nil
  "source.csv"
  :type 'string
  :group 'takaxp-utility)

(defcustom default-timeline-xml-business-file nil
  "XML file for business schedule"
  :type 'string
  :group 'takaxp-utility)

(defcustom default-timeline-xml-private-file nil
  "XML file for private schedule"
  :type 'string
  :group 'takaxp-utility)

(defcustom default-timeline nil
  "a template index.html"
  :type 'string
  :group 'takaxp-utility)

(defun export-timeline-business ()
  "Export schedule table as an XML source to create an web page"
  (interactive)
  (when (and default-timeline
	     (and default-timeline-csv-file default-timeline-xml-business-file))
    (shell-command-to-string (concat "rm -f " default-timeline-csv-file))
    (org-table-export default-timeline-csv-file "orgtbl-to-csv")
    (shell-command-to-string (concat "org2gantt.pl > "
				     default-timeline-xml-business-file))
    (shell-command-to-string (concat "open " default-timeline))))

(defun export-timeline-private ()
  "Export schedule table as an XML source to create an web page"
  (interactive)
  (when (and default-timeline
	     (and default-timeline-csv-file default-timeline-xml-private-file))
    (shell-command-to-string (concat "rm -f " default-timeline-csv-file))
    (org-table-export default-timeline-csv-file "orgtbl-to-csv")
    (shell-command-to-string (concat "org2gantt.pl > "
				     default-timeline-xml-private-file))
    (shell-command-to-string (concat "open " default-timeline))))

;(defun org-open-at-point-with-e2wm (&optional arg)
;  (interactive)
;  (message "OVERWRIDE: org-open-at-point")
;  (when arg
;    (toggle-double-wide-frame arg))
;  (org-open-at-point))

(defun set-alarms-from-file (file)
  "Make alarms from org-mode tables. If you have an org-mode file with tables with the following format:
|----+--------+-------------------------------------------------------------|
| ID |   Time | Content                                                     |
|----+--------+-------------------------------------------------------------|
|  1 |  07:00 | Wakeup                                                      |
|  2 |        | Read papers                                                 |
|  3 |  12:00 | Clean up your desk                                          |
When it is 7:00 and 12:00, Growl notify with a message which is specified
content column from the table. The line ID number is 2 will be ignored."
  (let
      ((lines (read-line file)))
    (while lines
      (set-alarm-from-line (decode-coding-string (car lines) 'utf-8))
      (setq lines (cdr lines))
      (message ""))))

(defun set-alarm-from-line (line)
  "NOTE: this function need (require 'todochiku)"
  (when (require 'todochiku nil t)
    (let
	((hour nil)
	 (min nil)
	 (current-hour nil)
	 (current-min nil)
	 (action nil))
      (when (string-match "\\([0-2]?[0-9]\\):\\([0-5][0-9]\\)" line)
	(setq hour (substring line (match-beginning 1) (match-end 1)))
	(setq min (substring line (match-beginning 2) (match-end 2)))
	(when (string-match
	       "\|\\s-*\\([^\|]+[^ ]\\)\\s-*\|$" line (match-end 2))
	  (setq action (substring line (match-beginning 1) (match-end 1)))))
      (when (and (and hour min) action)
;	(message "[%s:%s] => %s" hour min action)
	(setq current-hour (format-time-string "%H" (current-time)))
	(setq current-min (format-time-string "%M" (current-time)))
	(when (> (+ (* (string-to-number hour) 60) (string-to-number min))
		 (+ (* (string-to-number current-hour) 60)
		    (string-to-number current-min)))
	  (run-at-time (format "%s:%s" hour min) nil
		       'todochiku-message
		       "== REMINDER ==" (format "%s:%s %s" hour min action)
		       "Emacs" 'sticky))))))
  
(defun read-line (file)
  "Make a list from a file, which is divided by LF code"
  (with-temp-buffer
    (insert-file-contents-literally file)
    (split-string
     (buffer-string) "\n" t)))


;; http://stackoverflow.com/questions/4506249/how-to-make-emacs-org-mode-open-links-to-sites-in-google-chrome
;; http://www.koders.com/lisp/fidD53E4053393F9CD578FA7D2AA58BD12FDDD8EB89.aspx?s="skim
(defun browse-url-chrome (url &optional new-window)
  "Set default browser to open a URL"
  (interactive (browse-url-interactive-arg "URL: "))
  (start-process "google-chrome" nil "google-chrome" url))
;; Open a link with google-chrome for Linux
(when (not (eq window-system 'ns))
  (setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "google-chrome")
)
;(setq browse-url-browser-function 'browse-url-default-macosx-browser)
;(setq browse-url-browser-function 'browse-url-default-windows-browser)
;(setq browse-url-browser-function 'browse-url-chrome)


(defun sleep-after-reload ()
  "Automatic call functions when Emacs enters idle time"
  (interactive)
  (cond (e2wm:pst-minor-mode
	 (message "Do nothing, when e2wm is active :-)"))
	(t
	 (save-excursion
	   (save-restriction
	     (message "%s" "reloading...")
	     (reload-ical-export)
	     (message "%s" "MobileOrg sync ... [push]")
	     (org-mobile-push)
	     ;; Not good for e2wm when the cursor is left side (meybe)
	     (recentf-save-list)
	     (message "%s" "done")
	     (sit-for 0.5)
	     (message "%s" ""))))))
  
(defun reload-ical-export ()
  "Export org files as an iCal format file"
  (interactive)
  (when (string= major-mode 'org-mode)
    (org-export-icalendar-combine-agenda-files)))

(provide 'takaxp-utility)

;;;
;; Not available
;;
;;(defun do-sleep-after-reload ()
;;  "a routine function when Emacs is idle"
;;  (interactive)
;;  ;; Avoid application failure related to IME inline-patch
;;  (if ns-marked-overlay
;;      (message "ns-marked-overlay: %s" ns-marked-overlay)
;;    (sleep-after-reload)))
