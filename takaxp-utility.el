;;; Utility functions
;;;                                       Takaaki ISHIKAWA  <takaxp@ieee.org>

;;; Move frame to any position on display 
;; Cite: http://www.bookshelf.jp/soft/meadow_30.html#SEC411
(defvar my-move-frame-distance 293) ;; mounts // mbp=394, 1frame=586
(defun my-move-frame (&optional vertical horizon)
  "Move frame to a new position with vertical and horizon value"
  (when window-system
    (let* ((meadowy (featurep 'meadow))
           (top (frame-parameter (selected-frame) 'top))
           (left (frame-parameter (selected-frame) 'left))
           (mpos (if meadowy
                     (cdr (mouse-position))
                   (cdr (mouse-pixel-position))))
           (hoff (* (or horizon 0) my-move-frame-distance))
           (voff (* (or vertical 0) my-move-frame-distance)))
      (set-frame-position (selected-frame) (+ left hoff) (+ top voff))
;; The mouse follow the window moving.
;      (if meadowy
;          (set-mouse-position (selected-frame) (car mpos) (cdr mpos))
;        (set-mouse-pixel-position (selected-frame)
;                                  (+ (car mpos) hoff)
;                                  (+ (cdr mpos) voff)))
)))
(defun my-move-frame-up ()
  "Move frame up"
  (interactive)
  (my-move-frame -1 0))
(defun my-move-frame-down ()
  "Move frame down"
  (interactive)
  (my-move-frame 1 0))
(defun my-move-frame-left ()
  "Move frame left"
  (interactive)
  (my-move-frame 0 -1))
(defun my-move-frame-right ()
  "Move frame right"
  (interactive)
  (my-move-frame 0 1))

;;; Export org files as an iCal format file
(defun reload-ical-export ()
  (interactive)
  (if (string= major-mode 'org-mode)
      (progn
	(org-export-icalendar-combine-agenda-files))))

;;; Insert "  - " at the head of line
(defun add-itemize-head ()
  (interactive)
  (move-beginning-of-line 1)
  (insert "  - "))

;;; Show a file on the current buffer
(defun show-org-buffer (file)
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
	(switch-to-buffer buffer)
	(message "%s" file))
    (find-file "~/Dropbox/org/next.org")))

;;; Show yes or no when you try to kill Emacs
;; Cite: http://flex.ee.uec.ac.jp/texi/emacs-jp/emacs-jp_12.html
;; Cite: http://d.hatena.ne.jp/Ubuntu/20090417/1239934416
(defun confirm-save-buffers-kill-emacs ()
  (interactive)
  (if (yes-or-no-p "Are you sure to quit Emacs now? ")
    (save-buffers-kill-emacs)
    (kill-buffer (buffer-name))))

;;; Insert a date and time quickly
;; Cite: http://www.fan.gr.jp/~ring/doc/elisp_20/elisp_38.html#SEC608
(defun insert-formatted-current-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))
(defun insert-formatted-current-time ()
  (interactive)
  (insert (format-time-string "%H:%M")))

;;; Visible-bell
;; visible-bell
(setq visible-bell t)
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
	   'face '(:foreground "#FFFFFF" :background "#C1252D")
	   ))
    (setq echo-area-bell-cached-string echo-area-bell-string))
  (message echo-area-bell-propertized-string)
  (sit-for echo-area-bell-delay)
  (message ""))
(setq ring-bell-function 'echo-area-bell)


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


;;; Test function to verify a function (C-c C-b)
(defun testfunc ()
  (interactive)
;  (message buffer-file-coding-system);
;  (message (coding-system-get buffer-file-coding-system 'mime-charset))
;  (message "%s" (system-name))
;  (if (string-match "^/Users" (buffer-file-name))
      (message "%s" (buffer-file-name))
)

;;; Send focused region to the end of buffer
(defun forward-region-to-tail ()
  (interactive)
  (progn
    (setq save-current-pos (region-beginning))
    (kill-region (region-beginning) (region-end))
    (goto-char (point-max))
    (yank)
    (goto-char save-current-pos)
    (message "Forward the region to the end of buffer ... done")
))

;;; Test function for AppleScript
;; Cite: http://sakito.jp/emacs/emacsobjectivec.html
(defun do-test-applescript ()
  (interactive)
  (do-applescript
   (format
    (concat
     "display dialog \"Hello world!\" \r"
     ))))


;;; Automatic call functions when Emacs enters idle time ;;;;;;;;;;;;;;;;;;;;
(defun sleep-after-reload ()
  (interactive)
  (message "%s" "reloading...")
  (sleep-for 0.5)

  ; Set alarms of org-agenda
  (message "%s" "set alarms")
  (sleep-for 0.5)
  (org-agenda-to-appt)
  (sleep-for 0.5)

  ; Export an iCal file
  (message "%s" "iCal export")
  (sleep-for 0.5)
  (reload-ical-export)
  (sleep-for 0.5)

  ; Send org files to the server
  (message "%s" "MobileOrg sync ... [push]")
  (sleep-for 0.5)
  (org-mobile-push)

; add new functions here
;
  (message "%s" "done")
  (sleep-for 0.5)
  (message "%s" "")
)

(provide 'takaxp-utility)
