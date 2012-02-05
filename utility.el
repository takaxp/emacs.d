
(defun eval-org-buffer ()
  "Load init.org/utility.org and tangle init.el/utility.el."
  (interactive)
  (when (and (eq major-mode 'org-mode)
             (or (string= (buffer-name) "init.org")
                 (string= (buffer-name) "utility.org")))
    (org-babel-tangle)
    (let ((tangled-file
           (concat (file-name-sans-extension (buffer-file-name)) ".el")))
      (when (file-exists-p tangled-file)
        (load tangled-file)
        (byte-compile-file tangled-file)))))

(defvar kyoko-mad-mode nil)
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

(defun org2dokuwiki-cp-kill-ring ()
  "Convert the current org-file to dokuwiki text, and copy it to kill-ring."
  (interactive)
  (when (eq major-mode 'org-mode)
    (cond (buffer-file-name
           (kill-new
            (shell-command-to-string
             (concat "cat " buffer-file-name "| perl "
                     (expand-file-name "~/Dropbox/scripts/org2dokuwiki.pl"))))
           (message "Copying %s ... done" buffer-file-name)
           (sit-for 1.5)
           (message ""))
          (t (message "There is NOT such a file.")))))

(defcustom open-current-directory-console-program "iTerm2.app"
  "Specify a console program"
  :type 'string
  :group 'takaxp-mac)

(defun open-current-directory ()
  " Open Current Directory for MacOSX
  0) Put this function in your .emacs
  1) M-x open-current-directory
  2) Terminal will open automatically
  3) Type M-v to paste and move to a path to the current directory in Emacs"
  (interactive)
  (let ((file-path (buffer-file-name (current-buffer))))
    (unless (string= file-path nil)
      (let ((directory
            (substring file-path 0
                       (-
                        (length file-path)
                        (length (buffer-name (current-buffer)))))))
        (message "%s" directory)
        (shell-command-to-string (concat "echo cd " directory " |pbcopy"))
        (shell-command-to-string
         (concat "open -a " open-current-directory-console-program))))))

(setq confirm-kill-emacs 'y-or-n-p)

(defun set-alarms-from-file (file)
  "Make alarms from org-mode tables. If you have an org-mode file
   with tables with the following format:
|----+--------+----------------------------------------------------------|
| ID |   Time | Content                                                  |
|----+--------+----------------------------------------------------------|
|  1 |  07:00 | Wakeup                                                   |
|  2 |        | Read papers                                              |
|  3 |  12:00 | Clean up your desk                                       |
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
             (setq action
                   (substring line (match-beginning 1) (match-end 1)))))
         (when (and (and hour min) action)
           ;;       (message "[%s:%s] => %s" hour min action)
           (setq current-hour (format-time-string "%H" (current-time)))
           (setq current-min (format-time-string "%M" (current-time)))
           (when (> (+ (* (string-to-number hour) 60)
                       (string-to-number min))
                    (+ (* (string-to-number current-hour) 60)
                       (string-to-number current-min)))
             (run-at-time (format "%s:%s" hour min) nil
                          'todochiku-message
                          "== REMINDER =="
                          (format "%s:%s %s" hour min action)
                          "Emacs" 'sticky))))))
  
   (defun read-line (file)
     "Make a list from a file, which is divided by LF code"
     (with-temp-buffer
       (insert-file-contents-literally file)
       (split-string
        (buffer-string) "\n" t)))

(defvar my-file-ring nil)
(defun takaxp:make-file-ring (files)
  (setq my-file-ring (copy-sequence files))
  (setf (cdr (last my-file-ring)) my-file-ring))
(takaxp:make-file-ring '("~/devel/mygit/emacs.d/init.org"
                                         "~/devel/mygit/emacs.d/utility.org"
                         "~/Dropbox/org/next.org" "~/Dropbox/org/buffer.org"))

(defun takaxp:open-file-ring ()
  (interactive)
  (find-file (car my-file-ring))
  (setq my-file-ring (cdr my-file-ring)))

(defun show-org-buffer (file)
  "Show an org-file on the current buffer"
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (concat "~/Dropbox/org/" file))))

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

(defun init-auto-install ()
  "Setup auto-install.el.
1. Set my-auto-install-batch-list-el-url
2. M-x init-auto-install
3. M-x auto-install-batch hoge"
  (interactive)
  (when (and (require 'auto-install nil t)
             my-auto-install-batch-list-el-url)
    (setq auto-install-batch-list-el-url my-auto-install-batch-list-el-url)
    (setq auto-install-directory default-path)
    (setq auto-install-wget-command "/opt/local/bin/wget")
    (auto-install-update-emacswiki-package-name t)
    ;; compatibility
    (auto-install-compatibility-setup))) ; for install-elisp users

(defun add-itemize-head (arg)
  "Insert \"  - \" at the head of line.
  If the cursor is already at the head of line, it is NOT returned back to the
  original position again. Otherwise, the cursor is moved to the right of the
  inserted string. \"  - [ ] \" will be inserted using C-u prefix."
  (interactive "P")
  (let ((item-string "  - "))
    (when arg
      (setq item-string "  - [ ] "))
    (cond ((= (point) (line-beginning-position))
           (insert item-string))
          (t (save-excursion
               (move-beginning-of-line 1)
               (insert item-string))))))

(global-set-key (kbd "C-M--") 'add-itemize-head)

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

(global-set-key (kbd "C-0") 'insert-formatted-current-date)
(global-set-key (kbd "C--") 'insert-formatted-current-time)
(global-set-key (kbd "C-=") 'insert-formatted-signature)

(defcustom my-auto-install-batch-list-el-url nil
  "URL of a auto-install-batch-list.el"
  :type 'string
  :group 'takaxp-utility)

;; Publish an xml file to show a Gantt Chart
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
             (and default-timeline-csv-file
                  default-timeline-xml-business-file))
    (shell-command-to-string (concat "rm -f " default-timeline-csv-file))
    (org-table-export default-timeline-csv-file "orgtbl-to-csv")
    (shell-command-to-string (concat "org2gantt.pl > "
                                     default-timeline-xml-business-file))
    (shell-command-to-string (concat "open " default-timeline))))

(defun export-timeline-private ()
  "Export schedule table as an XML source to create an web page"
  (interactive)
  (when (and default-timeline
             (and default-timeline-csv-file
                  default-timeline-xml-private-file))
    (shell-command-to-string (concat "rm -f " default-timeline-csv-file))
    (org-table-export default-timeline-csv-file "orgtbl-to-csv")
    (shell-command-to-string (concat "org2gantt.pl > "
                                     default-timeline-xml-private-file))
    (shell-command-to-string (concat "open " default-timeline))))

(run-with-idle-timer 600 t 'reload-ical-export)
(run-with-idle-timer 1000 t 'org-mobile-push)
  
(defun reload-ical-export ()
  "Export org files as an iCal format file"
  (interactive)
  (when (string= major-mode 'org-mode)
    (org-export-icalendar-combine-agenda-files)))

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

;;; Test function for AppleScript
;;; Cite: http://sakito.jp/emacs/emacsobjectivec.html
(defun do-test-applescript ()
  (interactive)
  (do-applescript
   (format
    (concat
     "display dialog \"Hello world!\" \r"))))

(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-event (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))

(provide 'utility)