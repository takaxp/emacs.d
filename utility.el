
(defun eval-org-buffer ()
  "Load init.org/utility.org and tangle init.el/utility.el."
  (interactive)
  (if (and (eq major-mode 'org-mode)
           (or (string= (buffer-name) "init.org")
               (string= (buffer-name) "utility.org")))
      (progn
        (org-babel-tangle)
        (let ((tangled-file
               (concat (file-name-sans-extension (buffer-file-name)) ".el")))
          (when (file-exists-p tangled-file)
            (byte-compile-file tangled-file))))
    (message "Nothing to do for this buffer.")))

(defvar kyoko-mad-mode nil)
(defun kyoko-mad-mode-toggle ()
  (interactive)
  (setq kyoko-mad-mode (not kyoko-mad-mode))
  (cond (kyoko-mad-mode
         (message "Kyoko mad mode: ON"))
        (t
         (message "Kyoko mad mode: OFF"))))
;; She will be mad if you do nothing within 10 min.
(run-with-idle-timer
 600 t
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

(with-eval-after-load "todochiku"
  (setq todochiku-icons-directory "~/Dropbox/emacs.d/todochiku-icons")
  (add-to-list 'todochiku-icons '(emacs . "emacs.png"))
  (message "--- todochiku-icons are ready."))

(defun set-alarms-from-file (file)
  "Make alarms from org-mode tables. If you have an org-mode file
           with tables with the following format:
        |------+-------+--------------------|
        | Flag |  Time | Content            |
        |------+-------+--------------------|
        |      | 07:00 | Wakeup             |
        |      |       | Read papers        |
        | X    | 12:00 | Clean up your desk |
        When it is 7:00 and 12:00, Growl notify with a message which is specified
        content column from the table. \"Read papers\" will be ignored.
        \"Clean up your desk\" will be shown by sticky mode"
  (let
      ((lines (read-line file)))
    (cancel-function-timers 'my:desktop-notify) ;; clear existing timers
    (while lines
      (set-alarm-from-line (decode-coding-string (car lines) 'utf-8))
      (setq lines (cdr lines))
      (message ""))))

(defun set-alarm-from-line (line)
  "NOTE: this function need (require 'todochiku)"
  (require 'cl)
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
          (let
              ((s nil))
            (when (string-match "^\|\\s-*X\\s-*\|" line)
              (setq s 'sticky))
            ;;      (set-notify-growl hour min action s)
            (set-notify-osx-native hour min action s)
            (set-notify-mail hour min action s)
            ))))))

(defun my:desktop-notify (type title hour min action s)
  (cond
   ((string= type "growl")
    (require 'cl)
    (when (require 'todochiku nil t)
      (todochiku-message
       title
       (format "%s:%s %s" hour min action)
       "Emacs" s)))
   ((string= type "osx-native")
    (shell-command-to-string
     (concat "terminal-notifier -title \"Emacs\" -message \""
             (format "%s:%s %s" hour min action) "\"")))
   (t nil)))

(defun set-notify-mail (hour min action s)
  (run-at-time (format "%s:%s" hour min) nil
               'my:desktop-notify
               "mail" "りまいんだ" hour min action nil))

(defun set-notify-growl (hour min action s)
  (run-at-time (format "%s:%s" hour min) nil
               'my:desktop-notify
               "growl" "== REMINDER ==" hour min action s))

(defun set-notify-osx-native (hour min action s)
  "terminal-notifier is required."
  (run-at-time (format "%s:%s" hour min) nil
               'my:desktop-notify
               "osx-native" "Emacs" hour min action nil))

(defun read-line (file)
  "Make a list from a file, which is divided by LF code"
  (with-temp-buffer
    (insert-file-contents-literally file)
    (split-string
     (buffer-string) "\n" t)))

(defun set-alarm-hook ()
  (when (string-match (file-name-base "today.org") (buffer-name))
    (message "--- The alarm list has been updated.")
    (set-alarms-from-file alarm-table)))

(defvar my-file-ring nil)
(defun takaxp:make-file-ring (files)
  (setq my-file-ring (copy-sequence files)))
;;    (setf (cdr (last my-file-ring)) my-file-ring))
(takaxp:make-file-ring
 '("~/Dropbox/org/work.org" "~/Dropbox/emacs.d/config/init.org"
   "~/Dropbox/org/buffer.org" "~/Dropbox/emacs.d/config/utility.org"
   "~/Dropbox/org/research.org" "~/Dropbox/org/next.org"))

(defun takaxp:open-file-ring ()
  (interactive)
  (find-file (car my-file-ring))
  (setq my-file-ring
        (append (cdr my-file-ring)
                (list (car my-file-ring)))))

;;    (setq my-file-ring (cdr my-file-ring)))

(defun show-org-buffer (file)
  "Show an org-file on the current buffer"
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (concat "~/Dropbox/org/" file))))

(defun insert-org-file-header-template ()
  (interactive)
  (when (string= major-mode 'org-mode)
    (let ((title "#+TITLE:\t\n")
          (date "#+DATE: \t\n")
          (update "#+UPDATE:\t\n")
          (author "#+AUTHOR:\tTakaaki ISHIKAWA <takaxp@ieee.org>\n")
          (option "#+OPTIONS:\t\\n:t\n")
          (other "\n"))
      (goto-char 0)
      (save-excursion
        (insert title date update author option other))
      (org-end-of-line))))

(defun insert-minutes-template ()
  (interactive)
  (when (string= major-mode 'org-mode)
    (let ((date "日時：\n")
          (place "場所：\n")
          (attendance "出席者：\n")
          (documents "資料：\n\n"))
      (save-excursion
        (insert date place attendance documents)))))

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
  "Insert a timestamp at the cursor position. C-u will add [] brackets."
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
    (my-ox-icalendar)))

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

;;;###autoload
(defun takaxp:date ()
  (interactive)
  (message "%s" (concat
                 (format-time-string "%Y-%m-%d") " ("
                 (format-time-string "%a") ") "
                 (format-time-string "%H:%M"))))
(global-set-key (kbd "C-c t") 'takaxp:date)

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

;;;###autoload
(defun takaxp:window-resizer ()
  "Control separated window size and position.
   Type {j,k,l,m} to adjust windows size."
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
