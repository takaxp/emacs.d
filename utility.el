(require 'org nil t)

;;;###autoload
(defun eval-org-buffer ()
  "Load init.org/utility.org and tangle init.el/utility.el."
  (interactive)
  (if (and (eq major-mode 'org-mode)
           (member (buffer-name) '("init.org" "utility.org")))
      (progn
        (org-babel-tangle)
        (let ((tangled-file
               (concat (file-name-sans-extension (buffer-file-name)) ".el")))
          (when (file-exists-p tangled-file)
            (byte-compile-file tangled-file))))
    (message "Nothing to do for this buffer.")))

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

;;;###autoload
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

(eval-when-compile
  (require 'init nil t))

(defun my:update-alarms-from-file ()
  (when (string= "trigger.org" (buffer-name))
    (set-alarms-from-file "~/Dropbox/org/trigger.org")))

;;;###autoload
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
      (setq lines (cdr lines)))))

;;;###autoload
(defun set-alarm-from-line (line)
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
      ;;        (message "[%s:%s] => %s" hour min action)
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
          ;;            (set-notify-mail hour min action s)
          )))))

;; (when (autoload-if-found
;;        '(todochiku-message)
;;        "todochiku" nil t)
;;   (eval-when-compile
;;     (require 'todochiku nil t))
;;   (with-eval-after-load "todochiku"
;;     (setq todochiku-icons-directory "~/Dropbox/emacs.d/todochiku-icons")
;;     (add-to-list 'todochiku-icons '(emacs . "emacs.png"))
;;     (require 'cl-lib)))

;;;###autoload
(defun my:desktop-notify (type title hour min action s)
  (cond
   ;; ((string= type "growl")
   ;;  (todochiku-message
   ;;   title (format "%s:%s %s" hour min action) "Emacs" s))
   ((string= type "osx-native")
    (terminal-notifier-notify
     title
     (format "%s:%s %s" hour min action)))
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
  ;;    (message "%s:%s %s %s" hour min action s)
  (run-at-time (format "%s:%s" hour min) nil
               'my:desktop-notify
               "osx-native" "Emacs" hour min action nil))

(defun read-line (file)
  "Make a list from a file, which is divided by LF code"
  (with-temp-buffer
    (insert-file-contents-literally file)
    (split-string
     (buffer-string) "\n" t)))

(defvar my:file-ring nil)

;;;###autoload
(defun my:make-file-ring (files)
  (setq my:file-ring (copy-sequence files)))
;;    (setf (cdr (last my:file-ring)) my:file-ring))
(my:make-file-ring
 '("~/Dropbox/org/work.org" "~/Dropbox/org/daily.org" "~/Dropbox/org/wg1.org"
   "~/Dropbox/org/research.org" "~/Dropbox/emacs.d/config/init.org"))

;;;###autoload
(defun my:open-file-ring ()
  (interactive)
  (find-file (car my:file-ring))
  (setq my:file-ring
        (append (cdr my:file-ring)
                (list (car my:file-ring)))))

;;    (setq my:file-ring (cdr my:file-ring)))

;;;###autoload
(defun show-org-buffer (file)
  "Show an org-file on the current buffer"
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (concat "~/Dropbox/org/" file))))

;;;###autoload
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
      (when (require 'org nil t)
        (org-end-of-line)))))

;;;###autoload
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

;;;###autoload
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
(global-set-key (kbd "C-9") 'insert-formatted-current-time)

(defcustom my:auto-install-batch-list-el-url nil
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

(with-eval-after-load "org"
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
      (shell-command-to-string (concat "open " default-timeline)))))

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

(defvar ox-icalendar-activate nil)
(with-eval-after-load "org"
  (run-with-idle-timer 600 t
                       '(lambda ()
                          (reload-ical-export)))
  ;;    (run-with-idle-timer 1000 t 'org-mobile-push)
  ;; FIXME
  (add-hook 'focus-in-hook '(lambda () (setq ox-icalendar-activate nil)))
  (add-hook 'focus-out-hook '(lambda () (setq ox-icalendar-activate t))))

;;;###autoload
(defun reload-ical-export ()
  "Export org files as an iCal format file"
  (interactive)
  (when (and (string= major-mode 'org-mode) ox-icalendar-activate)
    (my:ox-icalendar)))

;; http://stackoverflow.com/questions/4506249/how-to-make-emacs-org-mode-open-links-to-sites-in-google-chrome
;; http://www.koders.com/lisp/fidD53E4053393F9CD578FA7D2AA58BD12FDDD8EB89.aspx?s="skim
(when (autoload-if-found
       '(browse-url)
       "browse-url" nil t)
  (with-eval-after-load "browse-url"
    (cond
     ((eq window-system 'ns)
      (setq browse-url-generic-program 'google-chrome))
     ((eq window-system 'mac)
      (setq browse-url-browser-function 'browse-url-generic)
      (setq browse-url-generic-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))
     (t
      nil))))

;;(setq browse-url-browser-function 'browse-url-default-macosx-browser)
;;(setq browse-url-browser-function 'browse-url-default-windows-browser)
;;(setq browse-url-browser-function 'browse-url-chrome)

;;;###autoload
(defun my:date ()
  (interactive)
  (message "%s" (concat
                 (format-time-string "%Y-%m-%d") " ("
                 (format-time-string "%a") ") "
                 (format-time-string "%H:%M"))))
(global-set-key (kbd "C-c t") 'my:date)

;; find ~/.emacs.d/backup  -type f -name '*15-04-24_*' -print0 | while read -r -d '' file; do echo -n " \"$file\""; done | xargs -0
(defun recursive-delete-backup-files (count)
  (if (= count 1)
      1
    (recursive-delete-backup-files (1- count)))
  (delete-backup-files count))

;;;###autoload
(defun delete-backup-files (&optional day-shift)
  "Delete backup files created in yesterday.
  > find ~/.emacs.d/backup -type f -name '*YY-MM-DD_*' -print0 | xargs -0"
  (interactive)
  (unless day-shift
    (setq day-shift 1))
  (let* ((backup-dir "~/.emacs.d/backup")
         (cmd (concat "find " backup-dir "  -type f -name \'*"
                      (format-time-string
                       "%y-%m-%d_"
                       (time-subtract (current-time)
                                      (seconds-to-time
                                       (* day-shift (* 24 3600)))))
                      "*\' -print0 | while read -r -d \'\' file; "
                      " do echo -n \" \\\"$file\\\"\"; done | xargs -0"))
         (files (shell-command-to-string cmd)))
  ;;;      (message "%s" cmd)
    (unless (string= files "")
      (message "%s" files)
      (shell-command-to-string (concat "rm -r " files)))))

;;;###autoload
(defun my:daylight-theme ()
  (interactive)
  (when (require 'daylight-theme nil t)
    (mapc 'disable-theme custom-enabled-themes)
    (load-theme 'daylight t)
    (moom-reset-font-size)))

;;;###autoload
(defun my:night-theme ()
  (interactive)
  (when (require 'night-theme nil t) ;; atom-one-dark-theme
    (mapc 'disable-theme custom-enabled-themes)
    (load-theme 'night t)
    (moom-reset-font-size)))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

;;;###autoload
(defun my:cmd-to-open-iterm2 ()
  (interactive)
  (shell-command-to-string "open -a iTerm2.app"))

(defun my:lingr-login ()
  (when (string= "Sat" (format-time-string "%a"))
    (lingr-login)))

(defun my:backup (files &optional dropbox)
  "Backup a file to `Dropbox/backup' directory. If `dropbox' option is provided then the value is uased as a root directory."
  (interactive "P")
  (let ((system (system-name))
        (rootdir (or dropbox "~/Dropbox")))
    (if (and system
             (stringp rootdir)
             (file-directory-p (or rootdir (expand-file-name rootdir))))
        (mapc
         (lambda (file)
           (if (and (stringp file)
                    (file-readable-p (or file (expand-file-name file))))
               (shell-command-to-string
                (concat "cp -f " file " " rootdir "/backup/" system "/"))
             (message (format "--- backup failure: %s" file))))
         (if (listp files)
             files
           (list files)))
      (message (format "--- backup-dir does not exist: %s" rootdir)))))

;;; Test function from GNU Emacs (O'REILLY, P.328)
;;;###autoload
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
(defun describe-timer ()
  "see http://masutaka.net/chalow/2009-12-05-1.html"
  (interactive)
  (let ((tl timer-list) time
        (timer nil))
    (pop-to-buffer (get-buffer-create "*timer*"))
    (erase-buffer)
    (insert
     "TIME           FUNCTION\n"
     "-------------- ----------------------\n")
    (while tl
      (setq timer (car tl))
      (insert
       (concat
        (format-time-string "%m/%d %T"
                            (list (aref timer 1)
                                  (aref timer 2)
                                  (aref timer 3)))
        " "
        (symbol-name (aref timer 5))
        "\n"))
      (setq tl (cdr tl)))
    (read-only-mode 1)))

;;;###autoload
(defun my:window-resizer ()
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
