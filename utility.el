;; utility.el --- My utilities -*- lexical-binding: t -*-
;;
(require 'init-autoloads nil t)

;; https://en.wikipedia.org/wiki/Darwin_(operating_system)
;;;###autoload
(defun macos-name (version)
  "Return macOS name according to the VERSION number."
  (if (stringp version)
      (cond ((version<= "21.0" version) "Monterey")
	          ((version<= "20.0" version) "Big Sur")
	          ((version<= "19.0" version) "Catalina")
	          ((version<= "18.0" version) "Mojave")
	          ((version<= "17.0" version) "High Sierra")
	          ((version<= "16.0" version) "Sierra")
	          ((version<= "15.0" version) "El Capitan")
	          ((version<= "14.0" version) "Yosemite")
	          ((version<= "13.0" version) "Mavericks")
	          ((version<= "12.0" version) "Mountain Lion")
	          ((version<= "11.0" version) "Lion")
	          ((version<= "10.0" version) "Snow Leopard")
	          ((version<= "9.0" version) "Leopard")
	          ((version<= "8.0" version) "Tiger")
	          ((version<= "7.0" version) "Panther")
	          (t "undefined"))
    nil))

;;;###autoload
(defun macos-version ()
  (let ((macos-type-version (nth 2 (split-string system-configuration "-"))))
		(string-match "darwin\\(.*\\)" macos-type-version)
		(match-string 1 macos-type-version)))

;;;###autoload
(defun my-cmd-to-open-iterm2 (&optional arg)
  (interactive "P")
  (shell-command-to-string
   (concat "open -a iTerm.app "
           (when arg default-directory))))

(defvar my-kyoko-mad-mode nil)
;;;###autoload
(defun my-kyoko-mad-mode-toggle ()
  (interactive)
  (setq my-kyoko-mad-mode (not my-kyoko-mad-mode))
  (message (concat "Kyoko mad mode: "
                   (if my-kyoko-mad-mode "ON" "OFF"))))
;;;###autoload
(defun my-kyoko-mad ()
  (interactive)
  (when my-kyoko-mad-mode
    (shell-command-to-string
     "say -v Kyoko おいおまえ，遊んでないで，仕事しろ")))

;; She will be mad if you do nothing within 10 min.
(run-with-idle-timer 600 t 'my-kyoko-mad)

(defcustom open-current-directory-console-program "iTerm2.app"
  "Specify a console program"
  :type 'string
  :group 'takaxp-mac)

;;;###autoload
(defun my-open-current-directory-in-terminal ()
  " Open Current Directory for macOS
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

;;;###autoload
(defun my-update-alarms-from-file ()
  (interactive)
  (let ((bname (buffer-name)))
    (when (string= bname "daily.org")
      (my-set-alarms-from-file (concat "~/Dropbox/org/db/" bname)))))

(defun my-set-alarms-from-file (file)
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
    (cancel-function-timers 'my-desktop-notify) ;; clear existing timers
    (while lines
      (set-alarm-from-line (decode-coding-string (car lines) 'utf-8))
      (setq lines (cdr lines)))))

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
             "\|\\s-*\\([^\|]+[^ ]\\)\\s-*\|" line (match-end 2))
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
        (let ((s nil))
          (when (string-match "^\|\\s-*X\\s-*\|" line)
            (setq s 'sticky))
          (set-notify-macos hour min action s))))))

(defun set-notify-macos (hour min action sticky)
  "`alerter' is required."
  (run-at-time (format "%s:%s" hour min) nil
               'my-desktop-notify
               "macos" "Org Mode" hour min action sticky))

(declare-function my-desktop-notification "init-org")
(defun my-desktop-notify (type title hour min action sticky)
  "An interface to `my-desktop-notification'."
  (cond
   ((string= type "macos")
    (my-desktop-notification
     title (format "%s:%s %s" hour min action) sticky))))

(defun read-line (file)
  "Make a list from a file, which is divided by LF code"
  (with-temp-buffer
    (insert-file-contents-literally file)
    (split-string
     (buffer-string) "\n" t)))

(defvar my-file-ring nil)

;;;###autoload
(defun my-make-file-ring (files)
  (setq my-file-ring (copy-sequence files)))
;;    (setf (cdr (last my-file-ring)) my-file-ring))
(my-make-file-ring
 '("~/Dropbox/org/tr/work.org" "~/Dropbox/org/db/daily.org"
   "~/Dropbox/org/minutes/wg1.org" "~/Dropbox/org/tr/work.org"
   "~/Dropbox/org/academic.org" "~/Dropbox/org/org2ja.org"
   "~/Dropbox/org/db/article.org" "~/Dropbox/emacs.d/config/init.org"))

;;;###autoload
(defun my-open-file-ring ()
  (interactive)
  (find-file (car my-file-ring))
  (setq my-file-ring
        (append (cdr my-file-ring)
                (list (car my-file-ring)))))

;;    (setq my-file-ring (cdr my-file-ring)))

;;;###autoload
(defun my-show-org-buffer (file)
  "Show an org-file on the current buffer."
  (interactive)
  (message "%s" file)
  (let ((tbuffer (get-buffer file))
        (cbuffer (current-buffer)))
    (if tbuffer
        (switch-to-buffer tbuffer)
      (find-file (concat (getenv "SYNCROOT") "/org/" file)))
    (when (and (fboundp 'my-org-agenda-to-appt)
               (not (eq cbuffer tbuffer)))
      (my-org-agenda-to-appt 'force))))

(declare-function org-end-of-line "org")

;;;###autoload
(defun insert-org-file-header-template ()
  (interactive)
  (when (string= major-mode 'org-mode)
    (let ((title "#+title:\t\n")
          (date "#+date: \t\n")
          (author "#+author:\tTakaaki ISHIKAWA <takaxp@ieee.org>\n")
          (option "#+options:\t\\n:t\n")
          (other "\n"))
      (goto-char 0)
      (save-excursion
        (insert title date author option other))
      (when (require 'org nil t)
        (org-end-of-line)))))

(defun my-insert-empty-pgp-tree ()
  (interactive)
  (insert "** TODO hoge\n")
  (insert "-----BEGIN PGP MESSAGE-----\n\n-----END PGP MESSAGE-----\n")
  (forward-line -2))

(defun my-insert-enc2me-pgp-tree ()
    (interactive)
    (insert "** TODO share with me\n")
    (insert "   :PROPERTIES:\n")
    (insert "   :CRYPTKEY: takaxp@ieee.org\n")
    (insert "   :END:\n")
    (insert "\n")
    (forward-line -1))

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

;;;###autoload
(defun my-get-random-string (length)
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

(defvar ox-icalendar-activate nil)
    ;;;###autoload
(defun my-ox-icalendar-activate ()
  (setq ox-icalendar-activate (frame-focus-state)))
(with-eval-after-load "org"
  (when (eq system-type 'ns)
    (run-with-idle-timer 180 t 'my-reload-ical-export)
    ;;    (run-with-idle-timer 1000 t 'org-mobile-push)
    (add-function :after after-focus-change-function
                  #'my-ox-icalendar-activate)))

(declare-function my-ox-upload-icalendar "init.org")
;;;###autoload
(defun my-reload-ical-export ()
  "Export org files as an iCal format file"
  (interactive)
  (when (and (string= major-mode 'org-mode)
             ox-icalendar-activate)
    (my-ox-upload-icalendar)))

(when (autoload-if-found
       '(browse-url)
       "browse-url" nil t)
  (with-eval-after-load "browse-url"
    (cond
     ((eq window-system 'ns)
      (custom-set-variables
       '(browse-url-generic-program 'google-chrome)))
     ((eq window-system 'mac)
      (custom-set-variables
       '(browse-url-browser-function 'browse-url-generic)
       '(browse-url-generic-program
         "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
       ))
     (t
      nil))))
;;(setq browse-url-browser-function 'browse-url-default-macosx-browser)
;;(setq browse-url-browser-function 'browse-url-default-windows-browser)
;;(setq browse-url-browser-function 'browse-url-chrome)

;; find ~/.emacs.d/backup  -type f -name '*15-04-24_*' -print0 | while read -r -d '' file; do echo -n " \"$file\""; done | xargs -0

;;;###autoload
(defun recursive-delete-backup-files (days)
  (if (= days 1)
      1
    (recursive-delete-backup-files (1- days)))
  (delete-backup-files days))

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
    ;; (message "%s" cmd)
    (unless (string= (chomp files) "")
      (message "%s" (chomp files))
      (let ((trash (if (eq system-type 'darwin)
                       " ~/.Trash" "~/.local/share/Trash")))
        (shell-command-to-string (concat "mv -v " (chomp files) trash))))))

;;;###autoload
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun my-backup (files &optional dropbox)
  "Backup a file to `Dropbox/backup' directory.
If `dropbox' option is provided then the value is uased as a root directory."
  (interactive "P")
  (let ((system (system-name))
        (rootdir (or dropbox (getenv "SYNCROOT"))))
    (if (and system
             (stringp rootdir)
             (file-directory-p (or rootdir (expand-file-name rootdir))))
        (mapc
         (lambda (file)
           (if (and (stringp file)
                    (file-readable-p (or file (expand-file-name file))))
               (shell-command-to-string
                (concat "cp -f " file " " rootdir "/backup/" system "/"))
             (warn (format "--- backup failure: %s" file))))
         (if (listp files)
             files
           (list files)))
      (user-error (format "--- backup-dir does not exist: %s" rootdir)))))

;;;###autoload
(defun mac:delete-files-in-trash-bin ()
  (interactive)
  (do-applescript
   (concat
    "tell application \"Finder\"\n"
    "set itemCount to count of items in the trash\n"
    "if itemCount > 0 then\n"
    "empty the trash\n"
    "end if\n"
    "end tell\n"))
  (my-desktop-notification "Emacs" "Empty the trash, done."))

;;;###autoload
(defun my-kill-emacs ()
    (switch-to-buffer "*Messages*")
    (message "3: %s" kill-emacs-hook)
    (y-or-n-p "Sure? "))

;;;###autoload
(defun my-kill-emacs-hook-show ()
  "Test Emacs killing sequence."
  (add-hook 'after-init-hook
            (lambda () (message "1: %s" kill-emacs-hook)) t)
  (with-eval-after-load "postpone"
    (message "2: %s" kill-emacs-hook))
  (add-hook 'kill-emacs-hook #'my-kill-emacs))

;;;###autoload
(defun my-setup-package-el ()
  "Setting up for installing packages via built-in package.el.
Downloaded packages will be stored under ~/.eamcs.d/elpa."
  (when (and (require 'package nil t)
             (boundp 'package-archives))
    (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                        (not (gnutls-available-p))))
           (proto (if no-ssl "http" "https")))
      (add-to-list 'package-archives
                   (cons "melpa" (concat proto "://melpa.org/packages/")) t)
      (add-to-list 'package-archives
                   (cons "takaxp" "~/devel/git/melpa/packages/") t))
    (package-initialize)))

(declare-function org-babel-tangle "org-babel")

;;;###autoload
(defun my-eval-org-buffer ()
  "Load init.org/utility.org and tangle init.el/utility.el."
  (interactive)
  (if (and (require 'org nil t)
           (eq major-mode 'org-mode)
           (member (buffer-name) '("init.org" "utility.org")))
      (progn
        (org-babel-tangle)
        (let ((tangled-file
               (concat (file-name-sans-extension (buffer-file-name)) ".el")))
          (when (file-exists-p tangled-file)
            (byte-compile-file tangled-file))))
    (message "Nothing to do for this buffer.")))

(defvar my-org-bullet-regexp
  "\\(^[ \t]*[-\\+\\*][ \t]\\|^[ \t]*[a-z0-9A-Z]*[\\.)][ \t]\\)")

(defvar my-org-bullet-with-checkbox-regexp
  (concat my-org-bullet-regexp "\\[.\\][ \t]+"))

;;;###autoload
(defun my-org-insert-bullet (begin end)
  (interactive "r")
  (unless mark-active
    (setq begin (line-beginning-position))
    (setq end (line-end-position)))
  (let* ((bullet " - ")
         (len (string-width bullet)))
    (goto-char begin)
    (while (and (re-search-forward (concat "\\(^[ \t]*\\)") end t)
                (not (looking-at "[-\\+\\*][ \t]\\|[a-z0-9A-Z]*[\\.)][ \t]"))
                (not (equal (point) end)))
      (replace-match (concat "\\1" bullet) nil nil)
      (setq end (+ end len)))
    (goto-char begin)))

;;;###autoload
(defun my-org-delete-bullet (begin end)
  (interactive "r")
  (unless mark-active
    (setq begin (line-beginning-position))
    (setq end (line-end-position)))
  (goto-char begin)
  (while (and (re-search-forward
               "^[ \t]*\\([-\\+\\*][ \t]\\|[a-z0-9A-Z]*[\\.)][ \t]\\)" end t)
              (not (looking-at "\\[.\\][ \t]+")))
    (let ((len (- (match-end 0) (match-beginning 0))))
      (replace-match "" nil nil)
      (setq end (- end len))))
  (goto-char begin))

(defun my-org-toggle-checkbox (begin end)
  (interactive "r")
  (unless mark-active
    (setq begin (line-beginning-position))
    (setq end (line-end-position)))
  (goto-char begin)
  (if (re-search-forward
       my-org-bullet-with-checkbox-regexp (point-at-eol) t)
      (my-org-delete-checkbox-from-bullet begin end)
    (my-org-insert-checkbox-into-bullet begin end)))

;;;###autoload
(defun my-org-insert-checkbox-into-bullet (begin end)
  (interactive "r")
  (unless mark-active
    (setq begin (line-beginning-position))
    (setq end (line-end-position)))
  (let* ((checkbox "[ ] ")
         (len (string-width checkbox)))
    (goto-char begin)
    (while (and (re-search-forward my-org-bullet-regexp end t)
                (not (looking-at "\\[.\\][ \t]+")))
      (replace-match (concat "\\1" checkbox) nil nil)
      (setq end (+ end len)))
    (goto-char begin)))

;;;###autoload
(defun my-org-delete-checkbox-from-bullet (begin end)
  (interactive "r")
  (unless mark-active
    (setq begin (line-beginning-position))
    (setq end (line-end-position)))
  (let ((len (string-width "[ ] ")))
    (goto-char begin)
    (while (re-search-forward my-org-bullet-with-checkbox-regexp end t)
      (replace-match "\\1" nil nil)
      (setq end (- end len)))
    (goto-char begin)))

;;;###autoload
(defun my-org-insert-bullet-and-checkbox (begin end)
  (interactive "r")
  (unless mark-active
    (setq begin (line-beginning-position))
    (setq end (line-end-position)))
  (let* ((bullet " - ")
         (checkbox "[ ] ")
         (blen (string-width bullet))
         (clen (string-width checkbox)))
    (goto-char begin)
    (while (and (re-search-forward (concat "\\(^[ \t]*\\)") end t)
                (not (looking-at "[-\\+\\*][ \t]\\|[a-z0-9A-Z]*[\\.)][ \t]"))
                (not (equal (point) end)))
      (replace-match (concat "\\1" bullet checkbox) nil nil)
      (setq end (+ end blen clen)))
    (goto-char begin)))

;;;###autoload
(defun my-org-delete-bullet-and-checkbox (begin end)
  (interactive "r")
  (unless mark-active
    (setq begin (line-beginning-position))
    (setq end (line-end-position)))
  (goto-char begin)
  (while (re-search-forward my-org-bullet-with-checkbox-regexp end t)
    (let ((len (- (match-end 0) (match-beginning 0))))
      (replace-match "" nil nil)
      (setq end (- end len))))
  (goto-char begin))

;;;###autoload
(defun my-cycle-bullet-at-heading (arg)
  "Add a bullet of \" - \" if the line is NOT a bullet line."
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (let ((bullet "- ")
          (point-at-eol (point-at-eol)))
      (cond
       ((re-search-forward
         my-org-bullet-with-checkbox-regexp point-at-eol t)
        (replace-match (if arg "" "\\1") nil nil))
       ((re-search-forward
         "\\(^[ \t]*[-\\+\\*][ \t]\\|^[ \t]*[a-z0-9A-Z]*[\\.)][ \t]\\)"
         point-at-eol t)
        (replace-match (if arg "" (concat "\\1[ ] ")) nil nil))
       ((re-search-forward
         (concat "\\(^[ \t]*\\)") point-at-eol t)
        (replace-match (concat "\\1 " bullet) nil nil))
       (t nil)))))

;;;###autoload
(defun my-replace-punctuation-to-normal ()
  (interactive)
  (my-replace-punctuation 'normal))

;;;###autoload
(defun my-replace-punctuation-to-scientific ()
  (interactive)
  (my-replace-punctuation 'scientific))

(defun my-replace-punctuation (to)
  (let ((pos (point))
        (source (cond ((eq to 'normal) "\\(，\\)\\|\\(．\\)")
                      ((eq to 'scientific) "\\(、\\)\\|\\(。\\)"))))
    (if (not source)
        (error "Target punctuation is wrong")
      (goto-char (point-min))
      (while (re-search-forward source nil :noerror)
        (let ((w (match-string-no-properties 0)))
          (cond ((equal w "，") (replace-match "、"))
                ((equal w "．") (replace-match "。"))
                ((equal w "、") (replace-match "，"))
                ((equal w "。") (replace-match "．")))))
      (goto-char pos))))

(defvar my-garbage-collect-height max-mini-window-height)
(defun my-garbage-collect-activate ()
  (setq max-mini-window-height 16)
  (add-hook 'pre-command-hook #'my-garbage-collect-deactivate))
(defun my-garbage-collect-deactivate ()
  (setq max-mini-window-height my-garbage-collect-height)
  (remove-hook 'pre-command-hook #'my-garbage-collect-deactivate))
(defun my-garbage-collect ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (my-garbage-collect-activate)
  (message
   (concat
    (format "\n%-12s\t%-6s + %-6s = %s\n" "type" "used" "free" "total")
    (make-string (frame-width) ?-)
    (cl-loop
     for (type size used free) in (garbage-collect)
     for used1 = (* used size)
     for free1 = (* (or free 0) size)
     for total = (file-size-human-readable (+ used1 free1))
     for used2 = (file-size-human-readable used1)
     for free2 = (file-size-human-readable free1)
     concat
     (format "\n%-12s\t%-6s + %-6s = %s" type used2 free2 total)))))

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
;;;###autoload
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
  (let ((tl timer-list)
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

;; (defun insert-formatted-current-date (arg)
;;   "Insert a timestamp at the cursor position. C-u will add [] brackets."
;;   (interactive "p")
;;   (cl-case
;;       (4 (if (equal major-mode 'org-mode)
;;              (org-time-stamp-inactive)
;;            (insert (format-time-string "[%Y-%m-%d]"))))
;;     (t (insert (format-time-string "%Y-%m-%d")))))

(defun insert-formatted-current-date ()
  "Insert a timestamp at the cursor position."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun insert-formatted-current-time ()
  (interactive)
  (insert (format-time-string "%H:%M")))

(defun insert-formatted-signature ()
  (interactive)
  (insert (concat (format-time-string "%Y-%m-%d") "  " user-full-name
                  "  <" user-mail-address ">")))

(global-set-key (kbd "C-c 0") 'insert-formatted-current-date)
(global-set-key (kbd "C-c 9") 'insert-formatted-current-time)

;;;###autoload
(defun org2dokuwiki-cp-kill-ring ()
  "Convert the current org-file to dokuwiki text, and copy it to kill-ring."
  (interactive)
  (when (eq major-mode 'org-mode)
    (cond (buffer-file-name
           (kill-new
            (shell-command-to-string
             (concat "cat " buffer-file-name "| perl "
                     (expand-file-name "~/Dropbox/scripts/org2dokuwiki.pl"))))
           (minibuffer-message "Copying %s ... done" buffer-file-name))
          (t (message "There is NOT such a file.")))))

;;;###autoload
(defun my-window-resizer ()
  "Control separated window size and position.
   Type {j,k,l,m} to adjust windows size."
  (interactive)
  (let (
;;        (window-obj (selected-window))
;;        (current-width (window-width))
;;        (current-height (window-height))
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
