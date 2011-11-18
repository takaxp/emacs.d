;;;; Configuration for org-mode
;;;;                                       Last Update: 2011-11-14@14:07
;;;;                                       Takaaki ISHIKAWA  <takaxp@ieee.org>

(message "* --[ Loading an init file, takaxp-org-mode.el ] --")

(require 'org-install)
(require 'org-extension nil t)
(require 'org-habit)
(require 'org-mobile)
(require 'org-tree-slide nil t)
;(autoload 'tree-slide-play "org-tree-slide" "Start to play slide" t nil)

(setq auto-mode-alist
      (cons (cons "\\.org$" 'org-mode) auto-mode-alist))

;; contribution を使う
;(setq load-path (append '("~/devel/taka/org-mode/contrib/lisp") load-path))

;; Set checksum program path for windows
(when (eq window-system 'w32)
  (setq org-mobile-checksum-binary "~/env/do/cksum.exe"))

;; org ファイルの集中管理
(setq org-directory "~/Dropbox/org/")

;; Set default table export format
(setq org-table-export-default-format "orgtbl-to-csv")

;; Toggle inline images display at startup
(setq org-startup-with-inline-images t)

;;; dvipng
(setq org-export-with-LaTeX-fragments t)

;; orgバッファ内の全ての動的ブロックを保存直前に変更する
(add-hook 'before-save-hook 'org-update-all-dblocks)

;; アーカイブファイルの名称を指定
(setq org-archive-location "%s_archive::")

;; タイムスタンプによるログ収集設定
(setq org-log-done t) ; t ではなく，'(done), '(state) を指定できる

;; ログをドロアーに入れる
(setq org-log-into-drawer t)

;; タイマーの音
; (lsetq org-clock-sound "");

;;; Org-Agenda ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the view span as day in an agenda view, the default is week
(setq org-agenda-span 'day)
;; アジェンダに警告を表示する期間
(setq org-deadline-warning-days 7)
;; アジェンダビューでFOLLOWを設定
;(setq org-agenda-start-with-follow-mode t)
;;; Customized Time Grid
(setq org-agenda-time-grid
      '((daily today require-timed)
	"----------------"
	(800 1000 1200 1400 1600 1800 2000 2200 2400 2600)))
;;; Org files for creating agenda
;(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
; '(org-agenda-files '("~/Dropbox/org/next.org" "~/Dropbox/org/today.org")))
; "~/Dropbox/org/note.org" "~/Dropbox/org/stack.org" "~/Dropbox/org/support.org"

(setq org-agenda-files
      '("~/Dropbox/org/next.org" "~/Dropbox/org/today.org"))

;; アジェンダ作成対象（指定しないとagendaが生成されない）
;; ここを間違うと，MobileOrg, iCal export もうまくいかない
;(setq org-agenda-files (file-expand-wildcards (concat org-directory "*.org")))

;;; iCal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; アジェンダの iCal エクスポートファイル
;; ~/Dropbox/Public は第三者に探索される可能性があるので要注意
;; (setq org-combined-agenda-icalendar-file "~/Dropbox/Public/orgAgenda.ics")
;; iCal の説明文
(setq org-icalendar-combined-description "OrgModeのスケジュール出力")
;; カレンダーに適切なタイムゾーンを設定する（google 用には nil が必要）
(setq org-icalendar-timezone "Asia/Tokyo")
;;; エクスポート後に，AppleScript で新しいカレンダーをリロードさせる
;(add-hook 'org-after-save-iCalendar-file-hook
;	  (lambda ()
;	    (shell-command
;	     "osascript -e 'tell application \"iCal\" to reload calendars'")))
;; DONE になった TODO はアジェンダから除外する
(setq org-icalendar-include-todo t)
;; （通常は，<>--<> で区間付き予定をつくる．非改行入力で日付がNoteに入らない）
(setq org-icalendar-use-scheduled '(event-if-todo))
;;; DL 付きで終日予定にする：締め切り日（スタンプで時間を指定しないこと）
(setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo))

;;; Org-capture ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2010-06-13 の形式では，タグとして認識されない
(defun get-current-date-tags () (format-time-string "%Y%m%d"))
(setq org-default-notes-file (concat org-directory "next.org"))
(defvar org-capture-words-notes-file (concat org-directory "words.org"))
(defvar org-capture-notes-file (concat org-directory "note.org"))
(defvar org-capture-research-file (concat org-directory "research.org"))
(defvar org-capture-buffer-file (concat org-directory "buffer.org"))
(defvar org-capture-today-file (concat org-directory "today.org"))

; see org.pdf:p73
(setq org-capture-templates
 `(("t" "TODO 項目を INBOX に貼り付ける" entry (file+headline nil "INBOX")
    "** TODO %?\n\t")
   ("l" "本日のチェックリスト" entry
    (file+headline ,org-capture-today-file "Today")
    "** FOCUS 本日のチェックリスト %T\n（起床時間の記録）[[http://www.hayaoki-seikatsu.com/users/takaxp/][早起き日記]] \n（朝食）\n  - [ ] %?\n（昼食）\n（帰宅／夕食）\n----\n（研究速報）\n  - [ ] \n")
   ("i" "アイディアを書き込む" entry (file+headline nil "INBOX")
    "** %?\n  - \n\t%U")
   ("b" "Bug タグ付きの TODO 項目を貼り付ける" entry (file+headline nil "INBOX")
    "** TODO %? :bug:\n %i\n %a %t")
   ("w" ,(concat "英単語を " org-capture-words-notes-file " に書き込む") entry
    (file+headline ,org-capture-words-notes-file "WORDS")
    "** %? :%(get-current-date-tags):\n「」\n  - ")
   ("g" ,(concat "英語ノートを " org-capture-words-notes-file " に書き込む")
    entry (file+headline ,org-capture-words-notes-file "GRAMMER")
    "** %? :%(get-current-date-tags):\n\n%U")
   ("c" "時間付きエントリー" entry (file+headline nil "INBOX")
    "** %? %T--\n")
   ("n" "ノートとしてINBOXに貼り付ける" entry (file+headline nil "INBOX")
    "** note %?\n\t%U")
   ("d" "「ドラッカー365の金言」をノートする" entry
    (file+headline ,org-capture-notes-file "The Daily Drucker")
    "** 「%?」\nDrucker) \n  - \n  - \nACTION POINT:\n  - \nQUESTION:\n  - \n")
   ("r" ,(concat "研究ノートを " org-capture-research-file " に書き込む")
    entry (file+headline ,org-capture-research-file "Survey")
    "** note %?\n# \n  - \n\t%U")
   ("`" ,(concat "ノートをバッファ "org-capture-buffer-file " に書き込む")
    entry (file+headline ,org-capture-buffer-file "Buffer")
    "** %(get-random-string 16) %U\n\n%?\n\n----")
))


;;; appt.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; アラーム表示を有効にする
(appt-activate 1)
;; window を フレーム内に表示する
(setq appt-display-format 'window)
;; window を継続表示する時間[s]
(setq appt-display-duration 3)
;; ビープ音の有無
(setq appt-audible t)
;; 何分前から警告表示を開始するか[m]
(setq appt-message-warning-time 3)
;; モードラインにアラームを表示する
(setq appt-display-mode-line t)
;; org-agenda の内容をアラームに登録する
(org-agenda-to-appt t '((headline "TODO")))
;; 保存時にアラームを登録
(add-hook 'org-mode-hook
	  (lambda() (add-hook 'before-save-hook
			      'org-agenda-to-appt t '((headline "TODO")))))

;;; org-refile
(setq org-refile-targets
      (quote (("next.org" :level . 1)
	      ("sleep.org" :level . 1))))


;;; Pomodoro ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://orgmode.org/worg/org-gtd-etc.html
(add-to-list 'org-modules 'org-timer)
(setq org-timer-default-timer 25)
(add-hook 'org-clock-in-hook
	  '(lamda ()
		  (if (not org-timer-current-timer)
		      (org-timer-set-timer '(16)))))
(setq growl-pomodoro-default-task-name "doing the task")
(setq growl-pomodoro-task-name 'growl-pomodoro-default-task-name)

(defun set-growl-pomodoro-task-name ()
  (interactive "P")
  (setq growl-pomodoro-task-name
	(read-from-minibuffer "Task Name: " growl-pomodoro-default-task-name)))
(add-hook 'org-timer-set-hook 'set-growl-pomodoro-task-name)

(defun growl-pomodoro-timer ()
  (interactive)
  (shell-command-to-string
   (concat "growlnotify -s -a Emacs -t \"++ Pomodoro ++\" -m \""
	   "The end of " growl-pomodoro-task-name "!\""))
  (shell-command-to-string
;   (concat "say The end of " growl-pomodoro-task-name)
   (concat "say -v Kyoko " growl-pomodoro-task-name)
   ))
(add-hook 'org-timer-done-hook 'growl-pomodoro-timer)


;;; MobileOrg ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://orgmode.org/manual/Setting-up-the-staging-area.html
;(setq org-mobile-files '("~/Dropbox/org/next.org" "1.org" "2.org"))
(setq org-mobile-files '("~/Dropbox/org/next.org"))
;(setq org-mobile-force-id-on-agenda-items nil)

;; Set a file to capture data from iOS devices
(setq org-mobile-inbox-for-pull (concat org-directory "captured.org"))

; Upload location stored org files (index.org will be created)
(setq org-mobile-directory "~/Dropbox/MobileOrg/")

;;; Menu to push or pull org files using MobileOrg
(defun org-mobile-sync ()
  (interactive)
  (let
      (org-mobile-sync-type
       (read-from-minibuffer "How do you sync the org files? (pull or push) "))
    (message "%s" org-mobile-sync-type)
    (cond
     ((string= "pull" org-mobile-sync-type)(org-mobile-pull))
     ((string= "push" org-mobile-sync-type)(org-mobile-push)))))
;  (if (yes-or-no-p "How do you sync the org files? ")


;;; Speed commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-use-speed-commands t)
(setq org-speed-commands-user
      (quote (("n" . show-next-org)
	      ("t" . show-today-org))))
(defun show-next-org () (show-org-buffer "next.org"))
(defun show-today-org () (show-org-buffer "today.org"))


;;; Face ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-font-lock-mode 1)
(add-hook 'org-mode-hook 'turn-on-font-lock)
;; 起動時にすべてのツリーを閉じておく
(setq org-startup-truncated nil)
;; サブツリー以下の * を略式表示する
(setq org-hide-leading-stars t)
;; Color setting for TODO keywords
(setq org-todo-keyword-faces
      '(("note"   :foreground "#9966FF")
	("check"  :foreground "#CC00FF")
	("sleep"  :foreground "#6699FF")
	("NOTICE" :foreground "#FFFFFF" :background "#FF0000")
	("FOCUS"  :foreground "#FF0000" :background "#FFCC66")
	("TIME"   :foreground "#FF9900")))
;; Color for priorities
;; (setq org-priority-faces
;;       '(("?A" :foreground "#E01B4C" :background "#FFFFFF" :weight bold)
;; 	("?B" :foreground "#1739BF" :background "#FFFFFF" :weight bold)
;; 	("?C" :foreground "#575757" :background "#FFFFFF" :weight bold)))
;; Color setting for Tags
(setq org-tag-faces
      '(("Achievement" :foreground "#66CC66")
	("Background"  :foreground "#66CC99")
	("Chore"       :foreground "#6699CC")
	("Domestic"    :foreground "#6666CC")
	("Ongoing"     :foreground "#CC6666") ; for non scheduled / reminder
	("Repeat"      :foreground "#CC9999") ; for interval tasks
	("Mag"         :foreground "#9966CC")
	("buy"         :foreground "#9966CC")
	("Implements"  :foreground "#CC9999" :weight bold)
	("Editing"     :foreground "#CC9999" :weight bold)
	("Duty"        :foreground "#CC9999" :weight bold)
	("Survey"      :foreground "#CC9999" :weight bold)
	("Home"        :foreground "#CC9999" :weight bold)
	("Open"        :foreground "#CC9999" :weight bold)
	("Test"        :foreground "#FF0000" :weight bold)
	("DEBUG"       :foreground "#FFFFFF" :background "#9966CC")
	("EVENT"       :foreground "#FFFFFF" :background "#9966CC")
	("Thinking"    :foreground "#FFFFFF" :background "#96A9FF")
	("Schedule"    :foreground "#FFFFFF" :background "#FF7D7D")
	("OUTPUT"      :foreground "#FFFFFF" :background "#66CC99");;#5BDF8D
	("waiting"     :foreground "#008500")	
	("Log"         :foreground "#008500")))

;;; Customized TODO keywords ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq org-todo-keywords '("TODO" "DONE")
;;      org-todo-interpretation 'sequence)
;;(setq org-todo-keywords '("A" "B" "C" "Done")
;;      org-todo-interpretation 'type)
;; TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "FOCUS(f)" "|" "DONE(d)")
	(sequence "check(c)" "|" "PASS(p)")
	(sequence "TIME(T)" "|")
	(sequence "|" "note(n)")
	(sequence "NOTICE" "|" "READ(r)")
	(sequence "sleep(s)" "|")
	))
;; Global counting of TODO items
(setq org-hierarchical-todo-statistics nil)
;; Global counting of checked TODO items
(setq org-hierarchical-checkbox-statistics nil)


;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; block-update-time
(defun org-dblock-write:block-update-time (params)
  (let ((fmt (or (plist-get params :format) "%Y-%m-%d")))
    (insert "" (format-time-string fmt (current-time)))))

(defun reload-ical-export ()
  "Export org files as an iCal format file"
  (interactive)
  (when (string= major-mode 'org-mode)
      (org-export-icalendar-combine-agenda-files)))

;;; すべてのチェックボックスの cookies を更新する
(defun do-org-update-statistics-cookies ()
  (interactive)
  (org-update-statistics-cookies 'all))

;; org-tree-slide
(setq alarm-table "~/Dropbox/org/today.org")
(run-at-time "00:00" nil 'set-alarms-from-file alarm-table)

;; Keybindings for org-mode
(define-key org-mode-map (kbd "C-c 1") 'reload-ical-export)
(define-key org-mode-map (kbd "C-c 2") 'do-org-update-statistics-cookies)
(define-key org-mode-map (kbd "C-c m") 'org-mobile-sync)
(define-key org-mode-map (kbd "<f5>") 'org-narrow-to-subtree)
(define-key org-mode-map (kbd "<S-f5>") 'widen)

;; 起動時にモバイルで環境で編集したファイルを読み込む
(message "%s" "MobileOrg sync ... [pull]")
(org-mobile-pull) ;; need org-mode

;; Rich calendar
(autoload 'cfw:open-org-calendar  "calfw-org" "Rich calendar for org-mode" t)

(provide 'takaxp-org-mode)

;;; Tex export (org-mode -> tex with beamer class) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode の latex エクスポート関数をオーバーライド
;; (setq org-export-latex-classes
;;   '(("article"
;;      "\\documentclass[11pt]{article}
;; \\usepackage[AUTO]{inputenc}
;; \\usepackage[T1]{fontenc}
;; \\usepackage{graphicx}
;; \\usepackage{longtable}
;; \\usepackage{float}
;; \\usepackage{wrapfig}
;; \\usepackage{soul}
;; \\usepackage{amssymb}
;; \\usepackage{hyperref}"
;;      ("\\section{%s}" . "\\section*{%s}")
;;      ("\\subsection{%s}" . "\\subsection*{%s}")
;;      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;      ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
;;     ("report"
;;      "\\documentclass[11pt]{report}
;; \\usepackage[AUTO]{inputenc}
;; \\usepackage[T1]{fontenc}
;; \\usepackage{graphicx}
;; \\usepackage{longtable}
;; \\usepackage{float}
;; \\usepackage{wrapfig}
;; \\usepackage{soul}
;; \\usepackage{amssymb}
;; \\usepackage{hyperref}"
;;      ("\\part{%s}" . "\\part*{%s}")
;;      ("\\chapter{%s}" . "\\chapter*{%s}")
;;      ("\\section{%s}" . "\\section*{%s}")
;;      ("\\subsection{%s}" . "\\subsection*{%s}")
;;      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
;;     ("book"
;;      "\\documentclass[11pt]{book}
;; \\usepackage[AUTO]{inputenc}
;; \\usepackage[T1]{fontenc}
;; \\usepackage{graphicx}
;; \\usepackage{longtable}
;; \\usepackage{float}
;; \\usepackage{wrapfig}
;; \\usepackage{soul}
;; \\usepackage{amssymb}
;; \\usepackage{hyperref}"
;;      ("\\part{%s}" . "\\part*{%s}")
;;      ("\\chapter{%s}" . "\\chapter*{%s}")
;;      ("\\section{%s}" . "\\section*{%s}")
;;      ("\\subsection{%s}" . "\\subsection*{%s}")
;;      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
;;     ("beamer"
;;      "\\documentclass{beamer}
;; \\usepackage[AUTO]{inputenc}
;; \\usepackage{graphicx}
;; \\usepackage{longtable}
;; \\usepackage{float}
;; \\usepackage{wrapfig}
;; \\usepackage{amssymb}
;; \\usepackage{hyperref}"
;;      org-beamer-sectioning)))
