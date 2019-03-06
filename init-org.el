;; -*- lexical-binding: t -*-

(with-eval-after-load "org"
  (setq mode-line-modes
        (mapcar
         (lambda (entry)
           (if (equal entry "%n")
               '(:eval (if (buffer-narrowed-p) " N" ""))
             entry))
         mode-line-modes)))

(when (autoload-if-found
       '(org-mode)
       "org" "Org Mode" t)

  ;; テキストファイルを Org Mode で開きます．
  (push '("\\.txt$" . org-mode) auto-mode-alist)

  ;; Font lock を使う
  (add-hook 'org-mode-hook #'turn-on-font-lock)

  ;; ホームポジション的な Orgファイルを一発で開きます．
  (global-set-key (kbd "C-M-o")
                  (lambda () (interactive)
                    (my-show-org-buffer "next.org")))

  (with-eval-after-load "postpone"
    (global-set-key (kbd "C-c r") 'org-capture)
    (global-set-key (kbd "C-c l") 'org-store-link)
    (global-set-key (kbd "C-c a") 'org-agenda))

  (with-eval-after-load "ox"
    (add-to-list 'org-modules 'ox-odt)
    (add-to-list 'org-modules 'ox-org))

  (with-eval-after-load "org"

    ;; 関連モジュールの読み込み
    (add-to-list 'org-modules 'org-habit)
    (require 'org-mobile nil t)
    (require 'org-eldoc nil t) ;; org-eldoc を読み込む

    ;; モジュールの追加
    (add-to-list 'org-modules 'org-id)
    (when (version< "9.1.4" (org-version))
      (add-to-list 'org-modules 'org-tempo))

    ;; 不必要なモジュールの読み込みを停止する
    (delq 'org-gnus org-modules)
    ;; (setq org-modules (delete 'org-bibtex org-modules))

    ;; org ファイルの集中管理
    (setq org-directory "~/Dropbox/org/")

    ;; アーカイブファイルの名称を指定
    (setq org-archive-location "%s_archive::")

    ;; タイムスタンプによるログ収集設定
    (setq org-log-done 'time) ; 'time 以外に，'(done), '(state) を指定できる

    ;; ログをドロアーに入れる
    (setq org-log-into-drawer t)

    ;; Set checksum program path for windows
    (when (eq window-system 'w32)
      (setq org-mobile-checksum-binary "~/Dropbox/do/cksum.exe"))

    ;; Set default table export format
    (setq org-table-export-default-format "orgtbl-to-csv")

    ;; Toggle inline images display at startup
    (setq org-startup-with-inline-images t)

    ;; dvipng
    (setq org-export-with-LaTeX-fragments t)

    ;; 数式をハイライト
    (setq org-highlight-latex-and-related '(latex entities))

    ;; orgバッファ内の全ての動的ブロックを保存直前に変更する
    ;; (add-hook 'before-save-hook #'org-update-all-dblocks)

    ;; アンダースコアをエクスポートしない（_{}で明示的に表現できる）
    (setq org-export-with-sub-superscripts nil)

    ;; #+options: \n:t と同じ
    (setq org-export-preserve-breaks t)

    ;; タイマーの音
    ;; (lsetq org-clock-sound "");

    ;; org-clock の計測時間をモードラインではなくタイトルに表示する
    (setq org-clock-clocked-in-display 'frame-title)

    ;; 1分未満は記録しない
    (setq org-clock-out-remove-zero-time-clocks t)

    ;; 非表示状態の領域への書き込みを防ぐ
    ;; "Editing in invisible areas is prohibited, make them visible first"
    (setq org-catch-invisible-edits 'show-and-error)
    (defun ad:org-return (f &optional arg)
      "An extension for checking invisible editing when you hit the enter."
      (interactive "P")
      (org-check-before-invisible-edit 'insert)
      (apply f arg))
    (advice-add 'org-return :around #'ad:org-return)

    ;; - を優先．親のブリッツ表示を継承させない
    (setq org-list-demote-modify-bullet
          '(("+" . "-")
            ("*" . "-")
            ("1." . "-")
            ("1)" . "-")
            ("A)" . "-")
            ("B)" . "-")
            ("a)" . "-")
            ("b)" . "-")
            ("A." . "-")
            ("B." . "-")
            ("a." . "-")
            ("b." . "-")))

    ;; 完了したタスクの配色を変える
    ;; https://fuco1.github.io/2017-05-25-Fontify-done-checkbox-items-in-org-mode.html
    (font-lock-add-keywords
     'org-mode
     `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
        1 'org-headline-done prepend))
     'append)

    (defun my-do-org-update-staistics-cookies ()
      (interactive)
      (message "Update statistics...")
      (do-org-update-statistics-cookies)
      (message "Update statistics...done"))
    (define-key org-mode-map (kbd "C-c f 2")
      'my-do-org-update-staistics-cookies)

    ;; C-c & が yasnippet にオーバーライドされているのを張り替える
    (define-key org-mode-map (kbd "C-c 4") 'org-mark-ring-goto)

    ;; (org-transpose-element) が割り当てられているので取り返す．
    (org-defkey org-mode-map "\C-\M-t" 'beginning-of-buffer))

  (with-eval-after-load "org-clock"
    (defun my-org-clock-out-and-save-when-exit ()
      "Save buffers and stop clocking when kill emacs."
      (when (org-clocking-p)
        (org-clock-out)
        (save-some-buffers t)))
    (add-hook 'kill-emacs-hook #'my-org-clock-out-and-save-when-exit)))

;; ~/Dropbox/Public は第三者に探索される可能性があるので要注意
;; default = ~/org.ics
;; C-c C-e i org-export-icalendar-this-file
;; C-c C-e I org-export-icalendar-all-agenda-files
;; C-c C-e c org-export-icalendar-all-combine-agenda-files
(when (autoload-if-found
       '(my-ox-icalendar my-ox-icalendar-cleanup)
       "ox-icalendar" nil t)

  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c f 1") 'my-ox-icalendar))

  (with-eval-after-load "ox-icalendar"
     (defvar org-ical-file-in-orz-server nil) ;; see private.el

    ;; 生成するカレンダーファイルを指定
    ;; 以下の設定では，このファイルを一時ファイルとして使う（削除する）
    (setq org-icalendar-combined-agenda-file "~/Desktop/org-ical.ics")

    ;; iCal の説明文
    (setq org-icalendar-combined-description "OrgModeのスケジュール出力")

    ;; カレンダーに適切なタイムゾーンを設定する（google 用には nil が必要）
    (setq org-icalendar-timezone "Asia/Tokyo")

    ;; DONE になった TODO はアジェンダから除外する
    (setq org-icalendar-include-todo t)

    ;; （通常は，<>--<> で区間付き予定をつくる．非改行入力で日付がNoteに入らない）
    (setq org-icalendar-use-scheduled '(event-if-todo))

    ;; DL 付きで終日予定にする：締め切り日（スタンプで時間を指定しないこと）
    ;; (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo))
    (setq org-icalendar-use-deadline '(event-if-todo))

    (defun my-ox-icalendar ()
      (interactive)
      (let ((message-log-max nil)
            (temp-agenda-files org-agenda-files))
        (setq org-agenda-files '("~/Dropbox/org/org-ical.org"))
        ;; org-icalendar-export-to-ics を使うとクリップボードが荒れる
        (org-icalendar-combine-agenda-files)
        (setq org-agenda-files temp-agenda-files)
        ;; Dropbox/Public のフォルダに公開する
        ;;           (shell-command
        ;;            (concat "cp " org-icalendar-combined-agenda-file " "
        ;;                    org-icalendar-dropbox-agenda-file))

        ;; 自サーバにアップロード
        (message "Uploading...")
        (if (eq 0 (shell-command
                   (concat "scp -o ConnectTimeout=5 "
                           org-icalendar-combined-agenda-file " "
                           org-ical-file-in-orz-server)))
            (message "Uploading...done")
          (message "Uploading...miss!"))
        (my-ox-icalendar-cleanup)))

    (defun my-ox-icalendar-cleanup ()
      (interactive)
      (when (file-exists-p
             (expand-file-name org-icalendar-combined-agenda-file))
        (shell-command-to-string
         (concat "rm -rf " org-icalendar-combined-agenda-file))))))

(with-eval-after-load "org"
  (setq org-use-speed-commands t)

  (add-to-list 'org-speed-commands-user '("d" org-todo "DONE"))
  (add-to-list 'org-speed-commands-user '("S" call-interactively 'widen))
  (add-to-list 'org-speed-commands-user
               '("D" my-org-todo-complete-no-repeat "DONE"))
  ;; (add-to-list 'org-speed-commands-user '("N" org-shiftmetadown))
  ;; (add-to-list 'org-speed-commands-user '("P" org-shiftmetaup))
  (add-to-list 'org-speed-commands-user '("." my-org-deadline-today))
  (add-to-list 'org-speed-commands-user '("!" my-org-set-created-property))
  (add-to-list 'org-speed-commands-user
               '("$" call-interactively 'org-archive-subtree))

  ;; 周期タクスを終了させます．
  (defun my-org-todo-complete-no-repeat (&optional ARG)
    (interactive "P")
    (when (org-get-repeat)
      (org-cancel-repeater))
    (org-todo ARG))

  ;; 締切を今日にする =FIXME=
  (defun my-org-deadline-today ()
    (when (org-entry-is-todo-p)
      (let ((date (org-entry-get (point) "DEADLINE"))
            (today (format-time-string "%F")))
        (org-deadline 'deadline
                      (if date
                          (format "<%s%s"
                                  today
                                  (substring date 11 (string-width date)))
                        (format "<%s>" today)))))))

(with-eval-after-load "org"
  ;; Font lock を使う
  (global-font-lock-mode 1)

  ;; ウィンドウの端で折り返す
  (setq org-startup-truncated nil)

  ;; サブツリー以下の * を略式表示する
  (setq org-hide-leading-stars t)

  ;; Color setting for TODO keywords
  ;; Color for priorities
  ;; (setq org-priority-faces
  ;;  '(("?A" :foreground "#E01B4C" :background "#FFFFFF" :weight bold)
  ;;    ("?B" :foreground "#1739BF" :background "#FFFFFF" :weight bold)
  ;;    ("?C" :foreground "#575757" :background "#FFFFFF" :weight bold)))
  ;; Color setting for Tags

  ;; #CC3333
  (setq org-todo-keyword-faces
        '(("FOCUS"    :foreground "#FF0000" :background "#FFCC66")
          ("BUG"      :foreground "#FF0000" :background "#FFCC66")
          ("CHECK"    :foreground "#FF9900" :background "#FFF0F0" :underline t)
          ("ICAL"     :foreground "#33CC66")
          ("APPROVED" :foreground "#66CC66")
          ("QUESTION" :foreground "#FF0000")
          ("WAIT"     :foreground "#CCCCCC" :background "#666666")
          ("MAIL"     :foreground "#CC3300" :background "#FFEE99")
          ("PLAN"     :foreground "#FF6600")
          ("PLAN2"    :foreground "#FFFFFF" :background "#FF6600")
          ("REV1"     :foreground "#3366FF")
          ("REV2"     :foreground "#3366FF" :background "#99CCFF")
          ("REV3"     :foreground "#FFFFFF" :background "#3366FF")
          ("SLEEP"    :foreground "#9999CC")))

  ;; (:foreground "#0000FF" :bold t)     ; default. do NOT put this bottom
  (setq org-tag-faces
        '(("Achievement" :foreground "#66CC66")
          ("Bug"         :foreground "#FF0000")
          ("Report"      :foreground "#66CC66")
          ("Background"  :foreground "#66CC99")
          ("Chore"       :foreground "#6699CC")
          ("read"        :foreground "#6666CC")
          ("book"        :foreground "#6666CC")
          ("Doing"       :foreground "#FF0000")
          ("Draft"       :foreground "#9933CC") ;; Draft(r1,r2,r3)->Review(1,2)
          ("Review"      :foreground "#6633CC")
          ("Revisit"     :foreground "#6633CC")
          ("Redmine"     :foreground "#CC6666")
          ("Ongoing"     :foreground "#CC6666") ; for non scheduled/reminder
          ("Repeat"      :foreground "#CC9999") ; for interval tasks
          ("Mag"         :foreground "#9966CC")
          ("buy"         :foreground "#9966CC")
          ("pay"         :foreground "#CC6699")
          ("try"         :foreground "#FF3366")
          ("secret"      :foreground "#FF0000")
          ("emacs"       :foreground "#6633CC")
          ("note"        :foreground "#6633CC")
          ("print"       :foreground "#6633CC")
          ("study"       :foreground "#6666CC")
          ("Implements"  :foreground "#CC9999" :weight bold)
          ("Coding"      :foreground "#CC9999")
          ("Editing"     :foreground "#CC9999" :weight bold)
          ("work"        :foreground "#CC9999" :weight bold)
          ("Survey"      :foreground "#CC9999" :weight bold)
          ("Home"        :foreground "#CC9999" :weight bold)
          ("Open"        :foreground "#CC9999" :weight bold)
          ("Blog"        :foreground "#9966CC")
          ("story"       :foreground "#FF7D7D")
          ("Test"        :foreground "#FF0000" :weight bold)
          ("attach"      :foreground "#FF0000")
          ("drill"       :foreground "#66BB66" :underline t)
          ("DEBUG"       :foreground "#FFFFFF" :background "#9966CC")
          ("EVENT"       :foreground "#FFFFFF" :background "#9966CC")
          ("Thinking"    :foreground "#FFFFFF" :background "#96A9FF")
          ("Schedule"    :foreground "#FFFFFF" :background "#FF7D7D")
          ("INPUT"       :foreground "#FFFFFF" :background "#CC6666")
          ("OUTPUT"      :foreground "#FFFFFF" :background "#66CC99")
          ("CYCLE"       :foreground "#FFFFFF" :background "#6699CC")
          ("weekend"     :foreground "#FFFFFF" :background "#CC6666")
          ("Log"         :foreground "#008500"))))

(with-eval-after-load "org"
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PLAN(p)" "PLAN2(P)" "|" "DONE(d)")
          (sequence "FOCUS(f)" "CHECK(C)" "ICAL(c)"  "|" "DONE(d)")
          (sequence "WAIT(w)" "SLEEP(s)" "QUESTION(q)" "|" "DONE(d)")
          (sequence "REV1(1)" "REV2(2)" "REV3(3)" "|" "APPROVED(a@/!)")))

  ;; Global counting of TODO items
  (setq org-hierarchical-todo-statistics nil)

  ;; Global counting of checked TODO items
  (setq org-hierarchical-checkbox-statistics nil)

  ;; block-update-time
  (defun org-dblock-write:block-update-time (params)
    (let ((fmt (or (plist-get params :format) "%Y-%m-%d")))
      (insert "" (format-time-string fmt (current-time)))))

  ;; すべてのチェックボックスの cookies を更新する
  (defun do-org-update-statistics-cookies ()
    (interactive)
    (org-update-statistics-cookies 'all)))

(with-eval-after-load "org"
  (setq org-image-actual-width '(256))
  (add-to-list 'image-file-name-extensions "jp2")
  ;; (add-to-list 'image-file-name-extensions "j2c")
  (add-to-list 'image-file-name-extensions "bmp")
  (add-to-list 'image-file-name-extensions "psd"))

(when (autoload-if-found
       '(org-mode)
       "org" nil t)

  (push '("[rR][eE][aA][dD][mM][eE]" . org-mode) auto-mode-alist))

(with-eval-after-load "org"
  (defun my-lowercase-org-keywords ()
    "Lower case Org keywords and block identifiers."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil)
            (count 0))
        (while (re-search-forward
                "\\(?1:#\\+[A-Z_]+\\(?:_[[:alpha:]]+\\)*\\)\\(?:[ :=~’”]\\|$\\)"
                nil :noerror)
          (setq count (1+ count))
          (let* ((prev (match-string-no-properties 1))
                 (new (downcase prev)))
            (replace-match new :fixedcase nil nil 1)
            (message "Updated(%d): %s => %s" count prev new)))
        (message "Lower-cased %d matches" count)))))

(with-eval-after-load "postpone"
  ;; Select from Preferences: { Funk | Glass | ... | Purr | Pop ... }
  (defvar ns-default-notification-sound "Pop")

  (defvar ns-alerter-command
    (executable-find "/Users/taka/Dropbox/bin/alerter")
    "Path to alerter command. see https://github.com/vjeantet/alerter")

  (defun my-desktop-notification (title message &optional sticky sound timeout)
    "Show a message by `alerter' command."
    (start-process
     "notification" "*notification*"
     ns-alerter-command
     "-title" title
     "-message" message
     "-sender" "org.gnu.Emacs"
     "-timeout" (format "%s" (if sticky 0 (or timeout 7)))
     "-sound" (or sound ns-default-notification-sound)))

  ;; eval (org-notify "hoge") to test this setting
  (defun my-desktop-notification-handler (message)
    (my-desktop-notification "Message from org-mode" message t))
  (with-eval-after-load "org"
    (when ns-alerter-command
      (setq org-show-notification-handler #'my-desktop-notification-handler))))

(unless noninteractive
  (with-eval-after-load "org"
    (let ((file "~/Dropbox/org/db/daily.org"))
      (when (and (file-exists-p file)
                 (require 'utility nil t))
        (my-set-alarms-from-file file) ;; init
        (add-hook 'after-save-hook #'my-update-alarms-from-file))))) ;; update

(when (autoload-if-found
       '(org-capture)
       "org-capture" nil t)

  (with-eval-after-load "org"
    ;; キャプチャ時に作成日時をプロパティに入れる
    ;; Thanks to https://emacs.stackexchange.com/questions/21291/add-created-timestamp-to-logbook
    (defvar my-org-created-property-name "CREATED"
      "The name of the org-mode property.
This user property stores the creation date of the entry")
    (defun my-org-set-created-property (&optional active NAME)
      "Set a property on the entry giving the creation time.

By default the property is called CREATED. If given the `NAME'
argument will be used instead. If the property already exists, it
will not be modified."
      (interactive)
      (let* ((created (or NAME my-org-created-property-name))
             (fmt (if active "<%s>" "[%s]"))
             (now (format fmt (format-time-string "%Y-%m-%d %a %H:%M")))
             (field (org-entry-get (point) created nil)))
        (unless (or field (equal "" field))
          (org-set-property created now)))))

  (with-eval-after-load "org-capture"
    (defun my-toggle-org-block-visibility ()
      "Testing..."
      (interactive)
	    (when (looking-at org-drawer-regexp)
	      (org-flag-drawer		; toggle block visibility
	       (not (get-char-property (match-end 0) 'invisible)))))

    (add-hook 'org-capture-before-finalize-hook #'my-org-set-created-property)

    ;; 2010-06-13 の形式では，タグとして認識されない
    (defun get-current-date-tags () (format-time-string "%Y%m%d"))
    (setq org-default-notes-file (concat org-directory "next.org"))
    (defvar org-capture-academic-file (concat org-directory "academic.org"))
    (defvar org-capture-ical-file (concat org-directory "org-ical.org"))
    (defvar org-capture-buffer-file (concat org-directory "db/buffer.org"))
    (defvar org-capture-notes-file (concat org-directory "db/note.org"))
    (defvar org-capture-english-file (concat org-directory "db/english.org"))
    (defvar org-capture-diary-file (concat org-directory "log/diary.org"))
    (defvar org-capture-article-file (concat org-directory "db/article.org"))
    (defvar org-capture-blog-file
      (concat org-directory "blog/entries/imadenale.org"))

    ;; see org.pdf:p73
    (setq org-capture-templates
          `(("t" "TODO 項目を INBOX に貼り付ける" entry
             (file+headline ,org-default-notes-file "INBOX") "** TODO %?\n\t")
            ("a" "記事リストにエントリー" entry
             (file+headline ,org-capture-article-file "INBOX")
             "** READ %?\n\t")
            ("c" "同期カレンダーにエントリー" entry
             (file+headline ,org-capture-ical-file "Scheduled")
             "** TODO %?\n\t")
            ("d" "Doingタグ付きのタスクをInboxに投げる" entry
             (file+headline ,org-default-notes-file "INBOX")
             "** TODO %? :Doing:\n  - \n")
            ("l" "本日のチェックリスト" entry
             (file+headline ,org-capture-diary-file "Today")
             "** FOCUS 本日のチェックリスト %T\n（起床時間の記録）[[http://www.hayaoki-seikatsu.com/users/takaxp/][早起き日記]] \n（朝食）\n  - [ ] %?\n（昼食）\n（帰宅／夕食）\n----\n（研究速報）\n  - [ ] \n")
            ("i" "アイディアを書き込む" entry (file+headline ,org-default-notes-file "INBOX")
             "** %?\n  - \n\t%U")
            ("b" "Create new post for imadenale blog" entry
             (file+headline ,org-capture-blog-file ,(format-time-string "%Y"))
             "** TODO \n:PROPERTIES:\n:EXPORT_FILE_NAME: %?\n:EXPORT_HUGO_TAGS: \n:EXPORT_HUGO_LASTMOD: \n:END:\n")
            ("B" "Create new post for imadenale blog (UUID)" entry
             (file+headline ,org-capture-blog-file ,(format-time-string "%Y"))
             "** TODO %?\n:PROPERTIES:\n:EXPORT_FILE_NAME: %(uuid-string)\n:EXPORT_HUGO_TAGS: \n:EXPORT_HUGO_LASTMOD: \n:END:\n")
            ;; ("b" "Bug タグ付きの TODO 項目を貼り付ける" entry
            ;;  (file+headline ,org-default-notes-file "INBOX")
            ;;  "** TODO %? :bug:\n %i\n %a %t")
            ("T" "時間付きエントリー" entry (file+headline ,org-default-notes-file "INBOX")
             "** %? %T--\n")
            ("n" "ノートとしてINBOXに貼り付ける" entry
             (file+headline ,org-default-notes-file "INBOX")
             "** %? :note:\n\t%U")
            ("D" "「ドラッカー365の金言」をノートする" entry
             (file+headline ,org-capture-notes-file "The Daily Drucker")
             "** 「%?」\nDrucker) \n  - \n  - \nACTION POINT:\n  - \nQUESTION:\n  - \n")
            ("r" ,(concat "研究ノートを " org-capture-academic-file
                          " に書き込む")
             entry (file+headline ,org-capture-academic-file "Survey")
             "** %? :note:\n# \n  - \n\t%U")
            ("`" ,(concat "ノートをバッファ " org-capture-buffer-file
                          " に書き込む")
             entry (file+headline ,org-capture-buffer-file "Buffers")
             "** %(get-random-string 16) %U\n\n%?\n\n----")
            ("w" ,(concat "英単語を " org-capture-english-file
                          " に書き込む") entry
                          (file+headline ,org-capture-english-file "WORDS")
                          "** %? :%(get-current-date-tags):\n「」\n  - ")
            ("g" ,(concat "英語ノートを " org-capture-english-file
                          " に書き込む")
             entry (file+headline ,org-capture-english-file "GRAMMER")
             "** %? :%(get-current-date-tags):\n\n%U")
            ))))

(with-eval-after-load "org-agenda"
  ;; アジェンダ作成対象（指定しないとagendaが生成されない）
  ;; ここを間違うと，MobileOrg, iCal export もうまくいかない
  (setq org-agenda-files
        '("~/Dropbox/org/org-ical.org" "~/Dropbox/org/next.org"
          "~/Dropbox/org/db/cooking.org" "~/Dropbox/org/minutes/wg1.org"
          "~/Dropbox/org/db/daily.org" "~/Dropbox/org/db/trigger.org"
          "~/Dropbox/org/tr/work.org" "~/Dropbox/org/academic.org"
          "~/Dropbox/org/org2ja.org"))

  ;; sorting strategy
  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up timestamp-up priority-down category-keep)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep)))

  ;; Set the view span as day in an agenda view, the default is week
  (setq org-agenda-span 'day)

  ;; アジェンダに警告を表示する期間
  (setq org-deadline-warning-days 0)

  ;; 時間幅が明示的に指定されない場合のデフォルト値（分指定）
  (setq org-agenda-default-appointment-duration 60)

  ;; アジェンダビューでFOLLOWを設定（自動的に別バッファに当該タスクを表示）
  (setq org-agenda-start-with-follow-mode t)

  ;; Customized Time Grid
  (setq org-agenda-time-grid ;; Format is changed from 9.1
        '((daily today require-timed)
          (0800 1000 1200 1400 1600 1800 2000 2200 2400)
          "......"
          "----------------"
          ))

  ;; (setq org-agenda-current-time-string "<  d('- ' ｲﾏｺｺ)")
  (setq org-agenda-current-time-string "< < < < < < < < < < < < < < < < ｲﾏｺｺ")
  (setq org-agenda-timegrid-use-ampm t)

  (with-eval-after-load "moom"
    (defvar my-org-tags-column org-tags-column)
    ;; Expand the frame width temporarily during org-agenda is activated.
    (defun my-agenda-frame-width ()
      (let ((width (floor (* 1.2 moom-frame-width-single))))
        (setq org-tags-column (- org-tags-column (- width 80)))
        ;; (org-align-tags t)
        (moom-change-frame-width width)))
    ;; (add-hook 'org-agenda-mode-hook #'my-agenda-frame-width)

    (defun ad:org-agenda--quit (&optional _bury)
      (setq org-tags-column my-org-tags-column)
      ;; (org-align-tags t)
      (moom-change-frame-width))
    ;; (advice-add 'org-agenda--quit :after #'ad:org-agenda--quit)
    )

  (add-hook 'org-finalize-agenda-hook #'my-org-agenda-to-appt)

  ;; 移動直後にagendaバッファを閉じる（ツリーの内容はSPACEで確認可）
  (org-defkey org-agenda-mode-map [(tab)]
              (lambda () (interactive)
                (org-agenda-goto)
                (with-current-buffer "*Org Agenda*"
                  (org-agenda-quit))))

  (custom-set-faces
   ;; '(org-agenda-clocking ((t (:background "#300020"))))
   '(org-agenda-structure ((t (:underline t :foreground "#6873ff"))))
   '(org-agenda-date-today ((t (:weight bold :foreground "#4a6aff"))))
   '(org-agenda-date ((t (:weight bold :foreground "#6ac214"))))
   '(org-agenda-date-weekend ((t (:weight bold :foreground "#ff8d1e"))))
   '(org-time-grid ((t (:foreground "#0a4796"))))
   '(org-warning ((t (:foreground "#ff431a"))))
   '(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   )

  ;; 所定の時刻に強制的にAgendaを表示
  (defvar my-org-agenda-auto-popup-list
    '("01:00" "11:00" "14:00" "17:00" "20:00" "23:00"))
  (defun my-popup-agenda ()
    (interactive)
    (let ((status use-dialog-box))
      (setq use-dialog-box nil)
      (when (y-or-n-p-with-timeout "Popup agenda now?" 10 nil)
        (org-agenda-list))
      (message "")
      (setq use-dialog-box status)))
  (defun my-popup-agenda-set-timers ()
    (interactive)
    (cancel-function-timers 'my-popup-agenda)
    (dolist (triger my-org-agenda-auto-popup-list)
      (when (future-time-p triger)
        (run-at-time triger nil 'my-popup-agenda))))
  (my-popup-agenda-set-timers)
  (run-at-time "24:00" nil 'my-popup-agenda-set-timers)

  ;; ついでに calendar.app も定期的に強制起動する
  (defun my-popup-calendar ()
    (interactive)
    (if (and (eq system-type 'darwin)
             (window-focus-p))
        (shell-command-to-string "open -a calendar.app")
      (message "--- input focus is currently OUT.")))

  (defun my-popup-calendar-set-timers ()
    (interactive)
    (cancel-function-timers 'my-popup-calendar)
    (dolist (triger my-org-agenda-auto-popup-list)
      (when (future-time-p triger)
        (run-at-time triger nil 'my-popup-calendar))))

  (when (memq window-system '(mac ns))
    (my-popup-calendar-set-timers)
    (run-at-time "24:00" nil 'my-popup-calendar-set-timers))

  ;; org-agenda でも "d" 押下で "DONE" にする
  (defun my-org-agenda-done ()
    (interactive)
    (org-agenda-todo "DONE")
    (my-org-agenda-to-appt)) ;; call with async
  (org-defkey org-agenda-mode-map "d" 'my-org-agenda-done))

;; Doing 管理
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "<f11>") 'my-toggle-doing-tag)
  (define-key org-mode-map (kbd "C-<f11>") 'my-sparse-doing-tree)

  ;; 特定タグを持つツリーリストに一発移動（org-tags-view, org-tree-slide）
  (defvar my-doing-tag "Doing")
  (defun my-sparse-doing-tree ()
    (interactive)
    (org-tags-view nil my-doing-tag))

  ;; Doingタグをトグルする
  (defun my-toggle-doing-tag ()
    (interactive)
    (when (eq major-mode 'org-mode)
      (save-excursion
        (save-restriction
          (org-back-to-heading t)
          ;; before 9
          ;; (unless (org-at-heading-p)
          ;;   (outline-previous-heading))
          (org-toggle-tag my-doing-tag
                          (if (string-match
                               (concat ":" my-doing-tag ":")
                               (org-get-tags-string))
                              'off 'on))))
      (org-reveal))))

(with-eval-after-load "org"
  (require 'orgbox nil t))

(when (autoload-if-found
       '(appt my-org-agenda-to-appt ad:appt-display-message
              ad:appt-disp-window)
       "appt" nil t)

  (with-eval-after-load "postpone"
    (global-set-key (kbd "C-c f 3") #'my-org-agenda-to-appt))

  (with-eval-after-load "appt"
    ;; モードラインに残り時間を表示しない
    (setq appt-display-mode-line nil)

    ;; window を フレーム内に表示する
    (setq appt-display-format 'echo)

    ;; window を継続表示する時間[s]
    (setq appt-display-duration 5)

    ;; ビープ音の有無
    (setq appt-audible nil)

    ;; 何分前から警告表示を開始するか[m]
    (setq appt-message-warning-time 10)

    ;; 警告表示開始から何分ごとにリマインドするか[m]
    (setq appt-display-interval 1)

    ;; appt-display-format が 'echo でも appt-disp-window-function を呼ぶ
    (defun ad:appt-display-message (string mins)
      "Display a reminder about an appointment.
The string STRING describes the appointment, due in integer MINS minutes.
The arguments may also be lists, where each element relates to a
separate appointment.  The variable `appt-display-format' controls
the format of the visible reminder.  If `appt-audible' is non-nil,
also calls `beep' for an audible reminder."
      (if appt-audible (beep 1))
      ;; Backwards compatibility: avoid passing lists to a-d-w-f if not necessary.
      (and (listp mins)
           (= (length mins) 1)
           (setq mins (car mins)
                 string (car string)))
      (when (memq appt-display-format '(window echo))
        (let ((time (format-time-string "%a %b %e "))
              err)
          (condition-case err
              (funcall appt-disp-window-function
                       (if (listp mins)
                           (mapcar 'number-to-string mins)
                         (number-to-string mins))
                       time string)
            (wrong-type-argument
             (if (not (listp mins))
                 (signal (car err) (cdr err))
               (message "Argtype error in `appt-disp-window-function' - \
update it for multiple appts?")
               ;; Fallback to just displaying the first appt, as we used to.
               (funcall appt-disp-window-function
                        (number-to-string (car mins)) time
                        (car string)))))
          err))
      (cond ((eq appt-display-format 'window)
             ;; TODO use calendar-month-abbrev-array rather than %b?
             (run-at-time (format "%d sec" appt-display-duration)
                          nil
                          appt-delete-window-function))
            ((eq appt-display-format 'echo)
             (message "%s" (if (listp string)
                               (mapconcat 'identity string "\n")
                             string)))))
    (advice-add 'appt-display-message :override #'ad:appt-display-message)

    (defun ad:appt-disp-window (min-to-app _new-time appt-msg)
      "Extension to support appt-disp-window."
      (if (string= min-to-app "0")
          (my-desktop-notification "### Expired! ###" appt-msg t "Glass")
        (my-desktop-notification
         (concat "in " min-to-app " min.") appt-msg nil "Tink")))
    (cond
     ((eq appt-display-format 'echo)
      (setq appt-disp-window-function 'ad:appt-disp-window))
     ((eq appt-display-format 'window)
      (advice-add 'appt-disp-window :before #'ad:appt-disp-window))))

  (with-eval-after-load "org"
    ;; アジェンダを開いたらアラームリストを更新して有効化する
    (unless noninteractive
      (add-hook 'org-agenda-mode-hook #'my-org-agenda-to-appt) ;; init
      (appt-activate 1))
    ;; 重複実行の抑制用フラグ
    (defvar my-org-agenda-to-appt-ready t)
    ;; org-agenda の内容をアラームに登録する
    (defun my-org-agenda-to-appt ()
      "Update `appt-time-mag-list'.  Use `async' if possible."
      (interactive)
      (if (not (require 'async nil t))
          (org-agenda-to-appt t '((headline "TODO")))
        (when my-org-agenda-to-appt-ready
          (setq my-org-agenda-to-appt-ready nil)
          (async-start
           `(lambda ()
              (setq org-agenda-files ',org-agenda-files)
              (org-agenda-to-appt t '((headline "TODO")))
              appt-time-msg-list)
           (lambda (result)
             (setq appt-time-msg-list result)
             (let ((cnt (length appt-time-msg-list)))
               (if (eq cnt 0)
                   (message "No event to add")
                 (message "Added %d event%s for today"
                          cnt (if (> cnt 1) "s" ""))))
             (setq my-org-agenda-to-appt-ready t))))))
    ;; 定期的に更新する
    (run-with-idle-timer 500 t 'my-org-agenda-to-appt)
    ;; キャプチャ直後に更新
    (add-hook 'org-capture-before-finalize-hook #'my-org-agenda-to-appt)))

(with-eval-after-load "org"
  ;; 履歴が生成されるのを抑制．
  ;; [2/3]のような完了数が見出しにある時に転送先候補が重複表示されるため．

  ;; リファイル先でサブディレクトリを指定するために一部フルパス化
  (let ((dir (expand-file-name org-directory)))
    (setq org-refile-targets
          `((,(concat dir "next.org") :level . 1)
            (,(concat dir "org-ical.org") :level . 1)
            (,(concat dir "academic.org") :level . 1)
            (,(concat dir "tr/work.org") :level . 1)
            (,(concat dir "minutes/wg1.org") :level . 1)
            (,(concat dir "db/article.org") :level . 1)
            (,(concat dir "db/maybe.org") :level . 1)
            (,(concat dir "db/english.org") :level . 1)
            (,(concat dir "db/money.org") :level . 1))))

  (defun ad:org-refile (f &optional arg default-buffer rfloc msg)
    "Extension to support keeping org-refile-history empty."
    (save-excursion
      (save-restriction
        (let ((l (org-outline-level))
              (b (buffer-name)))
          (apply f arg default-buffer rfloc msg)
          (if (> l (org-outline-level))
              (outline-backward-same-level 1)
            (outline-up-heading 1))
          (org-update-statistics-cookies nil) ;; Update in source
          ;; (org-sort-entries nil ?O)
          (org-refile-goto-last-stored)
          (org-update-parent-todo-statistics) ;; Update in destination
          (outline-up-heading 1)
          (org-sort-entries nil ?o)
          (unless (equal b (buffer-name))
            (switch-to-buffer b)))
        (setq org-refile-history nil)
        (org-refile-cache-clear))))
  (advice-add 'org-refile :around #'ad:org-refile)

  (defun ad:org-sort-entries (&optional _with-case _sorting-type
                                        _getkey-func _compare-func
                                        _property _interactive?)
    (outline-hide-subtree)
    (org-show-hidden-entry)
    (org-show-children))
  (advice-add 'org-sort-entries :after #'ad:org-sort-entries))

(with-eval-after-load "ob-core"
  (setq org-edit-src-content-indentation 0)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  ;; org-src-window-setup (current-window, other-window, other-frame)
  (require 'ob-http nil t)
  (require 'ob-gnuplot nil t)

  (unless (executable-find "ditaa")
    (message "--- ditaa is NOT installed."))

  ;; Add ":results output" after program name

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)
     (C . t)
     (ditaa . t)
     (perl . t)
     (shell . t)
     (latex . t)
     (sqlite . t)
     (R . t)
     (python . t)))

  ;; 実装済みの言語に好きな名前を紐付ける
  (add-to-list 'org-src-lang-modes '("cs" . csharp))
  (add-to-list 'org-src-lang-modes '("zsh" . sh)))

(with-eval-after-load "org"
  (add-to-list 'org-structure-template-alist
               (if (version< "9.1.4" (org-version))
                   '("S" . "src emacs-lisp")
                 '("S" "#+begin_src emacs-lisp\n?\n#+END_SRC" "<src lang=\"emacs-lisp\">\n\n</src>"))))

(with-eval-after-load "org"
  (custom-set-faces
   '(org-block-begin-line
     ((((background dark))
       (:foreground "#669966" :weight bold)) ;; :background "#444444"
      (t (:inherit org-meta-line :weight bold))) ;; :background "#EFEFEF"
     (org-block-end-line
      ((((background dark)) (:inherit org-block-begin-line))
       (t (:inherit org-block-begin-line)))))))

(when (autoload-if-found
       '(org-tree-slide-mode)
       "org-tree-slide" nil t)

  (with-eval-after-load "postpone"
    (global-set-key (kbd "<f8>") 'org-tree-slide-mode)
    (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle))

  (with-eval-after-load "org-tree-slide"
    ;; <f8>/<f9>/<f10>/<f11> are assigned to control org-tree-slide
    (define-key org-tree-slide-mode-map (kbd "<f9>")
      'org-tree-slide-move-previous-tree)
    (define-key org-tree-slide-mode-map (kbd "<f10>")
      'org-tree-slide-move-next-tree)
    (unless noninteractive
      (org-tree-slide-narrowing-control-profile))
    (setq org-tree-slide-modeline-display 'outside)
    (setq org-tree-slide-skip-outline-level 5)
    (setq org-tree-slide-skip-done nil))

  (defun my-reload-header-face ()
    (face-spec-set 'org-tree-slide-header-overlay-face
                   `((t (:bold t :foreground ,(face-foreground 'default)
                               :background ,(face-background 'default))))))
  (add-hook 'org-tree-slide-play-hook #'my-reload-header-face))

(with-eval-after-load "org-tree-slide"
  (when (require 'doom-modeline nil t)
    (add-hook 'org-tree-slide-stop-hook
              #'doom-modeline-update-buffer-file-state-icon)))

(with-eval-after-load "org-tree-slide"
  (defun my-tree-slide-autoclockin-p ()
    (save-excursion
      (save-restriction
        (widen)
        (goto-char 1)
        (let ((keyword "TREE_SLIDE:")
              (value "autoclockin")
              (result nil))
          (while
              (and (re-search-forward (concat "^#\\+" keyword "[ \t]*") nil t)
                   (re-search-forward value (point-at-eol) t))
            (setq result t))
          result))))

  (when (require 'org-clock nil t)
    (defun my-org-clock-in ()
      (unless (and (boundp 'doom-modeline-mode)
                   doom-modeline-mode)
        (setq vc-display-status nil)) ;; モードライン節約
      (when (and (my-tree-slide-autoclockin-p)
                 (looking-at (concat "^\\*+ " org-not-done-regexp))
                 (memq (org-outline-level) '(1 2 3 4)))
        (save-excursion
          (save-restriction
            (forward-line)
            (when (org-at-heading-p)
              (newline)))) ;; FIXME: remove empty line if clock will not be recorded.
        (org-clock-in)))

    (defun my-org-clock-out ()
      (setq vc-display-status t) ;; モードライン節約解除
      (when (org-clocking-p)
        (org-clock-out)))

    (add-hook 'org-tree-slide-before-move-next-hook #'my-org-clock-out)
    (add-hook 'org-tree-slide-before-move-previous-hook #'my-org-clock-out)
    ;; (add-hook 'org-tree-slide-before-content-view-hook #'my-org-clock-out)
    (add-hook 'org-tree-slide-stop-hook #'my-org-clock-out)
    (add-hook 'org-tree-slide-after-narrow-hook #'my-org-clock-in)))

(when (autoload-if-found
       '(org-tree-slide-mode my-toggle-proportional-font)
       "org-tree-slide" nil t)

  (with-eval-after-load "org-tree-slide"
    (defcustom use-proportional-font nil
      "The status of FONT property"
      :type 'boolean
      :group 'org-mode)

    (set-face-attribute 'variable-pitch nil
                        :family "Verdana"
                        ;; :family "Comic Sans MS"
                        :height 125)

    (defun my-toggle-proportional-font ()
      (interactive)
      (setq use-proportional-font (not use-proportional-font))
      (if use-proportional-font
          (org-entry-put nil "FONT" "PROPORTIONAL")
        (org-delete-property "FONT")))

    (add-hook 'org-tree-slide-before-narrow-hook
              (lambda ()
                  (if (equal "PROPORTIONAL"
                             (org-entry-get-with-inheritance "FONT"))
                      (buffer-face-set 'variable-pitch)
                    (buffer-face-mode 0))))
    (add-hook 'org-tree-slide-stop-hook
              (lambda ()
                  (buffer-face-mode 0)))))

(when (autoload-if-found
       '(my-cfw-open-org-calendar cfw:open-org-calendar)
       "calfw-org" "Rich calendar for org-mode" t)

  (with-eval-after-load "postpone"
    (global-set-key (kbd "C-c f c w") 'my-cfw-open-org-calendar))

  (with-eval-after-load "calfw-org"
    ;; icalendar との連結
    (custom-set-variables
     '(cfw:org-icalendars '("~/Dropbox/org/org-ical.org"))
     '(cfw:fchar-junction ?+) ;; org で使う表にフェイスを統一
     '(cfw:fchar-vertical-line ?|)
     '(cfw:fchar-horizontal-line ?-)
     '(cfw:fchar-left-junction ?|)
     '(cfw:fchar-right-junction ?|)
     '(cfw:fchar-top-junction ?+)
     '(cfw:fchar-top-left-corner ?|)
     '(cfw:fchar-top-right-corner ?|))

    (defun my-org-mark-ring-goto-calfw ()
      (interactive)
      (org-mark-ring-goto))

    (defun my-cfw-open-org-calendar ()
      (interactive)
      (moom-change-frame-width-double)
      (cfw:open-org-calendar))

    (defun my-cfw-burry-buffer ()
      (interactive)
      (bury-buffer)
      (moom-change-frame-width-single))

    (defun cfw:org-goto-date ()
      "Move the cursor to the specified date."
      (interactive)
      (cfw:navi-goto-date
       (cfw:emacs-to-calendar (org-read-date nil 'to-time))))

    (define-key cfw:calendar-mode-map (kbd "j") 'cfw:org-goto-date)
    (define-key cfw:org-schedule-map (kbd "q") 'my-cfw-burry-buffer)))

;;         (add-hook 'window-configuration-change-hook #'cfw:resize-calendar)
;; (defun cfw:resize-calendar ()
;;   (interactive)
;;   (when (eq major-mode 'cfw:calendar-mode)
;;     (cfw:refresh-calendar-buffer nil)
;;     (message "Calendar resized.")))

;; (defun open-calfw-agenda-org ()
;;   (interactive)
;;   (cfw:open-org-calendar))

;; (setq org-agenda-custom-commands
;;       '(("w" todo "FOCUS")
;;         ("G" open-calfw-agenda-org "Graphical display in calfw"))))))

(when (autoload-if-found
       '(ox-odt)
       "ox-odt" nil t)

  (with-eval-after-load "ox-odt"
    ;; (add-to-list 'org-odt-data-dir
    ;;              (concat (getenv "HOME") "/Dropbox/emacs.d/config/"))
    (setq org-odt-styles-file
          (concat (getenv "HOME") "/Dropbox/emacs.d/config/style.odt"))
    ;; (setq org-odt-content-template-file
    ;;       (concat (getenv "HOME") "/Dropbox/emacs.d/config/style.ott"))
    (setq org-odt-preferred-output-format "pdf") ;; docx
    ;; ;; ox-odt.el の 自作パッチの変数（DOCSTRINGが記述されていない）
    ;; (setq org-odt-apply-custom-punctuation t)
    (setq org-odt-convert-processes
          '(("LibreOffice"
             "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")
            ("unoconv" "unoconv -f %f -o %d %i")))))

(with-eval-after-load "ox"
  (require 'ox-twbs nil t))

(with-eval-after-load "org"
  (when (require 'org-crypt nil t)
    (setq org-crypt-key "") ;; <insert your key>
    ;; org-encrypt-entries の影響を受けるタグを指定
    (setq org-tags-exclude-from-inheritance (quote ("secret")))
    ;; 自動保存の確認を無効に
    (setq org-crypt-disable-auto-save 'nil)))

(with-eval-after-load "org"
  ;; (add-to-list 'org-modules 'org-mac-iCal)
  ;; (add-to-list 'org-modules 'org-mac-link) ;; includes org-mac-message
  (when (and (require 'org-mac-iCal nil t)
             (require 'org-mac-link nil t))
    (define-key org-mode-map (kbd "C-c c") 'org-mac-grab-link)))

(with-eval-after-load "org"
  (when (require 'org-download nil t)
    (setq org-download-screenshot-method 'screencapture)
    (setq org-download-method 'attach)))

(when (autoload-if-found
       '(org-grep)
       "org-grep" nil t)

  (with-eval-after-load "postpone"
    (global-set-key (kbd "C-M-g") 'org-grep))

  (with-eval-after-load "org-grep"
    (setq org-grep-extensions '(".org" ".org_archive"))
    (add-to-list 'org-grep-directories "~/.emacs.d")
    (add-to-list 'org-grep-directories "~/.emacs.d/.cask/package")

    ;; "q"押下の挙動を調整
    (defun ad:org-grep-quit ()
      (interactive)
      (delete-window))
    (advice-add 'org-grep-quit :override #'ad:org-grep-quit)

    ;; for macOS
    (when (memq window-system '(mac ns))
      (defun org-grep-from-org-shell-command (regexp)
        (if org-grep-directories
            (concat "find -E "
                    (if org-grep-directories
                        (mapconcat #'identity org-grep-directories " ")
                      org-directory)
                    (and org-grep-extensions
                         (concat " -regex '.*("
                                 (mapconcat #'regexp-quote org-grep-extensions
                                            "|")
                                 ")$'"))
                    " -print0 | xargs -0 grep " org-grep-grep-options
                    " -n -- " (shell-quote-argument regexp))
          ":")))))

(with-eval-after-load "ox"
  (when (and (require 'ox-reveal nil t)
             (version< "9.1.4" (org-version)))
    (setq org-reveal-note-key-char ?n)))

(when (autoload-if-found
       '(org-dashboard-display)
       "org-dashboard" nil t)

  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c f y") 'org-dashboard-display)))

(with-eval-after-load "org"
  (defun ad:org-clock-sum-today (&optional headline-filter)
    "Sum the times for each subtree for today."
    (let ((range (org-clock-special-range 'today nil t))) ;; TZ考慮
      (org-clock-sum (car range) (cadr range)
		                 headline-filter :org-clock-minutes-today)))
  (advice-add 'org-clock-sum-today :override #'ad:org-clock-sum-today)

  (when (require 'org-clock-today nil t)
    (defun ad:org-clock-today-update-mode-line ()
      "Calculate the total clocked time of today and update the mode line."
      (setq org-clock-today-string
            (if (org-clock-is-active)
                ;; ナローイングの影響を排除し，subtreeに限定しない．
                (save-excursion
                  (save-restriction
                    (with-current-buffer (org-clock-is-active)
                      (widen)
                      (let* ((current-sum (org-clock-sum-today))
                             (open-time-difference (time-subtract
                                                    (float-time)
                                                    (float-time
                                                     org-clock-start-time)))
                             (open-seconds (time-to-seconds open-time-difference))
                             (open-minutes (/ open-seconds 60))
                             (total-minutes (+ current-sum
                                               open-minutes)))
                        (concat " " (org-minutes-to-clocksum-string
                                     total-minutes))))))
              ""))
      (force-mode-line-update))
    (advice-add 'org-clock-today-update-mode-line
                :override #'ad:org-clock-today-update-mode-line)

    (unless noninteractive
      (org-clock-today-mode 1))))

(autoload-if-found
 '(org-random-todo org-random-todo-goto-current)
 "org-random-todo" nil t)

;; see https://ox-hugo.scripter.co/doc/deprecation-notices/#org-hugo-auto-export-feature-now-a-minor-mode
;; (with-eval-after-load "org"
;; No need for latest ox-hugo
;;   ;; Require ox-hugo-auto-export.el explictly before loading ox-hugo.el
;;   (require 'ox-hugo-auto-export nil t))

(with-eval-after-load "org"
  (when (require 'ox-hugo nil t)
    (setq org-hugo-auto-set-lastmod nil) ;; see my-hugo-export-md
    (setq org-hugo-suppress-lastmod-period 86400.0) ;; 1 day
    ;; never copy files to under /static/ directory
    (setq org-hugo-external-file-extensions-allowed-for-copying nil)

    ;; see https://pxaka.tokyo/blog/2018/a-link-to-the-original-org-source-file
    (defun org-hugo-get-link-to-orgfile (uri alt)
      "Return a formatted link to the original Org file.
To insert the formatted into an org buffer for Hugo, use an appropriate
macro, e.g. {{{srclink}}}.

Note that this mechanism is still under consideration."
      (let ((line (save-excursion
                    (save-restriction
                      (unless (org-at-heading-p)
                        (org-previous-visible-heading 1))
                      (line-number-at-pos)))))
        (concat "[[" uri (file-name-nondirectory (buffer-file-name))
                "#L" (format "%d" line) "][" alt "]]")))

    (defun my-add-ox-hugo-lastmod ()
      "Add `lastmod' property with the current time."
      (interactive)
      (org-set-property "EXPORT_HUGO_LASTMOD"
                        (format-time-string "[%Y-%m-%d %a %H:%M]")))

    (defun ad:ox-hugo:org-todo (&optional ARG)
      "Export subtree for Hugo if the TODO status in ARG is changing to DONE."
      (when (and (equal (buffer-name) "imadenale.org")
                 ;; FIXME C-c C-t d に反応しない．speed command はOK．
                 (or (eq ARG 'done)
                     (equal ARG "DONE")))
        (org-hugo-export-wim-to-md)
        (message "[ox-hugo] \"%s\" has been exported."
                 (nth 4 (org-heading-components)))
        (let ((command "~/Dropbox/scripts/push-hugo.sh"))
          (if (require 'async nil t)
              (async-start
               `(lambda () (shell-command-to-string ',command)))
            (shell-command-to-string command)))))
    (advice-add 'org-todo :after #'ad:ox-hugo:org-todo)))

(with-eval-after-load "org"
  (defun my-add-custom-id ()
    "Add \"CUSTOM_ID\" to the current tree if not assigned yet."
    (interactive)
    (my-org-custom-id-get nil t))

  (defun my-get-custom-id ()
    "Return a part of UUID with an \"org\" prefix.
e.g. \"org3ca6ef0c\"."
    (let* ((id (org-id-new "")))
      (when (org-uuidgen-p id)
        (downcase (concat "org"  (substring (org-id-new "") 0 8))))))

  (defun my-org-custom-id-get (&optional pom create)
    "Get the CUSTOM_ID property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point.  If the entry does
not have an CUSTOM_ID, the function returns nil.  However, when
CREATE is non nil, create a CUSTOM_ID if none is present
already.  In any case, the CUSTOM_ID of the entry is returned.

See https://writequit.org/articles/emacs-org-mode-generate-ids.html"
    (interactive)
    (org-with-point-at pom
      (let ((id (org-entry-get nil "CUSTOM_ID")))
        (cond
         ((and id (stringp id) (string-match "\\S-" id))
          id)
         (create
          (setq id (my-get-custom-id))
          (unless id
            (error "Invalid ID"))
          (org-entry-put pom "CUSTOM_ID" id)
          (message "--- CUSTOM_ID assigned: %s" id)
          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
          id)))))

  ;;;###autoload
  (defun my-add-org-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current file.
Which do not already have one.  Only adds ids if the
`auto-id' option is set to `t' in the file somewhere. ie,
#+options: auto-id:t

See https://writequit.org/articles/emacs-org-mode-generate-ids.html"
    (interactive)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t" (point-max) t)
        (org-map-entries
         (lambda () (my-org-custom-id-get (point) 'create)))))))

(with-eval-after-load "ob-core"
  (when (require 'ob-async nil t)
    (custom-set-variables
     '(ob-async-no-async-languages-alist '("ipython")))))

(when (autoload-if-found
       '(org-tree-slide-mode)
       "org-tree-slide" nil t)

  (with-eval-after-load "org-tree-slide"
    ;; FIXME 複数のバッファで並行動作させるとおかしくなる．hide-lines の問題？
    (when (and nil (require 'hide-lines nil t))
      (defvar my-org-src-block-faces nil)
      (defun my-show-headers ()
        (setq org-src-block-faces 'my-org-src-block-faces)
        (hide-lines-show-all))
      (defun my-hide-headers ()
        (setq my-org-src-block-faces 'org-src-block-faces)
        (setq org-src-block-faces
              '(("emacs-lisp" (:background "cornsilk"))))
        (hide-lines-matching "#\\+BEGIN_SRC")
        (hide-lines-matching "#\\+END_SRC"))
      (add-hook 'org-tree-slide-play-hook #'my-hide-headers)
      (add-hook 'org-tree-slide-stop-hook #'my-show-headers)

      ;; (defun ad:org-edit-src-code (&optional code edit-buffer-name)
      (defun ad:org-edit-src-code ()
        (interactive)
        (my-show-headers))
      (advice-add 'org-edit-src-code :before #'ad:org-edit-src-code)
      ;; Block 外で呼ばれると，my-show-headers が呼ばれてしまう
      (defun ad:org-edit-src-exit ()
        (interactive)
        (my-hide-headers))
      (advice-add 'org-edit-src-exit :after #'ad:org-edit-src-exit))))

(with-eval-after-load "ox"
  ;; (setq org-export-default-language "ja")
  (require 'ox-pandoc nil t)
  (require 'ox-qmd nil t) ;; Quita-style
  (require 'ox-gfm nil t)) ;; GitHub-style

(with-eval-after-load "org-mac-link"
  (defcustom org-mac-grab-Papers-app-p t
    "Add menu option [P]apers to grab links from the Papers.app."
    :tag "Grab Papers.app links"
    :group 'org-mac-link
    :type 'boolean)

  (defun org-mac-papers-insert-frontmost-paper-link ()
    (interactive)
    (let ((result (org-mac-papers-get-frontmost-paper-link)))
      (if result
          (insert result)
        (message "Please open Papers.app and select a paper."))))

  (defun org-mac-papers-get-frontmost-paper-link ()
    (interactive)
    (message "Applescript: Getting Papers link...")
    (let ((result (org-as-mac-papers-get-paper-link)))
      (if (or (eq result nil) (string= result ""))
          nil
        (org-mac-paste-applescript-links result))))

  (defun org-as-mac-papers-get-paper-link ()
    (do-applescript
     (concat
      "if application \"Papers\" is running then\n"
      "	tell application \"Papers\" to activate\n"
      "	delay 0.3\n"
      "	set the clipboard to \"\"\n"
      "	tell application \"System Events\" to tell process \"Papers\"\n"
      "		keystroke \"l\" using {command down, shift down}\n"
      "	end tell\n"
      "	delay 0.2\n"
      "	set aLink to the clipboard\n"
      "	tell application \"System Events\" to tell process \"Papers\"\n"
      ;; "		keystroke \"c\" using {command down, alt down\}\n"
      "		keystroke \"m\" using {command down, option down\}\n"
      "	end tell\n"
      "	delay 0.2\n"
      "	set aName to the clipboard\n"
      "	tell application \"Emacs\" to activate\n"
      "	return (get aLink) & \"::split::\" & (get aName) as string\n"
      "else\n"
      "	return\n"
      "end if\n")))

  (when (boundp 'org-mac-link-descriptors)
    (add-to-list 'org-mac-link-descriptors
                 `("P" "apers" org-mac-papers-insert-frontmost-paper-link
                   ,org-mac-grab-Papers-app-p) t)))

(with-eval-after-load "org"
  (when (eq system-type 'darwin)
    ;; Open `papers3://' link by C-c C-o.
    ;; (org-add-link-type will be obsoleted from Org 9.
    (when (fboundp 'org-link-set-parameters)
      (org-link-set-parameters
       "papers3"
       :follow (lambda (path)
                 (let ((cmd (concat "open papers3:" path)))
                   (shell-command-to-string (shell-quote-argument cmd))
                   (message "%s" cmd)))))))

(when (autoload-if-found
       '(org-attach du-org-attachments)
       "org-attach" nil t)

  (with-eval-after-load "org-attach"
    ;; org-insert-link で添付ファイルへのリンクをペーストできるようにする
    (setq org-attach-store-link-p t)
    ;; 自動付与されるタグの名前を変更
    (setq org-attach-auto-tag "Attach")
    ;; git-annex を使用しない
    (setq org-attach-git-annex-cutoff nil)

    (defvar org-attach-directory-absolute
      (concat (getenv "HOME")
              "/Dropbox/org/"
              (if (boundp 'org-attach-directory)
                  "data/")))

    (defun du-org-attachments ()
      "Show directory size for org-attachments."
      (interactive)
      (message "--- %s"
               (chomp (shell-command-to-string
                       (concat "/usr/bin/du -sh "
                               org-attach-directory-absolute)))))))

(when (autoload-if-found
       '(org-recent-headings-helm org-recent-headings-mode)
       "org-recent-headings" nil t)

  (with-eval-after-load "postpone"
    (global-set-key (kbd "C-c f r") 'org-recent-headings-helm))

  (with-eval-after-load "org-recent-headings"
    (custom-set-variables
     '(org-recent-headings-save-file "~/.emacs.d/org-recent-headings.dat"))
    (if shutup-p
        (shut-up (org-recent-headings-mode 1))
      (org-recent-headings-mode 1))))

(when (autoload-if-found
       '(orgnav-search-root)
       "orgnav" nil t)

  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c f n")
      (lambda () (interactive)
          (orgnav-search-root 3 'orgnav--goto-action)))))

(autoload-if-found '(toc-org-insert-toc) "toc-org" nil t)

(when (autoload-if-found
       '(org-attach-screenshot)
       "org-attach-screenshot" nil t)

  (with-eval-after-load "org-attach-screenshot"
    (when (executable-find "screencapture")
      (setq org-attach-screenshot-command-line "screencapture -w %f"))
    (defun my-org-attach-screenshot ()
      (interactive)
      (org-attach-screenshot t (format-time-string
                                "screenshot-%Y%m%d-%H%M%S.png")))))

;; (my-tick-init-time "Org Mode")
(provide 'init-org)
