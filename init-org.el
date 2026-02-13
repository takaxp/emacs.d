;; init-org.el --- My config for org mode -*- lexical-binding: t -*-
(defvar my-init-org-start (current-time))
(unless (featurep 'postpone)
  (call-interactively 'postpone-pre))

(add-hook 'org-mode-hook #'my-org-mode-indent-conf)

;; テキストファイルを Org Mode で開く．
(push '("\\.txt$" . org-mode) auto-mode-alist)

;; Font lock を使う
(add-hook 'org-mode-hook #'turn-on-font-lock)

(keymap-global-set "C-c r" 'org-capture)
(keymap-global-set "C-c l" 'org-store-link)
(keymap-global-set "C-c a" 'org-agenda)

(defvar my-org-modules org-modules) ;; Tricky!!
;; (setq org-modules-loaded t) ;; not a good way
(setq org-modules nil)
(unless noninteractive
  (run-with-idle-timer (+ 8 my-default-loading-delay)
                       nil #'my-org-modules-activate)) ;; will take 350[ms]

;; タイトルを少し強調
(custom-set-faces
 '(org-document-title ((t (:foreground "RoyalBlue1" :bold t :height 1.2))))
 '(org-document-info ((t (:foreground "DodgerBlue1" :height 1.0)))))

;; 関連モジュールの読み込み
(autoload 'org-eldoc-load "org-eldoc" nil t)

;; 少なくとも org 9.5 では問題が発生しなくなったので，advice 停止．
;; (advice-add 'org-eldoc-load :override #'my--org-eldoc-load)
(add-hook 'org-mode-hook #'org-eldoc-load)

;; org ファイルの集中管理
(setq org-directory (concat (getenv "SYNCROOT") "/org/"))

;; org-store-link で heading に自動的に挿入される id を使う
(setq org-id-link-to-org-use-id t)

;; ..org-id-locations の格納先
(setq org-id-locations-file
      (concat (getenv "SYNCROOT") "/usr/emacs.d/.org-id-locations"))

;; アーカイブファイルの名称を指定
(setq org-archive-location "%s_archive::")

;; タイムスタンプによるログ収集設定 DONE 時に CLOSED: を記入．
(setq org-log-done 'time) ; 'time 以外に，'(done), '(state) を指定できる

;; ログをドロアーに入れる
(setq org-log-into-drawer t)

;; タスク繰り返し時にログを残さない
(setq org-log-repeat nil)

;; タスク繰り返し時にログを残さないが，LAST_REPEAT は記録する．
(advice-add 'org-todo :after #'my--org-last-repeat)

;; indent を electric-indent-mode の振る舞いに合わせる
;; (setq org-adapt-indentation t) ;; t の場合，ドロアがインデントされる．

;; Set checksum program path for windows
(when (eq window-system 'w32)
  (setq org-mobile-checksum-binary (concat (getenv "SYNCROOT") "/do/cksum.exe")))

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

;; 再起動後に clock を復帰させる（clock-out で抜けない限り終了中の期間も計上されてしまう）
;; check also org-clock-persist in org-clock.el
(org-clock-persistence-insinuate)

;; org-clock-out 時にステータスを変える（also configure org-todo-keywords）
;; (setq org-clock-out-switch-to-state #'my-promote-todo-revision)

;; undo 時に reveal して表示を改善する
;; (defun my--org:undo (&optional _ARG)
;;   (when (and (eq major-mode 'org-mode)
;;                (not (org-before-first-heading-p)))
;;       (org-overview)
;;       (org-reveal)
;;       (org-cycle-hide-drawers 'all)
;;       (org-show-entry)
;;       (show-children)
;;       (org-show-siblings)))
;; (advice-add 'undo :after #'my--org:undo)

;; 非表示状態の領域への書き込みを防ぐ
;; "Editing in invisible areas is prohibited, make them visible first"
(setq org-catch-invisible-edits 'show-and-error)
(advice-add 'org-return :around #'my--org-return)

;; ブリッツにアルファベットを使う
(setq org-list-allow-alphabetical t)

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

;; プロパティ等を自動的閉じる．
(add-hook 'org-tab-first-hook 'my-org-hide-drawers)

(keymap-set org-mode-map "C-c f 2" 'my-do-org-update-statistics-cookies)

;; C-c & が yasnippet にオーバーライドされているのを張り替える
(keymap-set org-mode-map "C-c 4" 'org-mark-ring-goto)

;; (org-transpose-element) が割り当てられているので取り返す．
(org-defkey org-mode-map "\C-\M-t" 'beginning-of-buffer)

;; C-c C-o でファイルを開くとき，外部アプリケーションで開く
(add-to-list 'org-file-apps '("\\.xlsx\\'" . default))
(add-to-list 'org-file-apps '("\\.docx\\'" . default))
(add-to-list 'org-file-apps '("\\.pptx\\'" . default))


(with-eval-after-load "ox"
  (add-to-list 'org-modules 'ox-odt)
  (add-to-list 'org-modules 'ox-org)
  (add-to-list 'org-modules 'ox-json)) ;; FIXME

(with-eval-after-load "org-tempo"
  ;; 空行のとき "<" をインデントさせない
  (advice-add 'org-tempo-complete-tag :around #'my--org-tempo-complete-tag))

;; (Thanks to @conao3)
;; but when using `flet', byte-compile will warn a malformed function
;; and using `cl-flet' will not provide us the expected result...
;; (when (require 'cl-lib nil t)
;;   (defun my--org-tempo-complete-tag (f &rest arg)
;;     (if (save-excursion
;;           (beginning-of-line)
;;           (looking-at "<"))
;;         (cl-flet ((indent-according-to-mode () #'ignore))
;;           (apply f arg))
;;       (apply f arg)))
;;   (advice-add 'org-tempo-complete-tag :around #'my--org-tempo-complete-tag))

(with-eval-after-load "org-tempo"
  ;; 更新
  (advice-add 'org-tempo-add-block :override #'my--org-tempo-add-block)
  ;; 反映
  (org-tempo-add-templates))

(with-eval-after-load "org-clock"
  ;; nil or 'history ならば，org-onit が org-clock-out を実行する．
  (setq org-clock-persist 'history ;; {nil, t, 'clock, 'history}
        org-clock-in-resume t
        org-clock-persist-query-resume nil)

  (advice-add 'org-clock-load :around #'my--suppress-message)

  ;; 終了時に clock を止める．
  ;; implemented in `org-onit.el'. No need to hook this.
  ;; (add-hook 'kill-emacs-hook #'my-org-clock-out-and-save-when-exit)
  )

(with-eval-after-load "org-table"
  ;; エコー表示前に保存する
  (advice-add 'org-table-field-info :before #'my--org-table-field-info))

(keymap-set org-mode-map "C-c f 1" 'my-ox-upload-icalendar)

;; ~/Dropbox/Public は第三者に探索される可能性があるので要注意
;; default = ~/org.ics
;; C-c C-e i org-export-icalendar-this-file
;; C-c C-e I org-export-icalendar-all-agenda-files
;; C-c C-e c org-export-icalendar-all-combine-agenda-files
(when (autoload-if-found '(my-ox-icalendar
                           my-async-ox-icalendar my-ox-icalendar-cleanup)
                         "ox-icalendar" nil t)

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

    ;; 通常は，<>--<> で区間付き予定をつくる．非改行入力で日付がNoteに入らない
    (setq org-icalendar-use-scheduled '(event-if-todo))

    ;; DL 付きで終日予定にする：締め切り日（スタンプで時間を指定しないこと）
    ;; (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo))
    (setq org-icalendar-use-deadline '(event-if-todo))))

(setq org-use-speed-commands t)

(when (version< (org-version) "9.4.6")
  (defvaralias 'org-speed-commands 'org-speed-commands-user))

;; "C"(org-shifttab) をオーバーライド
(add-to-list 'org-speed-commands '("C" org-copy-subtree))
(add-to-list 'org-speed-commands '("d" my-done-with-update-list))
;; (add-to-list 'org-speed-commands '("S" call-interactively 'widen))
(add-to-list 'org-speed-commands
             '("D" my-org-todo-complete-no-repeat "DONE"))
;; (add-to-list 'org-speed-commands '("N" org-shiftmetadown))
;; (add-to-list 'org-speed-commands '("P" org-shiftmetaup))
(add-to-list 'org-speed-commands '("H" my-hugo-export-upload))
(add-to-list 'org-speed-commands '("h" org-hugo-export-wim-to-md))
(add-to-list 'org-speed-commands '("E" my-export-subtree-as-html))
(add-to-list 'org-speed-commands '("P" my-toggle-org-pin-subtree))
(add-to-list 'org-speed-commands '("." my-org-deadline-today))
(add-to-list 'org-speed-commands '("!" my-org-default-property))
(add-to-list 'org-speed-commands '("y" my-org-yank))
(add-to-list 'org-speed-commands '("x" my-org-move-subtree-to-the-last))
(add-to-list 'org-speed-commands
             '("$" call-interactively 'org-archive-subtree))


;; ツリーをカットする時に，カレントサブツリーと親の統計情報を更新する
;; kill時に[0/0]の色が変わるのが気になる場合は，volatile-highlights ロード後に
;; kill-region から vhl/.advice-callback-fn/.make-vhl-on-kill-region を
;; advice-remove する．
(advice-add 'kill-region :after #'my--kill-update-todo-statistics)

;; ツリーをペーストする時に，カレントサブツリーと親の統計情報を更新する
(advice-add 'org-yank :after #'my--yank-update-todo-statistics)

;; アーカイブする前に narrowing を解く
(advice-add 'org-archive-subtree :before #'widen)

;; バッファ表示時に指定のツリーのコンテンツを展開表示する(Toggle)
(defvar my-org-pin-tag "pin")

;; narrowing+編集開始時に領域の最後に改行を置く FIXME
(advice-add 'org-narrow-to-subtree :before #'my--add-newline-narrowed-end)

;; Font lock を使う
;; (global-font-lock-mode 1) ;; see org-mode-hook

;; ウィンドウの端で折り返す
(setq org-startup-truncated nil)

;; サブツリー以下の * を略式表示する
(setq org-hide-leading-stars t)

;; Color setting for TODO keywords
;; Color for priorities
;; (setq org-priority-faces
;;  '(("?A" :foreground "#E01B4C" :background "#FFFFFF" :weight bold)
;;      ("?B" :foreground "#1739BF" :background "#FFFFFF" :weight bold)
;;      ("?C" :foreground "#575757" :background "#FFFFFF" :weight bold)))
;; Color setting for Tags

;; #CC3333
(setq org-todo-keyword-faces
      '(("FOCUS"    :foreground "#FF0000" :background "#FFCC66")
        ("BUG"      :foreground "#FF0000" :background "#FFCC66")
        ("CHECK"    :foreground "#FF9900" :background "#FFF0F0" :underline t)
        ("ICAL"     :foreground "#33CC66")
        ("APPROVED" :foreground "#66CC66")
        ("QUESTION" :foreground "#FF0000")
        ("WAIT"     :foreground "#333333" :background "#CCCCCC")
        ("MAIL"     :foreground "#CC3300" :background "#FFEE99")
        ("PLAN"     :foreground "#FF6600")
        ("PLAN2"    :foreground "#FFFFFF" :background "#FF6600")
        ("REV1"     :foreground "#3366FF")
        ("REV2"     :foreground "#3366FF" :background "#99CCFF")
        ("REV3"     :foreground "#FFFFFF" :background "#3366FF")
        ("SLEEP"    :foreground "#9999CC")))

;; (:foreground "#0000FF" :bold t)       ; default. do NOT put this bottom
(setq org-tag-faces
      '(("Achievement" :foreground "#66CC66")
        ("Bug"   :foreground "#FF0000")
        ("Report"        :foreground "#66CC66")
        ("Background"    :foreground "#66CC99")
        ("Meeting"       :foreground "#66CC99")
        ("Chore"         :foreground "#6699CC")
        ("Remind"        :foreground "#6699CC")
        ("project"       :foreground "#6666CC")
        ("read"  :foreground "#6666CC")
        ("book"  :foreground "#6666CC")
        ("Doing"         :foreground "#FF0000")
        ("Draft"         :foreground "#9933CC") ;; Draft(r1,r2,r3)->Review(1,2)
        ("Review"        :foreground "#6633CC")
        ("Revisit"       :foreground "#6633CC")
        ("Redmine"       :foreground "#CC6666")
        ("Ongoing"       :foreground "#CC6666") ; for non scheduled/reminder
        ("Template"      :foreground "#66CC66")
        ("Repeat"        :foreground "#CC9999") ; for interval tasks
        ("Mag"   :foreground "#9966CC")
        ("pin"   :foreground "#FF0000")
        ("buy"   :foreground "#9966CC")
        ("pay"   :foreground "#CC6699")
        ("try"   :foreground "#FF3366")
        ("secret"        :foreground "#FF0000")
        ("emacs"         :foreground "#6633CC")
        ("note"  :foreground "#6633CC")
        ("print"         :foreground "#6633CC")
        ("study"         :foreground "#6666CC")
        ("Implements"    :foreground "#CC9999" :weight bold)
        ("Coding"        :foreground "#CC9999")
        ("Editing"       :foreground "#CC9999" :weight bold)
        ("work"  :foreground "#CC9999" :weight bold)
        ("Survey"        :foreground "#CC9999" :weight bold)
        ("Home"  :foreground "#CC9999" :weight bold)
        ("Open"  :foreground "#CC9999" :weight bold)
        ("Blog"  :foreground "#9966CC")
        ("story"         :foreground "#FF7D7D")
        ("plan"  :foreground "#FF7D7D")
        ("issue"         :foreground "#FF7D7D")
        ("Test"  :foreground "#FF0000" :weight bold)
        ("attach"        :foreground "#FF0000")
        ("drill"         :foreground "#66BB66" :underline t)
        ("DEBUG"         :foreground "#FFFFFF" :background "#9966CC")
        ("EVENT"         :foreground "#FFFFFF" :background "#9966CC")
        ("Thinking"      :foreground "#FFFFFF" :background "#96A9FF")
        ("Schedule"      :foreground "#FFFFFF" :background "#FF7D7D")
        ("INPUT"         :foreground "#FFFFFF" :background "#CC6666")
        ("OUTPUT"        :foreground "#FFFFFF" :background "#66CC99")
        ("CYCLE"         :foreground "#FFFFFF" :background "#6699CC")
        ("weekend"       :foreground "#FFFFFF" :background "#CC6666")
        ("Log"   :foreground "#008500")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "PLAN(p)" "PLAN2(P)" "|" "DONE(d)")
        (sequence "FOCUS(f)" "CHECK(C)" "ICAL(c)"  "|" "DONE(d)")
        (sequence "WAIT(w)" "SLEEP(s)" "QUESTION(q)" "|" "DONE(d)")
        (sequence "REV1(1)" "REV2(2)" "REV3(3)" "|" "APPROVED(a@/!)")))

;; Global counting of TODO items
(setq org-hierarchical-todo-statistics nil)

;; Global counting of checked TODO items
(setq org-hierarchical-checkbox-statistics nil)

;; ;;;###autoload
;; (defun org-dblock-write:block-update-time (params)
;;   "block-update-time"
;;   (let ((fmt (or (plist-get params :format) "%Y-%m-%d")))
;;     (i'nsert "" (format-time-string fmt (current-time)))))

(setq org-image-actual-width '(256))
(add-to-list 'image-file-name-extensions "jp2")
;; (add-to-list 'image-file-name-extensions "j2c")
(add-to-list 'image-file-name-extensions "bmp")
(add-to-list 'image-file-name-extensions "psd")

(push '("[rR][eE][aA][dD][mM][eE]" . org-mode) auto-mode-alist)

;; Select from Preferences: { Funk | Glass | ... | Purr | Pop ... }
(defvar ns-default-notification-sound "Pop")

(defvar ns-alerter-command (concat (getenv "HOME") "/Dropbox/bin/alerter")
  "Path to alerter command. see https://github.com/vjeantet/alerter")
(setq ns-alerter-command 'script) ;; the alerter is not work for now(2024-02-18).
(unless ns-alerter-command
  (setq ns-alerter-command "")) ;; FIXME
(when (or (eq ns-alerter-command 'script)
          (executable-find ns-alerter-command))
  (setq org-show-notification-handler #'my-desktop-notification-handler))

(unless noninteractive
  (let ((file "~/Dropbox/org/db/daily.org"))
    (when (and (file-exists-p file)
               (require 'utility nil t))
      (my-set-alarms-from-file file) ;; init
      (add-hook 'after-save-hook #'my-update-alarms-from-file)))) ;; update

(defvar my-org-link-prompt "Link:")

(org-defkey org-mode-map (kbd "M-p") #'my-org-meta-next)
(org-defkey org-mode-map (kbd "M-n") #'my-org-meta-previous)
(org-defkey org-mode-map (kbd "M-b") #'my-org-meta-backward)
(org-defkey org-mode-map (kbd "M-f") #'my-org-meta-forward)

(with-eval-after-load "eldoc"
  (defvar my-eldoc-disable-in-org-block nil)
  (advice-add 'eldoc-print-current-symbol-info :around
              #'my--eldoc-print-current-symbol-info))

(advice-add 'org-reveal :around #'my--org-reveal)

(defface my-org-emphasis-bold
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#5b5caf" :background "#e6ebfa") ;; #a60000 #4E4F97 #c7e9fa
    (((class color) (min-colors 88) (background dark))
     :foreground "#99B2FF")) ;; #ff8059 #BCBCDB #6666D6 #879EE2
  "My bold emphasis for Org."
  :group 'my)

(defface my-org-emphasis-italic
  '((default :inherit italic)
    (((class color) (min-colors 88) (background light))
     :foreground "#005e00" :background "#B4EAB4")
    (((class color) (min-colors 88) (background dark))
     :foreground "#44bc44"))
  "My italic emphasis for Org."
  :group 'my)

(defface my-org-emphasis-underline
  '((default :inherit underline)
    (((class color) (min-colors 88) (background light))
     :foreground "#813e00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#d0bc00"))
  "My underline emphasis for Org."
  :group 'my)

(defface my-org-emphasis-strike-through
  '((((class color) (min-colors 88) (background light))
     :strike-through "#972500" :foreground "#505050")
    (((class color) (min-colors 88) (background dark))
     :strike-through "#ef8b50" :foreground "#a8a8a8"))
  "My strike-through emphasis for Org."
  :group 'my)

(custom-set-variables ;; call org-set-emph-re
 '(org-emphasis-alist '(("~" org-code verbatim)
                        ("=" org-verbatim verbatim)
                        ("*" my-org-emphasis-bold)
                        ("/" my-org-emphasis-italic)
                        ("_" my-org-emphasis-underline)
                        ("+" my-org-emphasis-strike-through))))

(custom-set-faces
 '(org-code
   ((t (:foreground "red" :background "pink" :inherit shadow))))
 '(org-verbatim
   ((t (:foreground "#ff6059" :background "PeachPuff" :inherit shadow)))))

(when (featurep 'org-extra-emphasis)
  (org-extra-emphasis-update)) ;; to apply configured `org-emphasis-alist'

(keymap-set org-mode-map "C-c x" #'my-org-move-item-end)
(keymap-set org-mode-map "C-c X" #'my-org-move-item-begin)

;; (キャプチャ時に)作成日時をプロパティに入れる
;; Thanks to https://emacs.stackexchange.com/questions/21291/add-created-timestamp-to-logbook
(defvar my-org-created-property-name "CREATED"
  "The name of the org-mode property.
This user property stores the creation date of the entry")
(advice-add 'org-insert-todo-heading :after #'my--org-insert-todo-heading)

(when (autoload-if-found '(org-capture)
                         "org-capture" nil t)

  (with-eval-after-load "org-capture"
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
             (file+headline ,org-default-notes-file "INBOX") "** TODO %?\n")
            ("a" "記事リストにエントリー" entry
             (file+headline ,org-capture-article-file "INBOX")
             "** READ %?\n\t")
            ("c" "同期カレンダーにエントリー" entry
             (file+headline ,org-capture-ical-file "Scheduled")
             "** TODO %?\n\t")
            ("d" "Doingタグ付きのタスクをInboxに投げる" entry
             (file+headline ,org-default-notes-file "INBOX")
             "** TODO %? :Doing:\n  - \n"
             :clock-in t
             :clock-keep t)
            ("l" "本日のチェックリスト" entry
             (file+headline ,org-capture-diary-file "Today")
             "** FOCUS 本日のチェックリスト %T\n（起床時間の記録）[[http://www.hayaoki-seikatsu.com/users/takaxp/][早起き日記]] \n（朝食）\n  - [ ] %?\n（昼食）\n（帰宅／夕食）\n----\n（研究速報）\n  - [ ] \n")
            ("i" "アイディアを書き込む" entry (file+headline ,org-default-notes-file "INBOX")
             "** %?\n  - \n\t%U")
            ("b" "Create new post for imadenale blog" entry
             (file+headline ,org-capture-blog-file ,(format-time-string "%Y"))
             "** TODO \n:PROPERTIES:\n:EXPORT_FILE_NAME: %?\n:EXPORT_HUGO_TAGS: \n:EXPORT_HUGO_LASTMOD: \n:EXPORT_HUGO_IMAGES: \n:END:\n{{< tweet user=\"takaxp\" id=\"\" >}}\n")
            ("B" "Create new post for imadenale blog (UUID)" entry
             (file+headline ,org-capture-blog-file ,(format-time-string "%Y"))
             "** TODO %?\n:PROPERTIES:\n:EXPORT_FILE_NAME: %(uuid-string)\n:EXPORT_HUGO_TAGS: \n:EXPORT_HUGO_LASTMOD: \n:EXPORT_HUGO_IMAGES: \n:END:\n{{< tweet user=\"takaxp\" id=\"\" >}}\n")
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

;; アジェンダ作成対象（指定しないとagendaが生成されない）
;; ここを間違うと，MobileOrg, iCal export もうまくいかない
(dolist (file (mapcar
               (lambda (arg)
                 (concat (getenv "SYNCROOT") "/org/" arg))
               '("org-ical.org" "next.org" "db/cooking.org" "minutes/wg1.org"
                 "db/daily.org" "db/trigger.org"  "academic.org" "tr/work.org"
                 "org2ja.org" "itr.org" "db/books.org")))
  (when (file-exists-p (expand-file-name file))
    (add-to-list 'org-agenda-files file 'append)))

(when (eq system-type 'windows-nt) ;; FIXME
  (setq org-agenda-files '("~/Dropbox/org/next.org")))

(with-eval-after-load "org-agenda"
  ;; sorting strategy
  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up timestamp-up priority-down category-keep)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep)))

  ;; Set the view span as day in an agenda view, the default is week
  (setq org-agenda-span 'day)

  ;; アジェンダに警告を表示する期間
  (setq org-deadline-warning-days 8)

  ;; 時間幅が明示的に指定されない場合のデフォルト値（分指定）
  (setq org-agenda-default-appointment-duration 60)

  ;; アジェンダビューでFOLLOWを設定（自動的に別バッファに当該タスクを表示）
  (setq org-agenda-start-with-follow-mode t)

  ;; Customized Time Grid
  (setq org-agenda-time-grid ;; Format is changed from 9.1
        '((daily today require-timed)
          (0800 1000 1200 1400 1600 1800 2000 2200 2400)
          "......"
          "------------------------"
          ))

  ;; (setq org-agenda-current-time-string "<  d('- ' ｲﾏｺｺ)")
  (setq org-agenda-current-time-string "<<< ｲﾏｺｺ")
  (setq org-agenda-timegrid-use-ampm t)

  ;; org-agenda 表示の水平方向の冗長さを削減
  (setq org-agenda-prefix-format
        '((agenda  . "%-9c| %?-12t% s")
          (todo  . " %i %-12:c")
          (tags  . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-agenda-remove-tags t)
  (setq org-agenda-scheduled-leaders '("[S]" "S.%2dx:\t"))
  (setq org-agenda-deadline-leaders '("[D]" "In %3d d.:\t" "%2d d. ago:\t"))

  (with-eval-after-load "moom"
    (defvar my-org-tags-column org-tags-column)
    ;; (add-hook 'org-agenda-mode-hook #'my-agenda-frame-width)
    ;; (advice-add 'org-agenda--quit :after #'my--org-agenda--quit)
    )

  ;; 移動直後にagendaバッファを閉じる（ツリーの内容はSPACEで確認可）
  (org-defkey org-agenda-mode-map [(tab)]
              (lambda () (interactive)
                (org-agenda-goto)
                (with-current-buffer "*Org Agenda*"
                  (org-agenda-quit))))

  ;; agenda アイテムの内容を別バッファに表示する時に，内容の全体を表示する
  (add-hook 'org-agenda-after-show-hook #'my-recenter-top-bottom-top)

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
  (my-popup-agenda-set-timers)
  (run-at-time "24:00" nil 'my-popup-agenda-set-timers)

  (when (memq window-system '(mac ns))
    (my-popup-calendar-set-timers)
    (run-at-time "24:00" nil 'my-popup-calendar-set-timers))

  (org-defkey org-agenda-mode-map "d" 'my-org-agenda-done)
  (org-defkey org-agenda-mode-map "D" 'my-org-todo-complete-no-repeat)

  ;; org-agenda の表示高さを 50% に固定する
  (setq org-agenda-window-frame-fractions '(0.5 . 0.5))

  ;; ;; Distinguish Repeated Tasks in Org Agenda
  ;; ;; https://whhone.com/posts/org-agenda-repeated-tasks/
  ;; (defun my--org-agenda-repeater ()
  ;;   "The repeater shown in org-agenda-prefix for agenda."
  ;;   (if (org-before-first-heading-p)
  ;;       "-------"  ; fill the time grid
  ;;     (format "%5s: " (or (org-get-repeat) ""))))
  ;; ;; Add `my/org-agenda-repeater' to the agenda prefix.
  ;; (setcdr (assoc 'agenda org-agenda-prefix-format)
  ;;         " %i %-12:c%?-12t%s%(my--org-agenda-repeater)")
  )

;; M-x calendar の動作に近づける．なお today への移動は，"C-." で可能．
(with-eval-after-load "org-keys"
  (org-defkey org-read-date-minibuffer-local-map (kbd "C-n")
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-forward-week 1))))
  (org-defkey org-read-date-minibuffer-local-map (kbd "C-p")
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-backward-week 1))))
  (org-defkey org-read-date-minibuffer-local-map (kbd "C-b")
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-backward-day 1))))
  (org-defkey org-read-date-minibuffer-local-map (kbd "C-f")
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-forward-day 1))))
  (org-defkey org-read-date-minibuffer-local-map (kbd "q")
              (lambda () (interactive)
                (org-eval-in-calendar '(minibuffer-keyboard-quit)))))

(add-hook 'org-cycle-hook #'org-onit-clock-in-when-unfold)
(keymap-set org-mode-map "<f11>" 'org-onit-toggle-doing)
(keymap-set org-mode-map "M-<f11>" 'org-onit-toggle-auto)
(keymap-set org-mode-map "S-<f11>" 'org-onit-goto-anchor)

(when (autoload-if-found '(org-onit-toggle-doing
                           org-onit-mode
                           org-onit-toggle-auto org-clock-goto
                           my-sparse-doing-tree org-onit-clock-in-when-unfold
                           org-clock-goto org-onit-update-options)
                         "org-onit" nil t)
  (keymap-global-set "C-<f11>" 'org-clock-goto)

  (with-eval-after-load "org-onit"
    (autoload-if-found '(org-bookmark-jump org-bookmark-make-record)
                       "org-bookmark-heading" nil t)
    (when (require 'org-plist nil t)
      (add-to-list 'org-plist-dict '("OPTIONS_ONIT" org-onit-basic-options)))
    (custom-set-variables
     '(org-onit-basic-options '(:wakeup nil :nostate doing :unfold nil))))

  (with-eval-after-load "org-clock"
    (add-hook 'org-onit-after-jump-hook #'my-onit-reveal)
    (add-hook 'org-clock-in-hook #'my-clear-undo-list) ;; for testing...

    (setq org-clock-clocked-in-display 'frame-title) ;; or 'both
    (setq org-clock-frame-title-format
          '((:eval (format "%s%s |%s|%s"
                           (if (and (require 'org-clock-today nil t)
                                    org-clock-today-mode)
                               (if org-clock-today-count-subtree
                                   (format "%s / %s"
                                           org-clock-today-subtree-time
                                           org-clock-today-buffer-time)
                                 (format "%s" org-clock-today-buffer-time))
                             "")
                           (if org-onit--auto-clocking " Auto " "")
                           (org-onit-get-sign)
                           org-mode-line-string))
            " - %b"))))

(org-defkey org-mode-map (kbd "C-c C-s") 'orgbox-schedule)

(when (autoload-if-found '(orgbox-schedule orgbox-agenda-schedule)
                         "orgbox" nil t)
  
  (with-eval-after-load "org-agenda"
    (org-defkey org-agenda-mode-map (kbd "C-c C-s") 'orgbox-agenda-schedule)))
;; (require 'orgbox nil t)) ;; require org-agenda

;; キャプチャ直後に更新
(add-hook 'org-capture-before-finalize-hook #'my-org-agenda-to-appt)

;; アジェンダを開いたら・終了したらアラームリストを更新
(unless noninteractive
  (add-hook 'org-agenda-mode-hook #'my-org-agenda-to-appt)
  (add-hook 'org-finalize-agenda-hook #'my-org-agenda-to-appt))

;; org-agenda-to-appt を非同期で使うための advice
(advice-add 'read-char-exclusive :around #'my--read-char-exclusive)
(advice-add 'org-check-agenda-file :override #'my--org-check-agenda-file)

(when (eq window-system 'w32)
  (message "--- my-org-agenda-to-appt-async was changed to nil for w32")
  (setq my-org-agenda-to-appt-async nil))

(when noninteractive
  (setq my-org-agenda-to-appt-ready nil)) ;; FIXME

;; (with-eval-after-load "org-agenda"
;;   (unless noninteractive
;;     (appt-activate 1)))

;; リファイル先でサブディレクトリを指定するために一部フルパス化
(let ((dir (expand-file-name org-directory)))
  (setq org-refile-targets
        `((,(concat dir "next.org") :level . 1)
          (,(concat dir "org-ical.org") :level . 1)
          (,(concat dir "itr.org") :level . 1)
          (,(concat dir "academic.org") :level . 1)
          (,(concat dir "tr/work.org") :level . 1)
          (,(concat dir "minutes/wg1.org") :level . 1)
          (,(concat dir "db/article.org") :level . 1)
          (,(concat dir "db/maybe.org") :level . 1)
          (,(concat dir "db/english.org") :level . 1)
          (,(concat dir "db/money.org") :level . 1))))

;; 不要な履歴が生成されるのを抑制し，常に最新を保つ．
;; [2/3]のような完了数が見出しにある時に転送先候補が重複表示されるため．
(advice-add 'org-refile :around #'my--org-refile)
(advice-add 'org-sort-entries :after #'my--org-sort-entries)

;; will take 200[ms]
(unless noninteractive
  (run-with-idle-timer (+ 7 my-default-loading-delay)
                       nil #'my-org-babel-load-activate))

(with-eval-after-load "ob-src"
  ;; 実装済みの言語に好きな名前を紐付ける
  (add-to-list 'org-src-lang-modes '("cs" . csharp))
  (add-to-list 'org-src-lang-modes '("zsh" . sh)))

(with-eval-after-load "ob-core"
  ;; Suppress showing of "Indentation variables are now local."
  (advice-add 'sh-make-vars-local :around #'my--suppress-message)
  ;; Suppress showing of "Setting up indent for shell type zsh" and
  ;; "Indentation setup for shell type zsh"
  (advice-add 'sh-set-shell :around #'my--suppress-message)

  (setq org-edit-src-content-indentation 0)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  ;; org-src-window-setup (current-window, other-window, other-frame)

  ;; ditta
  ;; (when (and (not noninteractive)
  ;;            (not (executable-find "ditaa")))
  ;;   (message "--- ditaa is NOT installed."))

  ;; (my-org-babel-load-activate)
  )

(add-to-list 'org-structure-template-alist
             (if (version< "9.1.4" (org-version))
                 '("S" . "src emacs-lisp")
               '("S" "#+begin_src emacs-lisp\n?\n#+END_SRC" "<src lang=\"emacs-lisp\">\n\n</src>")))

(with-eval-after-load "org-src"
  ;; (my-org-src-block-face)
  (add-hook 'ah-after-enable-theme-hook #'my-org-src-block-face)

  (custom-set-faces
   ;; org-block が効かない(2021-04-13@9.4.4)，org-src-block-faces で対応
   ;; '(org-block
   ;;   ((((background dark)) (:background "#383c4c" :extend t)
   ;;     (t (:background "#F9F9F9" :extend t)))))
   '(org-block-begin-line
     ((((background dark))
       (:foreground "#669966" :weight bold)) ;; :background "#444444"
      (t (:foreground "#CC3333" :weight bold)))) ;; :background "#EFEFEF"
   '(org-block-end-line
     ((((background dark)) (:foreground "#CC3333" :weight bold))
      (t (:foreground "#669966" :weight bold))))
   ;; '(org-block-end-line
   ;;   ((((background dark)) (:inherit org-block-begin-line))
   ;;    (t (:inherit org-block-begin-line))))
   ))

(when (autoload-if-found '(org-tree-slide-mode)
                         "org-tree-slide" nil t)

  (keymap-global-set "<f8>" 'org-tree-slide-mode)
  (keymap-global-set "S-<f8>" 'org-tree-slide-skip-done-toggle)

  (with-eval-after-load "org-tree-slide"
    ;; <f8>/<f9>/<f10>/<f11> are assigned to control org-tree-slide
    (keymap-set org-tree-slide-mode-map "<f9>"
                'org-tree-slide-move-previous-tree)
    (keymap-set org-tree-slide-mode-map "<f10>"
                'org-tree-slide-move-next-tree)
    (unless noninteractive
      (org-tree-slide-narrowing-control-profile))
    (setopt org-tree-slide-modeline-display 'outside
            org-tree-slide-skip-outline-level 5
            org-tree-slide-skip-done nil)))

(with-eval-after-load "org-tree-slide"
  (when (and (eq my-toggle-modeline-global 'doom)
             (require 'doom-modeline nil t))
    (add-hook 'org-tree-slide-stop-hook
              #'doom-modeline-update-buffer-file-state-icon)))

(when (autoload-if-found '(org-tree-slide-mode my-toggle-proportional-font)
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

    (add-hook 'org-tree-slide-before-narrow-hook
              (lambda ()
                (if (equal "PROPORTIONAL"
                           (org-entry-get-with-inheritance "FONT"))
                    (buffer-face-set 'variable-pitch)
                  (buffer-face-mode 0))))
    (add-hook 'org-tree-slide-stop-hook
              (lambda ()
                (buffer-face-mode 0)))))

(defvar my-hide-org-meta-line-p nil)
(add-hook 'org-tree-slide-play-hook #'my-hide-org-meta-line)
(add-hook 'org-tree-slide-stop-hook #'my-show-org-meta-line)
(add-hook 'ah-after-enable-theme-hook #'my-update-org-meta-line)

(when (autoload-if-found '(ox-odt)
                         "ox-odt" nil t)
  (with-eval-after-load "ox-odt"
    ;; (add-to-list 'org-odt-data-dir
    ;;              (concat (getenv "HOME") "/Dropbox/usr/emacs.d/config/"))
    (setq org-odt-styles-file
          (concat (getenv "SYNCROOT") "/usr/emacs.d/config/style.odt"))
    ;; (setq org-odt-content-template-file
    ;;       (concat (getenv "HOME") "/Dropbox/usr/emacs.d/config/style.ott"))
    (setq org-odt-preferred-output-format "pdf") ;; docx
    ;; ;; ox-odt.el の 自作パッチの変数（DOCSTRINGが記述されていない）
    ;; (setq org-odt-apply-custom-punctuation t)
    (setq org-odt-convert-processes
          '(("LibreOffice"
             "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")
            ("unoconv" "unoconv -f %f -o %d %i")))))

(with-eval-after-load "ox"
  (require 'ox-twbs nil t))

(when (require 'org-crypt nil t)
  (require 'epa)
  (setq org-crypt-key "") ;; <insert your key>
  ;; org-encrypt-entries の影響を受けるタグを指定
  (setq org-tags-exclude-from-inheritance (quote ("secret")))
  ;; 自動保存の確認を無効に
  (setq org-crypt-disable-auto-save 'nil))

;; (add-to-list 'org-modules 'org-mac-iCal)
;; (add-to-list 'org-modules 'org-mac-link) ;; includes org-mac-message

(autoload 'org-mac-link-get-link "org-mac-link" nil t)
(defun my-disable-org-mac-link-get-link ()
  (interactive)
  (message "As of 2025-10-18, `org-mac-link-get-link' is not working on Tahoe"))
;; (keymap-set org-mode-map "C-c c" 'org-mac-link-get-link)
(keymap-set org-mode-map "C-c c" 'my-disable-org-mac-link-get-link)
(with-eval-after-load "org-mac-link"
  (require 'org-mac-iCal nil t))

(with-eval-after-load "org-attach"
  (when (require 'org-download nil t)
    (setq org-download-screenshot-method 'screencapture)
    (setq org-download-method 'attach)))

(when (autoload-if-found '(org-grep)
                         "org-grep" nil t)
  ;;   (keymap-global-set "C-M-g" 'org-grep)
  (with-eval-after-load "org-grep"
    (setq org-grep-extensions '(".org" ".org_archive"))
    (add-to-list 'org-grep-directories "~/.emacs.d")
    (add-to-list 'org-grep-directories "~/.emacs.d/.cask/package")

    ;; "q"押下の挙動を調整
    (advice-add 'org-grep-quit :override #'my--org-grep-quit)))

(with-eval-after-load "ox"
  (when (and (require 'ox-reveal nil t)
             (version< "9.1.4" (org-version)))
    (setq org-reveal-note-key-char ?n)))

(with-eval-after-load "org-clock"
  (advice-add 'org-clock-sum-today :override #'my--org-clock-sum-today)

  ;; using folked package
  (when (require 'org-clock-today nil t)
    (unless noninteractive
      (setq org-clock-today-count-subtree t)
      (org-clock-today-mode 1))))

;; see https://ox-hugo.scripter.co/doc/deprecation-notices/#org-hugo-auto-export-feature-now-a-minor-mode
;; (with-eval-after-load "org"
;; No need for latest ox-hugo
;;   ;; Require ox-hugo-auto-export.el explictly before loading ox-hugo.el
;;   (require 'ox-hugo-auto-export nil t))

;; see https://pxaka.tokyo/blog/2018/a-link-to-the-original-org-source-file
(when (autoload-if-found
       '(org-hugo-export-wim-to-md)
       "ox-hugo" nil t)

  (with-eval-after-load "ox-hugo"
    (setq org-hugo-auto-set-lastmod nil) ;; see my-hugo-export-md
    (setq org-hugo-suppress-lastmod-period 86400.0) ;; 1 day
    ;; never copy files to under /static/ directory
    (setq org-hugo-external-file-extensions-allowed-for-copying nil)
    ;;    (advice-add 'org-todo :after #'my--ox-hugo:org-todo)
    ))

(defvar md-link-format "^!\\[\\(.+\\)\\](\\(.+\\))$")
(with-eval-after-load "ox-html"
  (setq org-html-text-markup-alist
        '((bold . "<b>%s</b>")
          (code . "<code class=\"org-code\">%s</code>")
          (italic . "<i>%s</i>")
          (strike-through . "<del>%s</del>")
          (underline . "<span class=\"org-underline\">%s</span>")
          (verbatim . "<code class=\"org-verbatim\">%s</code>"))))

(when (autoload-if-found '(orglink-mode
                           global-orglink-mode my-orglink-mode-activate)
                         "orglink" nil t)

  (dolist (hook '(emacs-lisp-mode-hook c-mode-common-hook yatex-mode))
    (add-hook hook #'my-orglink-mode-activate))

  (with-eval-after-load "orglink"
    (delq 'angle orglink-activate-links)
    (keymap-set orglink-mouse-map "C-c C-o" 'org-open-at-point-global)
    (keymap-set orglink-mouse-map "C-c C-l" 'org-insert-link)))

;; (add-to-list 'orglink-activate-in-modes 'c++-mode)
;; (add-to-list 'orglink-activate-in-modes 'c-mode)
;; (when (require 'orglink nil t)
;;   (global-orglink-mode))

(with-eval-after-load "ox"
  (defvar my-org-export-before-hook nil)
  (defvar my-org-export-after-hook nil)
  (defvar my-org-export-last-buffer nil)
  (advice-add 'org-export-dispatch :around #'my--org-export-dispatch)
  (advice-add 'org-export-insert-default-template :around
              #'my--org-export-insert-default-template)
  (advice-add 'org-export-to-buffer :after #'my--org-export-to-buffer)
  (add-hook 'my-org-export-after-hook #'my-copy-exported-buffer))

(autoload 'skewer-html-mode "skewer-html" nil t)
(unless noninteractive
  (add-hook 'org-mode-hook 'skewer-html-mode)
  (autoload-if-found '(org-extra-emphasis-mode) "org-extra-emphasis" nil t))

(when (autoload-if-found '(org-appear-mode)
                         "org-appear" nil t)
  (setq org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook 'org-appear-mode)
  (with-eval-after-load "org-appear"
    (setq org-appear-trigger 'on-change))) ;; 編集中だけマークアップを表示できる

(with-eval-after-load "ob-async"
  (custom-set-variables
   '(ob-async-no-async-languages-alist '("ipython"))))

(when (autoload-if-found '(org-tree-slide-mode)
                         "org-tree-slide" nil t)
  (with-eval-after-load "org-tree-slide"
    ;; FIXME 複数のバッファで並行動作させるとおかしくなる．hide-lines の問題？
    ;; prettify-symbols で置き換えるほうが良い
    (when (and nil (require 'hide-lines nil t))
      (defvar my-org-src-block-faces nil)
      (add-hook 'org-tree-slide-play-hook #'my-hide-headers)
      (add-hook 'org-tree-slide-stop-hook #'my-show-headers)

      ;; (defun my--org-edit-src-code (&optional code edit-buffer-name)
      (advice-add 'org-edit-src-code :before #'my--org-edit-src-code)
      ;; Block 外で呼ばれると，my-show-headers が呼ばれてしまう
      (advice-add 'org-edit-src-exit :after #'my--org-edit-src-exit))))

(with-eval-after-load "ox"
  ;; (setq org-export-default-language "ja")
  (if (eq system-type 'darwin)
      (require 'ox-pandoc nil t)
    (message "--- pandoc is NOT configured for Windows or Linux."))
  (require 'ox-qmd nil t) ;; Quita-style
  (require 'ox-gfm nil t)) ;; GitHub-style

(when (eq system-type 'darwin)
  ;; Open `papers3://' link by C-c C-o.
  ;; (org-add-link-type will be obsoleted from Org 9.
  (when (fboundp 'org-link-set-parameters)
    (org-link-set-parameters
     "papers3"
     :follow (lambda (path)
               (let ((cmd (concat "open papers3:" path)))
                 (shell-command-to-string (shell-quote-argument cmd))
                 (message "%s" cmd))))))

(when (autoload-if-found '(org-attach du-org-attachments)
                         "org-attach" nil t)
  (with-eval-after-load "org-attach"
    ;; org-insert-link で添付ファイルへのリンクをペーストできるようにする
    (setq org-attach-store-link-p t)
    ;; 自動付与されるタグの名前を変更
    (setq org-attach-auto-tag "Attach")
    ;; git-annex を使用しない
    (setq org-attach-git-annex-cutoff nil)

    (defvar org-attach-directory-absolute
      (concat (getenv "SYNCROOT")
              "/org/"
              (when (boundp 'org-attach-directory)
                "data/")))))

(autoload-if-found '(orgnav-search-root) "orgnav" nil t)
(keymap-set org-mode-map "C-c f n"
            (lambda () (interactive)
              (orgnav-search-root 3 'orgnav--goto-action)))

(autoload-if-found '(toc-org-insert-toc) "toc-org" nil t)

(when (autoload-if-found '(org-attach-screenshot)
                         "org-attach-screenshot" nil t)
  (with-eval-after-load "org-attach-screenshot"
    (when (executable-find "screencapture")
      (setq org-attach-screenshot-command-line "screencapture -w %f"))))

(with-eval-after-load "org-agenda"
  (advice-add 'org-agenda :after #'my--org-agenda)
  (advice-add 'org-agenda-redo :after #'my--org-agenda-redo))

(with-eval-after-load "org-src"
  ;; tab-width=8, indent-tabs-mode=t
  (advice-add 'org-edit-special :after #'my--format-emacs-lisp-buffer)
  ;; tab-width=2, indent-tabs-mode=nil
  (advice-add 'org-edit-src-abort :before
              #'my--format-emacs-lisp-for-org-buffer)
  (advice-add 'org-edit-src-exit :before
              #'my--format-emacs-lisp-for-org-buffer))

(defvar my-init-org-end (current-time))
(defun my-emacs-init-org-time ()
  (interactive)
  (let ((inhibit-message t))
    (message "Loading init-org.el...done (%4d [ms])"
             (* 1000
                (float-time (time-subtract
                             my-init-org-end
                             my-init-org-start))))))
(provide 'init-org)
