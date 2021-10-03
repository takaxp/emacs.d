;; init-org.el --- My config for org mode -*- lexical-binding: t -*-
(require 'init-autoloads nil t)
(require 'late-init-autoloads nil t)
(require 'utility-autoloads nil t)

(when (autoload-if-found
       '(org-mode)
       "org" nil t)

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

  (with-eval-after-load "org"
    ;; タイトルを少し強調
    (custom-set-faces
     '(org-document-title ((t (:foreground "RoyalBlue1" :bold t :height 1.2))))
     '(org-document-info ((t (:foreground "DodgerBlue1" :height 1.0)))))

    ;; 関連モジュールの読み込み
    ;; (require 'org-mobile nil t)
    (when (require 'org-eldoc nil t)
      (defun my-org-eldoc-load ()
        "Set up org-eldoc documentation function."
        (interactive)
        (add-function :before-until (local 'eldoc-documentation-function)
                      #'org-eldoc-documentation-function))
      (advice-add 'org-eldoc-load :override #'my-org-eldoc-load))

    ;; モジュールの追加
    (add-to-list 'org-modules 'org-id)
    (with-eval-after-load "org-agenda"
      ;; org-agenda を読んでしまうので org-mode 開始時には読み込ませない
      (add-to-list 'org-modules 'org-habit)) ;; require org and org-agenda
    (when (version< "9.1.4" (org-version))
      (add-to-list 'org-modules 'org-tempo))
    (when (require 'ol-bookmark nil t)
      ;; [[bookmark:hoge][hogehoge]] 形式のリンクを有効化
      (add-to-list 'org-modules 'ol-bookmark)
      (setq bookmark-save-flag 4) ;; N回 bookmark を操作したら保存
      ;; `bookmark-default-file' の読み込み
      (bookmark-maybe-load-default-file))

    ;; 不必要なモジュールの読み込みを停止する
    (delq 'ol-gnus org-modules)
    ;; (setq org-modules (delete 'org-bibtex org-modules))

    ;; org ファイルの集中管理
    (setq org-directory (concat (getenv "SYNCROOT") "/org/"))

    ;; org-store-link で heading に自動的に挿入される id を使う
    (setq org-id-link-to-org-use-id t)

    ;; アーカイブファイルの名称を指定
    (setq org-archive-location "%s_archive::")

    ;; タイムスタンプによるログ収集設定 DONE 時に CLOSED: を記入．
    (setq org-log-done 'time) ; 'time 以外に，'(done), '(state) を指定できる

    ;; ログをドロアーに入れる
    (setq org-log-into-drawer t)

    ;; indent を electric-indent-mode の振る舞いに合わせる
    (setq org-adapt-indentation t)    

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
    (defun my-promote-todo-revision (state)
      (cond ((member state '("TODO")) "REV1")
            ((member state '("REV1")) "REV2")
            ((member state '("REV2")) "REV3")
            (t state)))
    ;; (setq org-clock-out-switch-to-state #'my-promote-todo-revision)

    ;; undo 時に reveal して表示を改善する
    ;; (defun ad:org:undo (&optional _ARG)
    ;;   (when (and (eq major-mode 'org-mode)
    ;;              (not (org-before-first-heading-p)))
    ;;     (org-overview)
    ;;     (org-reveal)
    ;;     (org-cycle-hide-drawers 'all)
    ;;     (org-show-entry)
    ;;     (show-children)
    ;;     (org-show-siblings)))
    ;; (advice-add 'undo :after #'ad:org:undo)

    ;; 非表示状態の領域への書き込みを防ぐ
    ;; "Editing in invisible areas is prohibited, make them visible first"
    (setq org-catch-invisible-edits 'show-and-error)
    (defun ad:org-return (f &optional arg)
      "An extension for checking invisible editing when you hit the enter."
      (interactive "P")
      (org-check-before-invisible-edit 'insert)
      (apply f arg))
    (advice-add 'org-return :around #'ad:org-return)

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
    (defun my-org-hide-drawers ()
      "Hide all drawers in an org tree."
      (interactive)
      (save-excursion
        (beginning-of-line)
        (unless (looking-at-p org-drawer-regexp)
          (org-cycle-hide-drawers 'subtree))))
    (add-hook 'org-tab-first-hook 'my-org-hide-drawers)

    ;; CSV指定でテーブルを出力する．
    (defun my-org-table-export ()
      (interactive)
      (org-table-export nil "orgtbl-to-csv"))

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

  (with-eval-after-load "ox"
    (add-to-list 'org-modules 'ox-odt)
    (add-to-list 'org-modules 'ox-org)
    (add-to-list 'org-modules 'ox-json))

  (with-eval-after-load "org-tempo"
    ;; 空行のとき "<" をインデントさせない
    (defun ad:org-tempo-complete-tag (f &rest arg)
      (if (save-excursion
            (beginning-of-line)
            (looking-at "<"))
          (let ((indent-line-function 'ignore))
            (apply f arg))
        (apply f arg)))
    (advice-add 'org-tempo-complete-tag :around #'ad:org-tempo-complete-tag))
  ;; (Thanks to @conao3)
  ;; but when using `flet', byte-compile will warn a malformed function
  ;; and using `cl-flet' will not provide us the expected result...
  ;; (when (require 'cl-lib nil t)
  ;;   (defun ad:org-tempo-complete-tag (f &rest arg)
  ;;     (if (save-excursion
  ;;           (beginning-of-line)
  ;;           (looking-at "<"))
  ;;         (cl-flet ((indent-according-to-mode () #'ignore))
  ;;           (apply f arg))
  ;;       (apply f arg)))
  ;;   (advice-add 'org-tempo-complete-tag :around #'ad:org-tempo-complete-tag))

  (with-eval-after-load "org-tempo"
    (defun my-org-tempo-add-block (entry)
      "Add block entry from `org-structure-template-alist'."
      (let* ((key (format "<%s" (car entry)))
             (name (cdr entry))
             (special nil)) ;; FIXED
        (tempo-define-template
         (format "org-%s" (replace-regexp-in-string " " "-" name))
         `(,(format "#+begin_%s%s" name (if special " " ""))
           ,(when special 'p) '> n '> ,(unless special 'p) n
           ,(format "#+end_%s" (car (split-string name " ")))
           >)
         key
         (format "Insert a %s block" name)
         'org-tempo-tags)))
    ;; 更新
    (advice-add 'org-tempo-add-block :override #'my-org-tempo-add-block)
    ;; 反映
    (org-tempo-add-templates))

  (with-eval-after-load "org-clock"
    ;; nil or 'history ならば，org-onit が org-clock-out を実行する．
    (setq org-clock-persist 'history) ;; {nil, t, 'clock, 'history}
    (setq org-clock-in-resume t)
    (setq org-clock-persist-query-resume nil)

    ;; 終了時に clock を止める．
    (defun my-org-clock-out-and-save-when-exit ()
      "Save buffers and stop clocking when kill emacs."
      (when (org-clocking-p)
        (org-clock-out)
        (save-some-buffers t)))
    ;; implemented in `org-onit.el'. No need to hook this.
    ;; (add-hook 'kill-emacs-hook #'my-org-clock-out-and-save-when-exit)
    )

  (with-eval-after-load "org-table"
    ;; エコー表示前に保存する
    (defun ad:org-table-field-info (_arg)
      (save-buffer))
    (advice-add 'org-table-field-info :before #'ad:org-table-field-info)))

;; ~/Dropbox/Public は第三者に探索される可能性があるので要注意
;; default = ~/org.ics
;; C-c C-e i org-export-icalendar-this-file
;; C-c C-e I org-export-icalendar-all-agenda-files
;; C-c C-e c org-export-icalendar-all-combine-agenda-files
(when (autoload-if-found
       '(my-ox-icalendar my-async-ox-icalendar my-ox-icalendar-cleanup)
       "ox-icalendar" nil t)

  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c f 1") 'my-ox-upload-icalendar))

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
    (setq org-icalendar-use-deadline '(event-if-todo))

    (defun my-ox-upload-icalendar ()
      (interactive)
      (when (and org-ical-file-in-orz-server
                 (eq system-type 'darwin))
        (if (require 'async nil t)
            (my-async-ox-icalendar)
          (my-ox-icalendar))))

    (defun my-ox-icalendar ()
      (let ((message-log-max nil)
            (org-agenda-files '("~/Dropbox/org/org-ical.org")))
        ;; org-icalendar-export-to-ics を使うとクリップボードが荒れる
        (org-icalendar-combine-agenda-files))
      ;; 自サーバにアップロード
      (message "Uploading...")
      (if (eq 0 (shell-command
                 (concat "scp -o ConnectTimeout=5 "
                         org-icalendar-combined-agenda-file " "
                         org-ical-file-in-orz-server)))
          (message "Uploading...done")
        (message "Uploading...miss!"))
      (my-ox-icalendar-cleanup))

    (defun my-async-ox-icalendar ()
      (message "[async] Uploading...")
      (async-start
       `(lambda ()
          (when (and (load "~/.emacs" nil t)
                     (load "~/.emacs.d/lisp/init-org.el" nil t)
                     (require 'org nil t)
                     (require 'org-agenda nil t))
            (setq org-agenda-files '("~/Dropbox/org/org-ical.org"))
            (if (file-exists-p
                 (expand-file-name org-icalendar-combined-agenda-file))
                1
              (let ((ical (org-icalendar-combine-agenda-files))
                    (result (shell-command
                             (concat "scp -o ConnectTimeout=5 "
                                     ',org-icalendar-combined-agenda-file " "
                                     ',org-ical-file-in-orz-server))))
                (my-ox-icalendar-cleanup)
                result))))
       (lambda (result)
         (unless (active-minibuffer-window)
           (message (format "[async] Uploading...%s"
                            (cond ((eq result 0) "done")
                                  ((eq result 1) "skipped")
                                  (t "miss!"))))))))

    (defun my-ox-icalendar-cleanup ()
      (interactive)
      (when (file-exists-p
             (expand-file-name org-icalendar-combined-agenda-file))
        (shell-command-to-string
         (concat "rm -rf " org-icalendar-combined-agenda-file))))))

(with-eval-after-load "org"
  (setq org-use-speed-commands t)

  (when (version< (org-version) "9.4.6")
    (defvaralias 'org-speed-commands 'org-speed-commands-user))

  (add-to-list 'org-speed-commands '("d" my-done-with-update-list))
  ;; (add-to-list 'org-speed-commands '("S" call-interactively 'widen))
  (add-to-list 'org-speed-commands
               '("D" my-org-todo-complete-no-repeat "DONE"))
  ;; (add-to-list 'org-speed-commands '("N" org-shiftmetadown))
  ;; (add-to-list 'org-speed-commands '("P" org-shiftmetaup))
  (add-to-list 'org-speed-commands '("H" my-hugo-export-upload))
  (add-to-list 'org-speed-commands '("." my-org-deadline-today))
  (add-to-list 'org-speed-commands '("!" my-org-default-property))
  (add-to-list 'org-speed-commands
               '("$" call-interactively 'org-archive-subtree))

  ;; done にして，apptを更新する
  (defun my-done-with-update-list ()
    (org-todo "DONE")
    (my-org-agenda-to-appt))

  ;; 周期タクスを終了させます．
  (defun my-org-todo-complete-no-repeat (&optional ARG)
    (interactive "P")
    (when (org-get-repeat)
      (org-cancel-repeater))
    (org-todo ARG))

  (defun my-org-replace-punc-in-buffer ()
    "Replace \"，\" and \"．\" with \"、\" and \"。\" in a buffer."
    (interactive)
    (goto-char (point-min))
    (while (re-search-forward "\\(，\\)\\|\\(．\\)" nil :noerror)
      (let ((w (match-string-no-properties 0)))
        (cond ((equal w "，") (replace-match "、"))
              ((equal w "．") (replace-match "。"))))))

  (defun my-org-replace-punc-in-tree ()
    "Replace \"，\" and \"．\" with \"、\" and \"。\" in an org tree."
    (interactive)
    (let* ((element (org-element-at-point))
           (begin (org-element-property :begin element))
           (end (org-element-property :end element)))
      (when (eq (org-element-type element) 'headline)
        (goto-char begin)
        (while (re-search-forward "\\(，\\)\\|\\(．\\)" end :noerror)
          (let ((w (match-string-no-properties 0)))
            (cond ((equal w "，") (replace-match "、"))
                  ((equal w "．") (replace-match "。")))))
        (goto-char begin))))

  ;; Hugo の記事を書き出し&アップロード
  (defun my-hugo-export-upload ()
    "Export subtree for Hugo and upload the engty."
    (when (member (buffer-name) '("imadenale.org" "archive.org"))
      (if (not (org-entry-is-done-p))
          (message "The state of the entry is not \"DONE\" yet.")
        (my-org-replace-punc-in-tree)
        (save-buffer)
        ;; (let ((outfile (org-hugo-export-wim-to-md)))
        ;;   (sit-for 2)
        ;;   (when (and outfile
        ;;              (file-exists-p outfile))
        ;;     (switch-to-buffer
        ;;      (find-file-noselect outfile)
        ;;      (my-org-replace-punc-in-buffer))))
        (org-hugo-export-wim-to-md)
        (let ((command "/Users/taka/Dropbox/scripts/push-hugo.sh")
              (filename (org-entry-get (point) "EXPORT_FILE_NAME"))
              (exported (format "[ox-hugo] \"%s\" has been exported."
                                (nth 4 (org-heading-components)))))
          (when filename
            ;; (when (file-exists-p (concat outfile ".md"))
            ;;   (switch-to-buffer
            ;;    (find-file-noselect (concat outfile ".md"))
            ;;    (my-org-replace-punc-in-buffer)
            ;;    (save-buffer)))
            (save-excursion
              (save-restriction
                (outline-up-heading 1)
                (setq filename
                      (concat (nth 4 (org-heading-components)) "/" filename))
                (setq command (concat command " -e" (downcase filename)))))
            (if (require 'async nil t)
                (progn
                  (message "%s\n[async] Uploading..." exported)
                  (async-start
                   `(lambda () (shell-command-to-string ',command))
                   `(lambda (result)
                      (message "%s\n[async] Uploading...%s"
                               ',exported (when result "done") ))))
              (message "%s\nUploading..." exported)
              (shell-command-to-string command)
              (message "%s\nUploading...done" exported)))))))

  ;; 締切を今日にする．agenda から起動したカレンダー内では "C-." でOK（標準）
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
          ("project"     :foreground "#6666CC")
          ("read"        :foreground "#6666CC")
          ("book"        :foreground "#6666CC")
          ("Doing"       :foreground "#FF0000")
          ("Draft"       :foreground "#9933CC") ;; Draft(r1,r2,r3)->Review(1,2)
          ("Review"      :foreground "#6633CC")
          ("Revisit"     :foreground "#6633CC")
          ("Redmine"     :foreground "#CC6666")
          ("Ongoing"     :foreground "#CC6666") ; for non scheduled/reminder
          ("Template"    :foreground "#66CC66")
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
          ("plan"        :foreground "#FF7D7D")
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

(unless noninteractive
  (with-eval-after-load "org"
    (let ((file "~/Dropbox/org/db/daily.org"))
      (when (and (file-exists-p file)
                 (require 'utility nil t))
        (my-set-alarms-from-file file) ;; init
        (add-hook 'after-save-hook #'my-update-alarms-from-file))))) ;; update

(with-eval-after-load "org"
  (defun my-countdown-timer-notify ()
    (when mode-line-format
      (my-mode-line-off))
    (when ns-alerter-command
      (setq org-show-notification-handler #'my-desktop-notification-handler))
    (remove-hook 'org-timer-done-hook #'my-countdown-timer-notify)
    (remove-hook 'org-timer-stop-hook #'my-countdown-timer-notify)
    (my-desktop-notification "### Expired! ###" "Time is up!" t "Glass"))

  (defalias 'run-timer 'my-countdown-timer)
  (defun my-countdown-timer ()
    (interactive)
    (unless mode-line-format
      (my-mode-line-on))
    (when (eq org-show-notification-handler #'my-desktop-notification-handler)
      (setq org-show-notification-handler nil))
    (with-temp-buffer
      (org-mode)
      (add-hook 'org-timer-done-hook #'my-countdown-timer-notify)
      (add-hook 'org-timer-stop-hook #'my-countdown-timer-notify)
      (org-timer-set-timer))))

(when (autoload-if-found
       '(org-mode my-load-echo-org-link)
       "org" nil t)

  (add-hook 'org-mode-hook #'my-load-echo-org-link)

  (with-eval-after-load "org"
    (defun my-echo-org-link ()
      (when (org-in-regexp org-link-bracket-re 1)
        (let ((link "Link:")
              (msg (org-link-unescape (match-string-no-properties 1))))
          (put-text-property 0 (length link) 'face 'minibuffer-prompt link)
          (eldoc-message (format "%s %s" link msg)))))

    (defun my-load-echo-org-link ()
      (add-function :before-until (local 'eldoc-documentation-function)
                    #'my-echo-org-link)
      ;; (setq-local eldoc-documentation-function #'my-echo-org-link)
      )))

(defun my-org-table-copy-as (&optional format)
  "Copy converted table."
  (interactive)
  (let ((format (or format
                    (org-entry-get (point) "TABLE_EXPORT_FORMAT" t)
                    org-table-export-default-format)))
    (if (string-match "\\([^ \t\r\n]+\\)\\( +.*\\)?" format)
	      (let ((transform (intern (match-string 1 format)))
	            (params (and (match-end 2)
			                     (read (concat "(" (match-string 2 format) ")"))))
	            (table (org-table-to-lisp)))
          (if (not (org-at-table-p))
              (user-error "The cursor is not at a table")
	          (with-temp-buffer
		          (insert (funcall transform table params) "\n")
              (clipboard-kill-ring-save (point-min) (point-max)))))
      (user-error "TABLE_EXPORT_FORMAT invalid"))))

(defun my-org-table-convert-to (&optional format)
  "Convert a table to FORMAT.
If FORMAT is nil, it is set equal to a property value specified
by \"TABLE_EXPORT_FORMAT\" or `org-table-export-default-format'.
Converted table is copied to kill ring for further use.
The core part is extracted from `org-table-export'."
  (interactive)
  (let ((format (or format
                    (org-entry-get (point) "TABLE_EXPORT_FORMAT" t)
                    org-table-export-default-format)))
    (if (string-match "\\([^ \t\r\n]+\\)\\( +.*\\)?" format)
	      (let ((transform (intern (match-string 1 format)))
	            (params (and (match-end 2)
			                     (read (concat "(" (match-string 2 format) ")"))))
	            (table (org-table-to-lisp)))
          (if (not (org-at-table-p))
              (user-error "The cursor is not at a table")
	          (kill-region (org-table-begin) (org-table-end))
	          (let ((begin (point)))
	            (insert (funcall transform table params))
	            (clipboard-kill-ring-save begin (point))
              (insert "\n"))))
      (user-error "TABLE_EXPORT_FORMAT invalid"))))

(when (autoload-if-found
       '(org-capture)
       "org-capture" nil t)

  (with-eval-after-load "org"
    ;; キャプチャ時に作成日時をプロパティに入れる
    ;; Thanks to https://emacs.stackexchange.com/questions/21291/add-created-timestamp-to-logbook
    (defun my-org-default-property ()
      "Set the creation date and org-id."
      (interactive)
      (my-org-set-created-property)
      (org-id-get-create))
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
          (org-set-property created now)
          (org-cycle-hide-drawers 'children))))
    (defun ad:org-insert-todo-heading (_arg &optional _force-heading)
      (unless (org-at-item-checkbox-p)
        (my-org-default-property)))
    (advice-add 'org-insert-todo-heading :after #'ad:org-insert-todo-heading))

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
             "** TODO \n:PROPERTIES:\n:EXPORT_FILE_NAME: %?\n:EXPORT_HUGO_TAGS: \n:EXPORT_HUGO_LASTMOD: \n:EXPORT_HUGO_IMAGES: \n:END:\n")
            ("B" "Create new post for imadenale blog (UUID)" entry
             (file+headline ,org-capture-blog-file ,(format-time-string "%Y"))
             "** TODO %?\n:PROPERTIES:\n:EXPORT_FILE_NAME: %(uuid-string)\n:EXPORT_HUGO_TAGS: \n:EXPORT_HUGO_LASTMOD: \n:EXPORT_HUGO_IMAGES: \n:END:\n")
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

(with-eval-after-load "org"
  ;; アジェンダ作成対象（指定しないとagendaが生成されない）
  ;; ここを間違うと，MobileOrg, iCal export もうまくいかない
  (dolist (file (mapcar
                 (lambda (arg)
                   (concat (getenv "SYNCROOT") "/org/" arg))
                 '("org-ical.org" "next.org" "db/cooking.org" "minutes/wg1.org"
                   "db/daily.org" "db/trigger.org"  "academic.org" "tr/work.org"
                   "org2ja.org" "itr.org")))
    (when (file-exists-p (expand-file-name file))
      (add-to-list 'org-agenda-files file 'append)))
  (when (eq system-type 'windows-nt) ;; FIXME
    (setq org-agenda-files '("~/Dropbox/org/next.org"))))

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
             (frame-focus-state))
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
  (org-defkey org-agenda-mode-map "d" 'my-org-agenda-done)

  ;; org-agenda の表示高さを 50% に固定する
  (setq org-agenda-window-frame-fractions '(0.5 . 0.5)))

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

(when (autoload-if-found
       '(org-onit-toggle-doing
         org-onit-mode
         org-onit-toggle-auto org-clock-goto my-sparse-doing-tree
         org-clock-goto org-onit-clock-in-when-unfold
         org-onit-update-options)
       "org-onit" nil t)

  (global-set-key (kbd "C-<f11>") 'org-clock-goto)

  (with-eval-after-load "org"
    (add-hook 'org-cycle-hook #'org-onit-clock-in-when-unfold)
    (define-key org-mode-map (kbd "<f11>") 'org-onit-toggle-doing)
    (define-key org-mode-map (kbd "M-<f11>") 'org-onit-toggle-auto)
    (define-key org-mode-map (kbd "S-<f11>") 'org-onit-goto-anchor)
    (unless (require 'org-bookmark-heading nil t)
      (message "--- org-bookmark-heading.el is NOT installed."))

    (defun my-sparse-doing-tree ()
      (interactive)
      (org-tags-view nil org-onit-tag)))

  (with-eval-after-load "org-onit"
    (when (require 'org-plist nil t)
      (add-to-list 'org-plist-dict '("OPTIONS_ONIT" org-onit-basic-options)))
    (custom-set-variables
     '(org-onit-basic-options '(:wakeup nil :nostate doing :unfold nil))))

  (with-eval-after-load "org-clock"
    (defun my-onit-reveal ()
      ;; (widen)
      (org-overview)
      (org-reveal)
      (org-cycle-hide-drawers 'all)
      (org-show-entry)
      (show-children)
      (org-show-siblings))
    (add-hook 'org-onit-after-jump-hook #'my-onit-reveal)

    (defun my-clear-undo-list ()
      (when (and (fboundp 'org-clocking-p)
                 (org-clocking-p))
        (setq buffer-undo-list nil)))
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

(when (autoload-if-found
	 '(orgbox-schedule orgbox-agenda-schedule)
	 "orgbox" nil t)

  (with-eval-after-load "org"
    (org-defkey org-mode-map (kbd "C-c C-s") 'orgbox-schedule))
  (with-eval-after-load "org-agenda"
    (org-defkey org-agenda-mode-map (kbd "C-c C-s") 'orgbox-agenda-schedule)))
  ;; (require 'orgbox nil t)) ;; require org-agenda

(when (autoload-if-found
       '(appt my-org-agenda-to-appt ad:appt-display-message
              ad:appt-disp-window appt-check)
       "appt" nil t)

  (with-eval-after-load "postpone"
    (global-set-key (kbd "C-c f 3") #'my-org-agenda-to-appt)
    (run-at-time "20 sec" nil #'my-org-agenda-to-appt))

  (with-eval-after-load "appt"
    (autoload 'my-org-agenda-to-appt "org" nil t)

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
    ;; Need review
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
      (when (memq appt-display-format '(window echo)) ;; modified
        (let ((time (format-time-string "%a %b %e "))
              (err nil))
          (condition-case err
              (funcall appt-disp-window-function
                       (if (listp mins)
                           (mapcar 'number-to-string mins)
                         (number-to-string mins))
                       time string)
            (wrong-type-argument
             (if (not (listp mins))
                 (signal (car err) (cdr err))
               ;; suppress
               ;; (message "Argtype error in `appt-disp-window-function' - update it for multiple appts?")
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

  (with-eval-after-load "ivy"
    (defvar counsel-appt-time-msg-list nil)
    (defun counsel-appt-list ()
      "Create a list of appt."
      (setq counsel-appt-time-msg-list nil)
      (when (boundp 'appt-time-msg-list)
        (dolist (msg appt-time-msg-list)
          (when msg
            (add-to-list 'counsel-appt-time-msg-list
                         (substring-no-properties (nth 1 msg)) t))))
      counsel-appt-time-msg-list)

    (defun counsel-appt ()
      "List active appt."
      (interactive)
      (ivy-read "Appt: "
                (counsel-appt-list)
                :require-match t
                :caller 'counsel-appt)))

  ;; (with-eval-after-load "org-agenda"
  ;;   (unless noninteractive
  ;;     (appt-activate 1)))

  (with-eval-after-load "org"
    ;; 定期的に更新する
    (run-with-idle-timer 500 t 'my-org-agenda-to-appt)

    ;; キャプチャ直後に更新
    (add-hook 'org-capture-before-finalize-hook #'my-org-agenda-to-appt)

    ;; アジェンダを開いたらアラームリストを更新
    (unless noninteractive
      (add-hook 'org-agenda-mode-hook #'my-org-agenda-to-appt))

    ;; org-agenda-to-appt を非同期で使うための advice
    (defvar read-char-default-timeout 10)
    (defun ad:read-char-exclusive (f &optional PROMPT INHERIT-INPUT-METHOD SECONDS)
      (funcall f PROMPT INHERIT-INPUT-METHOD
               (or SECONDS read-char-default-timeout)))
    (advice-add 'read-char-exclusive :around #'ad:read-char-exclusive)

    (defun ad:org-check-agenda-file (file)
      "Make sure FILE exists.  If not, ask user what to do."
      (let ((read-char-default-timeout 0)) ;; not nil
        (unless (file-exists-p file)
          (message "Non-existent agenda file %s.  [R]emove from list or [A]bort?"
	                 (abbreviate-file-name file))
          (let ((r (downcase (or (read-char-exclusive) ?r))))
            (cond
             ((equal r ?r)
	            (org-remove-file file)
	            (throw 'nextfile t))
             (t (user-error "Abort")))))))
    (advice-add 'org-check-agenda-file :override #'ad:org-check-agenda-file)

    ;; 重複実行の抑制用フラグ
    (defvar my-org-agenda-to-appt-ready t)
    (defun my-unlock-org-agenda-to-appt ()
      (interactive)
      (setq my-org-agenda-to-appt-ready t)
      (my-org-agenda-to-appt))

    (defun my-add-prop-to-appt-time-msg-list () ;; FIXME
      (let ((msgs appt-time-msg-list))
        (setq appt-time-msg-list nil)
        (dolist (msg msgs)
          (add-to-list 'appt-time-msg-list
                       (list (nth 0 msg)
                             (let ((str (nth 1 msg)))
                               (add-text-properties 6 10 '(org-heading t) str)
                               str)
                             (nth 2 msg))
                       ) t)
        ;; just for sure
        (delq nil appt-time-msg-list)))

    (defvar my-org-agenda-to-appt-async t)
    (when (eq window-system 'w32)
      (message "--- my-org-agenda-to-appt-async was changed to nil for w32")
      (setq my-org-agenda-to-appt-async nil))

    ;; org-agenda の内容をアラームに登録する
    (defun my-org-agenda-to-appt (&optional force)
      "Update `appt-time-mag-list'.  Use `async' if possible."
      (interactive)
      (if (or (not (require 'async nil t))
              (my-native-comp-p)
              (not my-org-agenda-to-appt-async))
          (unless (active-minibuffer-window)
            (org-agenda-to-appt t '((headline "TODO"))))
        (when force
          (setq my-org-agenda-to-appt-ready t))
        (if (not my-org-agenda-to-appt-ready)
            (message "[appt] Locked")
          (setq my-org-agenda-to-appt-ready nil)
          (async-start
           `(lambda ()
              (setq load-path ',load-path)
              (require 'org-agenda)
              (require 'appt)
              (setq org-agenda-files ',org-agenda-files)
              (org-agenda-to-appt t '((headline "TODO")))
              ;; Remove tags
              (let ((msgs appt-time-msg-list))
                (setq appt-time-msg-list nil)
                ;; (message "%s -- %s" (org-trim (substring (nth 1 msg) 0 match)) (nth 1 msg))
                (dolist (msg msgs)
                  (add-to-list 'appt-time-msg-list
                               (let ((match (string-match
                                             org-tag-group-re (nth 1 msg))))
                                 (if match
                                     (list (nth 0 msg)
                                           (org-trim (substring-no-properties
                                                      (nth 1 msg)
                                                      0 match))
                                           (nth 2 msg))
                                   msg)
                                 ) t))
                ;; just for sure
                (delq nil appt-time-msg-list)))
           (lambda (result)
             (setq appt-time-msg-list result) ;; nil means No event
             ;; (my-add-prop-to-appt-time-msg-list)
             (appt-check) ;; remove past events
             (unless (active-minibuffer-window)
               (let ((cnt (length appt-time-msg-list)))
                 (if (eq cnt 0)
                     (message "[async] No event to add")
                   (message "[async] Added %d event%s for today"
                            cnt (if (> cnt 1) "s" "")))))
             (setq my-org-agenda-to-appt-ready t))))))))

(with-eval-after-load "org"
  ;; 履歴が生成されるのを抑制．
  ;; [2/3]のような完了数が見出しにある時に転送先候補が重複表示されるため．

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
    (org-show-children)
    (org-cycle-hide-drawers 'children))
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
  (require 'ob-octave nil t)
  (require 'ob-go nil t)

  (when (and (not noninteractive)
             (not (executable-find "ditaa")))
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
  (defun my-org-src-block-face ()
    (setq org-src-block-faces
          (if (eq 'light (frame-parameter nil 'background-mode))
              '(("emacs-lisp" (:background "#F9F9F9" :extend t))
                ("conf" (:background "#F9F9F9" :extend t))
                ("org" (:background "#F9F9F9" :extend t))
                ("html" (:background "#F9F9F9" :extend t)))
            '(("emacs-lisp" (:background "#383c4c" :extend t))
              ("conf" (:background "#383c4c" :extend t))
              ("org" (:background "#383c4c" :extend t))
              ("html" (:background "#383c4c" :extend t)))))
    (font-lock-fontify-buffer))
  (add-hook 'ah-after-enable-theme-hook #'my-org-src-block-face)
  (my-org-src-block-face)

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
    (setq org-tree-slide-skip-done nil)))

(with-eval-after-load "org-tree-slide"
  (when (and (eq my-toggle-modeline-global 'doom)
             (require 'doom-modeline nil t))
    (add-hook 'org-tree-slide-stop-hook
              #'doom-modeline-update-buffer-file-state-icon)))

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
       '(ox-odt)
       "ox-odt" nil t)

  (with-eval-after-load "ox-odt"
    ;; (add-to-list 'org-odt-data-dir
    ;;              (concat (getenv "HOME") "/Dropbox/emacs.d/config/"))
    (setq org-odt-styles-file
          (concat (getenv "SYNCROOT") "/emacs.d/config/style.odt"))
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

(with-eval-after-load "org-clock-today"
  (defun my-print-working-clocks ()
    (interactive)
    (let ((clocked-item (org-duration-from-minutes
                         (org-clock-get-clocked-time))))
      (if org-clock-today-mode
          (message "Today Subtree %s Total %s | Past %s"
                   org-clock-today--buffer-time
                   org-clock-today--subtree-time
                   clocked-item)
        (message "Past %s" clocked-item)))))

(with-eval-after-load "org-clock"
  (defun ad:org-clock-sum-today (&optional headline-filter)
    "Sum the times for each subtree for today."
    (let ((range (org-clock-special-range 'today nil t))) ;; TZ考慮
      (org-clock-sum (car range) (cadr range)
                     headline-filter :org-clock-minutes-today)))
  (advice-add 'org-clock-sum-today :override #'ad:org-clock-sum-today)

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
        (let ((command "/Users/taka/Dropbox/scripts/push-hugo.sh"))
          (if (require 'async nil t)
              (async-start
               `(lambda () (shell-command-to-string ',command)))
            (shell-command-to-string command)))))
;;    (advice-add 'org-todo :after #'ad:ox-hugo:org-todo)
    ))

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
      (when (re-search-forward "^#\\+options:.*auto-id:t" (point-max) t)
        (org-map-entries
         (lambda () (my-org-custom-id-get (point) 'create))))))

  (defvar md-link-format "^!\\[\\(.+\\)\\](\\(.+\\))$")
  (defun my-convert-md-link-to-html ()
    (interactive)
    (goto-char (point-min))
    (while (re-search-forward md-link-format nil :noerror)
      (let* ((prev (match-string-no-properties 0))
             (alt (match-string-no-properties 1))
             (src (match-string-no-properties 2))
             (new (concat "<p><img src=\"" src "\" alt=\"" alt "\" /></p>")))
        (replace-match new)
        (message "====\ninput:\t%s\noutput:\t%s" prev new)))
    (message "--- done.")))

(with-eval-after-load "org"
  (defun my-delete-all-id-in-file ()
    (interactive)
    (goto-char 1)
    (while (not (eq (point) (point-max)))
      (org-next-visible-heading 1)
      (let ((id (org-entry-get (point) "ID")))
        (when id
          (message "ID: %s" id)
          (org-delete-property "ID"))))
    (message "--- done.")))

(when (autoload-if-found
       '(orglink-mode global-orglink-mode my-orglink-mode-activate)
       "orglink" nil t)

  (defun my-orglink-mode-activate ()
    (orglink-mode 1)
    (setq orglink-mode-lighter "")
    ;; バッファローカルに色つけを消す
    (face-remap-add-relative 'org-link
                             :underline nil
                             :inherit font-lock-comment-delimiter-face))

  (dolist (hook '(emacs-lisp-mode-hook c-mode-common-hook yatex-mode))
    (add-hook hook #'my-orglink-mode-activate))

  (with-eval-after-load "orglink"
    (delq 'angle orglink-activate-links)
    (define-key orglink-mouse-map (kbd "C-c C-o") 'org-open-at-point-global)
    (define-key orglink-mouse-map (kbd "C-c C-l") 'org-insert-link)))

;; (add-to-list 'orglink-activate-in-modes 'c++-mode)
;; (add-to-list 'orglink-activate-in-modes 'c-mode)
;; (when (require 'orglink nil t)
;;   (global-orglink-mode))

(with-eval-after-load "ox"
  (defvar my-org-export-before-hook nil)
  (defvar my-org-export-after-hook nil)
  (defvar my-org-export-last-buffer nil)

  (defun my-org-export--post-processing ()
    (when (eq this-command 'org-export-dispatch)
      (let ((moom-verbose nil))
        (run-hooks 'my-org-export-after-hook)
        moom-verbose) ;; to avoid a warning on lexical variable
      (remove-hook 'my-org-export-before-hook 'moom-split-window)
      (remove-hook 'my-org-export-before-hook 'split-window-horizontally)
      (remove-hook 'my-org-export-after-hook 'moom-delete-windows)
      (remove-hook 'my-org-export-after-hook 'delete-window))
    (when this-command
      (remove-hook 'post-command-hook #'my-org-export--post-processing)))

  (defun my-org-export-dispatch (f ARG)
    (cond
     (org-export-dispatch-use-expert-ui
      nil)
     ((eq (frame-width) 80)
      (when (require 'moom nil t)
        (add-hook 'my-org-export-before-hook 'moom-split-window)
        (add-hook 'my-org-export-after-hook 'moom-delete-windows)))
     ((> (frame-width) 160)
      (add-hook 'my-org-export-before-hook 'split-window-horizontally)
      (add-hook 'my-org-export-after-hook 'delete-window)))
    (when my-org-export-after-hook
      (add-hook 'post-command-hook #'my-org-export--post-processing))
    (run-hooks 'my-org-export-before-hook)
    (apply f ARG))
  (advice-add 'org-export-dispatch :around #'my-org-export-dispatch)

  (defun my-org-export-insert-default-template (f &optional backend subtreep)
    (let ((this-command nil))
      (apply f backend subtreep)))
  (advice-add 'org-export-insert-default-template :around
              #'my-org-export-insert-default-template)

  (defun my-org-export-to-buffer (_backend
                                  buffer
                                  &optional _async _subtreep _visible-only
                                  _body-only _ext-plist _post-process)
    (setq my-org-export-last-buffer buffer))
  (advice-add 'org-export-to-buffer :after #'my-org-export-to-buffer)

  (defun my-copy-exported-buffer ()
    (interactive)
    (when my-org-export-last-buffer
      (with-current-buffer my-org-export-last-buffer
        (mark-whole-buffer)
        (kill-ring-save (point-min) (point-max))
        (message "Copied: %s" my-org-export-last-buffer))
      (setq my-org-export-last-buffer nil)))
  (add-hook 'my-org-export-after-hook #'my-copy-exported-buffer))

(with-eval-after-load "ob-core"
  (when (require 'ob-async nil t)
    (custom-set-variables
     '(ob-async-no-async-languages-alist '("ipython")))))

(when (autoload-if-found
       '(org-tree-slide-mode)
       "org-tree-slide" nil t)

  (with-eval-after-load "org-tree-slide"
    ;; FIXME 複数のバッファで並行動作させるとおかしくなる．hide-lines の問題？
    ;; prettify-symbols で置き換えるほうが良い
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
  (if (eq system-type 'darwin)
      (require 'ox-pandoc nil t)
    (message "--- pandoc is NOT configured for Windows or Linux."))
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
      (concat (getenv "SYNCROOT")
              "/org/"
              (when (boundp 'org-attach-directory)
                "data/")))

    (defun du-org-attachments ()
      "Show directory size for org-attachments."
      (interactive)
      (message "--- %s"
               (chomp (shell-command-to-string
                       (concat "/usr/bin/du -sh "
                               org-attach-directory-absolute)))))))

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

(provide 'init-org)
