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

(with-eval-after-load "org"
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
  (defun my--org-eldoc-load ()
    "Set up org-eldoc documentation function."
    (interactive)
    (add-function :before-until (local 'eldoc-documentation-function)
                  #'org-eldoc-documentation-function))
  ;; 少なくとも org 9.5 では問題が発生しなくなったので，advice 停止．
  ;; (advice-add 'org-eldoc-load :override #'my--org-eldoc-load)
  (add-hook 'org-mode-hook #'org-eldoc-load)

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
  (defun my-promote-todo-revision (state)
    (cond ((member state '("TODO")) "REV1")
          ((member state '("REV1")) "REV2")
          ((member state '("REV2")) "REV3")
          (t state)))
  ;; (setq org-clock-out-switch-to-state #'my-promote-todo-revision)

  ;; undo 時に reveal して表示を改善する
  ;; (defun my--org:undo (&optional _ARG)
  ;;   (when (and (eq major-mode 'org-mode)
  ;;              (not (org-before-first-heading-p)))
  ;;     (org-overview)
  ;;     (org-reveal)
  ;;     (org-cycle-hide-drawers 'all)
  ;;     (org-show-entry)
  ;;     (show-children)
  ;;     (org-show-siblings)))
  ;; (advice-add 'undo :after #'my--org:undo)

  ;; 非表示状態の領域への書き込みを防ぐ
  ;; "Editing in invisible areas is prohibited, make them visible first"
  (setq org-catch-invisible-edits 'show-and-error)
  (defun my--org-return (f &rest arg)
    "An extension for checking invisible editing when you hit the enter."
    (interactive "P")
    (org-check-before-invisible-edit 'insert)
    (apply f arg))
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
  ;;;###autoload
  (defun my-org-hide-drawers ()
    "Hide all drawers in an org tree."
    (interactive)
    (save-mark-and-excursion
      (beginning-of-line)
      (unless (looking-at-p org-drawer-regexp)
        (org-cycle-hide-drawers 'subtree))))
  (add-hook 'org-tab-first-hook 'my-org-hide-drawers)

  ;; CSV指定でテーブルを出力する．
  ;;;###autoload
  (defun my-org-table-export ()
    (interactive)
    (org-table-export nil "orgtbl-to-csv"))

  ;; すべてのチェックボックスの cookies を更新する
  ;;;###autoload
  (defun my-do-org-update-staistics-cookies ()
    (interactive)
    (message "Update statistics...")
    (org-update-statistics-cookies 'all)
    (let ((inhibit-message t)
          (message-log-max nil))
      (save-buffer))
    (message "Update statistics...done"))

  (keymap-set org-mode-map "C-c f 2" 'my-do-org-update-staistics-cookies)

  ;; C-c & が yasnippet にオーバーライドされているのを張り替える
  (keymap-set org-mode-map "C-c 4" 'org-mark-ring-goto)

  ;; (org-transpose-element) が割り当てられているので取り返す．
  (org-defkey org-mode-map "\C-\M-t" 'beginning-of-buffer))

(with-eval-after-load "ox"
  (add-to-list 'org-modules 'ox-odt)
  (add-to-list 'org-modules 'ox-org)
  (add-to-list 'org-modules 'ox-json)) ;; FIXME

(with-eval-after-load "org-tempo"
  ;; 空行のとき "<" をインデントさせない
  (defun my--org-tempo-complete-tag (f &rest arg)
    (if (save-excursion
          (beginning-of-line)
          (looking-at "<"))
        (let ((indent-line-function 'ignore))
          (apply f arg))
      (apply f arg)))
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
  (defun my--org-tempo-add-block (entry)
    "Add block entry from `org-structure-template-alist'."
    (let* ((key (format "<%s" (car entry)))
           (name (cdr entry))
           (special nil)) ;; FIXED
      (tempo-define-template
       (format "org-%s" (replace-regexp-in-string " " "-" name))
       `(,(format "#+begin_%s%s" name (if special " " ""))
         ,(when special 'p) '> n '> ,(unless special 'p) n
         ,(format "#+end_%s" (car (split-string name " ")))'
         >)
       key
       (format "Insert a %s block" name)
       'org-tempo-tags)))
  ;; 更新
  (advice-add 'org-tempo-add-block :override #'my--org-tempo-add-block)
  ;; 反映
  (org-tempo-add-templates))

(with-eval-after-load "org-clock"
  ;; nil or 'history ならば，org-onit が org-clock-out を実行する．
  (setq org-clock-persist 'history) ;; {nil, t, 'clock, 'history}
  (setq org-clock-in-resume t)
  (setq org-clock-persist-query-resume nil)

  (advice-add 'org-clock-load :around #'my--suppress-message)

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
  (defun my--org-table-field-info (_arg)
    (save-buffer))
  (advice-add 'org-table-field-info :before #'my--org-table-field-info))

;; ~/Dropbox/Public は第三者に探索される可能性があるので要注意
;; default = ~/org.ics
;; C-c C-e i org-export-icalendar-this-file
;; C-c C-e I org-export-icalendar-all-agenda-files
;; C-c C-e c org-export-icalendar-all-combine-agenda-files
(when (autoload-if-found '(my-ox-icalendar
                           my-async-ox-icalendar my-ox-icalendar-cleanup)
                         "ox-icalendar" nil t)
  (with-eval-after-load "org"
    (keymap-set org-mode-map "C-c f 1" 'my-ox-upload-icalendar))

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

;;;###autoload
(defun my-ox-upload-icalendar ()
  (interactive)
  (when (and org-ical-file-in-orz-server
             (eq system-type 'darwin))
    (if (require 'async nil t)
        (my-async-ox-icalendar)
      (my-ox-icalendar))))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun my-ox-icalendar-cleanup ()
  (interactive)
  (when (file-exists-p
         (expand-file-name org-icalendar-combined-agenda-file))
    (shell-command-to-string
     (concat "rm -rf " org-icalendar-combined-agenda-file))))

(with-eval-after-load "org"
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
               '("$" call-interactively 'org-archive-subtree)))

;; done にして，apptを更新する
;;;###autoload
(defun my-done-with-update-list ()
  (interactive)
  (org-todo "DONE")
  (my-org-agenda-to-appt))

;; 周期タクスを終了させます．
;;;###autoload
(defun my-org-todo-complete-no-repeat (&optional ARG)
  (interactive "P")
  (when (org-get-repeat)
    (org-cancel-repeater))
  (if (eq (current-buffer) org-agenda-buffer)
      (org-agenda-todo ARG)
    (org-todo ARG)))

;;;###autoload
(defun my-org-replace-punc-in-buffer ()
  "Replace \"，\" and \"．\" with \"、\" and \"。\" in a buffer."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(，\\)\\|\\(．\\)" nil :noerror)
    (let ((w (match-string-no-properties 0)))
      (cond ((equal w "，") (replace-match "、"))
            ((equal w "．") (replace-match "。"))))))

;;;###autoload
(defun my-org-replace-punc-in-tree ()
  "Replace \"，\" and \"．\" with \"、\" and \"。\" in an org tree."
  (interactive)
  (org-back-to-heading t)
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
;;;###autoload
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
      ;;    (file-exists-p outfile))
      ;;     (switch-to-buffer
      ;;  (find-file-noselect outfile)
      ;;  (my-org-replace-punc-in-buffer))))
      (org-hugo-export-wim-to-md)
      (let ((command "/Users/taka/Dropbox/local/scripts/push-hugo.sh")
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
              (setq command (concat command " -e " (downcase filename)))))
          (message "[hugo] %s" command)
          (if (require 'async nil t)
              (progn
                (message "%s\n[async] Uploading..." exported)
                (async-start
                 `(lambda () (shell-command-to-string ',command))
                 `(lambda (result)
                    (message "%s\n[async] Uploading...%s"
                             ',exported (when result "done"))
                    (message "[log] %s" result))))
            (message "%s\nUploading..." exported)
            (message "%s" (shell-command-to-string command))
            (message "%s\nUploading...done" exported)))))))

;; カーソル位置のサブツリーをデスクトップにHTMLエクスポートする
;;;###autoload
(defun my-export-subtree-as-html ()
  (interactive)
  (let ((file "~/Desktop/note.html"))
    (org-export-to-file 'html file nil t)
    (org-open-file file)))

;; 締切を今日にする．agenda から起動したカレンダー内では "C-." でOK（標準）
;;;###autoload
(defun my-org-deadline-today ()
  (when (org-entry-is-todo-p)
    (let ((date (org-entry-get (point) "DEADLINE"))
          (today (format-time-string "%F")))
      (org-deadline 'deadline
                    (if date
                        (format "<%s%s"
                                today
                                (substring date 11 (string-width date)))
                      (format "<%s>" today))))))

;; 現在のツリーを畳んでから同じレベルの最後の要素として移動する
(defcustom my-org-move-subtree-to-the-last-after-hook nil""
  :type 'hook :group 'org)
;;;###autoload
(defun my-org-move-subtree-to-the-last ()
  "Move the current heading to the last one of the same level."
  (interactive)
  (let ((cnt 0) beg)
    (org-back-to-heading)
    (outline-hide-subtree)
    (setq beg (point))
    (while (and (funcall 'org-get-next-sibling)
                (looking-at org-outline-regexp))
      (setq cnt (1+ cnt)))
    (goto-char beg)
    (when (> cnt 0)
      (org-move-subtree-down cnt)
      (goto-char beg)))
  (run-hooks 'my-org-move-subtree-to-the-last-after-hook))

;; ツリーをカットする時に，カレントサブツリーと親の統計情報を更新する
;;;###autoload
(defun my--kill-update-todo-statistics (_b _d &optional _arg)
  (when (and (derived-mode-p 'org-mode)
             (org-kill-is-subtree-p))
    (save-excursion
      (save-restriction
        (unless (eq 1 (point))
          (backward-char 1))
        (ignore-errors (outline-up-heading 1))
        (org-update-statistics-cookies nil)
        (org-update-parent-todo-statistics)))))
;; kill時に[0/0]の色が変わるのが気になる場合は，volatile-highlights ロード後に
;; kill-region から vhl/.advice-callback-fn/.make-vhl-on-kill-region を
;; advice-remove する．
(advice-add 'kill-region :after #'my--kill-update-todo-statistics)

;; ツリーをペーストする時に，カレントサブツリーと親の統計情報を更新する
;;;###autoload
(defun my--yank-update-todo-statistics (&optional _arg)
  (when (org-kill-is-subtree-p)
    (save-excursion
      (save-restriction
        (unless (eq 1 (point))
          (backward-char 1))
        (org-update-statistics-cookies nil)
        (org-update-parent-todo-statistics)))))
(advice-add 'org-yank :after #'my--yank-update-todo-statistics)

;; アーカイブする前に narrowing を解く
(advice-add 'org-archive-subtree :before #'widen)

;; バッファ表示時に指定のツリーのコンテンツを展開表示する(Toggle)
(defvar my-org-pin-tag "pin")
;;;###autoload
(defun my-toggle-org-pin-subtree ()
  "Toggle \"VISIBILITY\" of the current tree."
  (interactive)
  (save-excursion
    (save-restriction
      (unless (org-at-heading-p)
        (org-previous-visible-heading 1))
      (unless (org-before-first-heading-p)
        (let ((element (org-element-at-point)))
          (cond ((org-element-property :VISIBILITY element)
                 (org-delete-property "VISIBILITY")
                 (org-toggle-tag my-org-pin-tag 'off)
                 (message "Unpinned"))
                (t
                 (org-set-property "VISIBILITY" "children")
                 (org-toggle-tag my-org-pin-tag 'on)
                 (message "Pinned"))))))))

;; narrowing+編集開始時に領域の最後に改行を置く FIXME
;;;###autoload
(defun my--ensure-newline-end (&rest _arg)
  (when (buffer-narrowed-p)
    (save-excursion
      (goto-char (point-max))
      (unless (bolp)
        (newline))))
  (advice-remove 'next-line #'my--ensure-newline-end))

;;;###autoload
(defun my--newline-narrowed-end-of-buffer (&optional _arg)
  (when (and (buffer-narrowed-p)
             (not (bolp)))
    (newline))
  (advice-remove 'end-of-buffer #'my--newline-narrowed-end-of-buffer))

;;;###autoload
(defun my--add-newline-narrowed-end ()
  (advice-add 'next-line :before #'my--ensure-newline-end)
  (advice-add 'end-of-buffer :after #'my--newline-narrowed-end-of-buffer))
(advice-add 'org-narrow-to-subtree :before #'my--add-newline-narrowed-end)

(with-eval-after-load "org"
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
          ("Remind"      :foreground "#6699CC")
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
          ("issue"       :foreground "#FF7D7D")
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
  (setq org-hierarchical-checkbox-statistics nil))

;; ;;;###autoload
;; (defun org-dblock-write:block-update-time (params)
;;   "block-update-time"
;;   (let ((fmt (or (plist-get params :format) "%Y-%m-%d")))
;;     (i'nsert "" (format-time-string fmt (current-time)))))

(with-eval-after-load "org"
  (setq org-image-actual-width '(256))
  (add-to-list 'image-file-name-extensions "jp2")
  ;; (add-to-list 'image-file-name-extensions "j2c")
  (add-to-list 'image-file-name-extensions "bmp")
  (add-to-list 'image-file-name-extensions "psd"))

(push '("[rR][eE][aA][dD][mM][eE]" . org-mode) auto-mode-alist)

;;;###autoload
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
      (message "Lower-cased %d matches" count))))

(with-eval-after-load "org"
  ;; Select from Preferences: { Funk | Glass | ... | Purr | Pop ... }
  (defvar ns-default-notification-sound "Pop")

  (defvar ns-alerter-command (concat (getenv "HOME") "/Dropbox/bin/alerter")
    "Path to alerter command. see https://github.com/vjeantet/alerter")
  (setq ns-alerter-command 'script) ;; the alerter is not work for now(2024-02-18).
  (unless ns-alerter-command
    (setq ns-alerter-command "")) ;; FIXME
  (when (or (eq ns-alerter-command 'script)
      (executable-find ns-alerter-command))
    (setq org-show-notification-handler #'my-desktop-notification-handler)))

(unless noninteractive
  (with-eval-after-load "org"
    (let ((file "~/Dropbox/org/db/daily.org"))
      (when (and (file-exists-p file)
                 (require 'utility nil t))
        (my-set-alarms-from-file file) ;; init
        (add-hook 'after-save-hook #'my-update-alarms-from-file))))) ;; update

;;;###autoload
(defun my-countdown-timer-notify ()
  ;; (when mode-line-format
  ;;     (my-mode-line-off))
  (when ns-alerter-command
    (setq org-show-notification-handler #'my-desktop-notification-handler))
  (remove-hook 'org-timer-done-hook #'my-countdown-timer-notify)
  (remove-hook 'org-timer-stop-hook #'my-countdown-timer-notify)
  (my-desktop-notification "### Expired! ###" "Time is up!" t "Glass"))

;;;###autoload
(defun my-countdown-timer ()
  (interactive)
  ;; (unless mode-line-format
  ;;     (my-mode-line-on))
  (when (eq org-show-notification-handler #'my-desktop-notification-handler)
    (setq org-show-notification-handler nil))
  (with-temp-buffer
    (org-mode)
    (insert "* Countdown")
    (add-hook 'org-timer-done-hook #'my-countdown-timer-notify)
    (add-hook 'org-timer-stop-hook #'my-countdown-timer-notify)
    (org-timer-set-timer)))

(defalias 'run-timer 'my-countdown-timer)

(when (autoload-if-found '(org-mode my-load-echo-org-link)
                         "org" nil t)
  (add-hook 'org-mode-hook #'my-load-echo-org-link)

  (with-eval-after-load "org"
    (defvar my-org-link-prompt "Link:")))

;;;###autoload
(defun my-echo-org-link ()
  (when (org-in-regexp org-link-bracket-re 1)
    (let ((l (length my-org-link-prompt))
          (msg (org-link-unescape (match-string-no-properties 1))))
      (put-text-property 0 l 'face 'minibuffer-prompt my-org-link-prompt)
      (eldoc-message (format "%s %s" my-org-link-prompt msg)))))

;;;###autoload
(defun my-load-echo-org-link ()
  (add-function :before-until (local 'eldoc-documentation-function)
                #'my-echo-org-link)
  ;; (setq-local eldoc-documentation-function #'my-echo-org-link)
  )

(with-eval-after-load "org"
  (org-defkey org-mode-map (kbd "M-p") #'my-org-meta-next)
  (org-defkey org-mode-map (kbd "M-n") #'my-org-meta-previous)
  (org-defkey org-mode-map (kbd "M-b") #'my-org-meta-backward)
  (org-defkey org-mode-map (kbd "M-f") #'my-org-meta-forward))

;;;###autoload
(defun my-org-item-has-child-p ()
  "Return t, if the item has at least a child item."
  (save-excursion
    (beginning-of-line)
    (org-list-has-child-p (point) (org-list-struct))))

;;;###autoload
(defun my-org-heading-has-child-p ()
  "Return t, if the heading has at least a child heading."
  (save-excursion
    (org-goto-first-child)))

;;;###autoload
(defun my-org-meta-previous ()
  "Move item or subtree down, otherwise `scroll-up'."
  (interactive)
  (cond ((org-at-item-p)
         (call-interactively 'org-move-item-down))
        ((or (looking-at org-heading-regexp)
             (and (org-at-heading-p) (eolp)))
         (call-interactively 'org-move-subtree-down))
        ((org-at-table-p)
         (call-interactively 'org-table-move-row))
        (t nil))) ;; (call-interactively 'scroll-up)

;;;###autoload
(defun my-org-meta-next ()
  "Move item or subtree up, otherwise `scroll-down'."
  (interactive)
  (cond ((org-at-item-p)
         (call-interactively 'org-move-item-up))
        ((or (looking-at org-heading-regexp)
             (and (org-at-heading-p) (eolp)))
         (call-interactively 'org-move-subtree-up))
        ((org-at-table-p)
         (org-call-with-arg 'org-table-move-row 'up))
        (t nil))) ;; (call-interactively 'scroll-down))))

(defvar my-org-promote-demote-independently nil)
;;;###autoload
(defun my-inherit-struct-p ()
  (and (not my-org-promote-demote-independently)
       (or (my-org-item-has-child-p) (my-org-heading-has-child-p))))

;;;###autoload
(defun my-org-at-meta-fb-p ()
  "Return t, if the cursor stay at item, heading, or table."
  (or (org-at-item-p)
      (looking-at org-heading-regexp)
      (and (org-at-heading-p) (eolp))
      (org-at-table-p)))

;;;###autoload
(defun my-org-meta-forward ()
  (interactive)
  (if (my-org-at-meta-fb-p)
      (if (my-inherit-struct-p)
          (org-shiftmetaright)
        (org-metaright)) ;; FIXME similar check to my-org-at-meta-fb-p
    (if (and (fboundp 'syntax-subword-mode)
             syntax-subword-mode)
        (call-interactively 'syntax-subword-forward)
      (forward-word))))

;;;###autoload
(defun my-org-meta-backward ()
  (interactive)
  (if (my-org-at-meta-fb-p)
      (if (my-inherit-struct-p)
          (org-shiftmetaleft)
        (org-metaleft)) ;; FIXME similar check to my-org-at-meta-fb-p
    (if (and (fboundp 'syntax-subword-mode)
             syntax-subword-mode)
        (call-interactively 'syntax-subword-backward)
      (backward-word))))

;;;###autoload
(defun my-org-table-copy-as (&optional format)
  "Copy converted table."
  (interactive)
  (let ((format (or format
                    (org-entry-get (point) "<tab>LE_EXPORT_FORMAT" t)
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
      (user-error "<tab>LE_EXPORT_FORMAT invalid"))))

;;;###autoload
(defun my-org-table-convert-to (&optional format)
  "Convert a table to FORMAT.
 If FORMAT is nil, it is set equal to a property value specified
 by \"<tab>LE_EXPORT_FORMAT\" or `org-table-export-default-format'.
 Converted table is copied to kill ring for further use.
 The core part is extracted from `org-table-export'."
  (interactive)
  (let ((format (or format
                    (org-entry-get (point) "<tab>LE_EXPORT_FORMAT" t)
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
      (user-error "<tab>LE_EXPORT_FORMAT invalid"))))

(with-eval-after-load "eldoc"
  (defvar my-eldoc-disable-in-org-block nil)
  (advice-add 'eldoc-print-current-symbol-info :around
              #'my--eldoc-print-current-symbol-info))

;;;###autoload
(defun my--eldoc-print-current-symbol-info (f &optional interactive)
  "Run `eldoc' when the cursor is NOT located in org source block."
  (interactive '(t))
  (unless (or my-eldoc-disable-in-org-block
              (and (eq major-mode 'org-mode)
                   (eq (car (org-element-at-point)) 'src-block)))
    (funcall f interactive)))

(with-eval-after-load "org"
  (advice-add 'org-reveal :around #'my--org-reveal))

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

(with-eval-after-load "org"
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
    (org-extra-emphasis-update))) ;; to apply configured `org-emphasis-alist'

(with-eval-after-load "org"
  (keymap-set org-mode-map "C-c x" #'my-org-move-item-end)
  (keymap-set org-mode-map "C-c X" #'my-org-move-item-begin))

(when (autoload-if-found '(org-capture)
                         "org-capture" nil t)
  (with-eval-after-load "org"
    ;; キャプチャ時に作成日時をプロパティに入れる
    ;; Thanks to https://emacs.stackexchange.com/questions/21291/add-created-timestamp-to-logbook
    (defvar my-org-created-property-name "CREATED"
      "The name of the org-mode property.
This user property stores the creation date of the entry")
    (advice-add 'org-insert-todo-heading :after #'my--org-insert-todo-heading))

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

;;;###autoload
(defun my-toggle-org-block-visibility ()
  "Testing..."
  (interactive)
  (when (looking-at org-drawer-regexp)
    (org-flag-drawer          ; toggle block visibility
     (not (get-char-property (match-end 0) 'invisible)))))

;;;###autoload
(defun my--org-insert-todo-heading (_arg &optional _force-heading)
  (unless (org-at-item-checkbox-p)
    (my-org-default-property)))

;;;###autoload
(defun my-org-default-property ()
  "Set the creation date and org-id."
  (interactive)
  (my-org-set-created-property)
  (org-id-get-create))

;;;###autoload
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

(with-eval-after-load "org"
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
  (setq org-deadline-warning-days 2)

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

;; Expand the frame width temporarily during org-agenda is activated.
;;;###autoload
(defun my-agenda-frame-width ()
  (let ((width (floor (* 1.2 moom-frame-width-single))))
    (setq org-tags-column (- org-tags-column (- width 80)))
    ;; (org-align-tags t)
    (moom-change-frame-width width)))

;;;###autoload
(defun my--org-agenda--quit (&optional _bury)
  (setq org-tags-column my-org-tags-column)
  ;; (org-align-tags t)
  (moom-change-frame-width))

;;;###autoload
(defun my-popup-agenda ()
  (interactive)
  (let ((status use-dialog-box))
    (setq use-dialog-box nil)
    (when (y-or-n-p-with-timeout "Popup agenda now?" 10 nil)
      (org-agenda-list))
    (message "")
    (setq use-dialog-box status)))

;;;###autoload
(defun my-popup-agenda-set-timers ()
  (interactive)
  (cancel-function-timers 'my-popup-agenda)
  (dolist (triger my-org-agenda-auto-popup-list)
    (when (future-time-p triger)
      (run-at-time triger nil 'my-popup-agenda))))

;; ついでに calendar.app も定期的に強制起動する
;;;###autoload
(defun my-popup-calendar ()
  (interactive)
  (if (and (eq system-type 'darwin)
           (frame-focus-state))
      (shell-command-to-string "open -a calendar.app")
    (message "--- input focus is currently OUT.")))

;;;###autoload
(defun my-popup-calendar-set-timers ()
  (interactive)
  (cancel-function-timers 'my-popup-calendar)
  (dolist (triger my-org-agenda-auto-popup-list)
    (when (future-time-p triger)
      (run-at-time triger nil 'my-popup-calendar))))

;; org-agenda でも "d" 押下で "DONE" にする
;;;###autoload
(defun my-org-agenda-done ()
  (interactive)
  (org-agenda-todo "DONE")
  (my-org-agenda-to-appt)) ;; call with async

(when (autoload-if-found '(org-onit-toggle-doing
                           org-onit-mode
                           org-onit-toggle-auto org-clock-goto
                           my-sparse-doing-tree org-onit-clock-in-when-unfold
                           org-clock-goto org-onit-update-options)
                         "org-onit" nil t)
  (keymap-global-set "C-<f11>" 'org-clock-goto)

  (with-eval-after-load "org"
    (add-hook 'org-cycle-hook #'org-onit-clock-in-when-unfold)
    (keymap-set org-mode-map "<f11>" 'org-onit-toggle-doing)
    (keymap-set org-mode-map "M-<f11>" 'org-onit-toggle-auto)
    (keymap-set org-mode-map "S-<f11>" 'org-onit-goto-anchor))

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

;;;###autoload
(defun my-sparse-doing-tree ()
      (interactive)
      (org-tags-view nil org-onit-tag))

;;;###autoload
(defun my-clear-undo-list ()
      (when (and (fboundp 'org-clocking-p)
                 (org-clocking-p))
        (setq buffer-undo-list nil)))

;;;###autoload
(defun my-onit-reveal ()
      ;; (widen)
      (org-overview)
      (org-reveal)
      (org-cycle-hide-drawers 'all)
      (org-show-entry)
      (show-children)
      (org-show-siblings))

(when (autoload-if-found '(orgbox-schedule orgbox-agenda-schedule)
                   "orgbox" nil t)
  (with-eval-after-load "org"
    (org-defkey org-mode-map (kbd "C-c C-s") 'orgbox-schedule))
  (with-eval-after-load "org-agenda"
    (org-defkey org-agenda-mode-map (kbd "C-c C-s") 'orgbox-agenda-schedule)))
  ;; (require 'orgbox nil t)) ;; require org-agenda

;; appt-display-format が 'echo でも appt-disp-window-function を呼ぶ
;; Need review
;;;###autoload
(defun my--appt-display-message (string mins)
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
  (cond ((memq appt-display-format '(window echo)) ;; Modified
         ;; TODO use calendar-month-abbrev-array rather than %b?
         (let ((time (format-time-string "%a %b %e ")))
           (condition-case err
               (funcall appt-disp-window-function
                        (if (listp mins)
                            (mapcar #'number-to-string mins)
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
                         (car string))))))
         (run-at-time (format "%d sec" appt-display-duration)
                      nil
                      appt-delete-window-function))
        ((eq appt-display-format 'echo) ;; hidden
         (message "%s" (if (listp string)
                           (mapconcat #'identity string "\n")
                         string)))))

;;;###autoload
(defun my--appt-disp-window (min-to-app _new-time appt-msg)
  "Extension to support appt-disp-window."
  (if (string= min-to-app "0")
      (my-desktop-notification "### Expired! ###" appt-msg t "Glass")
    (my-desktop-notification
     (concat "in " min-to-app " min.") appt-msg nil "Tink")))

;;;###autoload
(defun counsel-appt-list ()
  "Create a list of appt."
  (setq counsel-appt-time-msg-list nil)
  (when (boundp 'appt-time-msg-list)
    (dolist (msg appt-time-msg-list)
      (when msg
        (add-to-list 'counsel-appt-time-msg-list
                     (substring-no-properties (nth 1 msg)) t))))
  counsel-appt-time-msg-list)

;;;###autoload
(defun counsel-appt ()
  "List active appt."
  (interactive)
  (ivy-read "Appt: "
            (counsel-appt-list)
            :require-match t
            :caller 'counsel-appt))

(defvar read-char-default-timeout 10)

;;;###autoload
(defun my--read-char-exclusive (f &optional PROMPT INHERIT-INPUT-METHOD SECONDS)
  (funcall f PROMPT INHERIT-INPUT-METHOD
           (or SECONDS read-char-default-timeout)))

;;;###autoload
(defun my--org-check-agenda-file (file)
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

;;;###autoload
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

(when (autoload-if-found '(appt my--appt-display-message
                                my--appt-disp-window appt-check)
                         "appt" nil t)

  (defvar my-org-agenda-to-appt-async t)
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

    (advice-add 'appt-display-message :override #'my--appt-display-message)
    
    (cond
     ((eq appt-display-format 'echo)
      (setq appt-disp-window-function 'my--appt-disp-window))
     ((eq appt-display-format 'window)
      (advice-add 'appt-disp-window :before #'my--appt-disp-window))))

  (with-eval-after-load "ivy"
    (defvar counsel-appt-time-msg-list nil))

  ;; (with-eval-after-load "org-agenda"
  ;;   (unless noninteractive
  ;;     (appt-activate 1)))

  (with-eval-after-load "org"
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
    ))

;;;###autoload
(defun my--org-refile (f &optional arg default-buffer rfloc msg)
  "Extension to support keeping org-refile-history empty."
  (save-excursion
    (save-restriction
      (let ((l (org-outline-level))
            (b (buffer-name)))
        (funcall f arg default-buffer rfloc msg)
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

;;;###autoload
(defun my--org-sort-entries (&optional _with-case _sorting-type
                                       _getkey-func _compare-func
                                       _property _interactive?)
  (outline-hide-subtree)
  (org-show-hidden-entry)
  (org-show-children)
  (org-cycle-hide-drawers 'children))

(with-eval-after-load "org"
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
  (advice-add 'org-sort-entries :after #'my--org-sort-entries))

(with-eval-after-load "org"
  ;; will take 200[ms]
  (unless noninteractive
    (run-with-idle-timer (+ 7 my-default-loading-delay)
                         nil #'my-org-babel-load-activate)))

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

(with-eval-after-load "org"
  (add-to-list 'org-structure-template-alist
               (if (version< "9.1.4" (org-version))
                   '("S" . "src emacs-lisp")
                 '("S" "#+begin_src emacs-lisp\n?\n#+END_SRC" "<src lang=\"emacs-lisp\">\n\n</src>"))))

;;;###autoload
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
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (font-lock-flush)))))

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
    (setq org-tree-slide-modeline-display 'outside)
    (setq org-tree-slide-skip-outline-level 5)
    (setq org-tree-slide-skip-done nil)))

(with-eval-after-load "org-tree-slide"
  (when (and (eq my-toggle-modeline-global 'doom)
             (require 'doom-modeline nil t))
    (add-hook 'org-tree-slide-stop-hook
              #'doom-modeline-update-buffer-file-state-icon)))

;;;###autoload
(defun my-toggle-proportional-font ()
  (interactive)
  (setq use-proportional-font (not use-proportional-font))
  (if use-proportional-font
      (org-entry-put nil "FONT" "PROPORTIONAL")
    (org-delete-property "FONT")))

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
;;;###autoload
(defun my-hide-org-meta-line ()
  (interactive)
  (setq my-hide-org-meta-line-p t)
  (set-face-attribute 'org-meta-line nil
                      :foreground (face-attribute 'default :background)))

;;;###autoload
(defun my-show-org-meta-line ()
  (interactive)
  (setq my-hide-org-meta-line-p nil)
  (set-face-attribute 'org-meta-line nil :foreground nil))

;;;###autoload
(defun my-toggle-org-meta-line ()
  (interactive)
  (if my-hide-org-meta-line-p
      (my-show-org-meta-line) (my-hide-org-meta-line)))

(add-hook 'org-tree-slide-play-hook #'my-hide-org-meta-line)
(add-hook 'org-tree-slide-stop-hook #'my-show-org-meta-line)

;; Option
;;;###autoload
(defun my-update-org-meta-line ()
  (interactive)
  (when my-hide-org-meta-line-p
    (my-hide-org-meta-line)))
(add-hook 'ah-after-enable-theme-hook #'my-update-org-meta-line)

(when (autoload-if-found '(ox-odt)
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
    (require 'epa)
    (setq org-crypt-key "") ;; <insert your key>
    ;; org-encrypt-entries の影響を受けるタグを指定
    (setq org-tags-exclude-from-inheritance (quote ("secret")))
    ;; 自動保存の確認を無効に
    (setq org-crypt-disable-auto-save 'nil)))

(with-eval-after-load "org"
  ;; (add-to-list 'org-modules 'org-mac-iCal)
  ;; (add-to-list 'org-modules 'org-mac-link) ;; includes org-mac-message

  (autoload 'org-mac-link-get-link "org-mac-link" nil t)
  (keymap-set org-mode-map "C-c c" 'org-mac-link-get-link)
  (with-eval-after-load "org-mac-link"
    (require 'org-mac-iCal nil t)))

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

;;;###autoload
(defun my--org-grep-quit ()
  (interactive)
  (delete-window))

;;;###autoload
(defun org-grep-from-org-shell-command (regexp)
  "Only for macOS"
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
    ":"))

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

;;;###autoload
(defun my-print-working-clocks ()
  "for org-clock-today"
  (interactive)
  (let ((clocked-item (org-duration-from-minutes
                       (org-clock-get-clocked-time))))
    (if org-clock-today-mode
        (message "Today Subtree %s Total %s | Past %s"
                 org-clock-today--buffer-time
                 org-clock-today--subtree-time
                 clocked-item)
      (message "Past %s" clocked-item))))

;;;###autoload
(defun my--org-clock-sum-today (&optional headline-filter)
  "Sum the times for each subtree for today."
  (let ((range (org-clock-special-range 'today nil t))) ;; TZ考慮
    (org-clock-sum (car range) (cadr range)
                   headline-filter :org-clock-minutes-today)))

;; see https://ox-hugo.scripter.co/doc/deprecation-notices/#org-hugo-auto-export-feature-now-a-minor-mode
;; (with-eval-after-load "org"
;; No need for latest ox-hugo
;;   ;; Require ox-hugo-auto-export.el explictly before loading ox-hugo.el
;;   (require 'ox-hugo-auto-export nil t))

;; see https://pxaka.tokyo/blog/2018/a-link-to-the-original-org-source-file
;;;###autoload
(defun org-hugo-get-link-to-orgfile (uri alt)
  "Return a formatted link to the original Org file.
To insert the formatted into an org buffer for Hugo, use an appropriate
macro, e.g. {{{srclink}}}.

Note that this mechanism is still under consideration."
  (let ((line (save-excursion
                (save-restriction
                  (org-back-to-heading t)
                  (line-number-at-pos)))))
    (concat "[[" uri (file-name-nondirectory (buffer-file-name))
            "#L" (format "%d" line) "][" alt "]]")))

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

(with-eval-after-load "ox-html"
  (setq org-html-text-markup-alist
        '((bold . "<b>%s</b>")
          (code . "<code class=\"org-code\">%s</code>")
          (italic . "<i>%s</i>")
          (strike-through . "<del>%s</del>")
          (underline . "<span class=\"org-underline\">%s</span>")
          (verbatim . "<code class=\"org-verbatim\">%s</code>"))))

(with-eval-after-load "org"
  (defvar md-link-format "^!\\[\\(.+\\)\\](\\(.+\\))$"))

;;;###autoload
(defun my-add-custom-id ()
  "Add \"CUSTOM_ID\" to the current tree if not assigned yet."
  (interactive)
  (my-org-custom-id-get (point) t))

;;;###autoload
(defun my-get-custom-id ()
  "Return a part of UUID with an \"org\" prefix.
e.g. \"org3ca6ef0c\"."
  (let* ((id (org-id-new "")))
    (when (org-uuidgen-p id)
      (downcase (concat "org"  (substring (org-id-new "") 0 8))))))

;;;###autoload
(defun my-org-custom-id-get (pom &optional create)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
If the entry does not have an CUSTOM_ID, the function returns nil.
However, when CREATE is non nil, create a CUSTOM_ID if none is present
already.  In any case, the CUSTOM_ID of the entry is returned.

See https://writequit.org/articles/emacs-org-mode-generate-ids.html"
  (interactive)
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
      id))))

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

;;;###autoload
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
    (message "--- done."))

;;;###autoload
(defun my-delete-all-id-in-file ()
  (interactive)
  (goto-char 1)
  (while (not (eq (point) (point-max)))
    (org-next-visible-heading 1)
    (let ((id (org-entry-get (point) "ID")))
      (when id
        (message "ID: %s" id)
        (org-delete-property "ID"))))
  (message "--- done."))

;;;###autoload
(defun my-orglink-mode-activate ()
  (orglink-mode 1)
  (setq orglink-mode-lighter "")
  ;; バッファローカルに色つけを消す
  (face-remap-add-relative 'org-link
                           :underline nil
                           :inherit font-lock-comment-delimiter-face))

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

;;;###autoload
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

;;;###autoload
(defun my--org-export-dispatch (f &optional ARG)
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
  (funcall f ARG))

;;;###autoload
(defun my--org-export-insert-default-template (f &optional backend subtreep)
  (let ((this-command nil))
    (funcall f backend subtreep)))

;;;###autoload
(defun my--org-export-to-buffer (_backend
                                 buffer
                                 &optional _async _subtreep _visible-only
                                 _body-only _ext-plist _post-process)
  (setq my-org-export-last-buffer buffer))

;;;###autoload
(defun my-copy-exported-buffer ()
  (interactive)
  (when my-org-export-last-buffer
    (with-current-buffer my-org-export-last-buffer
      (mark-whole-buffer)
      (kill-ring-save (point-min) (point-max))
      (message "Copied: %s" my-org-export-last-buffer))
    (setq my-org-export-last-buffer nil)))

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

;;;###autoload
(defun my-show-headers ()
  (setq org-src-block-faces 'my-org-src-block-faces)
  (hide-lines-show-all))

;;;###autoload
(defun my-hide-headers ()
  (setq my-org-src-block-faces 'org-src-block-faces)
  (setq org-src-block-faces
        '(("emacs-lisp" (:background "cornsilk"))))
  (hide-lines-matching "#\\+BEGIN_SRC")
  (hide-lines-matching "#\\+END_SRC"))

;;;###autoload
(defun my--org-edit-src-code ()
  (interactive)
  (my-show-headers))

;;;###autoload
(defun my--org-edit-src-exit ()
  (interactive)
  (my-hide-headers))

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

  (when (boundp 'org-mac-link-descriptors)
    (add-to-list 'org-mac-link-descriptors
                 `("P" "apers" org-mac-papers-insert-frontmost-paper-link
                   ,org-mac-grab-Papers-app-p) t)))

;;;###autoload
(defun org-mac-papers-insert-frontmost-paper-link ()
  (interactive)
  (let ((result (org-mac-papers-get-frontmost-paper-link)))
    (if result
        (insert result)
      (message "Please open Papers.app and select a paper."))))

;;;###autoload
(defun org-mac-papers-get-frontmost-paper-link ()
  (interactive)
  (message "Applescript: Getting Papers link...")
  (let ((result (org-as-mac-papers-get-paper-link)))
    (if (or (eq result nil) (string= result ""))
        nil
      (org-mac-paste-applescript-links result))))

;;;###autoload
(defun org-as-mac-papers-get-paper-link ()
  (do-applescript
   (concat
    "if application \"Papers\" is running then\n"
    " tell application \"Papers\" to activate\n"
    " delay 0.3\n"
    " set the clipboard to \"\"\n"
    " tell application \"System Events\" to tell process \"Papers\"\n"
    "           keystroke \"l\" using {command down, shift down}\n"
    " end tell\n"
    " delay 0.2\n"
    " set aLink to the clipboard\n"
    " tell application \"System Events\" to tell process \"Papers\"\n"
    ;; "                keystroke \"c\" using {command down, alt down\}\n"
    "           keystroke \"m\" using {command down, option down\}\n"
    " end tell\n"
    " delay 0.2\n"
    " set aName to the clipboard\n"
    " tell application \"Emacs\" to activate\n"
    " return (get aLink) & \"::split::\" & (get aName) as string\n"
    "else\n"
    " return\n"
    "end if\n")))

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

;;;###autoload
(defun du-org-attachments ()
  "Show directory size for org-attachments."
  (interactive)
  (message "--- %s"
           (chomp (shell-command-to-string
                   (concat "/usr/bin/du -sh "
                           org-attach-directory-absolute)))))

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

(when (autoload-if-found '(orgnav-search-root)
                         "orgnav" nil t)
  (with-eval-after-load "org"
    (keymap-set org-mode-map "C-c f n"
      (lambda () (interactive)
        (orgnav-search-root 3 'orgnav--goto-action)))))

(autoload-if-found '(toc-org-insert-toc) "toc-org" nil t)

;;;###autoload
(defun my-org-attach-screenshot ()
  (interactive)
  (org-attach-screenshot t (format-time-string
                            "screenshot-%Y%m%d-%H%M%S.png")))

(when (autoload-if-found '(org-attach-screenshot)
                         "org-attach-screenshot" nil t)
  (with-eval-after-load "org-attach-screenshot"
    (when (executable-find "screencapture")
      (setq org-attach-screenshot-command-line "screencapture -w %f"))))

;;;###autoload
(defun my--org-agenda (&optional _arg _org-keys _restriction)
  (my-linespacing))

;;;###autoload
(defun my--org-agenda-redo (&optional _all)
  (my-linespacing))

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

(unless noninteractive
  (when nil
    (let ((inhibit-message t))
      (message "Loading init-org.el...done (%4d [ms])"
               (* 1000
                  (float-time (time-subtract
                               (current-time)
                               my-init-org-start)))))))
(provide 'init-org)
