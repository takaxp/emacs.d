;; -*- lexical-binding: t -*-
;; Configurations for Emacs
;;                                         Takaaki ISHIKAWA  <takaxp@ieee.org>
;; see also https://takaxp.github.io/init.html

(with-eval-after-load "postpone"
  (require 'late-init nil t)
  (require 'init-org nil t)) ;; loading all with-eval-after-load for Org

(with-eval-after-load "org"
  (require 'postpone nil t)
  (require 'init-org nil t))

(defun my-load-init-time ()
  "Loading time of user init files including time for `after-init-hook'."
  (let ((time1 (float-time
                (time-subtract after-init-time my-before-load-init-time)))
        (time2 (float-time
                (time-subtract (current-time) my-before-load-init-time))))
    (message (concat "Loading init files: %.0f [msec], "
                     "of which %.f [msec] for `after-init-hook'.")
             (* 1000 time1) (* 1000 (- time2 time1)))))

(add-hook 'after-init-hook #'my-load-init-time t)

(defvar my-tick-previous-time my-before-load-init-time)
(defun my-tick-init-time (msg)
  "Tick boot sequence at loading MSG."
  (when my-loading-profile-p
    (let ((ctime (current-time)))
      (message "---- %5.2f[ms] %s"
               (* 1000 (float-time
                        (time-subtract ctime my-tick-previous-time)))
               msg)
      (setq my-tick-previous-time ctime))))

(defun my-emacs-init-time ()
  "Emacs booting time in msec."
  (message "Emacs booting time: %.0f [msec] = `emacs-init-time'."
           (* 1000
              (float-time (time-subtract
                           after-init-time
                           before-init-time)))))

(add-hook 'after-init-hook #'my-emacs-init-time)

(setq gc-cons-threshold (* 128 1024 1024)) ;; 128MB
(setq garbage-collection-messages t)

(setq byte-compile-warnings '(not obsolete))
(setq ad-redefinition-action 'accept)

(setq save-silently t) ;; No need shut-up.el for saving files.

(defun my-load-package-p (file)
  (let ((enabled t))
    (when (boundp 'my-loading-packages)
      (dolist (package my-loading-packages)
        (let ((name (car package))
              (flag (cdr package)))
          (when (and (stringp name)
                     (equal file name)
                     (not flag))
            (setq enabled nil)
            (message "--- `%s' was NOT loaded intentionally" name)))))
    enabled))

(defvar my-skip-check-autoload-file t)
(when (and (boundp 'my-loading-packages)
           my-loading-packages)
  (setq my-skip-check-autoload-file nil))

(defun autoload-if-found (functions file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (when (boundp 'my-required-libraries)
    (add-to-list 'my-required-libraries file))
  (when (or my-skip-check-autoload-file
            (and (my-load-package-p file)
                 (locate-library file)))
    (dolist (f functions)
      (autoload f file docstring interactive type))
    t))

(if (not (locate-library "postpone"))
    (message "postpone.el is NOT installed.")
  (autoload 'postpone-kicker "postpone" nil t)
  (defun my-postpone-kicker ()
    (interactive)
    (unless (memq this-command ;; specify commands for exclusion
                  '(self-insert-command
                    newline
                    delete-backward-char
                    save-buffer
                    save-buffers-kill-terminal
                    electric-newline-and-maybe-indent
                    exit-minibuffer))
      (message "Activating postponed packages...")
      (let ((t1 (current-time)))
        (postpone-kicker 'my-postpone-kicker)
        (setq postpone-init-time (float-time
                                  (time-subtract (current-time) t1))))
      (message "Activating postponed packages...done (%.3f seconds)"
               postpone-init-time)))
  (add-hook 'pre-command-hook #'my-postpone-kicker))

(defvar window-focus-p t)
(with-eval-after-load "postpone"
  (defun window-focus-p ()
    (if window-focus-p t nil))
  (add-hook 'focus-in-hook (lambda () (setq window-focus-p t)))
  (add-hook 'focus-out-hook (lambda () (setq window-focus-p nil))))

;;;###autoload
(defun library-p (libraries)
  "Return t when every specified library can be located. "
  (let ((result t))
    (mapc (lambda (library)
            (unless (locate-library library)
              (message "--- NOT FOUND: %s" library)
              (setq result nil)))
          (if (listp libraries)
              libraries
            (list libraries)))
    result))

(defvar shutup-p nil)
(with-eval-after-load "postpone"
  (setq shutup-p (when (require 'shut-up nil t) t)))
(setq message-log-max 5000) ;; メッセージバッファの長さ

(my-tick-init-time "startup")

(prefer-coding-system 'utf-8-unix)
;; (set-language-environment "Japanese") ;; will take 20-30[ms]
(set-locale-environment "en_US.UTF-8") ; "ja_JP.UTF-8"
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)

(when (fboundp 'mac-add-key-passed-to-system)
  (setq default-input-method "MacOSX")
  (mac-add-key-passed-to-system 'shift))

(when (eq system-type 'gnu/linux)
  (global-set-key (kbd "<hiragana-katakana>") 'toggle-input-method)
  (push "/usr/share/emacs/site-lisp/anthy" load-path)
  (push "/usr/share/emacs/site-lisp/emacs-mozc" load-path)
  (set-language-environment "Japanese")

  (if (require 'mozc nil t)
      (progn
        (setq default-input-method "japanese-mozc")
        (custom-set-variables
         '(mozc-candidate-style 'overlay)))

    (when (require 'anthy nil t) ;; sudo yum install emacs-anthy-el
      ;; if get error
      (load-file "/usr/share/emacs/site-lisp/anthy/leim-list.el")
      (setq default-input-method 'japanese-anthy))))

(when (eq system-type 'darwin)
  (when (boundp 'ns-command-modifier)
    (setq ns-command-modifier 'meta))
  (when (boundp 'ns-alternate-modifier)
    (setq ns-alternate-modifier 'super))
  (when (boundp 'ns-pop-up-frames)
    (setq ns-pop-up-frames nil))
  (global-set-key (kbd "M-v") 'yank)
  (global-set-key [ns-drag-file] 'ns-find-file))
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Limit the final word to a line break code (automatically correct)
(setq require-final-newline t)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;              (setq indent-tabs-mode t)
;;              (setq tab-width 8)
;;              (setq indent-line-function 'lisp-indent-line)))

(when (version< "27" emacs-version)
  (setq default-directory "~/"))

(when (memq window-system '(ns nil))
  (global-set-key (kbd "M-SPC") 'my-toggle-ime-ns) ;; toggle-input-method
  (global-set-key (kbd "S-SPC") 'my-toggle-ime-ns) ;; toggle-input-method
  (declare-function my-ns-org-heading-auto-ascii "init" nil)
  (declare-function my-ns-ime-restore "init" nil)
  (declare-function my-ime-active-p "init" nil))

(with-eval-after-load "postpone"
  (when (and (memq window-system '(ns nil))
             (fboundp 'mac-get-current-input-source))

    (defun my-toggle-ime-ns ()
      "Toggle IME."
      (interactive)
      (when (window-focus-p)
        (if (my-ime-active-p) (my-ime-off) (my-ime-on))))

    (define-key isearch-mode-map (kbd "M-SPC") 'my-toggle-ime-ns)
    (define-key isearch-mode-map (kbd "S-SPC") 'my-toggle-ime-ns)

    (defun my-ns-org-heading-auto-ascii ()
      "IME off, when the cursor on org headings."
      (when (and (window-focus-p)
                 (eq major-mode 'org-mode)
                 (or (looking-at org-heading-regexp)
                     (equal (buffer-name) org-agenda-buffer-name)))
        (my-ime-off)))
    (run-with-idle-timer 1 t #'my-ns-org-heading-auto-ascii)

    (defun my-ns-ime-restore ()
      "Restore the last IME status."
      (if my-ime-last (my-ime-on) (my-ime-off)))
    (add-hook 'focus-in-hook #'my-ns-ime-restore)))

(when (eq window-system 'mac)
  (global-set-key (kbd "M-SPC") 'mac-win-toggle-ime)
  (global-set-key (kbd "S-SPC") 'mac-win-toggle-ime)
  (declare-function mac-win-save-last-ime-status "init" nil)
  (declare-function ad:mac-auto-ascii-setup-input-source "init" nil)
  (declare-function mac-win-restore-ime "init" nil)
  (declare-function mac-win-restore-ime-target-commands "init" nil))

(with-eval-after-load "postpone"
  (when (and (eq window-system 'mac)
             (fboundp 'mac-select-input-source)
             (fboundp 'mac-auto-ascii-select-input-source)
             (fboundp 'mac-auto-ascii-setup-input-source)
             (fboundp 'mac-input-source)
             (fboundp 'mac-auto-ascii-mode))

    (defvar mac-win-last-ime-status 'off) ;; {'off|'on}
    (defun mac-win-save-last-ime-status ()
      (setq mac-win-last-ime-status
            (if (string-match "\\.\\(Roman\\|US\\)$" (mac-input-source))
                'off 'on)))
    (mac-win-save-last-ime-status) ;; 初期化

    (defun mac-win-restore-ime ()
      (when (and (boundp 'mac-auto-ascii-mode)
                 mac-auto-ascii-mode
                 (eq mac-win-last-ime-status 'on))
        (mac-select-input-source
         "com.google.inputmethod.Japanese.base")))

    (defun ad:mac-auto-ascii-setup-input-source (&optional _prompt)
      "Extension to store IME status"
      (mac-win-save-last-ime-status))
    (advice-add 'mac-auto-ascii-setup-input-source :before
                #'ad:mac-auto-ascii-setup-input-source)

    (defvar mac-win-target-commands
      '(find-file save-buffer other-window delete-window split-window))

    (defun mac-win-restore-ime-target-commands ()
      (when (and (boundp 'mac-auto-ascii-mode)
                 mac-auto-ascii-mode
                 (eq mac-win-last-ime-status 'on))
        (mapc (lambda (command)
                (when (string-match
                       (format "^%s" command) (format "%s" this-command))
                  (mac-select-input-source
                   "com.google.inputmethod.Japanese.base")))
              mac-win-target-commands)))
    (add-hook 'pre-command-hook #'mac-win-restore-ime-target-commands)

    ;; バッファリストを見るとき
    (add-to-list 'mac-win-target-commands 'helm-buffers-list)
    ;; ChangeLogに行くとき
    (add-to-list 'mac-win-target-commands 'add-change-log-entry-other-window)
    ;; 個人用の関数を使うとき
    ;; (add-to-list 'mac-win-target-commands 'my-)
    ;; 自分で作ったパッケージ群の関数を使うとき
    (add-to-list 'mac-win-target-commands 'change-frame)
    ;; org-mode で締め切りを設定するとき．
    (add-to-list 'mac-win-target-commands 'org-deadline)
    ;; org-mode で締め切りを設定するとき．
    ;; (add-to-list 'mac-win-target-commands 'org-capture)
    ;; query-replace で変換するとき
    (add-to-list 'mac-win-target-commands 'query-replace)

    ;; ミニバッファ利用後にIMEを戻す
    ;; M-x でのコマンド選択でIMEを戻せる．
    ;; これ移動先で q が効かないことがある
    (add-hook 'minibuffer-setup-hook #'mac-win-save-last-ime-status)
    (add-hook 'minibuffer-exit-hook #'mac-win-restore-ime)

    ;; タイトルバーの振る舞いを NS版に合わせる．
    (setq frame-title-format (format (if (buffer-file-name) "%%f" "%%b")))

    ;; なおテーマを切り替えたら，face の設定をリロードしないと期待通りにならない
    (when (require 'hl-line nil t)
      (custom-set-faces
       ;; 変換前入力時の文字列用 face
       `(mac-ts-converted-text
         ((((background dark)) :underline "orange"
           :background ,(face-attribute 'hl-line :background))
          (t (:underline "orange"
                         :background
                         ,(face-attribute 'hl-line :background)))))
       ;; 変換対象の文字列用 face
       `(mac-ts-selected-converted-text
         ((((background dark)) :underline "orange"
           :background ,(face-attribute 'hl-line :background))
          (t (:underline "orange"
                         :background
                         ,(face-attribute 'hl-line :background)))))))

    (when (fboundp 'mac-input-source)
      (run-with-idle-timer 3 t 'my-mac-keyboard-input-source))


    ;; あまりよいアプローチでは無い気がするけど，org-heading 上とagendaでは
    ;; 1秒アイドルすると，自動的に IME を OFF にする
    (defun my-mac-win-org-heading-auto-ascii ()
      (when (and (eq major-mode 'org-mode)
                 (or (looking-at org-heading-regexp)
                     (equal (buffer-name) org-agenda-buffer-name)))
        (setq mac-win-last-ime-status 'off)
        (mac-auto-ascii-select-input-source)))
    (when (fboundp 'mac-auto-ascii-select-input-source)
      (run-with-idle-timer 1 t 'my-mac-win-org-heading-auto-ascii))

    ;; EMP版Emacsの野良ビルド用独自設定群
    ;; IME toggleを Emacs内で有効にする
    (defun mac-win-toggle-ime ()
      (interactive)
      (when (fboundp 'mac-input-source)
        (mac-select-input-source
         (concat "com.google.inputmethod.Japanese"
                 (if (string-match "\\.base$" (mac-input-source))
                     ".Roman" ".base")))))

    ;; isearch 中にIMEを切り替えると，[I-Search] の表示が消える．
    ;; (define-key isearch-mode-map (kbd "M-SPC") 'mac-win-toggle-ime)
    (define-key isearch-mode-map (kbd "S-SPC") 'mac-win-toggle-ime)

    (when (boundp 'mac-win-ime-cursor-type)
      (setq mac-win-ime-cursor-type my-cursor-type-ime-on))
    ;; minibuffer では↑の背景色を無効にする
    (when (fboundp 'mac-min--minibuffer-setup)
      (add-hook 'minibuffer-setup-hook #'mac-min--minibuffer-setup))
    ;; echo-area でも背景色を無効にする
    (when (boundp 'mac-win-default-background-echo-area)
      (setq mac-win-default-background-echo-area t));; *-textのbackgroundを無視
    ;; デバッグ用
    (when (boundp 'mac-win-debug-log)
      (setq mac-win-debug-log nil))
    ;; Testing...
    (when (boundp 'mac-win-apply-org-heading-face)
      (setq mac-win-apply-org-heading-face t))

    (mac-auto-ascii-mode 1)))

(my-tick-init-time "core")

(my-tick-init-time "point")

(global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)

(when (autoload-if-found
       '(modern-c++-font-lock-mode)
       "modern-cpp-font-lock" nil t)
  (push '("\\.h$" . c++-mode) auto-mode-alist)
  (add-hook 'c-mode-hook #'modern-c++-font-lock-mode)
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(when (autoload-if-found
       '(csharp-mode)
       "csharp-mode" "Major mode for editing C# mode." nil t)

  (push '("\\.cs$" . csharp-mode) auto-mode-alist))

(when (autoload-if-found
       '(R-mode R)
       "ess-site" "Emacs Speaks Statistics mode" nil t)

  (push '("\\.[rR]$" . R-mode) auto-mode-alist))

(when (autoload-if-found
       '(yaml-mode)
       "yaml-mode" nil t)

  (push '("\\.yml$" . yaml-mode) auto-mode-alist))

(when (autoload-if-found
       '(json-mode)
       "json-mode" nil t)

  (push '("\\.json$" . json-mode) auto-mode-alist))

(when (autoload-if-found
       '(csv-mode)
       "csv-mode" nil t)

  (push '("\\.csv$" . csv-mode) auto-mode-alist))

(when (autoload-if-found
       '(cc-mode)
       "cc-mode" nil t)

  (push '("\\.pde$" . java-mode) auto-mode-alist) ;; Processing
  (push '("\\.java$" . java-mode) auto-mode-alist))

(when (autoload-if-found
       '(es-mode)
       "es-mode" nil t)

  (push '("\\.es$" . es-mode) auto-mode-alist))

(when (autoload-if-found
       '(gnuplot-mode)
       "gnuplot-mode" nil t)

  (push '("\\.plt$" . gnuplot-mode) auto-mode-alist))

(when (autoload-if-found
       '(markdown-mode)
       "markdown-mode" nil t)

  (push '("\\.markdown$" . markdown-mode) auto-mode-alist)
  (push '("\\.md$" . markdown-mode) auto-mode-alist))

(when (autoload-if-found
       '(logview-mode)
       "logview" nil t)
  (push '("\\.log$" . logview-mode) auto-mode-alist))

(when (autoload-if-found
       '(web-mode)
       "web-mode" "web-mode" t)

  ;; web-mode で開くファイルの拡張子を指定
  (push '("\\.phtml\\'" . web-mode) auto-mode-alist)
  (push '("\\.tpl\\.php\\'" . web-mode) auto-mode-alist)
  (push '("\\.jsp\\'" . web-mode) auto-mode-alist)
  (push '("\\.as[cp]x\\'" . web-mode) auto-mode-alist)
  (push '("\\.erb\\'" . web-mode) auto-mode-alist)
  (push '("\\.mustache\\'" . web-mode) auto-mode-alist)
  (push '("\\.djhtml\\'" . web-mode) auto-mode-alist)
  (push '("\\.html?\\'" . web-mode) auto-mode-alist)

  (with-eval-after-load "web-mode"
    (define-key web-mode-map (kbd "<tab>") 'my-web-indent-fold)

    (defun my-web-indent-fold ()
      (interactive)
      (web-mode-fold-or-unfold)
      (web-mode-buffer-indent)
      (indent-for-tab-command))

    ;; indent
    (setq web-mode-markup-indent-offset 1)

    ;; 色の設定
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(web-mode-comment-face ((t (:foreground "#D9333F"))))
     '(web-mode-css-at-rule-face ((t (:foreground "#FF7F00"))))
     '(web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))
     '(web-mode-css-rule-face ((t (:foreground "#A0D8EF"))))
     '(web-mode-doctype-face ((t (:foreground "#82AE46"))))
     '(web-mode-html-attr-name-face ((t (:foreground "#C97586"))))
     '(web-mode-html-attr-value-face ((t (:foreground "#82AE46"))))
     '(web-mode-html-tag-face ((t (:foreground "##4682ae" :weight bold))))
     '(web-mode-server-comment-face ((t (:foreground "#D9333F")))))))

;;(autoload 'po-mode "po-mode+" nil nil)
;;(autoload 'po-mode "po-mode" nil t)
(when (autoload-if-found
       '(po-mode)
       "po-mode" nil t)

  (push '("\\.po[tx]?\\'\\|\\.po\\$" . po-mode) auto-mode-alist))

(when (autoload-if-found
       '(yatex-mode)
       "yatex" "Yet Another LaTeX mode" t)

  (push '("\\.tex$" . yatex-mode) auto-mode-alist)

  ;; Disable auto line break
  (add-hook 'yatex-mode-hook
            (lambda ()
                (setq auto-fill-function nil)))

  (with-eval-after-load "yatex"
    ;; 1=Shift JIS, 2=JIS, 3=EUC, 4=UTF-8
    ;; (setq YaTeX-kanji-code nil)
    (modify-coding-system-alist 'file "\\.tex$'" 'utf-8)))

(when (autoload-if-found
       '(emmet-mode)
       "emmet-mode" nil t nil)

  (push '("\\.xml\\'" . nxml-mode) auto-mode-alist)
  (push '("\\.rdf\\'" . nxml-mode) auto-mode-alist)
  (dolist (hook
           '(sgml-mode-hook
             nxml-mode-hook css-mode-hook html-mode-hook web-mode-hook))
    (add-hook hook #'emmet-mode))

  (with-eval-after-load "emmet-mode"
    (setq emmet-indentation 2)
    (setq emmet-move-cursor-between-quotes t)))

(my-tick-init-time "editing")

(with-eval-after-load "vc-hooks"
  (setcdr (assq 'vc-mode mode-line-format)
          '((:eval (replace-regexp-in-string "^ Git" " " vc-mode)))))

;; mode-line
(set-face-attribute 'mode-line nil
                    :foreground "#FFFFFF"
                    :background "#b7655f"
;;                    :overline "#9d5446"
                    :box nil)
;; mode-line-inactive
(set-face-attribute 'mode-line-inactive nil
                    :foreground "#94bbf9"
                    :background "#d8e3fd"
                    :overline "#94bbf9"
                    :box nil)
;; Terminal
(unless (display-graphic-p)
  (set-face-foreground 'mode-line "#96CBFE")
  (set-face-background 'mode-line "#21252B"))

(setq line-number-display-limit-width 100000)

;;  (setq visible-bell nil) ;; default=nil
(setq ring-bell-function 'ignore)

(when (require 'empty-booting nil t)
  ;; (setq initial-buffer-choice t) ;; 引数付き起動すると画面分割される
  (setq initial-scratch-message nil)
  (setq initial-major-mode 'empty-booting-mode)
  ;;  :underline "#203e6f"
  (set-face-foreground 'header-line "#FFFFFF") ;; "#203e6f" #333333 "#FFFFFF"
  (set-face-background 'header-line "#5F7DB7") ;; "#ffb08c" "#7e59b5"
  (set-face-attribute 'header-line nil
                      :inherit nil
                      :overline nil
                      :underline nil)
  (setq header-line-format
        (concat
         " No day is a good day.                                       "
         (format-time-string "W%W: %Y-%m-%d %a."))))

(with-eval-after-load "postpone"
  (defun ad:split-window-below (&optional _size)
    "An extention to switch to \*scratch\* buffer after splitting window."
    (my-open-scratch))
  ;; (advice-add 'split-window-below :after #'ad:split-window-below)

  (defun my-open-scratch ()
    "Switch the current buffer to \*scratch\* buffer."
    (interactive)
    (switch-to-buffer "*scratch*"))
  (global-set-key (kbd "C-M-s") #'my-open-scratch))

;; Show scroll bar or not
(when (and (display-graphic-p)
           (eq window-system 'mac))
  (set-scroll-bar-mode nil)) ; 'right

;; Disable to show the tool bar.
(when (display-graphic-p)
  (tool-bar-mode -1))

(unless (display-graphic-p)
  (menu-bar-mode -1))

;; Disable to show the splash window at startup
(setq inhibit-startup-screen t)

(setq inhibit-default-init t)

(setq-default indicate-buffer-boundaries
              '((top . nil) (bottom . right) (down . right)))

(when (autoload-if-found
       '(helm-swoop)
       "helm-swoop" nil t)

  (with-eval-after-load "postpone"
        (global-set-key (kbd "M-s M-s") 'helm-swoop))

  (with-eval-after-load "helm-swoop"
    (require 'helm-config nil t)
    ;; カーソルの単語が org の見出し（*の集まり）なら検索対象にしない．
    (setq helm-swoop-pre-input-function
          (lambda()
            (unless (thing-at-point-looking-at "^\\*+")
              (thing-at-point 'symbol))))
    ;; 配色設定
    (set-face-attribute
     'helm-swoop-target-line-face nil :background "#FFEDDC")
    (set-face-attribute
     'helm-swoop-target-word-face nil :background "#FF5443")))

(when (autoload-if-found
       '(helm-M-x
         helm-buffers-list helm-recentf
         helm-locate helm-descbinds
         helm-occur helm-flycheck helm-bookmarks)
       "helm-config" nil t)

  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-M-r") 'helm-recentf)
  (global-set-key (kbd "M-x") 'helm-M-x)

  (with-eval-after-load "postpone"

    (global-set-key (kbd "C-M-l") 'helm-locate)
    (global-set-key (kbd "C-c f b") 'helm-bookmarks)
    (global-set-key (kbd "C-c o") 'helm-occur)
    (global-set-key (kbd "C-h d") 'helm-descbinds))

  (with-eval-after-load "projectile"
    (when (require 'helm-projectile nil t)
      ;; M-x helm-projectile-find-file (C-c p f)
      (setq projectile-completion-system 'helm)
      ;; projectile.el のキーバインドをオーバーライド
      (helm-projectile-toggle 1)))

  (with-eval-after-load "helm-config"
    ;; (when (require 'helm-files nil t)
    ;;   (define-key helm-find-files-map
    ;;     (kbd "<tab>") 'helm-execute-persistent-action)
    ;;   (define-key helm-read-file-map
    ;;     (kbd "<tab>") 'helm-execute-persistent-action))

    (when (memq window-system '(mac ns))
      (setq helm-locate-command "mdfind -name %s %s"))

    ;; (require 'helm-css-scss nil t)
    ;; (require 'helm-emmet nil t)
    ;; (require 'helm-descbinds nil t)

    ;; Configure helm-completing-read-handlers-alist after `(helm-mode 1)'
    (when (require 'helm nil t)
      (helm-mode 1))

    ;; helm-find-files を呼ばせない
    ;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
    ;; helm-mode-ag を呼ばせない
    (add-to-list 'helm-completing-read-handlers-alist '(ag . nil))
    ;; helm-mode-org-set-tags を呼ばせない
    (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . nil))

    (setq helm-display-source-at-screen-top nil)
    ;;         (setq helm-display-header-line nil)
    ;; helm-autoresize-mode を有効にしつつ 30% に固定
    (setq helm-autoresize-max-height 30)
    (setq helm-autoresize-min-height 30)
    (set-face-attribute 'helm-source-header nil
                        :height 1.0 :family "Verdana" :weight 'normal
                        :foreground "#666666" :background "#DADADA")
    (helm-autoresize-mode 1)))

(my-tick-init-time "presentation")

(my-tick-init-time "media")

(setq history-length 2000)

(setq undo-outer-limit nil)

;; *.~
(setq make-backup-files nil)
;; .#*
(setq auto-save-default nil)
;; auto-save-list
(setq auto-save-list-file-prefix nil)

(when (autoload-if-found
       '(session-initialize)
       "session" nil t)

  (unless noninteractive
    (when (display-graphic-p)
      (add-hook 'after-init-hook #'session-initialize)))

  (with-eval-after-load "session"
    (custom-set-variables
     '(session-set-file-name-exclude-regexp "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|COMMIT_EDITMSG")) ;; FIXME: not activated
    (add-to-list 'session-globals-exclude 'org-mark-ring)
    ;; Change save point of session.el
    (setq session-save-file
          (expand-file-name "~/Dropbox/emacs.d/.session"))
    (setq session-initialize '(de-saveplace session keys menus places)
          session-globals-include '((kill-ring 100)
                                    (session-file-alist 100 t)
                                    (file-name-history 200)
                                    ivy-dired-history-variable
                                    search-ring
                                    regexp-search-ring))
    (setq session-undo-check -1)))

;; FIXME
;;  (setq session-set-file-name-exclude-regexp
;;        "^/private/\\.\\*"))
;;          "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|^/private\\.*\\|^/var/folders\\.*"))

(my-tick-init-time "history")

(my-tick-init-time "development")

(cond
 ;; for Macintosh
 ((memq window-system '(mac ns))
  (setq initial-frame-alist
        (append
         '((top . 23)
           (left . 0)
           (alpha . (100 90))
           ;; (vertical-scroll-bars . nil)
           ;; (internal-border-width . 20)
           ;; (ns-appearance . nil) ;; 26.1 {light, dark}
           (ns-transparent-titlebar . t)) ;; 26.1
         initial-frame-alist)))

 ;; for Linux
 ((eq window-system 'x)
  (setq initial-frame-alist
        (append
         '((vertical-scroll-bars . nil)
           (top . 0)
           (left . 0)
           (width . 80)
           (height . 38))
         initial-frame-alist)))

 ;; for Windows
 (t (setq initial-frame-alist
          (append
           '((vertical-scroll-bars . nil)
             (top . 0)
             (left . 0)
             (width . 80)
             (height . 26))
           initial-frame-alist))))

;; Apply the initial setting to default
(setq default-frame-alist initial-frame-alist)
(with-eval-after-load "postpone"
  (set-face-foreground 'vertical-border (face-background 'default))
  (set-face-background 'vertical-border (face-background 'default)))
(set-face-background 'fringe (face-background 'default)) ;; 10-20[ms]

;; カーソルの色
(defconst my-cursor-color-ime-on "#FF9300")
(defconst my-cursor-color-ime-off "#91C3FF") ;; #FF9300, #999999, #749CCC
(defconst my-cursor-type-ime-on '(bar . 2))
(defconst my-cursor-type-ime-off '(bar . 2))
(defvar my-ime-last nil)

(when (and (memq window-system '(ns nil))
           (fboundp 'mac-get-current-input-source))
  (declare-function my-apply-cursor-config "init" nil)
  (defun my-ime-active-p ()
    (not (string-match "\\.Roman$" (mac-get-current-input-source))))
  (defun my-apply-cursor-config ()
    (interactive)
    (if (my-ime-active-p)
        (progn
          (setq cursor-type my-cursor-type-ime-on)
          (set-cursor-color my-cursor-color-ime-on))
      (setq cursor-type my-cursor-type-ime-off)
      (set-cursor-color my-cursor-color-ime-off)))
  (my-apply-cursor-config)
  (with-eval-after-load "postpone"
    (run-with-idle-timer 3 t #'my-apply-cursor-config)))

(with-eval-after-load "postpone"
  (cond
   ((and (memq window-system '(ns nil))
         (fboundp 'mac-get-current-input-source))
    (when (fboundp 'mac-set-input-method-parameter)
      (mac-set-input-method-parameter
       "com.google.inputmethod.Japanese.base" 'title " ")) ;; 

    (declare-function ad:mac-toggle-input-method "init" nil)
    (declare-function my-apply-cursor-config "init" nil)
    (declare-function my-ime-on "init" nil)
    (declare-function my-ime-off "init" nil)
    (declare-function my-ime-active-p "init" nil)

    (setq my-ime-last (my-ime-active-p))
    (defvar my-ime-on-hook nil)
    (defvar my-ime-off-hook nil)

    (defun my-ime-on ()
      (interactive)
      (when (fboundp 'mac-toggle-input-method)
        (mac-toggle-input-method t))
      (setq cursor-type my-cursor-type-ime-on)
      (set-cursor-color my-cursor-color-ime-on)
      (setq my-ime-last t)
      (run-hooks 'my-ime-on-hook))

    (defun my-ime-off ()
      (interactive)
      (when (fboundp 'mac-toggle-input-method)
        (mac-toggle-input-method nil))
      (setq cursor-type my-cursor-type-ime-off)
      (set-cursor-color my-cursor-color-ime-off)
      (setq my-ime-last nil)
      (run-hooks 'my-ime-off-hook))

    (defvar my-ime-flag nil)
    (add-hook 'activate-mark-hook
              (lambda ()
                (when (setq my-ime-flag (my-ime-active-p))
                  (my-ime-off))))
    (add-hook 'deactivate-mark-hook
              (lambda ()
                (when my-ime-flag
                  (my-ime-on))))

    (defun ad:mac-toggle-input-method (&optional arg)
      "Run hooks when IME changes."
      (interactive)
      (if arg
          (progn
            (setq cursor-type my-cursor-type-ime-on)
            (set-cursor-color my-cursor-color-ime-on)
            (run-hooks 'my-ime-on-hook))
        (progn
          (setq cursor-type my-cursor-type-ime-off)
          (set-cursor-color my-cursor-color-ime-off)
          (run-hooks 'my-ime-off-hook))))
    (advice-add 'mac-toggle-input-method
                :before #'ad:mac-toggle-input-method)

    ;; for init setup
    (setq-default cursor-type my-cursor-type-ime-on)

    (when (boundp 'mac-ime-cursor-type) ;; private patch
      (setq mac-ime-cursor-type my-cursor-type-ime-on))

    ;; Enter minibuffer with IME-off, and resture the latest IME
    (add-hook 'minibuffer-setup-hook
              (lambda ()
                (if (not (my-ime-active-p))
                    (setq my-ime-flag nil)
                  (setq my-ime-flag t)
                  (my-ime-off))))
    (add-hook 'minibuffer-exit-hook
              (lambda ()
                (when my-ime-flag
                  (my-ime-on))))

    ;; (defun ad:find-file (FILENAME &optional WILDCARDS)
    ;;   "Extension to find-file as before-find-file-hook."
    ;;   (message "--- ad:findfile")
    ;;   (apply FILENAME WILDCARDS))
    ;; (advice-add #'find-file :around #'ad:find-file)

    ;; http://tezfm.blogspot.jp/2009/11/cocoa-emacs.html
    ;; バッファ切替時に input method を切り替える
    ;; (with-eval-after-load "postpone"
    ;;   (when (and (fboundp 'mac-handle-input-method-change)
    ;;              (require 'cl nil t))
    ;;     (add-hook
    ;;      'post-command-hook
    ;;      (lexical-let ((previous-buffer nil))
    ;;        (message "Change IM %S -> %S" previous-buffer (current-buffer))
    ;;        (lambda ()
    ;;            (unless (eq (current-buffer) previous-buffer)
    ;;              (when (bufferp previous-buffer)
    ;;                (mac-handle-input-method-change))
    ;;              (setq previous-buffer (current-buffer))))))))
    )


   ;; EMP: Emacs Mac Port
   ((eq window-system 'mac)
    (when (fboundp 'mac-input-source)
      (defun my-mac-keyboard-input-source ()
        (if (string-match "\\.Roman$" (mac-input-source))
            (progn
              (setq cursor-type my-cursor-type-ime-off)
              (add-to-list 'default-frame-alist
                           `(cursor-type . ,my-cursor-type-ime-off))
              (set-cursor-color my-cursor-color-ime-off))
          (progn
            (setq cursor-type my-cursor-type-ime-on)
            (add-to-list 'default-frame-alist
                         `(cursor-type . ,my-cursor-type-ime-on))
            (set-cursor-color my-cursor-color-ime-on))))

      (when (fboundp 'mac-auto-ascii-mode)
        ;; (mac-auto-ascii-mode 1)
        ;; IME ON/OFF でカーソルの種別や色を替える
        (add-hook 'mac-selected-keyboard-input-source-change-hook
                  #'my-mac-keyboard-input-source)
        ;; IME ON の英語入力＋決定後でもカーソルの種別や色を替える
        ;; (add-hook 'mac-enabled-keyboard-input-sources-change-hook
        ;;           #'my-mac-keyboard-input-source)

        (declare-function my-mac-keyboard-input-source "init" nil)
        (my-mac-keyboard-input-source))))

   (t nil)))

;;(declare-function my-font-config "init" nil)
(with-eval-after-load "postpone"
  (defun ad:make-frame (&optional _parameters)
    (when (display-graphic-p)
      (my-theme))
    (when (require 'moom-font nil t)
      (moom-font-resize)))
  (advice-add 'make-frame :after #'ad:make-frame)
  (global-set-key (kbd "M-`") 'other-frame))

(defconst moom-autoloads
  '(moom-cycle-frame-height
    moom-move-frame-to-edge-top moom-move-frame my-frame-reset
    moom-toggle-frame-maximized
    moom-move-frame-to-center moom-move-frame-right moom-move-frame-left
    moom-fill-display-band moom-move-frame-to-edge-right moom-fill-band
    moom-change-frame-width moom-change-frame-width-double
    moom-change-frame-width-single))

(when (autoload-if-found
       moom-autoloads
       "moom" nil t)

  (global-set-key (kbd "<f2>") 'moom-cycle-frame-height)
  (global-set-key (kbd "M-2") 'moom-move-frame-to-center)
  (global-set-key (kbd "M-<f2>") 'moom-toggle-frame-maximized)

  (with-eval-after-load "moom"
    (setq moom-lighter "M")
    (setq moom-verbose t)
    (let ((moom-verbose nil))
      (moom-recommended-keybindings 'all))
    (moom-mode 1)
    (my-font-config)))  ;; this could increase `postpone-init-time'.

(when (autoload-if-found
       '(moom-font-increase
         moom-font-decrease moom-font-size-reset moom-font-resize)
       "moom-font" nil t)

  (add-hook 'moom-font-after-resize-hook #'moom-expand-height)
  (add-hook 'moom-font-after-resize-hook #'moom-move-frame-to-edge-top)

  (with-eval-after-load "moom-font"
    (setq moom-scaling-gradient (/ (float 50) 30))
    (setq moom-font-table
          '((50 30) (49 29) (48 29) (47 28) (46 28) (45 27) (44 26) (43 26)
            (42 25) (41 25) (40 24) (39 23) (38 23) (37 22) (36 22) (35 21)
            (34 20) (33 20) (32 19) (31 19) (30 18) (29 17) (28 17) (27 16)
            (26 16) (25 15) (24 14) (23 14) (22 13) (21 13) (20 12) (19 11)
            (18 11) (17 10) (16 10) (15 9) (14 8) (13 8) (12 7) (11 7) (10 6)
            (9 5) (8 5) (7 4) (6 4) (5 3)))))

(my-tick-init-time "frame and window")

;; 1) Monaco, Hiragino/Migu 2M : font-size=12, -apple-hiragino=1.2
;; 2) Inconsolata, Migu 2M     : font-size=14,
;; 3) Inconsolata, Hiragino    : font-size=14, -apple-hiragino=1.0
(defconst my-font-size 12)
(defconst my-ja-font "Migu 2M") ;; "Hiragino Maru Gothic Pro"
(defconst my-ascii-font "Monaco") ;; "Inconsolata", Monaco
;; (defconst my-ja-font "Hiragino Maru Gothic Pro") ;; "Hiragino Maru Gothic Pro"
;; (defconst my-ascii-font "Inconsolata") ;; "Inconsolata", Menlo, "Ricty Diminished"
(defun my-ja-font-setter (spec)
  (set-fontset-font nil 'japanese-jisx0208 spec)
  (set-fontset-font nil 'katakana-jisx0201 spec)
  (set-fontset-font nil 'japanese-jisx0212 spec)
  (set-fontset-font nil '(#x0080 . #x024F) spec)
  (set-fontset-font nil '(#x0370 . #x03FF) spec)
  (set-fontset-font nil 'mule-unicode-0100-24ff spec))
(defun my-ascii-font-setter (spec)
  (set-fontset-font nil 'ascii spec))
(defun my-unicode-font-setter (spec)
  (set-fontset-font nil 'unicode spec))
(defun my-font-config (&optional size ascii ja)
  "Font config.
- SIZE: font size for ASCII and Japanese (default: 12)
- ASCII: ascii font family (default: \"Monaco\")
- JA: Japanese font family (default: \"Migu 2M\")
"
  (when (memq window-system '(mac ns))
    (let ((font-size (or size my-font-size))
          ;; (unicode-font (or uc "FreeSerif"))
          (ascii-font (or ascii my-ascii-font))
          (ja-font (or ja my-ja-font)))
      ;; (my-unicode-font-setter
      ;;  (font-spec :family unicode-font :size font-size))
      (my-ascii-font-setter (font-spec :family ascii-font :size font-size))
      (my-ja-font-setter (font-spec :family ja-font :size font-size)))))

(cond
 ;; CocoaEmacs
 ((memq window-system '(mac ns))
  (when (>= emacs-major-version 23)

    ;; Fix ratio provided by set-face-attribute for fonts display
    (setq face-font-rescale-alist
          '(("^-apple-hiragino.*" . 1.0) ; 1.2
            (".*Migu.*" . 1.2)
            (".*Ricty.*" . 1.0)
            (".*Inconsolata.*" . 1.0)
            (".*osaka-bold.*" . 1.0)     ; 1.2
            (".*osaka-medium.*" . 1.0)   ; 1.0
            (".*courier-bold-.*-mac-roman" . 1.0) ; 0.9
            ;; (".*monaco cy-bold-.*-mac-cyrillic" . 1.0)
            ;; (".*monaco-bold-.*-mac-roman" . 1.0) ; 0.9
            ("-cdac$" . 1.0))))) ; 1.3
 ;; (my-font-config) ;; see `my-theme'

 ((eq window-system 'ns)
  ;; Anti aliasing with Quartz 2D
  (when (boundp 'mac-allow-anti-aliasing)
    (setq mac-allow-anti-aliasing t)))

 ((eq window-system 'w32) ; windows7
  (let ((font-size 14)
        (font-height 100)
        (ascii-font "Inconsolata")
        (ja-font "メイリオ")) ;; Meiryo UI
    (my-ja-font-setter
     (font-spec :family ja-font :size font-size :height font-height))
    (my-ascii-font-setter (font-spec :family ascii-font :size font-size))
    (setq face-font-rescale-alist '((".*Inconsolata.*" . 1.0))))) ; 0.9

 ((eq window-system 'x) ; for SuSE Linux 12.1
  (let
      ((font-size 14)
       (font-height 100)
       (ascii-font "Inconsolata")
       ;; (ja-font "MigMix 1M")
       (ja-font "Migu 2M"))
    (my-ja-font-setter
     (font-spec :family ja-font :size font-size :height font-height))
    (my-ascii-font-setter (font-spec :family ascii-font :size font-size)))
  (setq face-font-rescale-alist '((".*MigMix.*" . 2.0)
                                  (".*Inconsolata.*" . 1.0))))) ; 0.9

(set-default 'line-spacing 0.2)

(declare-function my-daylight-theme "init" nil)
(declare-function my-night-theme "init" nil)
(declare-function my-terminal-theme "init" nil)
(if (not (display-graphic-p))
    (defun my-terminal-theme ()
      (interactive)
      (when (require 'terminal-theme nil t)
        (mapc 'disable-theme custom-enabled-themes)
        (load-theme 'terminal t)
        (setq my-cursor-color-ime-on "#FF9300")))

  (defun my-daylight-theme ()
    (when (require 'daylight-theme nil t)
      (mapc 'disable-theme custom-enabled-themes)
      (load-theme 'daylight t)
      (setq my-cursor-color-ime-on "#FF9300")
      (setq default-frame-alist
            (delete (assoc 'ns-appearance default-frame-alist)
                    default-frame-alist))
      (setq default-frame-alist
            (delete (assoc 'ns-transparent-titlebar default-frame-alist)
                    default-frame-alist))
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . light))
      (modify-frame-parameters nil '((ns-transparent-titlebar . t)
                                     (ns-appearance . light)))))

  (defun my-night-theme ()
    (when (require 'night-theme nil t) ;; atom-one-dark-theme
      (mapc 'disable-theme custom-enabled-themes)
      (load-theme 'night t)
      (setq my-cursor-color-ime-on "RosyBrown") ;; #cebcfe
      (setq default-frame-alist
            (delete (assoc 'ns-appearance default-frame-alist)
                    default-frame-alist))
      (setq default-frame-alist
            (delete (assoc 'ns-transparent-titlebar default-frame-alist)
                    default-frame-alist))
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . dark))
      (modify-frame-parameters nil '((ns-transparent-titlebar . t)
                                     (ns-appearance . dark))))))

(declare-function my-apply-cursor-config "init" nil)
(declare-function my-font-config "init" nil)
(defun my-night-time-p (begin end)
  (let* ((ch (string-to-number (format-time-string "%H" (current-time))))
         (cm (string-to-number (format-time-string "%M" (current-time))))
         (ct (+ cm (* 60 ch))))
    (if (> begin end)
        (or (<= begin ct) (<= ct end))
      (and (<= begin ct) (<= ct end)))))

(defvar my-frame-appearance nil) ;; {nil, 'dark, 'light}
(defun my-theme (&optional type)
  (interactive "MType (light or dark): ")
  (setq my-frame-appearance
        (cond ((member type '("light" "l")) 'light)
              ((member type '("dark" "d")) 'dark)
              (t
               my-frame-appearance)))
  (if (display-graphic-p)
      (cond ((eq my-frame-appearance 'dark)
             (my-night-theme))
            ((eq my-frame-appearance 'light)
             (my-daylight-theme))
            (t
             (let ((night-time-in 21)
                   (night-time-out 5))
               (if (my-night-time-p
                    (* night-time-in 60) (* night-time-out 60))
                   (my-night-theme)
                 (my-daylight-theme)))))
    (my-terminal-theme))

  (unless noninteractive
    (my-font-config)
    ;; remove unintentional colored frame border
    (select-frame-set-input-focus (selected-frame))
    (when (and (display-graphic-p)
               (fboundp 'mac-get-current-input-source))
      (my-apply-cursor-config)))) ;; apply font setting

;; init. This may override or reset font setting
(with-eval-after-load "postpone"
  (my-theme))

;; (with-eval-after-load "postpone"
;;   (run-at-time "21:00" 86400 'my-theme)
;;   (run-at-time "05:00" 86400 'my-theme)) ;; FIXME: it makes frame blink

(my-tick-init-time "font")

(when (autoload-if-found
       '(my-cmd-to-open-iterm2)
       "utility" nil t)

  (global-set-key (kbd "C-M-i") #'my-cmd-to-open-iterm2)

  (with-eval-after-load "flyspell"
    (define-key flyspell-mode-map (kbd "C-M-i") #'my-cmd-to-open-iterm2))

  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-M-i") #'my-cmd-to-open-iterm2)))

(when (autoload-if-found
       '(counsel-osx-app)
       "counsel-osx-app" nil t)

  (global-set-key (kbd "C-M-1") 'counsel-osx-app)

  (with-eval-after-load "counsel-osx-app"
    (custom-set-variables
     '(counsel-osx-app-location
       '("/Applications" "/Applications/Utilities"
         "/Applications/Microsoft Remote Desktop.localized")))))

(when (autoload-if-found
       '(lingr-login)
       "lingr" nil t)

  (with-eval-after-load "lingr"
    (defun my-lingr-login ()
      (when (string= "Sat" (format-time-string "%a"))
        (lingr-login)))))

(my-tick-init-time "utility")
(provide 'init)
