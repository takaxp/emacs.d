;; late-init.el --- My config with postpone.el -*- lexical-binding: t -*-
(require 'init-autoloads nil t)

(setq message-log-max 5000) ;; メッセージバッファの長さ
;; (eval-when-compile
;;   (require 'shut-up nil t))
(defvar shutup-p nil)
;;(setq shutup-p (require 'shut-up nil t))
(setq shutup-p nil)
;; (setq shutup-p (and (not (my-native-comp-p)) (require 'shut-up nil t)))

(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)
(setq-default fringe-indicator-alist
              (append (list '(continuation . (nil right-curly-arrow)))
                      (remove (assoc 'continuation fringe-indicator-alist)
                              fringe-indicator-alist)))
;; fringeに表示するマークの形状を変更
(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b01111110
   #b01111110
   #b00000110
   #b00000110])

(setq mouse-drag-copy-region t)

(setq compilation-scroll-output t)

(setq hscroll-margin 40)

(setq confirm-kill-emacs 'y-or-n-p)

(autoload-if-found
 '(el-get-version el-get-bundle
   my-elget-list my-elget-reset-links
   el-get-cd el-get-install el-get-remove el-get-update)
 "elget-config" nil t)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)
(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-conf)
(add-hook 'emacs-lisp-mode-hook #'turn-on-font-lock)

(setq vc-follow-symlinks t)

(unless noninteractive
  (global-auto-revert-mode 1)
  ;; revert されるのが org バッファのとき，自動的にドロワをたたむ
  ;; カーソルが (point-max) に移動してしまう場合は非推奨
  (with-eval-after-load "org"
    (add-hook 'after-revert-hook 'my-org-hide-drawers-all)))

(unless noninteractive
  (when (fboundp 'pixel-scroll-mode)
    (pixel-scroll-mode 1))) ;; 26.1

(when (autoload-if-found
       '(aggressive-indent-mode)
       "aggressive-indent" nil t)

  (dolist (hook
           '(;; python-mode-hook
             ;; nxml-mode-hook
             ;; web-mode-hook
             emacs-lisp-mode-hook
             lisp-mode-hook perl-mode-hook c-mode-common-hook))

    (add-hook hook #'aggressive-indent-mode)))

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(when (autoload-if-found
       '(ws-butler-mode ws-butler-global-mode)
       "ws-butler" nil t)

  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-mode-hook perl-mode-hook c-mode-common-hook))
    (add-hook hook #'ws-butler-mode))

  (with-eval-after-load "ws-butler"
    (custom-set-variables
     '(ws-butler-global-exempt-modes
       (append '(org-mode empty-booting-mode change-log-mode epa-mode)
		           ws-butler-global-exempt-modes)))))

;; using both could load twice since find-file also kicks while loading the gpg.
;; (add-hook 'find-file-hook #'my-private-conf-activate)
(run-with-idle-timer 2 nil #'my-private-conf-activate)

(with-eval-after-load "epa"
  ;; Suppress message when saving encrypted file (hoge.org.gpg)
  (defun ad:epa-file-write-region (f start end file &optional append visit
                                     lockname mustbenew)
    (let ((message-log-max nil))
      (funcall f start end file append visit lockname mustbenew)))
  (advice-add 'epa-file-write-region :around #'ad:epa-file-write-region))

(when (memq window-system '(ns nil))
  ;; toggle-input-method
  ;; (declare-function my-ns-org-heading-auto-ascii "init" nil)
  ;; (declare-function my-ime-active-p "init" nil)

  (when (fboundp 'mac-ime-toggle)
    (defalias 'my-toggle-ime-ns 'mac-ime-toggle)
    (defalias 'my-ime-active-p 'mac-ime-active-p)) ;; FIXME

  (global-set-key (kbd "M-SPC") 'my-toggle-ime-ns)
  (global-set-key (kbd "S-SPC") 'my-toggle-ime-ns)
  (define-key isearch-mode-map (kbd "M-SPC") 'my-toggle-ime-ns)
  (define-key isearch-mode-map (kbd "S-SPC") 'my-toggle-ime-ns)

  (custom-set-faces
   '(ns-marked-text-face
     ((t (:foreground "black"
                      :background "light pink" :underline "OrangeRed2"))))
   '(ns-unmarked-text-face
     ((t (:foreground "black"
                      :background "light sky blue" :underline "royal blue")))))

  (when (and (memq window-system '(ns nil))
             (fboundp 'mac-get-current-input-source))

    (when (version< "27.0" emacs-version)
      ;; "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese" for Big Sur
      (custom-set-variables
       '(mac-default-input-source "com.google.inputmethod.Japanese.base"))
      (mac-input-method-mode 1)

      ;; FIXME Conflict with migemo...
      ;; To fix this issue, you need to update migemo.el to support minor-mode.
      (when (and (executable-find "cmigemo")
                 (require 'migemo nil t))
        (defun my-isearch-ime-deactivate-sticky ()
          (unless (region-active-p)
            (mac-ime-deactivate-sticky)))
        ;; see also activate-mark-hook, deactivate-mark-hook
        (add-hook 'isearch-mode-hook #'my-isearch-ime-deactivate-sticky)
        (add-hook 'isearch-mode-end-hook #'mac-ime-activate-sticky))))

  (with-eval-after-load "org"
    ;; カーソル移動で heading に来たときは即座にIMEをOFFにする
    ;; (add-hook 'after-move-cursor-hook #'my-ns-org-heading-auto-ascii)
    ;; カーソル移動で heading に留まった時にIMEをOFFにする
    (run-with-idle-timer 0.2 t #'my-ns-org-heading-auto-ascii))

  (with-eval-after-load "hl-line"
    (add-hook 'input-method-activate-hook #'my-working-text-face-on)
    (add-hook 'input-method-deactivate-hook #'my-working-text-face-off)))

(global-set-key (kbd "C-M-t") 'beginning-of-buffer)
(global-set-key (kbd "C-M-b") 'end-of-buffer)
;; Backward page scrolling instead of M-v
(global-set-key (kbd "C-t") 'scroll-down)
;; Frontward page scrolling instead of C-v
;; (global-set-key (kbd "M-n") 'scroll-up)
;; Move cursor to a specific line
(global-set-key (kbd "C-c g") 'goto-line)

(global-set-key (kbd "C-M-p") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-M-n") (lambda () (interactive) (other-window 1)))

(defun ad:mark-sexp (f &optional arg allow-extend)
  "Set mark ARG sexps from point.
When the cursor is at the end of line or before a whitespace, set ARG -1."
  (interactive "P\np")
  (funcall f (if (and (not (bolp))
                      (not (eq (preceding-char) ?\ ))
                      (or (eolp)
                          (eq (following-char) ?\ )
                          (memq (preceding-char) '(?\) ?\> ?\] ?\}))))
                 -1 arg)
           allow-extend))
(advice-add 'mark-sexp :around #'ad:mark-sexp)

(with-eval-after-load "expand-region"
    (defun ad:er:mark-sexp (f &optional arg allow-extend)
      "If the cursor is on a symbol, expand the region along the symbol."
      (interactive "P\np")
      (if (and (not (use-region-p))
               (symbol-at-point))
          (er/mark-symbol)
        (funcall f arg allow-extend)))
  (advice-add 'mark-sexp :around #'ad:er:mark-sexp))

;; Scroll window on a line-by-line basis
(setq scroll-conservatively 1000)
(setq scroll-step 1)
(setq scroll-preserve-screen-position t) ;; スクロール時にスクリーン内で固定
;;  (setq scroll-margin 0) ; default=0

;; Scroll window on a page-by-page basis with N line overlapping
(setq next-screen-context-lines 10)

(setq set-mark-command-repeat-pop t)
(setq mark-ring-max 32)
(setq global-mark-ring-max 64)

(when (require 'ah nil t)
  (setq ah-lighter "")
  (unless noninteractive
    (ah-mode 1)))

(when (autoload-if-found
       '(smooth-scroll-mode)
       "smooth-scroll" nil t)

  (with-eval-after-load "smooth-scroll"
    (custom-set-variables
     '(smooth-scroll/vscroll-step-size 6)
     '(smooth-scroll/hscroll-step-size 6)))

  (unless noninteractive
    (smooth-scroll-mode t)))

(global-set-key (kbd "M-]") 'bs-cycle-next)
(when (display-graphic-p)
  (global-set-key (kbd "M-[") 'bs-cycle-previous))

(with-eval-after-load "bs"
  (custom-set-variables
   '(bs-cycle-configuration-name "files-and-scratch")
   '(bs-max-window-height 10))

  ;; リストを縦表示する
  (when (require 'bsv nil t)
    (setq bsv-max-height 5
          bsv-message-timeout 9)))

(when (autoload-if-found
       '(my-toggle-bm
         my-bm-next bm-buffer-save bm-buffer-restore bm-buffer-save-all
         bm-repository-save bm-repository-load counsel-bm)
       "bm" nil t)

  ;; ファイルオープン時にブックマークを復帰
  (global-set-key (kbd "<f10>") 'my-toggle-bm)
  (global-set-key (kbd "<C-f10>") 'my-bm-next)
  (global-set-key (kbd "<S-f10>") 'bm-show-all)
  (add-hook 'find-file-hook #'bm-buffer-restore)

  ;; ビルトイン bookmark の配色を無効にする(as of 28.1)
  (setq bookmark-fontify nil)

  ;; ビルトイン bookmark がfringeに出すマークを無効にする(as of 28.1)
  (setq bookmark-set-fringe-mark nil)

  (with-eval-after-load "ivy"
    (global-set-key (kbd "<S-f10>") 'counsel-bm))

  (with-eval-after-load "bm"
    ;; (setq bm-annotation-width 30)
    (setq-default bm-buffer-persistence t)
    (setq bm-restore-repository-on-load t)
    (setq bm-cycle-all-buffers t)
    ;; (setq bm-toggle-buffer-persistence t)
    (setq bm-buffer-persistence t)
    (setq bm-persistent-face 'bm-face)
    (setq bm-repository-file
          (expand-file-name
           (concat (getenv "SYNCROOT") "/emacs.d/.bm-repository")))

    (unless noninteractive
      (bm-repository-load)
      (add-hook 'kill-buffer-hook 'bm-buffer-save)
      (add-hook 'after-save-hook 'bm-buffer-save)
      (add-hook 'after-revert-hook 'bm-buffer-restore)
      (add-hook 'kill-emacs-hook #'my-bm-save-all))

    (defun ad:bm-show-mode ()
      "Enable truncate mode when showing bm list."
      (toggle-truncate-lines 1))
    (advice-add 'bm-show-mode :after #'ad:bm-show-mode)
    ))

(when (autoload-if-found
       '(centered-cursor-mode)
       "centered-cursor-mode" nil t)

  ;; isearch の時はOFFにする
  (defun my-centered-cursor-activate () (centered-cursor-mode 1))
  (defun my-centered-cursor-deactivate () (centered-cursor-mode -1))
  (add-hook 'isearch-mode-hook #'my-centered-cursor-activate)
  (add-hook 'isearch-mode-end-hook #'my-centered-cursor-deactivate))

(when (autoload-if-found
       '(smart-mark-mode)
       "smart-mark" nil t)

  (add-hook 'find-file-hook #'my-smart-mark-activate)

  (with-eval-after-load "smart-mark"
    (progn ;; C-M-SPC SPC SPC ... C-g の場合に正しくカーソルと元に戻す．
      (defun ad:smart-mark-restore-cursor ()
        "Restore cursor position saved just before mark."
        (when smart-mark-point-before-mark
          (when (> smart-mark-point-before-mark 1)
            ;; To avoid to jump to the beginning of the buffer
            (goto-char smart-mark-point-before-mark))
          (setq smart-mark-point-before-mark nil)))
      (advice-add 'smart-mark-restore-cursor :override
                  #'ad:smart-mark-restore-cursor)

      (defun ad:smart-mark-set-restore-before-mark (&rest _arg)
        (unless (memq this-command
                      '(er/expand-region er/mark-symbol er/contract-region))
          (setq smart-mark-point-before-mark (point))))
      (advice-add 'smart-mark-set-restore-before-mark :override
                  #'ad:smart-mark-set-restore-before-mark)

      (when (require 'expand-region-core nil t)
        (defun ad:er:keyboard-quit ()
          (when (memq last-command '(er/expand-region er/contract-region))
            (when smart-mark-point-before-mark
              (goto-char smart-mark-point-before-mark))))
        (advice-add 'keyboard-quit :after #'ad:er:keyboard-quit)

        (defun ad:er:pre:keyboard-quit ()
          (when (memq last-command '(er/expand-region er/contract-region))
            (er/contract-region 0)
            ;; (when (> smart-mark-point-before-mark 1) ;; FIXME
            ;;   (goto-char smart-mark-point-before-mark))
            )))
      ;; (advice-add 'keyboard-quit :before #'ad:er:pre:keyboard-quit)
      )))
;; (defun my-smart-mark-activate () (smart-mark-mode 1))
;; (defun my-smart-mark-dectivate () (smart-mark-mode -1))
;; (add-hook 'isearch-mode-hook #'my-smart-mark-dectivate)
;; (add-hook 'isearch-mode-end-hook #'my-smart-mark-activate)

(when (autoload-if-found
       '(global-syntax-subword-mode)
       "syntax-subword" nil t)

  (add-hook 'find-file-hook #'my-syntax-subword-activate))

(setq yank-excluded-properties t)

(add-hook 'before-save-hook #'my-time-stamp)

(with-eval-after-load "time-stamp"
  (setq time-stamp-start "#\\+date:[ \t]*") ;; "Time-stamp:[ \t]+\\\\?[\"<]+"
  (setq time-stamp-end "$") ;; "\\\\?[\">]"
  (setq time-stamp-line-limit 10)) ;; def=8

(defadvice isearch-mode
    (around isearch-mode-default-string
            (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;; C-g を isearch-exit に割り当てて途中中断とする．（カーソルを留めておきたい）カーソルを検索開始時点の場所に戻すには，別途 counsel-mark-ring を使う
(define-key isearch-mode-map (kbd "C-g") 'isearch-exit)

(add-hook 'change-log-mode-hook
          (lambda ()
            (view-mode 1)
            (my-orgalist-activate)
            (setq tab-width 4)
            (setq left-margin 4)))

(defun ad:add-change-log-entry-other-window ()
  (when view-mode
    (View-exit-and-edit)))

(advice-add 'add-change-log-entry-other-window
            :before #'ad:add-change-log-entry-other-window)

(when (autoload-if-found
       '(modern-c++-font-lock-mode)
       "modern-cpp-font-lock" nil t)
  (push '("\\.[hm]$" . c++-mode) auto-mode-alist)
  (add-hook 'c-mode-hook #'modern-c++-font-lock-mode)
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(with-eval-after-load "info"
  (add-to-list 'Info-additional-directory-list
               (expand-file-name "~/devel/mygit/org-ja/work/")))

(add-hook 'nxml-mode-hook
          (lambda ()
            (define-key nxml-mode-map "\r" 'newline-and-indent)
            (auto-fill-mode -1)
            (setq indent-tabs-mode t)
            (setq nxml-slash-auto-complete-flag t)
            (setq tab-width 1)
            (setq nxml-child-indent 1)
            (setq nxml-attribute-indent 0)))

(when (autoload-if-found
       '(yaml-mode)
       "yaml-mode" nil t)

  (push '("\\.yml$" . yaml-mode) auto-mode-alist))

(when (autoload-if-found
       '(json-mode)
       "json-mode" nil t)

  (push '("\\.json$" . json-mode) auto-mode-alist)

  (with-eval-after-load "json-mode"
    (add-hook 'before-save-hook #'my-json-mode-beautify)
    (add-hook 'after-save-hook #'my-json-pretty-print-buffer)))

(when (autoload-if-found
       '(csv-mode)
       "csv-mode" nil t)

  (push '("\\.csv$" . csv-mode) auto-mode-alist))

(autoload-if-found '(ascii-on ascii-off) "ascii" nil t)

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
       '(markdown-mode)
       "markdown-mode" nil t)

  (push '("\\.markdown$" . markdown-mode) auto-mode-alist)
  (push '("\\.md$" . markdown-mode) auto-mode-alist))

(if (executable-find "cmake")
    (when (autoload-if-found
           '(cmake-mode)
           "cmake-mode" nil t)

      (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
      (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode)))
  (message "--- cmake is NOT installed."))

(when (autoload-if-found
       '(logview-mode)
       "logview" nil t)
  (push '("\\.log$" . logview-mode) auto-mode-alist))

;; 特定の拡張子・ディレクトリ
(defvar my-auto-view-regexp "\\.el.gz$\\|\\.patch$\\|\\.xml$\\|\\.csv$\\|\\.emacs.d/[^/]+/el-get\\|config")

;; 特定のディレクトリ（絶対パス・ホームディレクトリ以下）
(defvar my-auto-view-dirs nil)
(add-to-list 'my-auto-view-dirs "~/devel/emacs-head/emacs/")
(add-to-list 'my-auto-view-dirs "~/devel/git/org-mode/lisp/")
(when (eq window-system 'w32)
  (add-to-list 'my-auto-view-dirs "c:/msys64/mingw64"))

;; (autoload 'my-auto-view "view" nil t)
(add-hook 'find-file-hook #'my-auto-view)

(with-eval-after-load "view"
  (define-key view-mode-map (kbd "i") 'View-exit-and-edit)
  (define-key view-mode-map (kbd "<SPC>") 'ignore)
  (define-key view-mode-map (kbd "<DEL>") 'ignore)
  (define-key view-mode-map (kbd "S-SPC") 'mac-ime-toggle)
  (define-key view-mode-map (kbd "e") 'my-view-exit)
  (when (require 'helpful nil t)
    (define-key view-mode-map (kbd "h") 'helpful-at-point))
  (define-key view-mode-map (kbd "f") 'forward-char)
  (define-key view-mode-map (kbd "b") 'backward-char)
  (define-key view-mode-map (kbd "n") 'my-org-view-next-heading)
  (define-key view-mode-map (kbd "p") 'my-org-view-previous-heading)
  (define-key view-mode-map (kbd "g") #'my-google-this)
  (define-key view-mode-map (kbd "<tab>") 'my-view-tab)
  (define-key view-mode-map (kbd "S-<tab>") 'my-view-shifttab)
  (defun ad:view--enable () (my-mode-line-on))
  (defun ad:view--disable () (my-mode-line-off))
  (unless my-toggle-modeline-global
    (advice-add 'view--enable :before #'ad:view--enable)
    (advice-add 'view--disable :before #'ad:view--disable)))

(when (autoload-if-found
       '(web-mode)
       "web-mode" "web mode" t)

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
    (define-key web-mode-map (kbd "S-<tab>") 'my-web-indent-fold)

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
       '(go-mode) "go-mode" nil t)

  (push '("\\.go\\'" . go-mode) auto-mode-alist))

(when (autoload-if-found
       '(ispell-region ispell-complete-word)
       "ispell" nil t)

  ;; Spell checking within a specified region
  (global-set-key (kbd "C-c f 7") 'ispell-region)
  ;; 補完候補の表示（flyspell が使える時はそちらを優先して <f7> にする．
  (global-set-key (kbd "<f7>") 'ispell-word)

  (with-eval-after-load "ispell"
    (setq ispell-encoding8-command t)
    ;; for English and Japanese mixed
    (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
    ;; http://endlessparentheses.com/ispell-and-org-mode.html
    (add-to-list 'ispell-skip-region-alist '("^#\\+begin_src" . "^#\\+end_src"))
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))

    (cond
     ((executable-find "hunspell")
      (setenv "LC_ALL" "en_US")
      ;; (message "--- hunspell loaded.")
      ;; (setenv "DICPATH" "/Applications/LibreOffice.app/Contents/Resources/extensions/dict-en")
      (setenv "DICPATH" (concat (getenv "SYNCROOT") "/emacs.d/hunspell/dict-en"))
      (setq ispell-local-dictionary-alist
            '(("ja_JP" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
               ("-d" "en_US") nil utf-8)
              ("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
               ("-d" "en_US") nil utf-8)))
      (setq ispell-local-dictionary "en_US")
      (setq ispell-dictionary ispell-local-dictionary)
      (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)
      (if shutup-p
          ;; 必要．しかも ispell-program-name 指定の前で．
          ;; ただし，ispell-local-dictionary-alist の後で．
          (shut-up (ispell-change-dictionary "en_US" t))
        (ispell-change-dictionary "en_US" t))
      (setq-default ispell-program-name (executable-find "hunspell"))
      ;; Not regal way, but it's OK (usually ispell-local-dictionary-alist)

      (setq ispell-personal-dictionary
            (concat (getenv "SYNCROOT") "/emacs.d/.hunspell.en.dic")))

     ((executable-find "aspell")
      ;; (message "--- aspell loaded.")
      (setq-default ispell-program-name "aspell")
      ;; (when (eq window-system 'w32)
      ;;   (setq-default ispell-program-name
      ;;                 "C:/Program Files/Aspell/bin/aspell.exe"))
      (setq ispell-dictionary "english")
      ;; This will also avoid an IM-OFF issue for flyspell-mode.
      ;; (setq ispell-aspell-supports-utf8 t) ;; Obsolete
      (setq ispell-local-dictionary-alist
            '((nil "[a-zA-Z]" "[^a-zA-Z]" "'" t
                   ("-d" "en" "--encoding=utf-8") nil utf-8)))
      (setq ispell-personal-dictionary
            (concat (getenv "SYNCROOT") "/emacs.d/config/aspell.en.pws")))
     (t
      nil))))

(when (autoload-if-found
       '(flyspell-mode-on flyspell-prog-mode flyspell-mode
                          my-flyspell-mode-on my-flyspell-prog-mode)
       "flyspell" nil t)

  (defvar major-mode-with-flyspell
    '(text-mode change-log-mode latex-mode yatex-mode
                git-commit-mode org-mode))
  (defvar major-mode-with-flyspell-prog
    '(c-mode-common emacs-lisp-mode perl-mode python-mode))
  (defvar my-flyspell-target-modes
    (append major-mode-with-flyspell
            major-mode-with-flyspell-prog))

  ;; バッファ内の全てをチェック対象にするモードの hook に flyspell 起動を登録
  (dolist (hook major-mode-with-flyspell)
    (add-hook (intern (format "%s-hook" hook)) #'flyspell-mode))

  ;; コメント行のみをチェック対象にする
  (dolist (hook major-mode-with-flyspell-prog)
    (add-hook (intern (format "%s-hook" hook)) #'flyspell-prog-mode))

  (with-eval-after-load "flyspell"

    ;; C-; をオーバーライド
    (define-key flyspell-mode-map (kbd "C-;") 'comment-dwim)
    (setq flyspell-duplicate-distance 0)
    ;; (setq flyspell-mode-line-string " F")
    (setq flyspell-mode-line-string "")
    ;; (setq flyspell-large-region 200)
    (set-face-attribute 'flyspell-duplicate nil
                        :foreground "#EA5506" :bold t
                        :background nil :underline t)
    (set-face-attribute 'flyspell-incorrect nil
                        :foreground "#BA2636" :bold nil
                        :background nil :underline t)

    ;; ispell-complete-word のキーバインドを上書き
    (global-set-key (kbd "<f7>") 'flyspell-correct-at-point)

    ;; ivy を用いる
    (when (require 'flyspell-correct-ivy nil t)
      (setq flyspell-correct-interface #'flyspell-correct-ivy))

    ;; Auto complete との衝突を回避
    (with-eval-after-load "auto-complete"
      (ac-flyspell-workaround))

    ;; [FIXME] nextstep+inline-patch版で flyspell すると，日本語nyuuのようになる場合があるので，それを回避（IME が ONになったら一時的に flyspell を止める）
    (add-hook 'input-method-activate-hook #'my-flyspell-off)
    (add-hook 'input-method-deactivate-hook #'my-flyspell-on)))

(global-set-key (kbd "M-=") 'count-words)

(autoload-if-found '(counsel-world-clock) "counsel-world-clock" nil t)

(when (autoload-if-found
       '(latex-math-preview-expression
         latex-math-preview-insert-symbol
         latex-math-preview-save-image-file
         latex-math-preview-beamer-frame)
       "latex-math-preview" nil t nil)

  (global-set-key (kbd "<f6>") 'latex-math-preview-expression)

  (with-eval-after-load "latex-math-preview"
    (setq latex-math-preview-command-path-alist
          '((latex . "latex")
            (dvipng . "dvipng")
            (dvips . "dvips")))
    (define-key latex-math-preview-expression-mode-map (kbd "<f6>")
      'latex-math-preview-delete-buffer)))

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
    (modify-coding-system-alist 'file "\\.tex$'" 'utf-8)
    (define-key YaTeX-mode-map (kbd "C-M-SPC") 'mark-sexp)
    (define-key YaTeX-mode-map (kbd "C-M-@") 'mark-sexp)))

(with-eval-after-load "yatex"
  (put 'YaTeX-insert-braces 'begend-guide 2)

  (defun ad:YaTeX-insert-begin-end (env region-mode)
    "Insert \\begin{mode-name} and \\end{mode-name}.
This works also for other defined begin/end tokens to define the structure."
    (setq YaTeX-current-completion-type 'begin)
    (let*((ccol (current-column)) beg beg2 exchange
          (_arg region-mode)		;for old compatibility
          (indent-column (+ ccol YaTeX-environment-indent))(_i 1) _func)
      (if (and region-mode (> (point) (mark)))
          (progn (exchange-point-and-mark)
                 (setq exchange t
                       ccol (current-column)
                       indent-column (+ ccol YaTeX-environment-indent))))
      ;;VER2 (insert "\\begin{" env "}" (YaTeX-addin env))
      (setq beg (point))
      (YaTeX-insert-struc 'begin env)
      (setq beg2 (point))
      (insert "\n")
      (indent-to indent-column)
      (save-excursion
        ;;indent optional argument of \begin{env}, if any
        (while (> (point-beginning-of-line) beg)
          (skip-chars-forward "\\s " (point-end-of-line))
          (indent-to indent-column)
          (forward-line -1)))
      (require 'yatexenv)
      (if region-mode
          ;;if region-mode, indent all text in the region
          (save-excursion
            (if (fboundp (intern-soft (concat "YaTeX-enclose-" env)))
                (funcall (intern-soft (concat "YaTeX-enclose-" env))
                         (point) (mark))
              (while (< (progn (forward-line 1) (point)) (mark))
                (if (eolp) nil
                  (skip-chars-forward " \t\n")
                  (indent-to indent-column))))))
      (if region-mode (exchange-point-and-mark))
      (indent-to ccol)
      ;;VER2 (insert "\\end{" env "}\n")
      (YaTeX-insert-struc 'end env)
      (YaTeX-reindent ccol)
      (if region-mode
          (progn
            (insert "\n")
            (or exchange (exchange-point-and-mark)))
        (goto-char beg2)
        (YaTeX-intelligent-newline nil)
        (YaTeX-indent-line))
      (YaTeX-package-auto-usepackage env 'env)
      (if YaTeX-current-position-register
          (point-to-register YaTeX-current-position-register))))

  (advice-add 'YaTeX-insert-begin-end
              :override #'ad:YaTeX-insert-begin-end))

(with-eval-after-load "yasnippet"
  (require 'ivy-yasnippet nil t))

(when (autoload-if-found
       '(osx-dictionary-search-pointer osx-dictionary-search-input)
       "osx-dictionary" nil t)

  (global-set-key (kbd "C-M-w") #'osx-dictionary-search-pointer)
  (global-set-key (kbd "C-c f w") #'osx-dictionary-search-input)

  (with-eval-after-load "osx-dictionary"
    (custom-set-variables
     '(osx-dictionary-dictionary-choice "英辞郎 第七版"))))

(if (executable-find "js-beautify")
    (when (autoload-if-found
           '(js2-mode)
           "js2-mode" nil t)

      (with-eval-after-load "js2-mode"
        (when (require 'web-beautify nil t)
          (define-key js2-mode-map (kbd "C-c b") 'web-beautify-js)
          (define-key js2-mode-map (kbd "C-c b") 'web-beautify-css))))

  (message "--- js-beautify is NOT installed.")
  (message "--- Note: npm -g install js-beautify"))

(when (autoload-if-found
       '(smartparens-global-mode turn-on-show-smartparens-mode)
       "smartparens" nil t)

  (add-hook 'yatex-mode-hook #'my-smartparens-mode)
  (add-hook 'org-mode-hook #'my-smartparens-mode)

  (with-eval-after-load "smartparens"
    (setq-default sp-highlight-pair-overlay nil)
    (setq-default sp-highlight-wrap-overlay nil)
    (setq-default sp-highlight-wrap-tag-overlay nil)
    (sp-pair "`" nil :actions :rem)
    (sp-pair "'" nil :actions :rem)
    (sp-pair "[" nil :actions :rem)
    (sp-local-pair 'org-mode "$" "$")
    (sp-local-pair 'org-mode "~" "~")
    ;; (sp-local-pair 'org-mode "[" "]")
    ;; (sp-local-pair 'org-mode "+" "+")
    (sp-local-pair 'org-mode "=" "=")
    (sp-local-pair 'org-mode "_" "_")
    (sp-local-pair 'yatex-mode "$" "$")))

(when (autoload-if-found
       '(grugru-default grugru)
       "grugru-default" nil t)

  (global-set-key (kbd "C-9") #'grugru)

  (with-eval-after-load "grugru-default"
    (custom-set-faces
     '(grugru-edit-completing-function #'ivy-completing-read)
     '(grugru-highlight-face ((t (:bold t :underline "#FF3333"))))
     '(grugru-highlight-idle-delay 1))

    (add-hook 'grugru-before-hook #'my-unlock-view-mode)
    (add-hook 'grugru-after-hook #'save-buffer)
    (add-hook 'ah-after-move-cursor-hook #'grugru--highlight-remove)
    (grugru-define-on-major-mode 'org-mode 'word '("TODO" "DONE"))
    (grugru-default-setup)
    (grugru-find-function-integration-mode 1)
    (grugru-highlight-mode 1)))

(autoload-if-found
 '(query-replace-from-region query-replace-regexp-from-region)
 "replace-from-region" nil t)

(autoload-if-found '(embark-act) "embark" nil t)

(when (autoload-if-found
       '(selected-global-mode)
       "selected" nil t)

  (add-hook 'activate-mark-hook #'my-activate-selected)

  (with-eval-after-load "selected"
    (define-key selected-keymap (kbd "a") #'embark-act)
    (define-key selected-keymap (kbd ";") #'comment-dwim)
    (define-key selected-keymap (kbd "e") #'my-eval-region)
    (define-key selected-keymap (kbd "E") #'my-eval-region-as-function)
    ;; (define-key selected-keymap (kbd "=") #'count-words-region)
    (when (require 'helpful nil t)
      (define-key selected-keymap (kbd "h") #'helpful-at-point)
      (define-key selected-keymap (kbd "v") #'my-helpful-variable))
    (define-key selected-keymap (kbd "w") #'osx-dictionary-search-pointer)
    (define-key selected-keymap (kbd "d") #'osx-dictionary-search-pointer)
    (define-key selected-keymap (kbd "5") #'query-replace-from-region)
    (define-key selected-keymap (kbd "g") #'my-google-this)
    (define-key selected-keymap (kbd "s") #'osx-lib-say-region)
    (define-key selected-keymap (kbd "q") #'selected-off)
    (define-key selected-keymap (kbd "x") #'my-hex-to-decimal)
    (define-key selected-keymap (kbd "X") #'my-decimal-to-hex)

    ;; (defun my-eval-region ()
    ;;   (interactive)
    ;;   (when (use-region-p)
    ;;     (eval-region (region-beginning) (region-end) t)))

    (setq selected-org-mode-map (make-sparse-keymap))
    (define-key selected-org-mode-map (kbd "t") #'org-toggle-checkbox)
    (define-key selected-org-mode-map (kbd "-") #'my-org-bullet-and-checkbox)

    (when (require 'expand-region nil t)
      (define-key selected-keymap (kbd "SPC") #'er/expand-region))

    (when (require 'counsel-selected nil t)
      (define-key selected-keymap (kbd "l") 'counsel-selected))

    (when (require 'help-fns+ nil t)
      (defun my-describe-selected-keymap ()
        (interactive)
        (describe-keymap 'selected-keymap))
      (define-key selected-keymap (kbd "H") #'my-describe-selected-keymap))))

(when (autoload-if-found
       '(git-complete)
       "git-complete" nil t)

  (global-set-key (kbd "C-c f <tab>") 'git-complete))

(when (autoload-if-found
       '(bratex-config)
       "bratex" nil t)

  (add-hook 'yatex-mode-hook #'bratex-config))

(setq echo-keystrokes 0.5)

(defvar my-narrow-modeline '("#426EBB" "#FFFFFF")) ;; background, foreground
(defvar my-buffer-narrowed-last nil)
(make-local-variable 'my-buffer-narrowed-last)
(defvar my-selected-window-last nil)
(add-hook 'buffer-list-update-hook #'my-update-modeline-face)


;; 下記3つを utility.el に持っていく時，読み込みの問題が出る FIXME
(defun my-update-modeline-face ()
  (setq my-selected-window-last (frame-selected-window))
  ;; (message "--- %s" my-selected-window-last)
  (unless (minibufferp)
    (my-modeline-face (buffer-narrowed-p))))

(defun my-modeline-face (buffer-narrowed)
  "Update modeline color.
If BUFFER-NARROWED is nil, then change the color to indicating `widen'.
Otherwise, indicating narrowing."
  (unless (eq my-buffer-narrowed-last
              buffer-narrowed) ;; block unnecessary request
    (setq my-buffer-narrowed-last buffer-narrowed)
    ;; (message "--- %s %s %s" this-command last-command buffer-narrowed)
    (when (not (memq this-command '(save-buffer))) ;; FIXME
      (if buffer-narrowed
          (custom-set-faces
           `(mode-line ((t (:background
                            ,(nth 0 my-narrow-modeline)
                            :foreground
                            ,(nth 1 my-narrow-modeline))))))
        (custom-set-faces '(mode-line ((t nil))))))))

(defun my-update-modeline-color ()
  "Update modeline face of the current selected window.
Call this function at updating `mode-line-mode'."
  (when (eq my-selected-window-last (frame-selected-window))
    (my-modeline-face (buffer-narrowed-p))))

(setq mode-line-modes
      (mapcar
       (lambda (entry)
         (if (equal entry "%n")
             '(:eval (progn
                       ;; org が widen を乱発するのでこちらをトリガーにする．
                       ;; 色の変更
                       (my-update-modeline-color)
                       ;; "Narrow" を "N" に短縮表示
                       (if (and (buffer-narrowed-p)
                                (fboundp 'icons-in-terminal-octicon))
                           (concat " " (icons-in-terminal-octicon
                                        "fold" :v-adjust 0.0)) "")))
           entry))
       mode-line-modes))

(when (require 'mlscroll nil t)
  (custom-set-variables
   '(mlscroll-in-color "#FFA07A") ;; light coral
   '(mlscroll-out-color "#FFFFE0")
   '(mlscroll-width-chars 12))
  (mlscroll-mode 1)

  (with-eval-after-load "moom"
    (add-hook 'moom-font-after-resize-hook #'my-reload-mlscroll)))

(with-eval-after-load "vc-hooks"
  (setcdr (assq 'vc-mode mode-line-format)
          '((:eval (replace-regexp-in-string "^ Git" "" vc-mode)))))

(with-eval-after-load "icons-in-terminal"
  ;; 変更がアリ時は赤アイコン，そうでない時に緑アイコンをモードラインに表示
  (make-face 'mode-line-vc-normal-face)
  (make-face 'mode-line-vc-modified-face)
  (set-face-attribute 'mode-line-vc-normal-face nil :foreground "#AFFFAF")
  (set-face-attribute 'mode-line-vc-modified-face nil :foreground "#EEAFAF")
  (setcdr (assq 'vc-mode mode-line-format)
          '((:eval (my-mode-line-vc-mode-icon)))))

(defun ad:split-window-below (&optional _size)
  "An extention to switch to \*scratch\* buffer after splitting window."
  (my-open-scratch))
;; (advice-add 'split-window-below :after #'ad:split-window-below)
(global-set-key (kbd "C-M-s") #'my-open-scratch)

(unless (display-graphic-p)
  ;; ターミナルの縦分割線をUTF-8できれいに描く
  (add-hook 'window-configuration-change-hook 'my-change-window-divider))

;; Show line number in the mode line.
(unless noninteractive
  (line-number-mode 1))

(when (autoload-if-found
       '(my-toggle-display-line-numbers-mode)
       "display-line-numbers" nil t)

  (global-set-key (kbd "C-<f12>") 'my-toggle-display-line-numbers-mode)

  (with-eval-after-load "hl-line"
    (my-update-display-line-numbers-face)
    (add-hook 'my-ime-off-hline-hook #'my-update-display-line-numbers-face)
    (add-hook 'my-ime-on-hline-hook #'my-update-display-line-numbers-face))

  (with-eval-after-load "display-line-numbers"
    (require 'moom nil t)
    (custom-set-faces
     '(line-number-current-line
       ((t (:bold t)))))

    (custom-set-variables
     '(display-line-numbers-width-start t))

    ;; ウィンドウ左に表示する行数の幅を5以上に固定する．
    (add-hook 'display-line-numbers-mode-hook
              #'my-display-line-numbers-width)))

(setq line-number-display-limit-width 100000)

;; モードラインの行数表示の前にアイコンを追加
(with-eval-after-load "icons-in-terminal"
  (setq mode-line-position-line-format
        `(,(icons-in-terminal-material "edit") "%3l")))

;; Show clock in in the mode line
(setq display-time-format "%H:%M") ;; %y%m%d. ;; "%H%M.%S"
(setq display-time-interval 1)
(setq display-time-default-load-average nil)
(unless noninteractive
  (display-time-mode 1))

(eval-when-compile
  (require 'mic-paren nil t))

(when (autoload-if-found
       '(paren-activate)
       "mic-paren" nil t)

  (add-hook 'find-file-hook #'my-mic-paren-activate)

  (with-eval-after-load "mic-paren"
    (setq paren-sexp-mode nil)
    (set-face-foreground 'paren-face-match "#FFFFFF")
    ;; Deep blue: #6666CC, orange: #FFCC66
    (set-face-background 'paren-face-match "#66CC66")

    ;; for ivy-mode, "Matches" と表示される関数との衝突をさける
    (defun ad:mic-paren-highlight (f)
      (if (active-minibuffer-window)
          (let ((paren-display-message 'never))
            (funcall f))
        (funcall f)))
    (advice-add 'mic-paren-highlight :around #'ad:mic-paren-highlight)))

;; スペース
(defface my-face-b-1
  '((t (:background "gray" :bold t :underline "red")))
  nil :group 'font-lock-highlighting-faces)
;; タブだけの行
(defface my-face-b-2
  '((t (:background "orange" :bold t :underline "red")))
  nil :group 'font-lock-highlighting-faces)
;; 半角スペース
(defface my-face-b-3 '((t (:background "orange")))
  nil :group 'font-lock-highlighting-faces)

(defun ad:font-lock-mode (&optional _ARG)
  (unless (memq major-mode '(vterm-mode))
    (font-lock-add-keywords major-mode
                            ;; "[\t]+$" 行末のタブ
                            '(("　" 0 'my-face-b-1 append)
                              ("[ ]+$" 0 'my-face-b-3 append)
                              ("[\t]+$" 0 'my-face-b-2 append)))))
(advice-add 'font-lock-mode :before #'ad:font-lock-mode)

;;show EOF
;; (defun set-buffer-end-mark()
;;   (let ((overlay (make-overlay (point-max) (point-max))))
;;     (overlay-put overlay 'before-string #("[EOF]" 0 5 (face highlight)))
;;     (overlay-put overlay 'insert-behind-hooks
;;                  '((lambda (overlay after beg end &optional len)
;;                      (when after
;;                        (move-overlay overlay (point-max) (point-max))))))))
;; (add-hook 'find-file-hook #'set-buffer-end-mark)

(unless (version< emacs-version "28.0")
    ;; 全角スペース"　"にデフォルトで黒下線が付くのを回避する
    (setq nobreak-char-display nil))

;; 改行文字の文字列表現
(set 'eol-mnemonic-dos "CRLF")
(set 'eol-mnemonic-unix "LF")
(set 'eol-mnemonic-mac "CR")
(set 'eol-mnemonic-undecided "?")

(make-face 'mode-line-file-icon-face)
(custom-set-faces
 '(mode-line-file-icon-face
   ((((background dark)) :foreground "VioletRed1")
    (t (:foreground "LightGoldenrod1")))))

;; `mode-line-mule-info' の文字エンコーディングの文字列表現を差し替える
(setq-default mode-line-mule-info
              (cl-substitute '(:eval (my-buffer-coding-system-mnemonic))
                             "%z" mode-line-mule-info :test 'equal))

(add-hook 'find-file-hook #'my-delight-activate)

(with-eval-after-load "delight"
  (delight
   '(;; Major modes
	   ;;     (c-mode "C" :major)
	   ;;     (c++mode "C++" :major)
	   (js2-mode "JS" :major)
	   (csharp-mode "C#" :major)
	   (prog-mode "Pr" :major)
	   (emacs-lisp-mode "El" :major)
	   (python-mode "Py" :major)
	   (perl-mode "Pl" :major)
	   (web-mode "W" :major)
	   (change-log-mode "CLog" :major)
	   (lisp-interaction-mode "Lisp" :major)

	   ;; Shorten for minor modes
	   (ggtags-mode " G" "ggtags")
	   ;; (orgstruct-mode " OrgS" "org")
	   (orgalist-mode " ol" "orgalist")
	   (view-mode " V" "view")
	   ;; Stop to display for minor modes
	   (org-fancy-priorities-mode nil "org-fancy-priorities")
	   (smooth-scroll-mode nil "smooth-scroll")
	   (eldoc-mode nil "eldoc")
	   (ivy-mode nil "ivy")
	   (counsel-mode nil "counsel")
	   (centered-cursor-mode nil "centered-cursor-mode")
	   (volatile-highlights-mode nil "volatile-highlights")
	   (aggressive-indent-mode nil "aggressive-indent")
	   (all-the-icons-dired-mode nil "all-the-icons-dired")
	   (icons-in-terminal-dired-mode nil "icons-in-terminal-dired")
	   (yas-minor-mode nil "yasnippet")
	   (auto-complete-mode nil "auto-complete")
	   (company-mode nil "company")
	   (ws-butler-mode nil "ws-butler")
	   (isearch-mode nil "isearch")
	   (auto-revert-mode nil "autorevert")
	   (global-whitespace-mode nil "whitespace")
	   (emmet-mode nil "emmet-mode")
	   (abbrev-mode nil "abbrev")
	   (doxymacs-mode nil "doxymacs")
	   (editorconfig-mode nil "editorconfig")
	   (rainbow-mode nil "rainbow-mode")
	   (highlight-symbol-mode nil "highlight-symbol")
	   (which-key-mode nil "which-key")
	   (fancy-narrow-mode nil "fancy-narrow")
	   (smartparens-mode nil "smartparens")
	   (projectile-mode nil "projectile")
	   (selected-minor-mode nil "selected")))

  ;; Override by icon
  (when (require 'icons-in-terminal nil t)
	  (delight
	   `((view-mode ,(concat " " (icons-in-terminal-faicon "lock")) "view")))))

(if (executable-find "cmigemo")
    (when (autoload-if-found
           '(migemo-init)
           "migemo" nil t)

      (when (and (locate-library "migemo");; overhead but should be checked here
                 (executable-find "cmigemo"))
        (add-hook 'isearch-mode-hook #'migemo-init))

      (with-eval-after-load "migemo"
        (custom-set-variables
         '(completion-ignore-case t) ;; case-independent
         '(migemo-command "cmigemo")
         '(migemo-options '("-q" "--emacs" "-i" "\a"))
         '(migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
         '(migemo-user-dictionary nil)
         '(migemo-regex-dictionary nil)
         '(migemo-use-pattern-alist t)
         '(migemo-use-frequent-pattern-alist t)
         '(migemo-pattern-alist-length 1024)
         '(migemo-coding-system 'utf-8-unix))))
  (message "--- cmigemo is NOT installed."))

(eval-when-compile
  (require 'fringe-helper))

(when (autoload-if-found
       '(git-gutter-mode)
       "git-gutter" nil t)

  (dolist (hook
           '(emacs-lisp-mode-hook
             lisp-mode-hook perl-mode-hook python-mode-hook
             c-mode-common-hook nxml-mode-hook web-mode-hook))
    (add-hook hook #'git-gutter-mode))

  (with-eval-after-load "git-gutter"
    (custom-set-variables
     '(git-gutter:lighter ""))

    (when (require 'git-gutter-fringe nil t)
      (custom-set-variables
       '(git-gutter-fr:side 'left-fringe))

      ;; (require 'fringe-helper nil t) ;; byte-compile 時に明示的に指定が必要．
      ;; "!"
      (eval '(fringe-helper-define 'git-gutter-fr:modified nil
               "...XX..."
               "...XX..."
               "...XX..."
               "...XX..."
               "...XX..."
               "........"
               "...XX..."
               "...XX..."))
      ;; "+"
      (eval '(fringe-helper-define 'git-gutter-fr:added nil
               "........"
               "...XX..."
               "...XX..."
               ".XXXXXX."
               ".XXXXXX."
               "...XX..."
               "...XX..."
               "........"))
      ;; "-"
      (eval '(fringe-helper-define 'git-gutter-fr:deleted nil
               "........"
               "........"
               "........"
               ".XXXXXX."
               ".XXXXXX."
               "........"
               "........"
               "........"))
      (set-face-foreground 'git-gutter-fr:added    "#FF2600")
      (set-face-foreground 'git-gutter-fr:modified "orange")
      (set-face-foreground 'git-gutter-fr:deleted  "medium sea green"))))

(with-eval-after-load "calendar"

  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)

  (when (require 'japanese-holidays nil t)
    (setq calendar-holidays
          (append japanese-holidays
                  holiday-local-holidays holiday-other-holidays))
    (setq calendar-mark-holidays-flag t)
    (setq mark-holidays-in-calendar t)
    (setq japanese-holiday-weekend-marker
          '(holiday nil nil nil nil nil japanese-holiday-saturday))
    (setq japanese-holiday-weekend '(0 6))
    (add-hook 'calendar-today-visible-hook #'japanese-holiday-mark-weekend)
    (add-hook 'calendar-today-invisible-hook #'japanese-holiday-mark-weekend)))

(when (require 'empty-booting nil t)
  (run-at-time "5 sec" 600 'my-empty-booting-header-line))

(when (autoload-if-found
       '(my-get-week-number)
       "calendar" nil t)

  (with-eval-after-load "calendar"
    (setq calendar-week-start-day 1)
    (copy-face 'default 'calendar-iso-week-header-face)
    (set-face-attribute 'calendar-iso-week-header-face nil
                        :height 1.0 :foreground "#1010FF"
                        :background (face-background 'default))
    (setq calendar-intermonth-header
          (propertize " w"
                      'font-lock-face 'calendar-iso-week-header-face))

    (copy-face font-lock-constant-face 'calendar-iso-week-face)
    (set-face-attribute 'calendar-iso-week-face nil
                        :height 1.0 :foreground "orange"
                        :background (face-background 'default))

    (setq calendar-intermonth-text
          '(propertize
            (format "%02d"
                    (car
                     (calendar-iso-from-absolute
                      (+ (calendar-absolute-from-gregorian
                          (list month day year))
                         calendar-week-start-day
                         ))))
            'font-lock-face 'calendar-iso-week-face))))

(when (autoload-if-found
       '(which-key-mode)
       "which-key" nil t)

  (with-eval-after-load "which-key"
    (custom-set-variables
     '(which-key-idle-delay 1.0)))

  (unless noninteractive
    (which-key-mode 1)))

(when (autoload-if-found
       '(highlight-symbol-mode highlight-symbol-nav-mode)
       "highlight-symbol" nil t)

  (dolist (hook '(emacs-lisp-mode-hook c-mode-common-hook prog-mode-hook))
    (add-hook hook #'highlight-symbol-mode))

  (with-eval-after-load "highlight-symbol"
    (custom-set-variables
     '(highlight-symbol-idle-delay 0.5))))

(when (autoload-if-found
       '(icons-in-terminal-dired-mode)
       "icons-in-terminal-dired" nil t)

  (with-eval-after-load "icons-in-terminal"
    (setq icons-in-terminal-scale-factor 1.0)))

(cond ((require 'icons-in-terminal nil t)
       (add-hook 'dired-mode-hook #'icons-in-terminal-dired-mode))
      ((require 'all-the-icons nil t)
       (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)))

(when (autoload-if-found
       '(turn-on-eldoc-mode)
       "eldoc" nil t)

  (dolist (hook '(emacs-lisp-mode-hook org-mode-hook c-mode-common-hook))
    (add-hook hook #'turn-on-eldoc-mode))

  (advice-add 'elisp-eldoc-funcall :after #'my-elisp-eldoc)
  (advice-add 'elisp-eldoc-var-docstring :after #'my-elisp-eldoc)

  (with-eval-after-load "eldoc"
    ;; for ivy-mode
    (defun ad:eldoc-message (f &optional string)
      (unless (active-minibuffer-window)
        (funcall f string)))
    (advice-add 'eldoc-message :around #'ad:eldoc-message)

    (custom-set-variables
     '(eldoc-idle-delay 1.0))))

(with-eval-after-load "go-mode"
  (if (executable-find "gocode")
      (when (autoload-if-found
             '(go-mode)
             "go-eldoc" nil t)
        (add-hook 'go-mode-hook #'go-eldoc-setup))
    (message "--- gocode is NOT installed.")))

(when (autoload-if-found
       '(keypression-mode)
       "keypression" nil t)

  (with-eval-after-load "keypression"
    (setq keypression-use-child-frame t)
    (setq keypression-frames-maxnum 3)
    (setq keypression-fade-out-delay 1.5)
    (setq keypression-font "Monaco")
    (setq keypression-font-face-attribute
          '(:width normal :height 200 :weight bold))
    ;; (progn
    ;;   (setq keypression-frame-origin 'keypression-origin-top-left)
    ;;   (setq keypression-x-offset -10)
    ;;   (setq keypression-y-offset +10))
    (progn
      (setq keypression-x-offset +8)
      (setq keypression-y-offset +16))
    (add-hook 'keypression-mode-hook #'dimmer-permanent-off)
    ;; (keypression-mode 1) ;; To start, M-x keypression-mode
    ))

(when (autoload-if-found
       '(counsel-ibuffer counsel-M-x counsel-yank-pop)
       "counsel" nil t)

  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-,") 'counsel-mark-ring)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
  (global-set-key (kbd "C-c i r") 'ivy-resume)

  (with-eval-after-load "flyspell"
    (define-key flyspell-mode-map (kbd "C-,") 'counsel-mark-ring))

  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-,") 'counsel-mark-ring))

  (with-eval-after-load "ivy"
    ;; counsel-mark-ring のリストをソートさせない
    (setf (alist-get 'counsel-mark-ring ivy-sort-functions-alist) nil)

    ;; M-o を ivy-dispatching-done-hydra に割り当てる．
    ;; (define-key ivy-minibuffer-map (kbd "M-o") 'ivy-dispatching-done-hydra)
    ;; ivy-dispatching-done を使う．
    ;; (define-key ivy-minibuffer-map (kbd "M-o") 'ivy-dispatching-done)
    (setq ivy-read-action-function #'ivy-hydra-read-action)

    (setq ivy-use-virtual-buffers nil)
    (when (setq enable-recursive-minibuffers t)
      (minibuffer-depth-indicate-mode 1))
    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
    ;; (setq ivy-truncate-lines nil) ;; 選択候補も折り返されてしまう．
    ;; (setq ivy-wrap t)
    (ivy-mode 1))

  (with-eval-after-load "counsel"
    ;; counsel-M-x, see also prescient.el section
    (setq ivy-initial-inputs-alist
          '((org-agenda-refile . "^")
            (org-capture-refile . "^")
            (counsel-describe-function . "^")
            (counsel-describe-variable . "^")
            (Man-completion-table . "^")
            (woman . "^")))

    (when (require 'smex nil t)
      (setq smex-history-length 35)
      (setq smex-completion-method 'ivy))

    ;;  https://github.com/abo-abo/swiper/issues/1294
    (setf (alist-get 'counsel-M-x ivy-sort-matches-functions-alist)
          #'ivy--sort-by-len)

    ;; Disable counsel-find-file
    ;; https://emacs.stackexchange.com/questions/45929/disable-ivy-for-find-file
    (setq read-file-name-function #'my-disable-counsel-find-file)
    (define-key counsel-mode-map [remap find-file]  nil)

    ;; オリジナルを非インタラクティブ化（上書きで可．advice不可）
    (when (require 'find-func nil t)
      (defun find-library (library)
        "Override the original `find-library' to hide in command list."
        (prog1
            (switch-to-buffer (find-file-noselect (find-library-name library)))
          (run-hooks 'find-function-after-hook))))))

;; プロンプトをカスタマイズ
(with-eval-after-load "ivy"
  (setq ivy-pre-prompt-function #'my-pre-prompt-function))

(when (autoload-if-found
       '(imenu-list)
       "imenu-list" nil t)

  (with-eval-after-load "imenu-list"
    (setq imenu-list-size 40)
    (setq imenu-list-position 'left)

    (add-hook 'imenu-list-major-mode-hook #'my-truncate-lines-activate)

    (when (require 'moom nil t)
      (add-hook 'imenu-list-update-hook #'my-imenu-list-update)
      (advice-add 'imenu-list-quit-window :after #'my-imenu-list-quit-window))))

(with-eval-after-load "prescient"
  (setq prescient-aggressive-file-save t) ;; Merged!
  (setq prescient-save-file
        (expand-file-name "~/.emacs.d/prescient-save.el"))
  (prescient-persist-mode 1))

(with-eval-after-load "ivy"
  (when (and (require 'prescient nil t)
             (require 'ivy-prescient nil t))
    (setq ivy-prescient-retain-classic-highlighting t)
    ;; (dolist (command '(counsel-world-clock ;; Merged!
    ;;                    counsel-app))
    ;;   (add-to-list 'ivy-prescient-sort-commands command t))
    (ivy-prescient-mode 1)
    (setf (alist-get 'counsel-M-x ivy-re-builders-alist)
          #'ivy-prescient-re-builder)
    (setf (alist-get t ivy-re-builders-alist) #'ivy--regex-ignore-order)))

(with-eval-after-load "company"
  (when (and (require 'prescient nil t)
             (require 'company-prescient nil t))
    (company-prescient-mode 1)))

(when (autoload-if-found
       '(command-log-mode global-command-log-mode)
       "command-log-mode" nil t)

  (with-eval-after-load "command-log-mode"
    (require 'keypression)
    (require 'moom)
    ;; (setq command-log-mode-window-font-size 0)
    (setq command-log-mode-window-size 60)))

(let* ((elp (expand-file-name
	     (concat "~/.emacs.d/" (format "%s" emacs-version) "/el-get/")))
 (ets (concat elp "emacs-tree-sitter/"))
 (tsl (concat elp "tree-sitter-langs/")))
  ;; (add-to-list 'load-path (concat ets "langs"))
  (add-to-list 'load-path (concat ets "core"))
  (add-to-list 'load-path (concat ets "lisp"))
  (add-to-list 'load-path tsl))
(defun my-enable-tree-sitter ()
  (unless (featurep 'tree-sitter)
    (require 'tree-sitter)
    (require 'tree-sitter-hl)
    (require 'tree-sitter-debug)
    (require 'tree-sitter-query)
    (require 'tree-sitter-langs))
  (tree-sitter-hl-mode))
(dolist (hook '(js-mode-hook))
  (add-hook hook #'my-enable-tree-sitter))

(when (autoload-if-found
       '(swiper-thing-at-point swiper-all-thing-at-point)
       "swiper" nil t)

  (global-set-key (kbd "M-s M-s") 'swiper-thing-at-point)
  (global-set-key (kbd "M-s M-a") 'swiper-all-thing-at-point)

  (with-eval-after-load "swiper"
    (defun ad:swiper-thing-at-point ()
      "`swiper' with `ivy-thing-at-point'."
      (interactive)
      (let ((thing (if (thing-at-point-looking-at "^\\*+") ;; org heading を除外
                       nil
                     (ivy-thing-at-point))))
        (when (use-region-p)
          (deactivate-mark))
        (swiper thing)))
    (advice-add 'swiper-thing-at-point :override #'ad:swiper-thing-at-point)))

(when (eq system-type 'darwin)
  (with-eval-after-load "ivy"
    (cond ((and (require 'icons-in-terminal nil t) ;; safeguard
                (require 'icons-in-terminal-ivy nil t))
           (dolist (command '(counsel-projectile-switch-project
                              counsel-ibuffer))
             (add-to-list 'icons-in-terminal-ivy-buffer-commands command))
           (icons-in-terminal-ivy-setup))
          ((and (require 'all-the-icons nil t) ;; safeguard
                (require 'all-the-icons-ivy nil t))
           (dolist (command '(counsel-projectile-switch-project
                              counsel-ibuffer))
             (add-to-list 'all-the-icons-ivy-buffer-commands command))
           (all-the-icons-ivy-setup)))))

(when (autoload-if-found
       '(dimmer-mode dimmer-process-all dimmer-off dimmer-on
				             my-toggle-dimmer dimmer-permanent-off
				             ad:dimmer-org-agenda--quit)
       "dimmer" nil t)

  (defvar my-dimmer-mode nil)

  (with-eval-after-load "dimmer"
    (custom-set-variables
     '(dimmer-exclusion-regexp
       "^\\*[Hh]elm\\|^ \\*Minibuf\\|^\\*scratch\\|^ \\*Neo\\|^ \\*Echo\\|^\\*Calendar\\|*Org\\|^ \\*LV*")
     '(dimmer-fraction 0.6))

    (if (version< emacs-version "27.1")
	      (progn
	        (add-hook 'focus-out-hook #'dimmer-off)
	        (add-hook 'focus-in-hook #'dimmer-on))
      (defun my-dimmer-update ()
	      (if (frame-focus-state) (dimmer-on) (dimmer-off)))
      (add-function :before after-focus-change-function #'my-dimmer-update))

    ;; for org-agenda
    (add-hook 'org-agenda-mode-hook #'dimmer-permanent-off)
    (defun ad:dimmer-org-agenda--quit (&optional _bury)
      (when (fboundp 'dimmer-on)
	      (setq my-dimmer-mode t)
	      (dimmer-on)
	      (redraw-frame)))
    (advice-add 'org-agenda--quit :after #'ad:dimmer-org-agenda--quit)

    ;; for swiper/helm-swoop
    (add-hook 'minibuffer-setup-hook #'dimmer-off)
    (add-hook 'minibuffer-exit-hook #'dimmer-on))

  (unless noninteractive
    (unless (version< "28.0" emacs-version)
      (setq my-dimmer-mode (dimmer-mode 1)))))

;; この場合は，interactive モードで init-eval.el にある記述をロードするはだめ．
(eval-when-compile
  (require 'transient))

(with-eval-after-load "transient"
  (transient-define-prefix my-org-bullet-and-checkbox ()
	  "Commands to handle bullet and checkbox"
	  [["Bullet"
		  ("i" "insert" my-org-insert-bullet)
		  ("d" "delete" my-org-delete-bullet)]
	   ["Checkbox"
		  ("[" "insert" my-org-insert-checkbox-into-bullet)
		  ("]" "delete" my-org-delete-checkbox-from-bullet)
		  ;;("a" "toggle checkbox" my-org-toggle-checkbox)
		  ;;("h" "cycle" my-cycle-bullet-at-heading) ;; single line
		  ]
	   ["Bullet and Checkbox"
		  ("I" "insert" my-org-insert-bullet-and-checkbox)
		  ("D" "delete" my-org-delete-bullet-and-checkbox)]]))

(when (autoload-if-found
       '(rencetf-mode
	       my-recentf-save-list-silence
	       my-recentf-cleanup-silence
	       recentf-open-files)
       "recentf" nil t)

  (with-eval-after-load "recentf"
    (custom-set-variables
     '(recentf-max-saved-items 2000)
     '(recentf-save-file (expand-file-name "~/.emacs.d/recentf"))
     '(recentf-auto-cleanup 'never)
     '(recentf-exclude
       '(".recentf" "bookmarks" "org-recent-headings.dat" "^/tmp\\.*"
	       "^/private\\.*" "^/var/folders\\.*" "/TAGS$")))

    (if (version< emacs-version "27.1")
	      (progn
	        (add-hook 'focus-out-hook #'my-recentf-save-list-silence)
	        (add-hook 'focus-out-hook #'my-recentf-cleanup-silence))
      (add-function :before after-focus-change-function
		                #'my-recentf-save-list-silence)
      (add-function :before after-focus-change-function
		                #'my-recentf-cleanup-silence)))

  (unless noninteractive
    (let ((message-log-max nil))
      (recentf-mode 1)))

  (when (autoload-if-found '(counsel-recentf) "counsel" nil t)
    (global-set-key (kbd "C-M-r") 'counsel-recentf))

  (with-eval-after-load "counsel"
    (defun ad:counsel-recentf ()
      "Find a file on `recentf-list'."
      (interactive)
      (require 'recentf)
      (recentf-mode)
      (ivy-read "Recentf: "
		            (mapcar (lambda (x) (abbreviate-file-name  ;; ~/
				                             (substring-no-properties x)))
			                  recentf-list)
		            :action #'my-counsel-recentf-action
		            :require-match t
		            :caller 'counsel-recentf))
    (advice-add 'counsel-recentf :override #'ad:counsel-recentf)
    (ivy-add-actions
     'counsel-recentf
     '(("g" my-counsel-ag-in-dir "switch to ag")
       ("r" my-counsel-fzf-in-dir "switch to fzf (in dir.)")
       ("z" my-counsel-fzf-in-default-dir "switch to fzf (default)")))))

;; (add-hook 'after-init-hook #'recentf-mode))

(when (require 'ah nil t)
  (add-hook 'ah-before-c-g-hook #'my-cg-bookmark))

(with-eval-after-load "recentf"
  (run-with-idle-timer 180 t 'my-backup-recentf))

(when (autoload-if-found
       '(backup-each-save my-auto-backup)
       "backup-each-save" nil t)

  (add-hook 'after-save-hook #'my-auto-backup)

  ;; %y-%m-%d_%M-%S で終わるファイルを本来のメジャーモードで開く
  (add-to-list 'auto-mode-alist '("-[0-9-]\\{8\\}_[0-9-]\\{5\\}$" nil t))

  (with-eval-after-load "backup-each-save"
    (setq backup-each-save-mirror-location "~/.emacs.d/backup")
    (setq backup-each-save-time-format "%y-%m-%d_%M-%S") ;; do not use ":" for w32
    (setq backup-each-save-size-limit 1048576))

  (when (eq window-system 'w32)
    (advice-add 'backup-each-save-compute-location :override
                #'my-backup-each-save-compute-location)))

(when (autoload-if-found
       '(dired-recent-open dired-recent-mode)
       "dired-recent" nil t)

  (global-set-key (kbd "C-x C-d") 'dired-recent-open)

  (with-eval-after-load "dired-recent"
    ;; (require 'helm-config nil t)
    (dired-recent-mode 1)))

(with-eval-after-load "dired"
  (setq dired-use-ls-dired nil)
  (when (require 'osx-trash nil t)
    (setq delete-by-moving-to-trash t)
    (osx-trash-setup)))

(when (autoload-if-found
       '(undo-fu-only-undo undo-fu-only-redo)
       "undo-fu" nil t)

  (global-set-key (kbd "C-/") 'undo-fu-only-undo)
  (global-set-key (kbd "C-M-/") 'undo-fu-only-redo))

(when (require 'auto-save-buffers nil t)
  (run-with-idle-timer 1.6 t #'my-auto-save-buffers))

(when (autoload-if-found
       '(neotree neotree-toggle)
       "neotree" nil t)

  (global-set-key (kbd "C-c n") #'neotree-toggle)

  (with-eval-after-load "neotree"
    (custom-set-variables
     '(neo-show-hidden-files t)
     '(neo-theme 'arrow)
     '(neo-smart-open t)
     '(neo-window-width 25)
     '(neo-show-hidden-files nil)
     '(neo-window-position 'left))
    ;; (setq neo-vc-integration '(face char)) ;; It's heavy at 2017-08-31

    ;; アイコン表示
    (when (require 'all-the-icons-dired nil t)
      (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

    (defvar my-neo-activated nil) ;; fail save
    (defvar my-neo-adjusted-window-width (+ 3 neo-window-width))
    (defun ad:neotree-show ()
      "Extension to support change frame width when opening neotree."
      (unless (neo-global--window-exists-p)
        (when (and (require 'moom nil t)
                   (not my-neo-activated))
          (setq moom-frame-width-single
                (+ moom-frame-width-single my-neo-adjusted-window-width))
          (setq moom-frame-width-double
                (+ moom-frame-width-double my-neo-adjusted-window-width)))
        (set-frame-width nil (+ (frame-width) my-neo-adjusted-window-width))
        (setq my-neo-activated t)))
    (advice-add 'neotree-show :before #'ad:neotree-show)

    (defun ad:neotree-hide ()
      "Extension to support change frame width when closing neotree."
      (when (neo-global--window-exists-p)
        (when (and (require 'moom nil t)
                   my-neo-activated)
          (setq moom-frame-width-single
                (- moom-frame-width-single my-neo-adjusted-window-width))
          (setq moom-frame-width-double
                (- moom-frame-width-double my-neo-adjusted-window-width)))
        (set-frame-width nil (- (frame-width) my-neo-adjusted-window-width))
        (when (> 80 (frame-width)) ;; fail safe
          (set-frame-width nil 80))
        (setq my-neo-activated nil)))
    (advice-add 'neotree-hide :before #'ad:neotree-hide)))

(when (autoload-if-found
       '(helpful-key helpful-function helpful-variable helpful-at-point
                     helpful-symbol)
       "helpful" nil t)

  (global-set-key (kbd "<f1> k") 'helpful-key)
  (global-set-key (kbd "<f1> f") 'helpful-function)
  (global-set-key (kbd "<f1> v") 'helpful-variable)
  (global-set-key (kbd "<f1> m") 'helpful-macro)
  (global-set-key (kbd "<f1> @") 'helpful-at-point)

  (with-eval-after-load "helpful"
    (defun ad:helpful-at-point ()
      (deactivate-mark))
    (advice-add 'helpful-at-point :before #'ad:helpful-at-point)))

(when (autoload-if-found
       '(facecheck-at-point facecheck-mode)
       "facecheck" nil t)

  (with-eval-after-load "facecheck"
    (facecheck-mode 1)))

(when (autoload-if-found
       '(keyfreq-mode keyfreq-autosave-mode ad:keyfreq-show)
       "keyfreq" nil t) ;; will require 'cl and 'gv (10-20[ms])

  (with-eval-after-load "keyfreq"
    (defun ad:keyfreq-show ()
      "Extension to make the buffer view-only."
      (interactive)
      (if shutup-p
          (shut-up (view-buffer keyfreq-buffer))
        (view-buffer keyfreq-buffer)))
    (advice-add 'keyfreq-show :after #'ad:keyfreq-show)
    ;; (define-key keyfreq-mode-map (kbd "q")
    ;;   (lambda () (interactive)
    ;;       (when (string= (buffer-name) keyfreq-buffer)
    ;;         (kill-buffer-and-window))))
    (setq keyfreq-file
          (expand-file-name (concat (getenv "SYNCROOT") "/emacs.d/.keyfreq")))
    (keyfreq-autosave-mode 1))

  (unless noninteractive
    (keyfreq-mode 1)))

(when (autoload-if-found
       '(disk-usage)
       "disk-usage" nil t)

  (with-eval-after-load "disk-usage"
    (when (eq system-type 'darwin)
      (custom-set-variables
       '(disk-usage-du-command "du")))))

(when (autoload-if-found '(counsel-ag) "counsel" nil t)
  (global-set-key (kbd "C-M-f") 'counsel-ag))

(with-eval-after-load "counsel"
  (require 'thingatpt nil t)
  (defun ad:counsel-ag (f &optional initial-input initial-directory extra-ag-args ag-prompt caller)
    (apply f (or initial-input
                 (and (not (thing-at-point-looking-at "^\\*+"))
                      (ivy-thing-at-point)))
           (unless current-prefix-arg
             (or initial-directory default-directory))
           extra-ag-args ag-prompt caller))
  (advice-add 'counsel-ag :around #'ad:counsel-ag)

  ;; 2文字でも検索が発動するようにする
  (add-to-list 'ivy-more-chars-alist '(counsel-ag . 2))

  (ivy-add-actions
   'counsel-ag
   '(("r" my-counsel-ag-in-dir "search in directory"))))

(when (autoload-if-found
       '(counsel-fzf)
       "counsel" nil t)

  (global-set-key (kbd "C-M-z") 'counsel-fzf)

  (with-eval-after-load "counsel"
    (defun ad:counsel-fzf (f &optional initial-input initial-directory fzf-prompt)
      (apply f (or initial-input
                   (if (thing-at-point-looking-at "^\\*+") ;; org heading を除外
                       nil
                     (ivy-thing-at-point)))
             (or initial-directory default-directory)
             fzf-prompt))
    (advice-add 'counsel-fzf :around #'ad:counsel-fzf)

    (ivy-add-actions
     'counsel-fzf
     '(("r" my-counsel-fzf-in-dir "search in directory")))))

(global-set-key (kbd "C-;") 'comment-dwim) ;; M-; is the defualt
(global-set-key (kbd "C-c c") 'compile)

(autoload-if-found '(gist-mode) "gist" nil t)

(eval-when-compile
  (require 'dash nil t))

(when (autoload-if-found
       '(flycheck-mode)
       "flycheck" nil t)

  (dolist (hook
           '(go-mode-hook
             js2-mode-hook
             c-mode-common-hook
             perl-mode-hook
             python-mode-hook))
    (add-hook hook #'flycheck-mode))

  (with-eval-after-load "flycheck"
    (setq flycheck-gcc-language-standard "c++14")
    (setq flycheck-clang-language-standard "c++14")
    ;; TODO: really needed?
    ;; (when (require 'flycheck-clang-tidy nil t)
    ;;   (add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup))
    ;; http://qiita.com/senda-akiha/items/cddb02cfdbc0c8c7bc2b
    ;; (when (require 'flycheck-pos-tip nil t)
    ;;   '(custom-set-variables
    ;;     '(flycheck-display-errors-function
    ;;       #'flycheck-pos-tip-error-messages)))
    ))

;; (flycheck-add-next-checker 'javascript-jshint
;; 'javascript-gjslint)

(when (autoload-if-found
       '(quickrun)
       "quickrun" nil t)

  (with-eval-after-load "go-mode"
    (define-key go-mode-map (kbd "<f5>") 'quickrun))
  (with-eval-after-load "c++-mode"
    (define-key c++-mode-map (kbd "<f5>") 'quickrun))
  (with-eval-after-load "python-mode"
    (define-key python-mode-map (kbd "<f5>") 'quickrun))
  (with-eval-after-load "perl-mode"
    (define-key perl-mode-map (kbd "<f5>") 'quickrun))
  (with-eval-after-load "gnuplot-mode"
    (define-key gnuplot-mode-map (kbd "<f5>") 'quickrun)))

(if (not (executable-find "gtags"))
    (message "--- global is NOT installed in this system.")

  (when (autoload-if-found
         '(ggtags-mode)
         "ggtags" nil t)

    (with-eval-after-load "ggtags"
      ;; (setq ggtags-completing-read-function t) ;; nil for helm
      (define-key ggtags-mode-map (kbd "M-]") nil))

    (dolist (hook (list 'c-mode-common-hook 'python-mode-hook))
      (add-hook hook (lambda () (ggtags-mode 1)))))

  (when (autoload-if-found
         '(counsel-gtags-mode)
         "counsel-gtags" nil t)
    (dolist (hook '(c-mode-hook c++-mode-hook))
      (add-hook hook 'counsel-gtags-mode))
    (with-eval-after-load "counsel-gtags"
      (custom-set-variables
       '(counsel-gtags-update-interval-second 10)))))

(when (autoload-if-found
       '(0xc-convert 0xc-convert-point my-decimal-to-hex my-hex-to-decimal)
       "0xc" nil t)

  (global-set-key (kbd "C-c f h") '0xc-convert))

(with-eval-after-load "hexl"
  (custom-set-variables
   '(hexl-bits 8)))

(autoload-if-found '(uuid-string my-uuid-string) "uuid" nil t)

(autoload-if-found '(package-lint-current-buffer) "package-lint" nil t)

(when (autoload-if-found
       '(projectile-mode)
       "projectile" nil t)

  (with-eval-after-load "neotree"
    ;; M-x helm-projectile-switch-project (C-c p p)
    (setq projectile-switch-project-action 'neotree-projectile-action)

    (defun ad:neotree-dir (path)
      "Extension to change the frame width automatically."
      (interactive "DDirectory: ")
      (unless (neo-global--window-exists-p)
        (neotree-show))
      (neo-global--open-dir path)
      (neo-global--select-window))
    ;; (advice-add 'neotree-dir :override #'ad:neotree-dir) ;; FIXME
    )

  (with-eval-after-load "projectile"
    (defun ad:projectile-visit-project-tags-table ()
      "Extensions to skip calling `visit-tags-table'."
      nil)
    (advice-add 'projectile-visit-project-tags-table :override
                #'ad:projectile-visit-project-tags-table)

    (setq projectile-mode-line-lighter "")
    (setq projectile-dynamic-mode-line nil)
    (setq projectile-tags-command "gtags")
    (setq projectile-tags-backend 'ggtags)
    (setq projectile-tags-file-name "GTAGS")

    (setq projectile-use-git-grep t)
    ;; (setq projectile-mode-line
    ;;       '(:eval (format " P:%s" (projectile-project-name))))
    (setq projectile-mode-line "")

    (setq icon-title-format
          (setq frame-title-format
                '((:eval
                   (let ((project-name (projectile-project-name)))
                     (unless (string= "-" project-name)
                       (format "(%s) - " project-name))))
                  "%b")))

    ;; counsel-projectile
    (when (require 'counsel-projectile nil t)
      (add-to-list 'counsel-projectile-switch-project-action
                   '("z" my-counsel-fzf-in-default-dir
                     "switch to fzf") t)
      (add-to-list 'counsel-projectile-find-file-action
                   '("z" my-counsel-fzf-in-default-dir
                     "switch to fzf") t)

      (setq projectile-completion-system 'ivy)
      (setq counsel-projectile-sort-files t) ;; 当該プロジェクト内リストをソート
      (setq counsel-projectile-sort-projects t) ;; プロジェクトリストをソート
      (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
      (counsel-projectile-mode 1)))

  (unless noninteractive
    (add-hook 'find-file-hook #'my-projectile-activate)))

(autoload-if-found '(relint-current-buffer) "relint" nil t)

(when (autoload-if-found
       '(magit-status ad:magit-mode-bury-buffer)
       "magit" nil t)

  (global-set-key (kbd "C-c m") 'magit-status)

  (with-eval-after-load "magit"
    (when (fboundp 'dimmer-off)
      (add-hook 'magit-status-mode-hook 'dimmer-off))
    (when (fboundp 'magit-mode-bury-buffer)
      (defun ad:magit-mode-bury-buffer (&optional _bury)
        (when (fboundp 'dimmer-on)
          (setq my-dimmer-mode t)
          (dimmer-on)
          (redraw-frame)))
      (advice-add 'magit-mode-bury-buffer :before #'ad:magit-mode-bury-buffer))
    (when (and (boundp 'magit-completing-read-function)
               (require 'ivy nil t))
      ;; ivy を使う
      (setq magit-completing-read-function 'ivy-completing-read))
    (when (boundp 'magit-repository-directories)
      (setq magit-repository-directories
            '(("~/devel/git" . 1)
              ("~/devel/mygit" . 1))))))

(add-hook 'find-file-hook #'my-editorconfig-activate)

(autoload-if-found '(cov-mode) "cov" nil t)

(autoload-if-found '(format-all-mode) "format-all" nil t)

(add-hook 'emacs-lisp-mode-hook #'my-company-activate)
(add-hook 'org-mode-hook #'my-company-activate)

(with-eval-after-load "company"
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  ;; To complete file path, move `company-files' to the fist item of the list
  (delq 'company-files company-backends)

  (add-to-list 'company-backends 'company-files)
  (when (require 'company-org-block nil t)
    (setq company-org-block-edit-style 'inline) ;; 'auto, 'prompt, or 'inline
    (setq company-org-block-auto-indent nil)
    (add-to-list 'company-backends 'company-org-block))

  ;; 補完候補に番号を表示
  (setq company-show-numbers t)
  ;; 補完候補を出すまでの猶予
  (setq company-idle-delay 0.8)
  (setq company-tooltip-idle-delay 0.8)
  (global-company-mode)
  (when (require 'company-quickhelp nil t)
    (company-quickhelp-mode))

  (defun ad:company-idle-begin (f buf win tick pos)
    (unless (and (boundp 'ns-put-text-p) ns-put-text-p)
      (funcall f buf win tick pos)))
  (advice-add 'company-idle-begin :around #'ad:company-idle-begin)
  (defun ad:company-pseudo-tooltip--ujofwd-on-timer (f command)
    (unless (and (boundp 'ns-put-text-p) ns-put-text-p)
      (funcall f command)))
  ;; (advice-add 'company-pseudo-tooltip--ujofwd-on-timer :around
  ;;             #'ad:company-pseudo-tooltip--ujofwd-on-timer)

  (when (boundp 'mac-ime-before-put-text-hook)
    ;; 補完候補が表示されたタイミングで入力を続けたら，補完候補を消す．
    (add-hook 'mac-ime-before-put-text-hook #'company-cancel)))

(autoload-if-found '(vterm) "vterm"  nil t)

;; Select from Preferences: { Funk | Glass | ... | Purr | Pop ... }
(defvar ns-default-notification-sound "Pop")

(defvar ns-alerter-command
  (executable-find (concat (getenv "HOME") "/Dropbox/bin/alerter"))
  "Path to alerter command. see https://github.com/vjeantet/alerter")

;; eval (org-notify "hoge") to test this setting
(with-eval-after-load "org"
  (when ns-alerter-command
    (setq org-show-notification-handler #'my-desktop-notification-handler)))

;; `org-agenda-prepare-buffers' は重い．agenda 実行時の最初に走るが，
;; 事前に走らせておくほうがいい．以下の例では，
;; 起動後，何もしなければ10秒後に org, org-agenda が有効になる
;; 起動後，org buffer を訪問して，10秒待つと，org-agenda が有効になる
;; 起動後，直接 org-agenda を叩く場合は重いまま（タイマー走ってもスルー）
;; これを (with-eval-after-load "org") の中に置くと振る舞いが変(2回実行)になる
(run-with-idle-timer 10 nil #'my-org-agenda-prepare-buffers)

(with-eval-after-load "icons-in-terminal"
  (setq-default prettify-symbols-alist '(;;("#+begin_src" . "")
                                         ("#+begin_src" . "▨")
                                         ("#+end_src" . "▨")
                                         ("#+RESULTS:" . "")
                                         ("[ ]" .  "") ;; ☐ 
                                         ("[X]" . "" ) ;; ☑ 
                                         ("[-]" . "" ))) ;; 
  (add-hook 'org-mode-hook 'prettify-symbols-mode))

;; 1. TODO/DOING/DONE に trello 側のカードを変えておく．
;; 2. M-x org-trello-install-key-and-token
;; ~/.emacs.d/.trello/<account>.el が作られる
;; 3. M-x org-trello-install-board-metadata
;; Trello 側の情報を基にして current-buffer にプロパティブロックが挿入される
;; 4. C-u M-x org-trello-sync-buffer で pull
;; 5. M-x org-trello-sync-buffer で push
(when (autoload-if-found
       '(my-push-trello my-pull-trello my-activate-org-trello)
       "org-trello" nil t)

  (defvar org-trello-current-prefix-keybinding nil) ;; To avoid an error
  (add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))

  (with-eval-after-load "org"
    (defun my-activate-org-trello ()
      (let ((filename (buffer-file-name (current-buffer))))
        (when (and filename
                   (string= "trello" (file-name-extension filename))
                   (require 'org-trello nil t))
          (org-trello-mode))))
    (add-hook 'org-mode-hook #'my-activate-org-trello))

  (with-eval-after-load "org-trello"
    (defun my-push-trello-card () (interactive) (org-trello-sync-card))
    (defun my-pull-trello-card () (interactive) (org-trello-sync-card t))
    (defun my-push-trello () (interactive) (org-trello-sync-buffer))
    (defun my-pull-trello () (interactive) (org-trello-sync-buffer t))))

(when (autoload-if-found
       '(org-recent-headings org-recent-headings-mode)
       "org-recent-headings" nil t)

  ;; (global-set-key (kbd "C-c f r") 'org-recent-headings-helm)
  (global-set-key (kbd "C-M-h") 'org-recent-headings)

  (with-eval-after-load "org-recent-headings"
    ;; デフォルトだと `ivy-string<' が使われてしまい，使用履歴が反映されない．
    (setf (alist-get 'org-recent-headings ivy-sort-functions-alist) nil)

    (defun ad:org-recent-headings-activate ()
      (interactive)
      (when (require 'org-recent-headings nil t)
        (org-recent-headings-mode 1) ;; one time activate
        (advice-remove 'org-recent-headings
                       #'ad:org-recent-headings-activate)))
    (advice-add 'org-recent-headings :before
                #'ad:org-recent-headings-activate)

    (setq org-recent-headings-save-file "~/.emacs.d/org-recent-headings.dat")
    (setq org-recent-headings-use-ids 'when-available)
    (setq org-recent-headings-show-entry-function
          'org-recent-headings--show-entry-direct) ;; 直接移動する
    (setq org-recent-headings-advise-functions
          '(org-cycle
            org-agenda-goto
            org-agenda-show
            org-agenda-show-mouse
            org-show-entry
            org-reveal
            org-refile
            org-tree-to-indirect-buffer
            org-bookmark-jump)))

  ;; (with-eval-after-load 'ivy
  ;;   ;; Helm, Ivy support was removed from the official package
  ;;   (defun org-recent-headings-ivy ()
  ;;     "Choose from recent Org headings with Ivy."
  ;;     (interactive)
  ;;     (let ((completing-read-function  #'ivy-completing-read))
  ;;       (org-recent-headings))))
  )

(cond
 ((memq window-system '(ns x))
  ;; モードラインにアイコンを出す
  (make-face 'mode-line-ime-on-face)
  (set-face-attribute 'mode-line-ime-on-face
                      nil :foreground (plist-get my-cur-color-ime :on))
  (when (fboundp 'mac-set-input-method-parameter)
    (mac-set-input-method-parameter
     "com.google.inputmethod.Japanese.base" 'title
     (concat
	    (if (require 'icons-in-terminal nil t)
          (icons-in-terminal-octicon "keyboard"
                                     :v-adjust 0.0
                                     :face 'mode-line-ime-on-face)
        "") " "))) ;; FIXME (the color is NOT changed, patch is wrong?)

  (declare-function my-ime-on "init" nil)
  (declare-function my-ime-off "init" nil)
  (declare-function my-ime-active-p "init" nil)

  ;; for private patch
  (when (boundp 'mac-ime-cursor-type)
    (setq mac-ime-cursor-type (plist-get my-cur-type-ime :on)))

  (defvar my-ime-last (my-ime-active-p))
  (defvar my-ime-before-action nil)

  (if (not (fboundp 'mac-ime-active-p))
	    (progn
	      ;; For selected.el
	      (add-hook 'activate-mark-hook #'my-ime-off-sticky)
	      (add-hook 'deactivate-mark-hook #'my-ime-on-sticky)
	      ;; 「M-x あ」対策
	      (add-hook 'minibuffer-setup-hook #'my-ime-off-sticky)
	      (add-hook 'minibuffer-exit-hook #'my-ime-on-sticky))
    ;; For selected.el
    (add-hook 'activate-mark-hook #'mac-ime-deactivate-sticky)
    (add-hook 'deactivate-mark-hook #'mac-ime-activate-sticky))

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
    (defun my-mac-keyboard-input-source () ;; Need update
	    (if (string-match "\\.Roman$" (mac-input-source))
	        (progn
	          (setq cursor-type (plist-get my-cur-type-ime :off))
	          (add-to-list 'default-frame-alist
			                   `(cursor-type . ,(plist-get my-cur-type-ime :off)))
	          (set-cursor-color (plist-get my-cur-color-ime :off)))
	      (progn
	        (setq cursor-type (plist-get my-cur-type-ime :on))
	        (add-to-list 'default-frame-alist
			                 `(cursor-type . ,(plist-get my-cur-type-ime :on)))
	        (set-cursor-color (plist-get my-cur-color-ime :on)))))

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

 (t nil))

;; (declare-function my-font-config "init" nil)
(global-set-key (kbd "M-`") 'other-frame)
(with-eval-after-load "frame"
  (defun ad:make-frame (&optional _parameters)
    (when (display-graphic-p)
      (my-theme)
      ;; (my-apply-cursor-config)
      (setq-default cursor-type
                    (if (my-ime-active-p)
                        (plist-get my-cur-type-ime :on)
                      (plist-get my-cur-type-ime :off)))
      (when (and (require 'moom-font nil t)
                 (require 'moom nil t))
        (moom-font-resize))))
  (advice-add 'make-frame :after #'ad:make-frame))

(global-set-key (kbd "<f12>") 'my-toggle-mode-line)
(with-eval-after-load "moom"
  (defun ad:moom-toggle-frame-maximized ()
    (when (eq major-mode 'org-mode)
      (org-redisplay-inline-images))
    (when (and mode-line-format
               (not my-toggle-modeline-global))
      (my-mode-line-off)))
  (advice-add 'moom-toggle-frame-maximized
              :after #'ad:moom-toggle-frame-maximized))

;; (make-variable-buffer-local 'my-mode-line-format)
(defvar-local my-mode-line-format nil)
(set-default 'my-mode-line-format mode-line-format)
(defvar my-toggle-modeline-global t)
(unless (display-graphic-p)
  (setq my-toggle-modeline-global t)) ;; Enforce modeline in Terminal

(add-hook 'find-file-hook
          (lambda () (unless my-toggle-modeline-global
                       (if shutup-p
                           (shut-up (my-mode-line-off))
                         (my-mode-line-off))))
          t) ;; 他の設定（olivetti.elなど）とぶつかるので最後に追加

;; init
(my-mode-line-off)

(unless noninteractive
  (postpone-message "winner-mode")
  (when (require 'winner nil t)
    (define-key winner-mode-map (kbd "C-x g") 'winner-undo)
    (define-key winner-mode-map (kbd "C-(") 'winner-undo)
    (define-key winner-mode-map (kbd "C-)") 'winner-redo)
    (winner-mode 1)))

(when (require 'shackle nil t)
  (setq shackle-default-ratio 0.33)
  (setq shackle-rules
        '(("*osx-dictionary*" :align above :popup t)
          ("*wclock*" :align above :popup t :select t)
          ("*Checkdoc Status*" :align above :popup t :noselect t)
          ;; ("*Help*" :align t :select 'above :popup t :size 0.3)
          ))
  (unless noninteractive
    (shackle-mode 1)))

(with-eval-after-load "checkdoc"
  (defun ad:checkdoc ()
    (interactive)
    (define-key checkdoc-minor-mode-map (kbd "q") 'my-delete-checkdoc-window)
    (define-key checkdoc-minor-mode-map (kbd "C-g") 'my-delete-checkdoc-window)
    (checkdoc-minor-mode 1))
  (advice-add 'checkdoc :before #'ad:checkdoc))

(with-eval-after-load "moom"
  (when (and (not noninteractive)
             (eq my-toggle-modeline-global 'doom)
             (require 'doom-modeline nil t))
    (custom-set-variables
     '(doom-modeline-buffer-file-name-style 'truncate-except-project)
     '(doom-modeline-bar-width 1)
     '(doom-modeline-height (let ((font (face-font 'mode-line)))
                              (if (and font (fboundp 'font-info))
                                  (floor (* 0.8 ;; 1.0
                                            (* 2 (aref (font-info font) 2))))
                                10)))
     '(doom-modeline-minor-modes t))
    (declare-function ad:doom-modeline-buffer-file-state-icon "init" nil)
    (defun ad:doom-modeline-buffer-file-state-icon
        (icon &optional text face height voffset)
      "Displays an ICON with FACE, HEIGHT and VOFFSET.
TEXT is the alternative if it is not applicable.
Uses `all-the-icons-material' to fetch the icon."
      (if doom-modeline-icon
          (when icon
            (doom-modeline-icon-material
             icon
             :face face
             :height (or height 0.85) ;; 1.1
             :v-adjust (or voffset -0.225))) ;; -0.225
        (when text
          (propertize text 'face face))))
    (advice-add 'doom-modeline-buffer-file-state-icon :override
                #'ad:doom-modeline-buffer-file-state-icon)
    (size-indication-mode 1)
    (doom-modeline-mode 1)))

(set-face-foreground 'font-lock-regexp-grouping-backslash "#66CC99")
(set-face-foreground 'font-lock-regexp-grouping-construct "#9966CC")

(unless noninteractive
  (require 'generic-x nil t))

(when (autoload-if-found
       '(hl-line-mode my-hl-line-enable)
       "hl-line" nil t)

  ;; (add-hook 'ah-after-move-cursor-hook #'my-hl-line-enable)

  (defvar my-hl-permanent-disabled '(dired-mode vterm-mode)
    "A list of major modes to disable `hl-line'.")

  (defvar my-ime-off-hline-hook nil)
  (defvar my-ime-on-hline-hook nil)

  (with-eval-after-load "hl-line"
    ;; my-hl-line-disable と my-hl-line-enable を utility.el に持っていく場合は
    ;; hl-line-modeの評価箇所を関数に置き換えるなど対策が必要
    (defun my-hl-line-disable ()
      "Disable `hl-line'."
      (hl-line-mode -1))

    (defun my-hl-line-enable ()
      "Enable `hl-line'."
      (unless (or hl-line-mode
		              (minibufferp)
			            (memq major-mode my-hl-permanent-disabled))
	      (hl-line-mode 1)))

    (unless (version< emacs-version "28.1")
      (setq hl-line-sticky-flag nil))

    (defvar my-hl-active-period 120
      "Disable `hl-line' after this period")

    ;; init
    (when (fboundp 'mac-ime-active-p)
      (if (version< emacs-version "27.1")
		      (if (my-ime-active-p) (my-ime-on-hline) (my-ime-off-hline))
	      (if (mac-ime-active-p) (my-ime-on-hline) (my-ime-off-hline))))

    (run-with-idle-timer my-hl-active-period t #'my-hl-line-disable)

    (if (version< emacs-version "27.1")
	      (progn
	        (add-hook 'focus-in-hook #'my-hl-line-enable)
	        (add-hook 'focus-out-hook #'my-hl-line-disable))
      (defun my-hl-line-update ()
	      (if (frame-focus-state) (my-hl-line-enable) (my-hl-line-disable)))
      (add-function :after after-focus-change-function #'my-hl-line-update))

    ;; (add-hook 'minibuffer-setup-hook #'my-hl-line-disable)
    ;; (add-hook 'minibuffer-exit-hook #'my-hl-line-enable)
    (add-hook 'input-method-activate-hook #'my-ime-on-hline)
    (add-hook 'input-method-deactivate-hook #'my-ime-off-hline)))

(setq blink-cursor-blinks 0)
(setq blink-cursor-interval 0.2)
(setq blink-cursor-delay 30)
(unless noninteractive
  (postpone-message "blink-cursor-mode")
  (blink-cursor-mode 1))

(my-setup-font)

;; set-default で global 指定すると，ミニバッファの message で制御不能になる
;; propertize で拡大できるが，global の値以下に縮小できなくなる．
;; (set-default 'line-spacing 2)
(add-hook 'buffer-list-update-hook #'my-linespacing)
(add-hook 'org-src-mode-hook #'my-linespacing)
(add-hook 'debugger-mode-hook #'my-linespacing)

(with-eval-after-load "diff-mode"
  (set-face-attribute 'diff-added nil
                      :background nil :foreground "lime green"
                      :weight 'normal)

  (set-face-attribute 'diff-removed nil
                      :background nil :foreground "firebrick1"
                      :weight 'normal)

  (set-face-attribute 'diff-file-header nil
                      :background nil :weight 'extra-bold)

  (set-face-attribute 'diff-hunk-header nil
                      :foreground "chocolate4"
                      :background "white" :weight 'extra-bold
                      :inherit nil))

(when (autoload-if-found
       '(global-hl-todo-mode my-hl-todo-activate)
       "hl-todo" nil t)

  ;; (defun my-hl-todo-activate ()
  ;;   (global-hl-todo-mode)
  ;;   (remove-hook 'pre-command-hook #'my-hl-todo-activate))
  ;; (add-hook 'pre-command-hook #'my-hl-todo-activate)

  (global-hl-todo-mode) ;; FIXME

  (with-eval-after-load "hl-todo"
    (add-hook 'my-light-theme-hook #'my-hl-todo-light-theme)
    (add-hook 'my-dark-theme-hook #'my-hl-todo-dark-theme)))

;; (declare-function my-font-config "init" nil)
;; This may override or reset font setting
(unless noninteractive
  (my-theme))
;; (run-at-time "21:00" 86400 'my-theme)
;; (run-at-time "05:00" 86400 'my-theme)) ;; FIXME: it makes frame blink

(when (autoload-if-found
       '(rainbow-mode)
       "rainbow-mode" nil t)

  (dolist (hook '(emmet-mode-hook emacs-lisp-mode-hook org-mode-hook))
    (add-hook hook #'rainbow-mode)))

(when (and (executable-find "qt_color_picker")
           (autoload-if-found
            '(edit-color-stamp)
            "edit-color-stamp" nil t))

  (global-set-key (kbd "C-c f c p") 'edit-color-stamp))

(with-eval-after-load "ivy"
  (custom-set-faces
   '(ivy-current-match
     ((((class color) (background light))
       :background "#FFF3F3" :distant-foreground "#000000" :extend t)
      (((class color) (background dark))
       :background "#404040" :distant-foreground "#abb2bf" :extend t)))
   '(ivy-minibuffer-match-face-1
     ((((class color) (background light)) :foreground "#666666")
      (((class color) (background dark)) :foreground "#999999")))
   '(ivy-minibuffer-match-face-2
     ((((class color) (background light)) :foreground "#c03333" :underline t)
      (((class color) (background dark)) :foreground "#e04444" :underline t)))
   '(ivy-minibuffer-match-face-3
     ((((class color) (background light)) :foreground "#8585ff" :underline t)
      (((class color) (background dark)) :foreground "#7777ff" :underline t)))
   '(ivy-minibuffer-match-face-4
     ((((class color) (background light)) :foreground "#439943" :underline t)
      (((class color) (background dark)) :foreground "#33bb33" :underline t)))))

(with-eval-after-load "ivy"
;;; 選択対象を "" にする (requires all-the-icons.el)
  (defface my-ivy-arrow-visible
    '((((class color) (background light)) :foreground "orange")
      (((class color) (background dark)) :foreground "#EE6363"))
    "Face used by Ivy for highlighting the arrow.")
  (defface my-ivy-arrow-invisible
    '((((class color) (background light)) :foreground "#FFFFFF")
      (((class color) (background dark)) :foreground "#31343F"))
    "Face used by Ivy for highlighting the invisible arrow.")

  (if window-system
      (cond ((require 'icons-in-terminal nil t)
             (defun my-ivy-format-function-arrow (cands)
               "Transform CANDS into a string for minibuffer."
               (ivy--format-function-generic
                (lambda (str)
                  (concat (icons-in-terminal-faicon
                           "hand-o-right"
                           :v-adjust -0.1
                           :face 'my-ivy-arrow-visible
                           :height 0.8)
                          " " (ivy--add-face (concat str "\n")
                                             'ivy-current-match)))
                (lambda (str)
                  (concat (icons-in-terminal-faicon
                           "hand-o-right"
                           :v-adjust -0.1
                           :face 'my-ivy-arrow-invisible
                           :height 0.8)
                          " " (concat str "\n")))
                cands
                ""))
             (setq ivy-format-functions-alist
                   '((t . my-ivy-format-function-arrow))))
            ((require 'all-the-icons nil t)
             (defun my-ivy-format-function-arrow (cands)
               "Transform CANDS into a string for minibuffer."
               (ivy--format-function-generic
                (lambda (str)
                  (concat (all-the-icons-faicon
                           "hand-o-right"
                           :v-adjust -0.2 :face 'my-ivy-arrow-visible)
                          " " (ivy--add-face str 'ivy-current-match)))
                (lambda (str)
                  (concat (all-the-icons-faicon
                           "hand-o-right" :face 'my-ivy-arrow-invisible)
                          " " str))
                cands
                "\n"))
             (setq ivy-format-functions-alist
                   '((t . my-ivy-format-function-arrow))))
            (t (setq ivy-format-functions-alist
                     '((t . ivy-format-function-arrow)))))
    (setq ivy-format-functions-alist '((t . ivy-format-function-arrow)))))

(when (autoload-if-found
       '(volatile-highlights-mode my-vhl-change-color)
       "volatile-highlights" nil t)

  (dolist (hook '(org-mode-hook emacs-lisp-mode-hook emmet-mode-hook))
    (add-hook hook #'volatile-highlights-mode))

  (global-set-key (kbd "M-v") 'my-yank)
  (global-set-key (kbd "C-y") 'my-yank)

  (with-eval-after-load "volatile-highlights"
    (set-face-attribute
     'vhl/default-face nil :foreground "#FF3333" :background "#FFCDCD")
    (volatile-highlights-mode t)

    (with-eval-after-load "vterm"
      (define-key vterm-mode-map (kbd "C-y") 'vterm-yank))

    (with-eval-after-load "org"
      (define-key org-mode-map (kbd "C-y") 'my-org-yank))))

(when (autoload-if-found
       '(my-google-this google-this google-this-word)
       "google-this" nil t)

  (global-set-key (kbd "C-c f g") 'my-google-this))

(when (autoload-if-found
       '(osx-lib-say osx-lib-say-region)
       "osx-lib" nil t)

  (with-eval-after-load "osx-lib"
    (custom-set-variables
     '(osx-lib-say-ratio 100)
     '(osx-lib-say-voice "Samantha"))))

(when (autoload-if-found
       '(my-cmd-to-open-iterm2)
       "utility" nil t)

  (global-set-key (kbd "C-M-i") #'my-cmd-to-open-iterm2)

  (with-eval-after-load "flyspell"
    (define-key flyspell-mode-map (kbd "C-M-i") #'my-cmd-to-open-iterm2))

  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-M-i") #'my-cmd-to-open-iterm2)))

(global-set-key (kbd "C-c f t") 'my-open-current-directory-in-terminal)

(when (autoload-if-found
       '(gif-screencast)
       "gif-screencast" nil t)

  (with-eval-after-load "gif-screencast"
    (setq gif-screencast-want-optimized nil)
    (setq gif-screencast-args '("-x"))
    (setq gif-screencast-capture-format "ppm")

    ;; Start... M-x gif-screencast
    (define-key gif-screencast-mode-map (kbd "<f5>") 'gif-screencast-stop)
    (define-key gif-screencast-mode-map (kbd "S-<f5>")
      'gif-screencast-toggle-pause)

    ;; 拡張
    (defcustom gif-screencast-additional-normal-hooks '()
      "A list of hooks. These hooks activate `gif-screencast-capture'."
      :group 'gif-screencast
      :type '(repeat hook))

    (add-to-list 'gif-screencast-additional-normal-hooks
                 'window-size-change-functions) ;; for which-key.el, as of 26.1
    (add-to-list 'gif-screencast-additional-normal-hooks
                 'window-configuration-change-hook) ;; for which-key.el
    (add-to-list 'gif-screencast-additional-normal-hooks 'focus-in-hook)
    (add-to-list 'gif-screencast-additional-normal-hooks 'focus-out-hook)
    (add-to-list 'gif-screencast-additional-normal-hooks 'minibuffer-setup-hook)
    (add-to-list 'gif-screencast-additional-normal-hooks 'minibuffer-exit-hook)
    ;; (add-to-list 'gif-screencast-additional-normal-hooks 'pre-redisplay-functions)
    ;; pre-redisplay-functions はやばい．

    ;; modification-hooks
    (defun ad:gif-screencast ()
      (dolist (hook gif-screencast-additional-normal-hooks)
        (add-hook hook #'gif-screencast-capture)))
    (advice-add 'gif-screencast :after #'ad:gif-screencast)

    (defun ad:gif-screencast-stop ()
      (dolist (hook gif-screencast-additional-normal-hooks)
        (remove-hook hook 'gif-screencast-capture)))
    (advice-add 'gif-screencast-stop :after #'ad:gif-screencast-stop)

    (defun ad:gif-screencast-toggle-pause ()
      (if (memq 'gif-screencast-capture (default-value 'pre-command-hook))
          (dolist (hook gif-screencast-additional-normal-hooks)
            (remove-hook hook 'gif-screencast-capture))
        (dolist (hook gif-screencast-additional-normal-hooks)
          (add-hook hook #'gif-screencast-capture))))
    (advice-add 'gif-screencast-toggle-pause
                :before #'ad:gif-screencast-toggle-pause)

    (defun ad:gif-screencast-opendir ()
      "Open the output directory when screencast is finished."
      (if (not (eq system-type 'darwin))
          (my-gif-screencast-opendir-dired)
        (shell-command-to-string
         (concat "open " gif-screencast-screenshot-directory))
        (shell-command-to-string
         (concat "open " gif-screencast-output-directory))))
    (advice-add 'gif-screencast-stop :before #'ad:gif-screencast-opendir)))

(global-set-key (kbd "C-M--") 'my-cycle-bullet-at-heading)
;; (global-set-key (kbd "<f12>") 'my-open-file-ring)
;;  (global-set-key (kbd "C-c t") 'my-date)
(global-set-key (kbd "C-c f 4") 'my-window-resizer)

(when (autoload-if-found
       '(manage-minor-mode)
       "manage-minor-mode" nil t)

  (with-eval-after-load "manage-minor-mode"
    (define-key manage-minor-mode-map (kbd "q")
      (lambda () (interactive)
          (delete-window (get-buffer-window "*manage-minor-mode*"))))))

(with-eval-after-load "counsel"
;;; auto fzf, 0件ヒットの時，1回だけ[y/n]で counsel-fzf に繋ぐか問う
  (defcustom my-nocand-then-fzf-commands '(counsel-recentf
                                           counsel-projectile-find-file
                                           counsel-projectile-switch-project)
    "List of commands for applying extension no candidates then `counsel-fzf'."
    :group 'ivy
    :type '(list symbol))

  (defcustom my-nocand-then-fzf-idle-time 2.0
    "Idle time for showing prompt."
    :group 'ivy
    :type 'float) ;; N[s] 無応答の時[y/n]を出す．

  (defvar my--nocand-then-fzf t)
  (defun my-nocand-then-fzf-reset ()
    (setq my--nocand-then-fzf t))
  (defun my-nocand-then-fzf (prompt)
    (when (= ivy--length 0)
      (if (eq (read-char prompt) ?y) ;; y-or-n-p is not applicable
          (ivy-exit-with-action
           (lambda (_x)
             (counsel-fzf ivy-text default-directory)))
        (setq my--nocand-then-fzf nil))))
  (defun ad:fzf:ivy--insert-prompt ()
    (when (and my--nocand-then-fzf
               (memq (ivy-state-caller ivy-last) my-nocand-then-fzf-commands)
               (= ivy--length 0))
      (let* ((std-props
              '(front-sticky t rear-nonsticky t field t read-only t))
             (prompt (concat (my-pre-prompt-function)
                             "Switch to Counsel-fzf? [y/n] ")))
        (set-text-properties 0 (length prompt)
                             `(face minibuffer-prompt ,@std-props) prompt)
        (run-with-idle-timer my-nocand-then-fzf-idle-time
                             nil #'my-nocand-then-fzf prompt))))
  (advice-add 'ivy--insert-prompt :before #'ad:fzf:ivy--insert-prompt)
  (add-hook 'minibuffer-setup-hook #'my-nocand-then-fzf-reset)
  (add-hook 'minibuffer-exit-hook #'my-nocand-then-fzf-reset))

(when (autoload-if-found
       '(elfeed elfeed-update elfeed-web-start)
       "elfeed" nil t)

  (with-eval-after-load "elfeed"
    (setq elfeed-db-directory "~/Dropbox/emacs.d/elfeed")
    (when (require 'elfeed-org nil t)
      (elfeed-org)
      (setq rmh-elfeed-org-files (list "~/Dropbox/org/elfeed.org")))
    ;; これで elfeed-feeds が更新される
    ;; その後，M-x elfeed, M-x elfeed-update する
    (when (require 'elfeed-web nil t)
      (setq elfeed-web-data-root (concat my-elget-package-dir "/web")))))

(provide 'late-init)
