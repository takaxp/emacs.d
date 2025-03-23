;; late-init.el --- My config with postpone.el -*- lexical-binding: t -*-
(defvar my-late-init-start (current-time))

(defvar measure-exec-time-list nil)
(dolist (f measure-exec-time-list)
  (advice-add f :around #'my--measure-exec-time))

;; (setq byte-compile-warnings '(obsolete))
;; Suppress warning on cl.el loading
(defvar my-exclude-deprecated-packages '(cl tls))
(advice-add 'do-after-load-evaluation :override #'my--do-after-load-evaluation)

(setq save-silently t) ;; No need shut-up.el for saving files.

;; originally defined in `diary-lib.el'
(defun diary-entry-time (s)
  "Return time at the beginning of the string S as a military-style integer.
For example, returns 1325 for 1:25pm.

Returns `diary-unknown-time' (default value -9999) if no time is recognized.
The recognized forms are XXXX, X:XX, or XX:XX (military time), and XXam,
XXAM, XXpm, XXPM, XX:XXam, XX:XXAM, XX:XXpm, or XX:XXPM.  A period (.) can
be used instead of a colon (:) to separate the hour and minute parts."
  (let (case-fold-search)
    (cond ((string-match                ; military time
            "\\`[ \t\n]*\\([0-9]?[0-9]\\)[:.]?\\([0-9][0-9]\\)\\(\\>\\|[^ap]\\)"
            s)
           (+ (* 100 (string-to-number (match-string 1 s)))
              (string-to-number (match-string 2 s))))
          ((string-match                ; hour only (XXam or XXpm)
            "\\`[ \t\n]*\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" s)
           (+ (* 100 (% (string-to-number (match-string 1 s)) 12))
              (if (equal ?a (downcase (aref s (match-beginning 2))))
                  0 1200)))
          ((string-match        ; hour and minute (XX:XXam or XX:XXpm)
            "\\`[ \t\n]*\\([0-9]?[0-9]\\)[:.]\\([0-9][0-9]\\)\\([ap]\\)m\\>" s)
           (+ (* 100 (% (string-to-number (match-string 1 s)) 12))
              (string-to-number (match-string 2 s))
              (if (equal ?a (downcase (aref s (match-beginning 3))))
                  0 1200)))
          (t diary-unknown-time))))

;; Avoid to load diary-lib to use `diary-entry-time'
(defun run-at-time (time repeat function &rest args)
  "Perform an action at time TIME.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
REPEAT may be an integer or floating point number.
TIME should be one of:

- a string giving today's time like \"11:23pm\"
  (the acceptable formats are HHMM, H:MM, HH:MM, HHam, HHAM,
  HHpm, HHPM, HH:MMam, HH:MMAM, HH:MMpm, or HH:MMPM;
  a period `.' can be used instead of a colon `:' to separate
  the hour and minute parts);

- a string giving a relative time like \"90\" or \"2 hours 35 minutes\"
  (the acceptable forms are a number of seconds without units
  or some combination of values using units in `timer-duration-words');

- nil, meaning now;

- a number of seconds from now;

- a value from `encode-time';

- or t (with non-nil REPEAT) meaning the next integral multiple
  of REPEAT.  This is handy when you want the function to run at
  a certain \"round\" number.  For instance, (run-at-time t 60 ...)
  will run at 11:04:00, 11:05:00, etc.

The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in
`cancel-timer'."
  (interactive "sRun at time: \nNRepeat interval: \naFunction: ")

  (when (and repeat
             (numberp repeat)
             (< repeat 0))
    (error "Invalid repetition interval"))

  (let ((timer (timer-create)))
    ;; Special case: nil means "now" and is useful when repeating.
    (unless time
      (setq time (current-time)))

    ;; Special case: t means the next integral multiple of REPEAT.
    (when (and (eq time t) repeat)
      (setq time (timer-next-integral-multiple-of-time nil repeat))
      (setf (timer--integral-multiple timer) t))

    ;; Handle numbers as relative times in seconds.
    (when (numberp time)
      (setq time (timer-relative-time nil time)))

    ;; Handle relative times like "2 hours 35 minutes".
    (when (stringp time)
      (when-let ((secs (timer-duration time)))
        (setq time (timer-relative-time nil secs))))

    ;; Handle "11:23pm" and the like.  Interpret it as meaning today
    ;; which admittedly is rather stupid if we have passed that time
    ;; already.  (Though only Emacs hackers hack Emacs at that time.)
    (when (stringp time)
      ;; (require 'diary-lib) ;; *Modified*
      (let ((hhmm (diary-entry-time time))
            (now (decode-time)))
        (when (>= hhmm 0)
          (setq time (encode-time 0 (% hhmm 100) (/ hhmm 100)
                                  (decoded-time-day now)
                                  (decoded-time-month now)
                                  (decoded-time-year now)
                                  (decoded-time-zone now))))))

    (timer-set-time timer time repeat)
    (timer-set-function timer function args)
    (timer-activate timer)
    timer))

(when (autoload-if-found '(gcmh-time gcmh-mode) "gcmh" nil t)
  (defvar my-gcmh-timer
    (unless noninteractive
      (run-with-idle-timer (+ 10 my-default-loading-delay)
                           nil #'my-gcmh-activate)))

  (with-eval-after-load "gcmh"
    (setq gcmh-verbose nil)
    (advice-add 'garbage-collect :around #'my--garbage-collect)
    (advice-add 'gcmh-idle-garbage-collect
                :around #'my--gcmh-idle-garbage-collect)))

(setq message-log-max 5000) ;; メッセージバッファの長さ
(defvar shutup-p nil)

(with-eval-after-load "comp"
  (setq native-comp-async-query-on-exit t)
  (setf comp-num-cpus (max 1 (- (num-processors) 2))))

(with-eval-after-load "comp"
  (add-hook 'native-comp-async-all-done-hook #'my-native-comp-packages-done))

;; Limit the final word to a line break code (automatically correct)
(setq require-final-newline t)

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

(autoload-if-found '(el-get-version
                     el-get-bundle my-elget-list my-elget-reset-links
                     el-get-cd el-get-remove el-get-update
                     el-get-install el-get-reinstall
                     my-elget-nativecomp-all-packages)
                   "elget-config" nil t)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)
(add-hook 'emacs-lisp-mode-hook #'turn-on-font-lock)

(setq vc-follow-symlinks t)

(unless noninteractive
  (add-hook 'find-file-hook #'my-auto-revert-activate)
  ;; revert されるのが org バッファのとき，自動的にドロワをたたむ
  ;; カーソルが (point-max) に移動してしまう場合は非推奨
  (with-eval-after-load "org"
    (add-hook 'after-revert-hook 'my-org-hide-drawers-all)))

(unless noninteractive
  (when (fboundp 'pixel-scroll-mode)
    (pixel-scroll-mode 1))) ;; 26.1

(add-hook 'find-file-hook #'my-shorten-default-directory 1)

(when (autoload-if-found '(aggressive-indent-mode)
                         "aggressive-indent" nil t)
  (dolist (hook
           '(;; python-mode-hook
             ;; nxml-mode-hook
             ;; web-mode-hook
             emacs-lisp-mode-hook
             lisp-mode-hook perl-mode-hook c-mode-common-hook))

    (add-hook hook #'aggressive-indent-mode)))

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(when (autoload-if-found '(ws-butler-mode ws-butler-global-mode)
                         "ws-butler" nil t)

  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-mode-hook perl-mode-hook c-mode-common-hook))
    (add-hook hook #'ws-butler-mode))

  (with-eval-after-load "ws-butler"
    (custom-set-variables
     '(ws-butler-global-exempt-modes
       (append '(org-mode empty-booting-mode change-log-mode epa-mode)
               ws-butler-global-exempt-modes)))))

(unless noninteractive
  (defvar my-private-conf-timer
    (run-with-idle-timer (+ 6 my-default-loading-delay)
                         nil #'my-private-conf-activate))
  (when (version< "27.0" emacs-version)
    ;; ミニバッファでパスワードを入力する
    (setq epg-pinentry-mode 'loopback)))

(with-eval-after-load "epa"
  ;; Suppress message when saving encrypted file (hoge.org.gpg)
  (advice-add 'epa-file-write-region :around #'my--suppress-message))

(autoload 'mail "~/Dropbox/config/my-mail.el.gpg" nil t)

(when (memq window-system '(ns nil))

  (custom-set-faces
   '(ns-marked-text-face
     ((t (:foreground "black"
          :background "light pink" :underline "OrangeRed2"))))
   '(ns-unmarked-text-face
     ((t (:foreground "black"
          :background "light sky blue" :underline "royal blue")))))

  (when (and (fboundp 'mac-get-current-input-source)
             (version< "27.0" emacs-version))
    ;; "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese" for Big Sur
    (custom-set-variables
     '(mac-default-input-source "com.google.inputmethod.Japanese.base"))
    (unless noninteractive
      (mac-input-method-mode 1))

    ;; see also activate-mark-hook, deactivate-mark-hook
    (add-hook 'isearch-mode-hook #'my-isearch-ime-deactivate-sticky)
    (add-hook 'isearch-mode-end-hook #'mac-ime-activate-sticky))

  (with-eval-after-load "org"
    ;; カーソル移動で heading に来たときは即座にIMEをOFFにする
    ;; (add-hook 'ah-after-move-cursor-hook #'my-ns-org-heading-auto-ascii)
    ;; カーソル移動で heading に留まった時にIMEをOFFにする
    (unless noninteractive
      (run-with-idle-timer 0.2 t #'my-ns-org-heading-auto-ascii)))

  (with-eval-after-load "hl-line"
    (add-hook 'input-method-activate-hook #'my-working-text-face-on)
    (add-hook 'input-method-deactivate-hook #'my-working-text-face-off)))

(autoload-if-found '(er/mark-symbol) "expand-region" nil t)
(advice-add 'mark-sexp :around #'my--mark-sexp)
(advice-add 'mark-sexp :around #'my--er:mark-sexp)

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

(unless noninteractive
  (when (require 'ah nil t)
    (setq ah-lighter "")
    (ah-mode 1)))

(when (autoload-if-found '(smooth-scroll-mode)
                         "smooth-scroll" nil t)

  (with-eval-after-load "smooth-scroll"
    (custom-set-variables
     '(smooth-scroll/vscroll-step-size 6)
     '(smooth-scroll/hscroll-step-size 6)))

  (unless noninteractive
    (smooth-scroll-mode t)))

(with-eval-after-load "bs"
  (custom-set-variables
   '(bs-cycle-configuration-name "files-and-scratch")
   '(bs-max-window-height 10))

  ;; リストを縦表示する
  (when (require 'bsv nil t)
    (setq bsv-max-height 5
          bsv-message-timeout 9)))

(when (autoload-if-found '(my-toggle-bm
                           my-bm-next bm-buffer-save bm-buffer-restore
                           bm-buffer-save-all bm-repository-save
                           bm-repository-load counsel-bm)
       "bm" nil t)

  ;; ファイルオープン時にブックマークを復帰
  (keymap-global-set "<f10>" 'my-toggle-bm)
  (keymap-global-set "C-<f10>" 'my-bm-next)
  (keymap-global-set "S-<f10>" 'bm-show-all)
  (add-hook 'find-file-hook #'bm-buffer-restore)

  ;; ビルトイン bookmark の配色を無効にする(as of 28.1)
  (setq bookmark-fontify nil)

  ;; ビルトイン bookmark がfringeに出すマークを無効にする(as of 28.1)
  (setq bookmark-set-fringe-mark nil)

  (with-eval-after-load "ivy"
    (keymap-global-set "S-<f10>" 'counsel-bm))

  (with-eval-after-load "bm"
    (advice-add 'bm-repository-load :around #'my--suppress-message)

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

    (advice-add 'bm-show-mode :after #'my--bm-show-mode)))

(when (autoload-if-found '(centered-cursor-mode)
                         "centered-cursor-mode" nil t)

  (with-eval-after-load "isearch"
    ;; isearch 実行時のみ有効化
    (add-hook 'isearch-mode-hook #'my-centered-cursor-activate)
    (add-hook 'isearch-mode-end-hook #'my-centered-cursor-deactivate)))

(when (autoload-if-found '(smart-mark-mode)
                         "smart-mark" nil t)

  (add-hook 'find-file-hook #'my-smart-mark-activate)

  (with-eval-after-load "smart-mark"
    (progn ;; C-M-SPC SPC SPC ... C-g の場合に正しくカーソルと元に戻す．
      (advice-add 'smart-mark-restore-cursor :override
                  #'my--smart-mark-restore-cursor)
      (advice-add 'smart-mark-set-restore-before-mark :override
                  #'my--smart-mark-set-restore-before-mark)

      (when (require 'expand-region-core nil t)
        (advice-add 'keyboard-quit :after #'my--er:keyboard-quit))
      ;; (advice-add 'keyboard-quit :before #'my--er:pre:keyboard-quit)
      )))
;; (defun my-smart-mark-activate () (smart-mark-mode 1))
;; (defun my-smart-mark-dectivate () (smart-mark-mode -1))
;; (add-hook 'isearch-mode-hook #'my-smart-mark-dectivate)
;; (add-hook 'isearch-mode-end-hook #'my-smart-mark-activate)

(when (autoload-if-found '(global-syntax-subword-mode
                           syntax-subword-backward-kill
                           syntax-subword-mode syntax-subword-kill)
                         "syntax-subword" nil t)

  (advice-add 'forward-word :before #'my--syntax-subword-activate)
  (advice-add 'backward-word :before #'my--syntax-subword-activate)

  (keymap-global-set "C-<backspace>" #'syntax-subword-backward-kill)

  (with-eval-after-load "syntax-subword"
    ;; C-<backspace> で，削除領域をコピーしない．
    (advice-add 'syntax-subword-kill :override #'my--syntax-subword-kill)))

(setq yank-excluded-properties t)

(add-hook 'before-save-hook #'my-time-stamp)

(with-eval-after-load "time-stamp"
  (setq time-stamp-start "#\\+date:[ \t]*") ;; "Time-stamp:[ \t]+\\\\?[\"<]+"
  (setq time-stamp-end "$") ;; "\\\\?[\">]"
  (setq time-stamp-line-limit 10)) ;; def=8

(with-eval-after-load "isearch"
  (advice-add 'isearch-mode :around #'my--isearch-mode)

  ;; C-g を isearch-exit に割り当てて途中中断とする．（カーソルを留めておきたい）カーソルを検索開始時点の場所に戻すには，別途 counsel-mark-ring を使う
  (keymap-set isearch-mode-map "C-g" 'isearch-exit))

(with-eval-after-load "add-log"
  (add-hook 'change-log-mode-hook
            (lambda ()
              (view-mode 1)
              (my-orgalist-activate)
              (setq tab-width 4)
              (setq left-margin 4)))

  (advice-add 'add-change-log-entry-other-window
              :before #'my--add-change-log-entry-other-window))

(when (autoload-if-found '(modern-c++-font-lock-mode)
                         "modern-cpp-font-lock" nil t)
  (push '("\\.[hm]$" . c++-mode) auto-mode-alist)
  (add-hook 'c-mode-hook #'modern-c++-font-lock-mode)
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(add-hook 'nxml-mode-hook
          (lambda ()
            (keymap-set nxml-mode-map "RET" 'newline-and-indent)
            (auto-fill-mode -1)
            (setq indent-tabs-mode t)
            (setq nxml-slash-auto-complete-flag t)
            (setq tab-width 1)
            (setq nxml-child-indent 1)
            (setq nxml-attribute-indent 0)))

(when (autoload-if-found '(yaml-mode)
                         "yaml-mode" nil t)
  (push '("\\.yml$" . yaml-mode) auto-mode-alist))

(when (autoload-if-found '(json-mode)
                         "json-mode" nil t)
  (push '("\\.json$" . json-mode) auto-mode-alist)
  (with-eval-after-load "json-mode"
    (add-hook 'before-save-hook #'my-json-mode-beautify)
    (add-hook 'after-save-hook #'my-json-pretty-print-buffer)))

(when (autoload-if-found '(csv-mode)
                         "csv-mode" nil t)
  (push '("\\.csv$" . csv-mode) auto-mode-alist))

(autoload-if-found '(ascii-on ascii-off) "ascii" nil t)

(when (autoload-if-found '(cc-mode)
                         "cc-mode" nil t)
  (push '("\\.pde$" . java-mode) auto-mode-alist) ;; Processing
  (push '("\\.java$" . java-mode) auto-mode-alist))

(when (autoload-if-found '(es-mode)
                         "es-mode" nil t)
  (push '("\\.es$" . es-mode) auto-mode-alist))

(when (autoload-if-found '(markdown-mode)
                         "markdown-mode" nil t)
  (push '("\\.markdown$" . markdown-mode) auto-mode-alist)
  (push '("\\.md$" . markdown-mode) auto-mode-alist))

(when (autoload-if-found '(cmake-mode)
                         "cmake-mode" nil t)
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
  (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

  (with-eval-after-load "cmake-mode"
    (unless (executable-find "cmake")
      (message "--- cmake is NOT installed."))))

(when (autoload-if-found '(logview-mode)
                         "logview" nil t)
  (push '("\\.log$" . logview-mode) auto-mode-alist))

;; 特定の拡張子・ディレクトリ
(defvar my-auto-view-regexp "\\.el.gz$\\|\\.patch$\\|\\.xml$\\|\\.gpg$\\|\\.csv$\\|\\.emacs.d/[^/]+/el-get\\|config")
(defvar my-auto-view-buffers '("*Messages*"))

;; 特定のディレクトリ（絶対パス・ホームディレクトリ以下）
(defvar my-auto-view-dirs nil)
(add-to-list 'my-auto-view-dirs "~/devel/emacs-head/emacs/")
(add-to-list 'my-auto-view-dirs "~/devel/git/org-mode/lisp/")
(when (eq window-system 'w32)
  (add-to-list 'my-auto-view-dirs "c:/msys64/mingw64"))

;; (autoload 'my-auto-view "view" nil t)
(add-hook 'find-file-hook #'my-auto-view)

(with-eval-after-load "view"
  ;; note: messages-buffer-mode-hook may not work
  (advice-add 'switch-to-buffer :after #'my--switch-to-buffer)

  (keymap-set view-mode-map "i" 'View-exit-and-edit)
  (keymap-set view-mode-map "SPC" 'ignore)
  (keymap-set view-mode-map "<delete>" 'ignore)
  (keymap-set view-mode-map "S-SPC" 'mac-ime-toggle)
  (keymap-set view-mode-map "e" 'my-view-exit)
  (when (require 'helpful nil t)
    (keymap-set view-mode-map "h" 'helpful-at-point))
  (keymap-set view-mode-map "f" 'forward-char)
  (keymap-set view-mode-map "b" 'backward-char)
  (keymap-set view-mode-map "n" 'my-org-view-next-heading)
  (keymap-set view-mode-map "p" 'my-org-view-previous-heading)
  (keymap-set view-mode-map "g" #'my-google-this)
  (keymap-set view-mode-map "<tab>" 'my-view-tab)
  (keymap-set view-mode-map "S-<tab>" 'my-view-shifttab)
  (unless my-toggle-modeline-global
    (advice-add 'view--enable :before #'my--view--enable)
    (advice-add 'view--disable :before #'my--view--disable)))

(when (autoload-if-found '(web-mode)
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
    (keymap-set web-mode-map "S-<tab>" 'my-web-indent-fold)

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
(when (autoload-if-found '(po-mode)
                         "po-mode" nil t)
  (push '("\\.po[tx]?\\'\\|\\.po\\$" . po-mode) auto-mode-alist))

(when (autoload-if-found '(go-mode)
                         "go-mode" nil t)
  (push '("\\.go\\'" . go-mode) auto-mode-alist))

(when (autoload-if-found '(emacs-lisp-mode)
                         "elisp-mode" nil t)
  ;; (push '("\\.el\\'" . emacs-lisp-mode) auto-mode-alist)
  (add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-indent-conf))

(when (autoload-if-found '(ispell-region ispell-complete-word)
                         "ispell" nil t)

  ;; Spell checking within a specified region
  (keymap-global-set "C-c f 7" 'ispell-region)
  ;; 補完候補の表示（flyspell が使える時はそちらを優先して <f7> にする．
  (keymap-global-set "<f7>" 'ispell-word)

  (with-eval-after-load "ispell"
    ;; This could hild other messages from loading functions regarding org-mode.
    (advice-add 'ispell-init-process :around #'my--suppress-message)

    ;; for English and Japanese mixed
    (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
    ;; http://endlessparentheses.com/ispell-and-org-mode.html
    (add-to-list 'ispell-skip-region-alist '("^#\\+begin_src" . "^#\\+end_src"))
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (setq ispell-encoding8-command t)

    (cond
     ((executable-find "hunspell")
      ;; (setenv "LC_ALL" "en_US") ;; Don't use this line.
      ;; (setq ispell-extra-args '("--lang=en_US"))
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
            (concat (getenv "SYNCROOT") "/emacs.d/hunspell.en.dic")))

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

(when (autoload-if-found '(flyspell-mode-on
                           flyspell-prog-mode flyspell-mode my-flyspell-on)
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
    (keymap-set flyspell-mode-map "C-;" 'comment-dwim)
    (setq flyspell-duplicate-distance 0)
    ;; (setq flyspell-mode-line-string " F")
    (setq flyspell-mode-line-string "")
    ;; (setq flyspell-large-region 200)
    (set-face-attribute 'flyspell-duplicate nil
                        :foreground "#EA5506" :bold t
                        :background 'unspecified :underline t)
    (set-face-attribute 'flyspell-incorrect nil
                        :foreground "#BA2636" :bold nil
                        :background 'unspecified :underline t)

    ;; ispell-complete-word のキーバインドを上書き
    (keymap-global-set "<f7>" 'flyspell-correct-at-point)

    ;; ivy を用いる
    (when (require 'flyspell-correct-ivy nil t)
      (setq flyspell-correct-interface #'flyspell-correct-ivy))

    ;; Auto complete との衝突を回避
    (with-eval-after-load "auto-complete"
      (ac-flyspell-workaround))

    ;; [FIXME] nextstep+inline-patch版で flyspell すると，日本語nyuuのようになる場合があるので，それを回避（IME が ONになったら一時的に flyspell を止める）
    (add-hook 'input-method-activate-hook #'my-flyspell-off)
    (add-hook 'input-method-deactivate-hook #'my-flyspell-on)))

(autoload-if-found '(counsel-world-clock) "counsel-world-clock" nil t)

(when (autoload-if-found '(latex-math-preview-expression
                           latex-math-preview-insert-symbol
                           latex-math-preview-save-image-file
                           latex-math-preview-beamer-frame)
                         "latex-math-preview" nil t nil)
  (keymap-global-set "<f6>" 'latex-math-preview-expression)
  (with-eval-after-load "latex-math-preview"
    (setq latex-math-preview-command-path-alist
          '((latex . "latex")
            (dvipng . "dvipng")
            (dvips . "dvips")))
    (keymap-set latex-math-preview-expression-mode-map "<f6>"
      'latex-math-preview-delete-buffer)))

(when (autoload-if-found '(yatex-mode)
                         "yatex" "Yet Another LaTeX mode" t)
  (push '("\\.tex$" . yatex-mode) auto-mode-alist)

  (with-eval-after-load "yatex"
    ;; Disable auto line break
    (add-hook 'yatex-mode-hook
              (lambda ()
                (setq auto-fill-function nil)))

    ;; 1=Shift JIS, 2=JIS, 3=EUC, 4=UTF-8
    ;; (setq YaTeX-kanji-code nil)
    (modify-coding-system-alist 'file "\\.tex$'" 'utf-8)
    (keymap-set YaTeX-mode-map "C-M-SPC" 'mark-sexp)
    (keymap-set YaTeX-mode-map "C-M-@" 'mark-sexp)))

(with-eval-after-load "yatex"
  (put 'YaTeX-insert-braces 'begend-guide 2)
  (advice-add 'YaTeX-insert-begin-end :override #'my--YaTeX-insert-begin-end))

(with-eval-after-load "yasnippet"
  (require 'ivy-yasnippet nil t))

(when (autoload-if-found '(osx-dictionary-search-pointer
                           osx-dictionary-search-input)
                         "osx-dictionary" nil t)
  (keymap-global-set "C-M-w" #'osx-dictionary-search-pointer)
  (keymap-global-set "C-c f w" #'osx-dictionary-search-input)
  (with-eval-after-load "osx-dictionary"
    (custom-set-variables
     '(osx-dictionary-dictionary-choice "英辞郎 第七版"))))

(when (autoload-if-found '(js2-mode)
                         "js2-mode" nil t)
  (with-eval-after-load "js2-mode"
    (if (executable-find "js-beautify")
        (when (require 'web-beautify nil t)
          (keymap-set js2-mode-map "C-c b" 'web-beautify-js)
          (keymap-set js2-mode-map "C-c b" 'web-beautify-css))
      (message "--- js-beautify is NOT installed.")
      (message "--- Note: brew install node")
      (message "---       npm -g install js-beautify"))))

(when (autoload-if-found '(smartparens-global-mode
                           turn-on-show-smartparens-mode)
                         "smartparens" nil t)
  (add-hook 'yatex-mode-hook #'my-smartparens-mode)
  (add-hook 'org-mode-hook #'my-smartparens-mode) ;; FIXME use activate()?
  (with-eval-after-load "smartparens"
    (setq-default sp-highlight-pair-overlay nil)
    (setq-default sp-highlight-wrap-overlay nil)
    (setq-default sp-highlight-wrap-tag-overlay nil)
    (sp-pair "`" nil :actions :rem)
    (sp-pair "'" nil :actions :rem)
    (sp-pair "[" nil :actions :rem)
    (sp-local-pair 'org-mode "=" "=")
    (sp-local-pair 'org-mode "$" "$" :actions '(wrap)) ;; 選択時のみ有効
    (sp-local-pair 'org-mode "'" "'" :actions '(wrap)) ;; 選択時のみ有効
    (sp-local-pair 'org-mode "<" ">" :actions '(wrap)) ;; 選択時のみ有効
    (sp-local-pair 'org-mode "_" "_" :actions '(wrap)) ;; 選択時のみ有効
    (sp-local-pair 'org-mode "~" "~" :actions '(wrap)) ;; 選択時のみ有効
    (sp-local-pair 'org-mode "[" "]" :actions '(wrap)) ;; 選択時のみ有効
    (sp-local-pair 'org-mode "+" "+" :actions '(wrap)) ;; 選択時のみ有効
    (sp-local-pair 'org-mode "/" "/" :actions '(wrap)) ;; 選択時のみ有効
    (sp-local-pair 'org-mode "*" "*" :actions '(wrap)) ;; 選択時のみ有効
    (sp-local-pair 'yatex-mode "$" "$" :actions '(wrap))))

(when (autoload-if-found '(grugru-default grugru)
                         "grugru-default" nil t)
  (keymap-global-set "C-9" #'grugru)
  (with-eval-after-load "grugru-default"
    (custom-set-faces
     '(grugru-edit-completing-function #'ivy-completing-read)
     '(grugru-highlight-face ((t (:bold t :underline "#FF3333"))))
     '(grugru-highlight-idle-delay 1))

    (add-hook 'grugru-before-hook #'my-unlock-view-mode)
    (add-hook 'grugru-after-hook #'save-buffer)
    (add-hook 'ah-after-move-cursor-hook #'grugru--highlight-remove)
    (grugru-define-on-major-mode 'org-mode 'word '("TODO" "DONE"))
    (grugru-define-global 'word '("True" "False"))
    (grugru-define-global 'word '("TRUE" "FALSE")) ;; FIXME
    (grugru-default-setup)
    (grugru-find-function-integration-mode 1)
    (grugru-highlight-mode 1)))

(autoload-if-found '(query-replace-from-region query-replace-regexp-from-region)
                   "replace-from-region" nil t)

(autoload-if-found '(embark-act) "embark" nil t)

(when (autoload-if-found '(selected-global-mode)
                         "selected" nil t)
  (add-hook 'activate-mark-hook #'my-activate-selected)
  (with-eval-after-load "selected"
    (keymap-set selected-keymap "a" #'embark-act)
    (keymap-set selected-keymap ";" #'comment-dwim)
    (keymap-set selected-keymap "e" #'my-eval-region)
    (keymap-set selected-keymap "E" #'my-eval-region-as-function)
    ;; (keymap-set selected-keymap "=" #'count-words-region)
    (when (require 'helpful nil t)
      (keymap-set selected-keymap "h" #'helpful-at-point)
      (keymap-set selected-keymap "v" #'my-helpful-variable))
    (keymap-set selected-keymap "w" #'osx-dictionary-search-pointer)
    (keymap-set selected-keymap "d" #'osx-dictionary-search-pointer)
    (keymap-set selected-keymap "5" #'query-replace-from-region)
    (keymap-set selected-keymap "g" #'my-google-this)
    (keymap-set selected-keymap "s" #'osx-lib-say-region)
    (keymap-set selected-keymap "q" #'selected-off)
    (keymap-set selected-keymap "x" #'my-hex-to-decimal)
    (keymap-set selected-keymap "X" #'my-decimal-to-hex)

    ;; (defun my-eval-region ()
    ;;   (interactive)
    ;;   (when (use-region-p)
    ;;     (eval-region (region-beginning) (region-end) t)))

    (setq selected-org-mode-map (make-sparse-keymap))
    (keymap-set selected-org-mode-map "t" #'org-toggle-checkbox)
    (keymap-set selected-org-mode-map "-" #'my-org-bullet-and-checkbox)

    (when (require 'expand-region nil t)
      (keymap-set selected-keymap "SPC" #'er/expand-region))

    (when (require 'counsel-selected nil t)
      (keymap-set selected-keymap "l" 'counsel-selected))

    (when (require 'help-fns+ nil t)
      (keymap-set selected-keymap "H" #'my-describe-selected-keymap))))

(when (autoload-if-found '(git-complete)
                         "git-complete" nil t)
  (keymap-global-set "C-c f <tab>" 'git-complete))

(when (autoload-if-found '(bratex-config)
                         "bratex" nil t)
  (add-hook 'yatex-mode-hook #'bratex-config))

(setq echo-keystrokes 0.5)

(defvar my-narrow-modeline '("#426EBB" "#FFFFFF")) ;; background, foreground
(defvar my-buffer-narrowed-last nil)
(make-local-variable 'my-buffer-narrowed-last)
(defvar my-selected-window-last nil)
(add-hook 'buffer-list-update-hook #'my-update-modeline-face)

(setq mode-line-modes
      (mapcar
       (lambda (entry)
         (if (equal entry "%n")
             '(:eval (progn
                       ;; org が widen を乱発するのでこちらをトリガーにする．
                       ;; 色の変更
                       (my-update-modeline-color)
                       ;; "Narrow" を "N" に短縮表示
                       ;; icons-in-terminal-octicon, "fold"
                       (if (and (buffer-narrowed-p)
                                (fboundp 'nerd-icons-octicon))
                           (concat " " (nerd-icons-octicon
                                        "nf-oct-fold" :v-adjust 0.0)) "")))
           entry))
       mode-line-modes))

(when (require 'mlscroll nil t)
  (custom-set-variables
   '(mlscroll-in-color "light coral") ;;  #FFA07A
   '(mlscroll-out-color "#FFFFEF")
   '(mlscroll-width-chars 10))
  (unless noninteractive
    (mlscroll-mode 1))

  (with-eval-after-load "moom"
    (add-hook 'moom-font-after-resize-hook #'my-reload-mlscroll)
    (add-hook 'moom-after-reset-hook #'my-reload-mlscroll)))

(with-eval-after-load "nerd-icons"
  ;; 変更がアリ時は赤アイコン，そうでない時に緑アイコンをモードラインに表示
  (make-face 'mode-line-vc-normal-face)
  (make-face 'mode-line-vc-modified-face)
  (set-face-attribute 'mode-line-vc-normal-face nil :foreground "#AFFFAF")
  (set-face-attribute 'mode-line-vc-modified-face nil :foreground "#EEAFAF"))

(with-eval-after-load "bindings" ;; "bindings"
  (let ((vc (assq 'vc-mode mode-line-format)))
    ;; (message "--- %s" vc)
    (when vc (setcdr vc '((:eval (my-mode-line-vc-mode-nerd-icons)))))))

(unless noninteractive
  (my-empty-booting-header-line)) ;; Update header of scratch buffer

(unless (display-graphic-p)
  ;; ターミナルの縦分割線をUTF-8できれいに描く
  (add-hook 'window-configuration-change-hook 'my-change-window-divider))

;; Show line number in the mode line.
(unless noninteractive
  (line-number-mode 1))

(when (autoload-if-found '(my-toggle-display-line-numbers-mode)
                         "display-line-numbers" nil t)
  (keymap-global-set "C-<f12>" 'my-toggle-display-line-numbers-mode)
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
(with-eval-after-load "nerd-icons"
  (setq mode-line-position-line-format
        `(,(nerd-icons-faicon "nf-fa-pencil_square_o") "%3l"))) ;; 

;; (with-eval-after-load "icons-in-terminal"
;;   (setq mode-line-position-line-format
;;         `(,(icons-in-terminal-material "edit") "%3l")))

;; Show clock in in the mode line
(setq display-time-format "%H:%M w%V") ;; %y%m%d. ;; "%H%M.%S"
(setq display-time-interval 1)
(setq display-time-default-load-average nil)
(unless noninteractive
  (display-time-mode 1))

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
(advice-add 'font-lock-mode :before #'my--font-lock-mode)

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
     ;;                 (c-mode "C" :major)
     ;;                 (c++mode "C++" :major)
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
     (nerd-icons-dired-mode nil "nerd-icons-dired")
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
     (selected-minor-mode nil "selected")
     (skewer-html-mode nil "skewer-html")
     (org-extra-emphasis-intraword-emphasis-mode nil "org-extra-emphasis")
     (gcmh-mode nil "gcmh")
     (super-save-mode nil "super-save")
     (rainbow-csv-mode nil "rainbow-csv")))

  ;; Override by icon
  (cond ((require 'nerd-icons nil t)
         (delight
          `((view-mode ,(concat " " (nerd-icons-mdicon "nf-md-file_lock"))
                       "view"))))
        ((require 'icons-in-terminal nil t)
         (delight
          `((view-mode ,(concat " " (icons-in-terminal-faicon "lock"))
                       "view"))))))

;; (eval-when-compile
;;   (message "Loading fringe-helper...")
;;   (require 'fringe-helper))

(when (autoload-if-found '(git-gutter-mode)
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

(keymap-global-set "M-c" #'calendar)
(with-eval-after-load "calendar"
  (when (require 'japanese-holidays nil t)
    (setq calendar-holidays
          (append japanese-holidays
                  holiday-local-holidays holiday-other-holidays))
    (setq calendar-mark-holidays-flag t)
    (setq mark-holidays-in-calendar t)
    ;; (setq japanese-holiday-weekend-marker
    ;;       '(holiday nil nil nil nil nil japanese-holiday-saturday))
    ;; (setq japanese-holiday-weekend '(0 6))
    (add-hook 'calendar-today-visible-hook #'japanese-holiday-mark-weekend)
    (add-hook 'calendar-today-invisible-hook #'japanese-holiday-mark-weekend))

  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)
  ;; hl-line を有効化
  ;; (add-hook 'calendar-today-visible-hook #'my-hl-line-enable)
  ;; (add-hook 'calendar-today-invisible-hook #'my-hl-line-enable)
)

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
          'font-lock-face 'calendar-iso-week-face)))

(with-eval-after-load "calendar"
  (add-hook 'calendar-today-visible-hook #'my-calendar-mark-selected)
  (add-hook 'calendar-move-hook #'my-calendar-mark-selected))

(when (autoload-if-found '(which-key-mode)
                         "which-key" nil t)
  (with-eval-after-load "which-key"
    (custom-set-variables
     '(which-key-idle-delay 1.0)))

  (unless noninteractive
    (which-key-mode 1)))

(when (autoload-if-found '(highlight-symbol-mode highlight-symbol-nav-mode)
                         "highlight-symbol" nil t)
  (dolist (hook '(emacs-lisp-mode-hook c-mode-common-hook prog-mode-hook))
    (add-hook hook #'highlight-symbol-mode))
  (with-eval-after-load "highlight-symbol"
    (custom-set-variables
     '(highlight-symbol-idle-delay 0.5))))

(autoload-if-found '(nerd-icons-dired-mode) "nerd-icons-dired" nil t)

(cond ((require 'nerd-icons-dired nil t)
       (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))
      ((require 'icons-in-terminal nil t)
       (add-hook 'dired-mode-hook #'icons-in-terminal-dired-mode))
      ((require 'all-the-icons nil t)
       (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)))

(when (autoload-if-found '(turn-on-eldoc-mode)
                         "eldoc" nil t)
  (dolist (hook '(emacs-lisp-mode-hook org-mode-hook c-mode-common-hook))
    (add-hook hook #'turn-on-eldoc-mode))
  (with-eval-after-load "eldoc"
    (advice-add 'elisp-eldoc-funcall :after #'my--elisp-eldoc)
    ;; (advice-add 'elisp-eldoc-var-docstring :after #'my--elisp-eldoc)

    ;; for ivy-mode
    (advice-add 'eldoc-message :around #'my--eldoc-message)

    (custom-set-variables
     '(eldoc-idle-delay 1.0))))

(when (autoload-if-found '(keypression-mode)
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

(when (autoload-if-found '(counsel-ibuffer counsel-M-x counsel-yank-pop)
                         "counsel" nil t)

  (keymap-global-set "M-x" 'counsel-M-x)
  (keymap-global-set "M-y" 'counsel-yank-pop)
  (keymap-global-set "C-," 'counsel-mark-ring)
  (keymap-global-set "C-x C-b" 'counsel-ibuffer)
  (keymap-global-set "C-M-g" 'ivy-resume)

  (unless (fboundp 'seq-sort-by) ;; emacs25
    (defalias 'seq-sort-by 'my-seq-sort-by))

  (with-eval-after-load "flyspell"
    (keymap-set flyspell-mode-map "C-," 'counsel-mark-ring))

  (with-eval-after-load "org"
    (keymap-set org-mode-map "C-," 'counsel-mark-ring))

  (with-eval-after-load "ivy"
    ;; 同一行に複数の mark がある場合，一つだけを候補として表示する．
    ;; mark を正確に辿れなくなるが，当該行に移動できることを重視．
    (advice-add 'counsel-mark-ring :override #'my--counsel-mark-ring)

    ;; counsel-mark-ring のリストをソートさせない
    (setf (alist-get 'counsel-mark-ring ivy-sort-functions-alist) nil)

    ;; M-o を ivy-dispatching-done-hydra に割り当てる．
    ;; (keymap-set ivy-minibuffer-map "M-o" 'ivy-dispatching-done-hydra)
    ;; ivy-dispatching-done を使う．
    ;; (keymap-set ivy-minibuffer-map "M-o" 'ivy-dispatching-done)
    (setq ivy-read-action-function #'ivy-hydra-read-action)

    (setq ivy-use-virtual-buffers nil)
    (when (setq enable-recursive-minibuffers t)
      (minibuffer-depth-indicate-mode 1))
    (keymap-set ivy-minibuffer-map "<escape>" 'minibuffer-keyboard-quit)
    (setq ivy-count-format "%d.%d ")
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
    ;; (define-key counsel-mode-map [remap find-file]  nil) ;; TODO
    ;;;; (keymap-substitute counsel-mode-map 'find-file nil) ;; FIXME
    ;; (substitute-key-definition 'find-file nil counsel-mode-map) ;; FIXME

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

(when (autoload-if-found '(imenu-list)
                         "imenu-list" nil t)
  (with-eval-after-load "imenu-list"
    (setq imenu-list-size 40)
    (setq imenu-list-position 'left)

    (add-hook 'imenu-list-major-mode-hook #'my--truncate-lines-activate)

    (when (require 'moom nil t)
      (add-hook 'imenu-list-update-hook #'my--imenu-list-update)
      (advice-add 'imenu-list-quit-window :after #'my--imenu-list-quit-window))))

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

(when (autoload-if-found '(command-log-mode global-command-log-mode)
                         "command-log-mode" nil t)
  (with-eval-after-load "command-log-mode"
    (require 'keypression)
    (require 'moom)
    ;; (setq command-log-mode-window-font-size 0)
    (setq command-log-mode-key-binding-open-log nil)
    (setq command-log-mode-window-size 60)))

(let* ((elp (expand-file-name
             (concat "~/.emacs.d/" (format "%s" emacs-version) "/el-get/")))
       (ets (concat elp "emacs-tree-sitter/"))
       (tsl (concat elp "tree-sitter-langs/")))
  ;; (add-to-list 'load-path (concat ets "langs"))
  (add-to-list 'load-path (concat ets "core"))
  (add-to-list 'load-path (concat ets "lisp"))
  (add-to-list 'load-path tsl))

(dolist (hook '(js-mode-hook))
  (add-hook hook #'my-enable-tree-sitter))

(when (autoload-if-found '(swiper-thing-at-point swiper-all-thing-at-point)
                         "swiper" nil t)
  (keymap-global-set "M-s M-s" 'swiper-thing-at-point)
  (keymap-global-set "M-s M-a" 'swiper-all-thing-at-point)
  (with-eval-after-load "swiper"
    (advice-add 'swiper-thing-at-point :override #'my--swiper-thing-at-point)))

(when (eq system-type 'darwin)
  (with-eval-after-load "ivy"
    (cond ((and (require 'nerd-icons nil t) ;; safeguard
                (require 'ivy-rich nil t)
                (require 'nerd-icons-ivy-rich nil t))
           (nerd-icons-ivy-rich-mode 1)
           (ivy-rich-mode 1))
          ((and (require 'icons-in-terminal nil t) ;; safeguard
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
           (all-the-icons-ivy-setup))))

  (with-eval-after-load "nerd-icons-ivy-rich"
    (my-update-nerd-icons-ivy-rich-display-transformers-list
     'counsel-M-x
     '(:columns
       ((nerd-icons-ivy-rich-function-icon)
        (counsel-M-x-transformer (:width 0.5))
        (ivy-rich-counsel-function-docstring (:face nerd-icons-ivy-rich-doc-face)))))

    (my-update-nerd-icons-ivy-rich-display-transformers-list
     'counsel-recentf
     '(:columns
       ((nerd-icons-ivy-rich-file-icon)
        (nerd-icons-ivy-rich-file-name) ;; (:width 0.8)
        ;; (nerd-icons-ivy-rich-file-id (:width 15 :face nerd-icons-ivy-rich-file-owner-face :align right))
        ;; (nerd-icons-ivy-rich-file-modes (:width 12))
        ;; (nerd-icons-ivy-rich-file-size (:width 7 :face nerd-icons-ivy-rich-size-face))
        ;; (ivy-rich-file-last-modified-time (:face nerd-icons-ivy-rich-time-face))
        )
       :delimiter " ")))

  (with-eval-after-load "counsel"
    ;; just in case, for surely applying the config.
    (when (and (require 'nerd-icons nil t)
               (require 'ivy-rich nil t))
      (let ((message-log-max nil))
        (nerd-icons-ivy-rich-reload)))))

(when (autoload-if-found '(dimmer-mode
                           dimmer-process-all dimmer-off dimmer-on
                           my-toggle-dimmer dimmer-permanent-off
                           my--dimmer-org-agenda--quit)
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
      (add-function :before after-focus-change-function #'my-dimmer-update))

    ;; for org-agenda
    (add-hook 'org-agenda-mode-hook #'dimmer-permanent-off)
    (advice-add 'org-agenda--quit :after #'my--dimmer-org-agenda--quit)

    ;; for swiper/helm-swoop
    (add-hook 'minibuffer-setup-hook #'dimmer-off)
    (add-hook 'minibuffer-exit-hook #'dimmer-on))

  (unless noninteractive
    (unless (version< "28.0" emacs-version)
      ;; FIXME
      (add-hook 'window-configuration-change-hook #'my-dimmer-activate))))

;; この場合は，interactive モードで init-eval.el にある記述をロードするはだめ．
;; (eval-when-compile
;;   (message "Loading transient...")
;;   (require 'transient))

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

(when (autoload-if-found '(rainbow-csv-mode) "rainbow-csv" nil t)
  (add-hook 'csv-mode-hook 'rainbow-csv-mode))

(when (autoload-if-found '(rencetf-mode
                           my-recentf-save-list-silence
                           my-recentf-cleanup-silence
                           recentf-open-files recentf-add-file)
                         "recentf" nil t)
  (with-eval-after-load "recentf"
    (custom-set-variables
     '(recentf-max-saved-items 2000)
     '(recentf-save-file (expand-file-name "~/.emacs.d/_recentf"))
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
                    #'my-recentf-cleanup-silence))

    (unless noninteractive
      (let ((message-log-max nil))
        (if (equal (system-name) "water.local")
            (recentf-mode 1)
          (message "--- recentf is not activated in %s" system-name)))))

  (with-eval-after-load "counsel"
    (advice-add 'counsel-recentf :override #'my--counsel-recentf)
    (ivy-add-actions
     'counsel-recentf
     '(("g" my-counsel-ag-in-dir "switch to ag")
       ("r" my-counsel-fzf-in-dir "switch to fzf (in dir.)")
       ("z" my-counsel-fzf-in-default-dir "switch to fzf (default)")))))

;; (add-hook 'after-init-hook #'recentf-mode))

(with-eval-after-load "ah"
  (advice-add 'my-cg-bookmark :around #'my--suppress-message)
  (add-hook 'ah-before-c-g-hook #'my-cg-bookmark))

(with-eval-after-load "recentf"
  (run-with-idle-timer 60 t 'my-backup-recentf))

(when (autoload-if-found '(backup-each-save my-auto-backup)
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
                #'my--backup-each-save-compute-location)))

;; late-init.el
(add-hook 'dired-mode-hook #'my-dired-activate)
(with-eval-after-load "dired"
  (setq dired-listing-switches "-lha"))

(with-eval-after-load "dired"
  (require 'dired-recent nil t))

(when (autoload-if-found '(dired-recent-open dired-recent-mode)
                         "dired-recent" nil t)
  (keymap-global-set "C-x C-d" 'dired-recent-open)
  (with-eval-after-load "dired-recent"
    ;; (require 'helm-config nil t)
    (dired-recent-mode 1)))

(with-eval-after-load "dired"
  (setq dired-use-ls-dired nil)
  (when (require 'osx-trash nil t)
    (setq delete-by-moving-to-trash t)
    (osx-trash-setup)))

(when (autoload-if-found '(undo-fu-only-undo undo-fu-only-redo)
                         "undo-fu" nil t)
  (keymap-global-set "C-/" 'undo-fu-only-undo)
  (keymap-global-set "C-M-/" 'undo-fu-only-redo))

(when (autoload-if-found '(super-save-mode) "super-save" nil t)
  (add-hook 'find-file-hook #'my-super-save-activate)
  (with-eval-after-load "super-save"
    (setq super-save-auto-save-when-idle t)
    (setq super-save-idle-duration 1.6)
    (setq super-save-exclude '("Org Src"))
    (add-to-list 'super-save-predicates
                 '(lambda () (my-super-save-predicates-p)) t)
    (advice-add 'super-save-command :override #'my--super-save-buffers-command)))

(when (autoload-if-found '(neotree neotree-toggle)
                         "neotree" nil t)
  (keymap-global-set "C-c n" #'neotree-toggle)
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
    (advice-add 'neotree-show :before #'my--neotree-show)
    (advice-add 'neotree-hide :before #'my--neotree-hide)))

(when (autoload-if-found '(helpful-key
                           helpful-function helpful-variable helpful-at-point
                           helpful-symbol)
                         "helpful" nil t)
  (keymap-global-set "<f1> k" 'helpful-key)
  (keymap-global-set "<f1> f" 'helpful-function)
  (keymap-global-set "<f1> v" 'helpful-variable)
  (keymap-global-set "<f1> m" 'helpful-macro)
  (keymap-global-set "<f1> @" 'helpful-at-point)
  (with-eval-after-load "helpful"
    (advice-add 'helpful-at-point :before #'my--helpful-at-point)))

(when (autoload-if-found '(facecheck-at-point facecheck-mode)
                         "facecheck" nil t)
  (with-eval-after-load "facecheck"
    (facecheck-mode 1)))

(when (autoload-if-found '(keyfreq-mode keyfreq-autosave-mode my--keyfreq-show)
                         "keyfreq" nil t) ;; will require 'cl and 'gv(10-20[ms])
  (with-eval-after-load "keyfreq"
    (advice-add 'keyfreq-show :after #'my--keyfreq-show)
    ;; (keymap-set keyfreq-mode-map "q"
    ;;   (lambda () (interactive)
    ;;       (when (string= (buffer-name) keyfreq-buffer)
    ;;         (kill-buffer-and-window))))
    (setq keyfreq-file
          (expand-file-name (concat (getenv "SYNCROOT") "/emacs.d/.keyfreq")))
    (keyfreq-autosave-mode 1))
  (unless noninteractive
    (keyfreq-mode 1)))

(when (autoload-if-found '(disk-usage)
                         "disk-usage" nil t)
  (with-eval-after-load "disk-usage"
    (when (eq system-type 'darwin)
      (custom-set-variables
       '(disk-usage-du-command "du")))))

(when (autoload-if-found '(counsel-ag)
                         "counsel" nil t)
  (keymap-global-set "C-M-f" 'counsel-ag)
  (with-eval-after-load "counsel"
    (require 'thingatpt nil t)
    (advice-add 'counsel-ag :around #'my--counsel-ag)

    ;; 2文字でも検索が発動するようにする
    (add-to-list 'ivy-more-chars-alist '(counsel-ag . 2))

    (ivy-add-actions
     'counsel-ag
     '(("r" my-counsel-ag-in-dir "search in directory")))))

(when (autoload-if-found '(counsel-fzf)
                         "counsel" nil t)
  (keymap-global-set "C-M-z" 'counsel-fzf)
  (with-eval-after-load "counsel"
    (advice-add 'counsel-fzf :around #'my--counsel-fzf)
    (ivy-add-actions
     'counsel-fzf
     '(("r" my-counsel-fzf-in-dir "search in directory")))))

(autoload-if-found '(gist-mode) "gist" nil t)

(when (autoload-if-found '(flycheck-mode)
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

(when (autoload-if-found '(quickrun)
                         "quickrun" nil t)
  (with-eval-after-load "go-mode"
    (keymap-set go-mode-map "<f5>" 'quickrun))
  (with-eval-after-load "c++-mode"
    (keymap-set c++-mode-map "<f5>" 'quickrun))
  (with-eval-after-load "python-mode"
    (keymap-set python-mode-map "<f5>" 'quickrun))
  (with-eval-after-load "perl-mode"
    (keymap-set perl-mode-map "<f5>" 'quickrun))
  (with-eval-after-load "gnuplot-mode"
    (keymap-set gnuplot-mode-map "<f5>" 'quickrun)))

(when (autoload-if-found '(ggtags-mode)
                         "ggtags" nil t)
  (dolist (hook (list 'c-mode-common-hook 'python-mode-hook))
    (add-hook hook (lambda () (ggtags-mode 1))))

  (with-eval-after-load "ggtags"
    (unless (executable-find "gtags")
      (message "--- global is NOT installed in this system."))

    ;; (setq ggtags-completing-read-function t) ;; nil for helm
    (keymap-set ggtags-mode-map "M-]" nil)))

(when (autoload-if-found '(counsel-gtags-mode)
                         "counsel-gtags" nil t)
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook 'counsel-gtags-mode))
  (with-eval-after-load "counsel-gtags"
    (custom-set-variables
     '(counsel-gtags-update-interval-second 10))))

(when (autoload-if-found '(0xc-convert
                           0xc-convert-point
                           my-decimal-to-hex my-hex-to-decimal)
       "0xc" nil t)
  (keymap-global-set "C-c f h" '0xc-convert))

(with-eval-after-load "hexl"
  (add-hook 'hexl-mode-hook 'view-mode)
  (custom-set-variables
   '(hexl-bits 8)))

(autoload-if-found '(uuid-string my-uuid-string) "uuid" nil t)

(autoload-if-found '(package-lint-current-buffer) "package-lint" nil t)

(when (autoload-if-found '(projectile-mode)
                         "projectile" nil t)
  (with-eval-after-load "neotree"
    ;; (advice-add 'neotree-dir :override #'my--neotree-dir) ;; FIXME
    ;; M-x helm-projectile-switch-project (C-c p p)
    (setq projectile-switch-project-action 'neotree-projectile-action))

  (with-eval-after-load "projectile"
    (advice-add 'projectile-visit-project-tags-table :override
                #'my--projectile-visit-project-tags-table)

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
      (keymap-set projectile-mode-map "C-c p" 'projectile-command-map)
      (keymap-set projectile-mode-map "C-M-f" 'my-counsel-projectile-ag)
      (counsel-projectile-mode 1)))

  (unless noninteractive
    (add-hook 'find-file-hook #'my-projectile-activate)))

(autoload-if-found '(relint-current-buffer) "relint" nil t)

(when (autoload-if-found '(magit-status my--magit-mode-bury-buffer)
                         "magit" nil t)
  (keymap-global-set "C-c m" 'magit-status)
  (with-eval-after-load "magit"
    (when (fboundp 'dimmer-off)
      (add-hook 'magit-status-mode-hook 'dimmer-off))
    (when (fboundp 'magit-mode-bury-buffer)
      (advice-add 'magit-mode-bury-buffer :before #'my--magit-mode-bury-buffer))
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

(when (autoload-if-found '(corfu-mode) "corfu" nil t)
  (add-hook 'emacs-lisp-mode-hook #'corfu-mode)
  (add-hook 'org-mode-hook #'corfu-mode)
  (add-hook 'sh-mode #'corfu-mode)
  (add-hook 'minibuffer-setup-hook #'my-advice-minibuffer-complete)

  (with-eval-after-load "corfu"
    (custom-set-variables
     ;; '(corfu-auto-prefix 2)
     '(corfu-min-width 20)
     '(corfu-count 5)
     '(corfu-auto-delay 0.5)
     '(corfu-auto t))

    (keymap-set corfu-mode-map "C-SPC" #'corfu-insert-separator)

    (advice-add 'corfu-insert-separator :override #'my--corfu-insert-separator)

    (when (require 'corfu-prescient nil t)
      (corfu-prescient-mode 1))

    (when (require 'kind-icon nil t)
      (setq kind-icon-default-face 'corfu-default)
      (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))))

(when (autoload-if-found '(org-block-capf-add-to-completion-at-point-functions)
                         "org-block-capf" nil t)
  (add-hook 'org-mode-hook
            #'org-block-capf-add-to-completion-at-point-functions -1)

  (with-eval-after-load "org-block-capf"
    (setq org-block-capf-edit-style 'inline)
    (setq org-block-capf-auto-indent nil)))

(when (autoload-if-found '(cape-elisp-block cape-file cape-dict)
                         "cape" nil t)
  (add-hook 'org-mode-hook #'my-load-cape-modules-for-org -2))

(unless (display-graphic-p)
  (when (autoload-if-found '(corfu-terminal-mode) "corfu-terminal" nil t)
    (defvar corfu-terminal-mode nil) ;; To suppress showing a warning
    (add-hook 'emacs-lisp-mode-hook #'corfu-terminal-mode)
    (add-hook 'org-mode-hook #'corfu-terminal-mode)))

(autoload-if-found '(vterm) "vterm"  nil t)

;; `org-agenda-prepare-buffers' は重い．agenda 実行時の最初に走るが，
;; 事前に走らせておくほうがいい．以下の例では，
;; 起動後，何もしなければ10秒後に org, org-agenda が有効になる
;; 起動後，org buffer を訪問して，10秒待つと，org-agenda が有効になる
;; 起動後，直接 org-agenda を叩く場合は重いまま（タイマー走ってもスルー）
;; これを (with-eval-after-load "org") の中に置くと振る舞いが変(2回実行)になる
(defvar my-org-agenda-pb-timer
  (unless noninteractive
    (run-with-idle-timer (+ 9 my-default-loading-delay)
                         nil #'my-org-agenda-prepare-buffers)))

(keymap-global-set "C-c f 3" #'my-org-agenda-to-appt)
(run-at-time "20 sec" nil #'my-org-agenda-to-appt)
;; (with-eval-after-load "org") 内で設定すると(何故か)複数回呼ばれてしまう．
(run-with-idle-timer 180 t #'my-org-agenda-to-appt)

(add-hook 'org-mode-hook 'prettify-symbols-mode)
(with-eval-after-load "nerd-icons"
  (setq-default prettify-symbols-alist
                '(("CLOSED:" . "󱫫") ;; nf-md-timer_stop_outline
                  ("SCHEDULED:" . "󰅕") ;; nf-md-clock_start
                  ("DEADLINE:" . "󰅑") ;; nf-md-clock_end
                  (":PROPERTIES:" . "»") ;;  » ;; nf-fa-angle_double_right  
                  (":LOGBOOK:" . "›") ;;  › ;; nf-fa-angle_right  
                  (":END:" . "›") ;;  › 
                  ("#+begin_src" . "▨") ;;  ▨
                  ("#+end_src" . "▨") ;; ▨
                  ("#+RESULTS:" . "") ;; nf-fa-share_square 
                  ("[ ]" .  "󰄱") ;; ☐  ;; nf-md-checkbox_blank_outline 󰄱
                  ("[X]" . "󰄵" ) ;; ☑  ;; nf-md-checkbox_marked_outline 󰄵
                  ("[-]" . "󰡖 " ))))
;; 󰡖 ;; nf-md-checkbox_intermediate
;; 󰄗 ;; nf-md-checkbox_blank_badge_outline

;; (with-eval-after-load "icons-in-terminal"
;;   (setq-default prettify-symbols-alist '((":PROPERTIES:" . "") ;;  »
;;       (":LOGBOOK:" . "") ;; ›
;;       (":END:" . "") ;; ›
;;       ("#+begin_src" . "▨") ;; 
;;       ("#+end_src" . "▨")
;;       ("#+RESULTS:" . "")
;;       ("[ ]" . "") ;; ☐ 
;;       ("[X]" . "" ) ;; ☑ 
;;       ("[-]" . "" )))) ;; ☒ 

(when (autoload-if-found '(org-recent-headings org-recent-headings-mode)
                         "org-recent-headings" nil t)
  ;; (keymap-global-set "C-c f r" 'org-recent-headings-helm)
  (keymap-global-set "C-M-h" 'org-recent-headings)
  (with-eval-after-load "org-recent-headings"
    (require 'ivy)
    ;; デフォルトだと `ivy-string<' が使われてしまい，使用履歴が反映されない．
    (setf (alist-get 'org-recent-headings ivy-sort-functions-alist) nil)
    (advice-add 'org-recent-headings :before
                #'my--org-recent-headings-activate)
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

(set-face-foreground 'vertical-border (face-foreground 'default))
(set-face-background 'vertical-border (face-background 'default))
;;(set-face-background 'fringe (face-background 'default)) ;; 10-20[ms]

(when (memq window-system '(ns x))
  ;; モードラインにアイコンを出す
  (make-face 'mode-line-ime-on-face)
  (set-face-attribute 'mode-line-ime-on-face
                      nil :foreground (plist-get my-cur-color-ime :on))
  (when (fboundp 'mac-set-input-method-parameter)
    (mac-set-input-method-parameter
     "com.google.inputmethod.Japanese.base" 'title
     (concat
      (cond ((require 'nerd-icons nil t)
             ;; (nerd-icons-octicon "nf-oct-typography"
             ;;					:face 'mode-line-ime-on-face)
             (nerd-icons-mdicon "nf-md-ideogram_cjk_variant" ;; IME
                                 :face 'mode-line-ime-on-face))
            ((require 'icons-in-terminal nil t)
             (icons-in-terminal-octicon "keyboard"
                                        :v-adjust 0.0
                                        :face 'mode-line-ime-on-face))
            (t ""))
      " "))) ;; FIXME (the color is NOT changed, patch is wrong?)

  (declare-function my-ime-on "init" nil)
  (declare-function my-ime-off "init" nil)
  (declare-function my-ime-active-p "init" nil)

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
    (add-hook 'deactivate-mark-hook #'mac-ime-activate-sticky)))

(keymap-global-set "M-`" 'other-frame)
(with-eval-after-load "frame"
  (advice-add 'make-frame :after #'my--make-frame))

(when (autoload-if-found '(moom-font-increase
                           moom-font-decrease
                           moom-font-size-reset moom-font-resize)
                         "moom-font" nil t)
  (add-hook 'moom-font-after-resize-hook #'moom-move-frame-to-edge-top)
  (add-hook 'moom-font-after-resize-hook #'moom-fill-height)
  (with-eval-after-load "moom-font"
    (setopt moom-scaling-gradient (/ (float 50) 30))
    (setopt moom-font-table
            '((50 30) (49 29) (48 29) (47 28) (46 28) (45 27) (44 26) (43 26)
              (42 25) (41 25) (40 24) (39 23) (38 23) (37 22) (36 22) (35 21)
              (34 20) (33 20) (32 19) (31 19) (30 18) (29 17) (28 17) (27 16)
              (26 16) (25 15) (24 14) (23 14) (22 13) (21 13) (20 12) (19 11)
              (18 11) (17 10) (16 10) (15 9) (14 8) (13 8) (12 7) (11 7) (10 6)
              (9 5) (8 5) (7 4) (6 4) (5 3)))))

(keymap-global-set "<f12>" 'my-toggle-mode-line)
(with-eval-after-load "moom"
  (advice-add 'moom-toggle-frame-maximized
              :after #'my--moom-toggle-frame-maximized))

;; (make-variable-buffer-local 'my-mode-line-format)
(defvar-local my-mode-line-format nil)
(set-default 'my-mode-line-format mode-line-format)
(defvar my-toggle-modeline-global t)
(unless (display-graphic-p)
  (setq my-toggle-modeline-global t)) ;; Enforce modeline in Terminal

(add-hook 'find-file-hook #'my-modeline-activate 1)

;; init
(unless noninteractive
  (my-mode-line-off))

(unless noninteractive
  (when (autoload-if-found '(winner-undo)
                           "winner" nil t)
    (keymap-global-set "C-x g" 'winner-undo)
    (with-eval-after-load "winner"
      (advice-add 'delete-window :after #'my--winner:delete-window)
      (keymap-set winner-mode-map "C-(" 'winner-undo)
      (keymap-set winner-mode-map "C-)" 'winner-redo)
      (winner-mode 1))))

(when (autoload-if-found '(shackle-mode)
                         "shackle" nil t)

  (unless noninteractive
    ;; (add-hook 'window-configuration-change-hook #'my-shackle-activate)
    (add-hook 'find-file-hook #'my-shackle-activate))

  (with-eval-after-load "shackle"
    (setq shackle-default-ratio 0.33)
    (setq shackle-rules
          '(("*osx-dictionary*" :align above :popup t)
            ("*wclock*" :align above :popup t :select t)
            ;; ("*Help*" :align t :select 'above :popup t :size 0.3)
            ("*Checkdoc Status*" :align above :popup t :noselect t)))))

(with-eval-after-load "checkdoc"
  (advice-add 'checkdoc :before #'my--checkdoc))

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
    ;; (declare-function my--doom-modeline-buffer-file-state-icon "init" nil)
    (advice-add 'doom-modeline-buffer-file-state-icon :override
                #'my--doom-modeline-buffer-file-state-icon)
    (size-indication-mode 1)
    (doom-modeline-mode 1)))

(with-eval-after-load "emacs-lisp-mode"
  (set-face-foreground 'font-lock-regexp-grouping-backslash "#66CC99")
  (set-face-foreground 'font-lock-regexp-grouping-construct "#9966CC"))

(unless noninteractive
  (add-hook 'find-file-hook #'my-generic-x-activate))

(when (autoload-if-found '(hl-line-mode my-hl-line-enable)
                         "hl-line" nil t)
  ;; Tricky!
  (add-hook 'ah-after-move-cursor-hook #'my-hl-line-activate)

  (defvar my-hl-permanent-disabled '(dired-mode vterm-mode)
    "A list of major modes to disable `hl-line'.")

  (defvar my-ime-off-hline-hook nil)
  (defvar my-ime-on-hline-hook nil)

  (with-eval-after-load "hl-line"
    ;; 別ウィンドウの同じバッファでもハイライトする
    ;; (setq hl-line-sticky-flag t)
    ;; (unless (version< emacs-version "28.1")
    ;;   (setq hl-line-sticky-flag nil))

    (defvar my-hl-active-period 120
      "Disable `hl-line' after this period")

    (if (my-ime-active-p) (my-ime-on-hline) (my-ime-off-hline))

    (run-with-idle-timer my-hl-active-period t #'my-hl-line-disable)

    (if (boundp 'after-focus-change-function)
        (add-function :after after-focus-change-function #'my-hl-line-update)
      (add-hook 'focus-in-hook #'my-hl-line-enable)
      (add-hook 'focus-out-hook #'my-hl-line-disable))

    ;; (add-hook 'minibuffer-setup-hook #'my-hl-line-disable)
    ;; (add-hook 'minibuffer-exit-hook #'my-hl-line-enable)
    (add-hook 'input-method-activate-hook #'my-ime-on-hline)
    (add-hook 'input-method-deactivate-hook #'my-ime-off-hline)))

(setq blink-cursor-blinks 0)
(setq blink-cursor-interval 0.2)
(setq blink-cursor-delay 30)
(unless noninteractive
  (blink-cursor-mode 1))

(my-setup-font) ;; this requires utility.el

;; set-default で global 指定すると，ミニバッファの message で制御不能になる
;; propertize で拡大できるが，global の値以下に縮小できなくなる．
;; (set-default 'line-spacing 2)
(add-hook 'buffer-list-update-hook #'my-linespacing)
(add-hook 'org-src-mode-hook #'my-linespacing)
(add-hook 'debugger-mode-hook #'my-linespacing)

(with-eval-after-load "diff-mode"
  (set-face-attribute 'diff-added nil
                      :background 'unspecified :foreground "lime green"
                      :weight 'normal)

  (set-face-attribute 'diff-removed nil
                      :background 'unspecified :foreground "firebrick1"
                      :weight 'normal)

  (set-face-attribute 'diff-file-header nil
                      :background 'unspecified :weight 'extra-bold)

  (set-face-attribute 'diff-hunk-header nil
                      :foreground "chocolate4"
                      :background "white" :weight 'extra-bold
                      :inherit nil))

(when (autoload-if-found '(global-hl-todo-mode)
                         "hl-todo" nil t)
  (add-hook 'my-light-theme-hook #'my-hl-todo-light-theme)
  (add-hook 'my-dark-theme-hook #'my-hl-todo-dark-theme)
  (add-hook 'find-file-hook #'my-hl-todo-activate))

;; (declare-function my-font-config "init" nil)
;; This may override or reset font setting
(unless noninteractive
  (my-theme)
  (run-at-time "00:00" 86400 #'my-update-theme-timers)
  ;; (run-at-time "05:00" 86400 'my-theme)
  ;; (run-at-time "23:00" 86400 'my-theme)
  ) ;; FIXME: it makes frame blink

(when (autoload-if-found '(rainbow-mode)
                         "rainbow-mode" nil t)
  (dolist (hook '(emmet-mode-hook emacs-lisp-mode-hook org-mode-hook))
    (add-hook hook #'rainbow-mode)))

(when (autoload-if-found '(edit-color-stamp)
                         "edit-color-stamp" nil t)
  (keymap-global-set "C-c f c p" 'edit-color-stamp)
  (with-eval-after-load "edit-color-stamp"
    (unless (executable-find "qt_color_picker")
      (message "--- qt_color_picker is NOT installed."))))

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

;;; 選択対象を "" にする (requires all-the-icons.el)
(defface my-ivy-arrow-visible
  '((((class color) (background light)) :foreground "orange")
    (((class color) (background dark)) :foreground "#EE6363"))
  "Face used by Ivy for highlighting the arrow.")
(defface my-ivy-arrow-invisible
  '((((class color) (background light)) :foreground "#FFFFFF")
    (((class color) (background dark)) :foreground "#31343F"))
  "Face used by Ivy for highlighting the invisible arrow.")

(with-eval-after-load "counsel"

  ;; Remove the default setting
  (delete '(t . ivy-format-function-default) ivy-format-functions-alist)

  (if window-system
      (cond ((require 'icons-in-terminal nil t)
             (add-to-list
              'ivy-format-functions-alist
              '(t . my-ivy-format-function-arrow-iit) t))
            ((require 'nerd-icons nil t)
             (add-to-list
              'ivy-format-functions-alist
              '(t . my-ivy-format-function-arrow-ni) t))
            ((require 'all-the-icons nil t)
             (add-to-list
              'ivy-format-functions-alist
              '(t . my-ivy-format-function-arrow-ati) t))
            (t
             (add-to-list
              'ivy-format-functions-alist
              '(t . ivy-format-function-arrow-line) t)))
    (add-to-list
     'ivy-format-functions-alist
     '(t . ivy-format-function-arrow-line) t)))

(when (autoload-if-found '(volatile-highlights-mode my-vhl-change-color)
                         "volatile-highlights" nil t)
  (keymap-global-set "M-v" 'my-yank)
  (keymap-global-set "C-y" 'my-yank)
  (when window-system
    (advice-add 'my-yank :before #'my--vhl-activate)
    (advice-add 'my-org-yank :before #'my--vhl-activate))

  (with-eval-after-load "volatile-highlights"
    (set-face-attribute
     'vhl/default-face nil :foreground "#FF3333" :background "#FFCDCD")
    (volatile-highlights-mode t))

  (with-eval-after-load "vterm"
    (keymap-set vterm-mode-map "C-y" 'vterm-yank))

  (with-eval-after-load "org"
    (keymap-set org-mode-map "C-y" 'my-org-yank)))

(unless noninteractive
  (when (and window-system
             my-skip-check-autoload-file)
    (my-find-missing-packages 5)))

(unless noninteractive
  (when window-system
    (my-delete-old-backup 3)))

(when (autoload-if-found '(my-google-this google-this google-this-word)
                         "google-this" nil t)
  (keymap-global-set "C-c f g" 'my-google-this))

(when (autoload-if-found '(osx-lib-say osx-lib-say-region)
                         "osx-lib" nil t)
  (with-eval-after-load "osx-lib"
    (custom-set-variables
     '(osx-lib-say-ratio 100)
     '(osx-lib-say-voice "Samantha"))))

(when (autoload-if-found '(my-cmd-to-open-iterm2)
                         "utility" nil t)
  (keymap-global-set "C-M-i" #'my-cmd-to-open-iterm2)
  (with-eval-after-load "flyspell"
    (keymap-set flyspell-mode-map "C-M-i" #'my-cmd-to-open-iterm2))
  (with-eval-after-load "org"
    (keymap-set org-mode-map "C-M-i" #'my-cmd-to-open-iterm2)))

(keymap-global-set "C-c f t" 'my-open-current-directory-in-terminal)

(when (autoload-if-found '(gif-screencast)
                         "gif-screencast" nil t)
  (with-eval-after-load "gif-screencast"
    (setq gif-screencast-want-optimized nil)
    (setq gif-screencast-args '("-x"))
    (setq gif-screencast-capture-format "ppm")

    ;; Start... M-x gif-screencast
    (keymap-set gif-screencast-mode-map "<f5>" 'gif-screencast-stop)
    (keymap-set gif-screencast-mode-map "S-<f5>" 'gif-screencast-toggle-pause)

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
    (advice-add 'gif-screencast :after #'my--gif-screencast)
    (advice-add 'gif-screencast-stop :after #'my--gif-screencast-stop)
    (advice-add 'gif-screencast-stop :before #'my--gif-screencast-opendir)
    (advice-add 'gif-screencast-toggle-pause
                :before #'my--gif-screencast-toggle-pause)))

(keymap-global-set "C-M--" 'my-cycle-bullet-at-heading)
;; (keymap-global-set "<f12>" 'my-open-file-ring)
;;  (keymap-global-set "C-c t" 'my-date)
(keymap-global-set "C-c f 4" 'my-window-resizer)

(when (autoload-if-found '(manage-minor-mode)
                         "manage-minor-mode" nil t)
  (with-eval-after-load "manage-minor-mode"
    (keymap-set manage-minor-mode-map "q"
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
  (advice-add 'ivy--insert-prompt :before #'my--fzf:ivy--insert-prompt)
  (add-hook 'minibuffer-setup-hook #'my-nocand-then-fzf-reset)
  (add-hook 'minibuffer-exit-hook #'my-nocand-then-fzf-reset))

(when (autoload-if-found '(elfeed elfeed-update elfeed-web-start)
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

(when (display-graphic-p)
  (keymap-global-set "C-x C-c" #'my-kill-all-file-buffers))

(keymap-global-set "C-c C-x" #'my-kill-emacs-when-scratch-buffer)

(when nil
  (unless noninteractive
    (let ((inhibit-message t))
      (message "Loading late-init.el...done (%4d [ms])"
         (* 1000
      (float-time (time-subtract
             (current-time)
             my-late-init-start)))))))
(provide 'late-init)
