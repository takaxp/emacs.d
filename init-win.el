;; -*- coding:utf-8-unix; lexical-binding: t; -*-
;; This initialize file should be loaded first in w32 environment.
;; Each setting could be overwritten in .emacs, see AppData/Roming/.emacs.
;; And the .emacs will contain, for insetance,
;; 1. Set (load "u:/.emacs.d/init-win.el") in the first line.
;; 2. Overwrite "PATH" and #'counsel-win-app-list in the .emacs if needed.
;; Note: all local and private settings should be configured in the .emacs.
;; runemacs.exe is extracted from a distributed zip package from
;;             https://ftp.jaist.ac.jp/pub/GNU/emacs/windows/
;;                                                    Last update: 2022-07-12
(when nil
  ;; advice of load function
  (defadvice load (around require-benchmark activate)
    (let* ((before (current-time))
           (result ad-do-it)
           (after  (current-time))
           (time (+ (* (- (nth 1 after) (nth 1 before)) 1000.0)
                    (/ (- (nth 2 after) (nth 2 before)) 1000.0)))
           (arg (ad-get-arg 0)))
      (message "--- %04d [ms]: (loading) %s" time arg)))
  ;; advice of require function
  (defadvice require (around require-benchmark activate)
    "http://memo.sugyan.com/entry/20120105/1325756767"
    (let* ((before (current-time))
           (result ad-do-it)
           (after  (current-time))
           (time (+ (* (- (nth 1 after) (nth 1 before)) 1000.0)
                    (/ (- (nth 2 after) (nth 2 before)) 1000.0)))
           (arg (ad-get-arg 0)))
      (unless (or (memq arg '(cl-lib macroexp))
                  (> 0.1 time))
        (message "--- %04d [ms]: %s" time arg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar do-profile nil) ;; M-x profiler-report
(when do-profile (profiler-start 'cpu))

(setq debug-on-error nil)
(setq inhibit-default-init t)
(setq initial-scratch-message nil
      initial-buffer-choice t ;; Starting from *scratch* buffer
      initial-major-mode 'fundamental-mode)
(setq byte-compile-warnings '(obsolete))
(setq system-time-locale "C") ;; format-time-string %a, not 日 but Sun
(setq make-backup-files nil)
(setq default-directory "~/")
(setq truncate-line nil
      truncate-partial-width-windows nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)

;; Language, will override default-input-method
(set-language-environment "Japanese")
(set-clipboard-coding-system 'utf-16le) ;; enable copy-and-paste correctly

;; AppData\Roaming\.emacs.d\lisp 以下に各追加パッケージを配置すること
;; smartparens requires dash.el.
(let ((default-directory (expand-file-name "~/.emacs.d/lisp")))
  (add-to-list 'load-path default-directory)
  ;; (normal-top-level-add-subdirs-to-load-path) ;; Slower than add-to-load
  (normal-top-level-add-to-load-path
   '("dash.el" "compat.el" "smex"
     "moom" "swiper" "selected" "expand-region.el" "counsel-osx-app"
     "smartparens" "emacs-htmlize" "emacs-undo-fu" "transient" "bsv"
     "japanese-holidays" "highlight-symbol.el" "tr-emacs-ime-module"
     "emacs-google-this" "volatile-highlights.el" "hl-todo" "bm"
     "replace-from-region" "session")))

;; Setting Home directory if needed.
;; (setenv "HOME" "C:/Users/******/AppData/Roaming")
;; (setenv "HOME" "c:/cygwin64/home/********")
;; Proxy
;; (setq url-proxy-services
;;       '(("http" . "http://hoge.org:8888")
;;         ("https" . "https://hoge.org:8888")))
;; For MSYS
;; (when (eq system-type 'windows-nt)
;;   (setenv "HOME" "C:\\cygwin64\\home\\takaxp")
;;   (setenv "PATH" (concat (getenv "PATH")
;;                          ";C:\\cygwin64\\usr\\local\\bin"
;;                          ";C:\\cygwin64\\opt\\bin"
;;                          ;; ";C:\\msys64\\mingw64\\bin"
;;                          ";C:\\cygwin64\\usr\\bin"))
;;   ;;  (setenv "PATH" (concat (getenv "PATH")
;;   ;;  ";C:\\msys64\\usr\\local\\bin" ";C:\\msys64\\opt\\bin"
;;   ;;  ";C:\\msys64\\mingw64\\bin" ";C:\\msys64\\usr\\bin"))
;;   (setq shell-file-name "C:/cygwin64/bin/bash"))

(unless noninteractive
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (display-time-mode 1)
  (set-face-background 'fringe (face-background 'default))
  (custom-set-faces ;; モードラインの配色
   '(mode-line
     ((t (:background "#7D60AF" :foreground "#FFFFFF" :box nil :height 1.0))))
   '(mode-line-inactive
     ((t (:background "#CCCCCC" :foreground "#FFFFFF" :box nil :height 1.0)))))

  ;; Basic key-binding
  (global-set-key (kbd "C-;") 'comment-dwim) ;; M-; is the defualt
  (global-set-key (kbd "C-c g") 'goto-line)
  (global-set-key (kbd "C-c c") 'compile)
  (global-set-key (kbd "C-M-t") 'beginning-of-buffer)
  (global-set-key (kbd "C-M-b") 'end-of-buffer)
  (global-set-key (kbd "C-M-p") (lambda () (interactive) (other-window -1)))
  (global-set-key (kbd "C-M-n") (lambda () (interactive) (other-window 1)))
  (global-set-key (kbd "M-v") 'yank)
  (global-set-key (kbd "C-t") 'scroll-down)
  (global-set-key (kbd "M-=") 'count-words)
  (global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)
  (global-set-key (kbd "C-c a") 'org-agenda)

  (setq scroll-conservatively 1000)
  (setq scroll-step 1)
  (setq scroll-preserve-screen-position t) ;; スクロール時にスクリーン内で固定
  (setq next-screen-context-lines 10)
  (setq mouse-drag-copy-region t)
  (setq yank-excluded-properties t)
  (setq ring-bell-function 'ignore)
  (setq confirm-kill-emacs 'yes-or-no-p)

  ;; Linspacing adjustment
  (defun my-linespacing ()
    (unless (minibufferp)
      (setq-local line-spacing 0)))
  (add-hook 'buffer-list-update-hook #'my-linespacing)
  (add-hook 'org-src-mode-hook #'my-linespacing)
  (add-hook 'debugger-mode-hook #'my-linespacing)

  ;; IME toggle (M-SPC/S-SPC)
  (defun my-ime-active-p ()
    (if current-input-method t nil))
  ;; Auto ascii for org-mode headings
  (defun my-ime-on ()
    "IME ON."
    (interactive)
    (activate-input-method default-input-method))
  (defun my-ime-off ()
    "IME OFF."
    (interactive)
    (deactivate-input-method))
  (defun my-toggle-ime ()
    "Toggle IME."
    (interactive)
    (if (my-ime-active-p) (my-ime-off) (my-ime-on)))
  (defvar my-ime-before-action nil)
  (defun my-ime-on-sticky ()
    (when my-ime-before-action
      (my-ime-on)))
  (defun my-ime-off-sticky ()
    (when (setq my-ime-before-action (my-ime-active-p))
      (my-ime-off)))
  (add-hook 'activate-mark-hook #'my-ime-off-sticky)
  (add-hook 'deactivate-mark-hook #'my-ime-on-sticky)
  (global-set-key (kbd "M-SPC") 'my-toggle-ime)
  (global-set-key (kbd "S-SPC") 'my-toggle-ime)

  ;; カーソル行の色
  (defvar my-ime-off-hline-hook nil)
  (defvar my-ime-on-hline-hook nil)
  (defvar my-hl-permanent-disabled '(dired-mode)
    "A list of major modes to disable `hl-line'.")
  (defun my-ime-off-hline ()
    (my-hl-line-enable)
    (let ((dark (eq (frame-parameter nil 'background-mode) 'dark)))
      (set-face-background hl-line-face (if dark "#484c5c" "#DEEDFF")))
    (run-hooks 'my-ime-off-hline-hook))
  (defun my-ime-on-hline ()
    (my-hl-line-enable)
    (let ((dark (eq (frame-parameter nil 'background-mode) 'dark)))
      (set-face-background hl-line-face (if dark "#594d5d" "#fff0de")))
    (run-hooks 'my-ime-on-hline-hook))
  (add-hook 'input-method-activate-hook #'my-ime-on-hline)
  (add-hook 'input-method-deactivate-hook #'my-ime-off-hline)
  (defun my-hl-line-disable ()
    "Disable `hl-line'."
    (hl-line-mode -1))
  (defun my-hl-line-enable ()
    "Enable `hl-line'."
    (unless (or hl-line-mode
                (minibufferp)
                (memq major-mode my-hl-permanent-disabled))
      (hl-line-mode 1)))
  (global-hl-line-mode)

  ;; カーソルの色
  (defconst my-cur-color-ime '(:on "#FF9300" :off "#91C3FF"))
  (defconst my-cur-type-ime '(:on (bar . 4) :off (bar . 4) :invisible nil))
  (setq-default cursor-type (if (my-ime-active-p)
                                (plist-get my-cur-type-ime :on)
                              (plist-get my-cur-type-ime :off)))
  (defun my-ime-on-cursor ()
    (interactive)
    (setq cursor-type (plist-get my-cur-type-ime :on))
    (set-cursor-color (plist-get my-cur-color-ime :on)))
  (defun my-ime-off-cursor ()
    (interactive)
    (setq cursor-type (plist-get my-cur-type-ime :off))
    (set-cursor-color (plist-get my-cur-color-ime :off)))
  (add-hook 'input-method-activate-hook #'my-ime-on-cursor)
  (add-hook 'input-method-deactivate-hook #'my-ime-off-cursor)

  ;; for init setup on hline and cursor
  (if (my-ime-active-p) (my-ime-on-hline) (my-ime-off-hline))
  (if (my-ime-active-p) (my-ime-on-cursor) (my-ime-off-cursor))

  ;; 特定の文字をバッファ内で強調表示する
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

  ;; Utilities
  (global-set-key (kbd "C-M-<return>") #'my-open-w32-explore)
  (global-set-key (kbd "C-M-o") 'my-open-hoge)
  (global-set-key (kbd "C-M-s") #'my-open-scratch)
  (global-set-key (kbd "C-c 0") 'insert-formatted-current-date)

  (defun my-open-w32-explore ()
    (interactive)
    ;; Need to create an alias to cygstart.exe as open.exe
    (shell-command-to-string "open ."))
  (defun my-open-hoge ()
    (interactive)
    (find-file "u://org/next.org"))
  (defun my-open-scratch ()
    "Switch the current buffer to \*scratch\* buffer."
    (interactive)
    (switch-to-buffer "*scratch*"))
  (defun insert-formatted-current-date ()
    "Insert a timestamp at the cursor position."
    (interactive)
    (insert (format-time-string "%Y-%m-%d")))

  ;; isearch with a selected reagion
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

  (defun ad:mark-sexp (f &optional arg allow-extend)
    "If the cursor is on a symbol, expand the region along the symbol.
Otherwise, set mark ARG sexps from point.
When the cursor is at the end of line or before a whitespace, set ARG -1."
    (interactive "P\np")
    (funcall f (if (and (not (bolp))
                        (not (eq (preceding-char) ?\ ))
                        (or (eolp)
                            (eq (following-char) ?\ )
                            (memq (preceding-char) '(?\) ?\> ?\] ?\}))))
                   -1 arg)
             allow-extend))
  (advice-add 'mark-sexp :around #'ad:mark-sexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part A: Scheduling of package loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto-revert
(defun my-activate-auto-revert ()
  (global-auto-revert-mode 1)
  (remove-hook 'find-file-hook #'my-activate-auto-revert))
(add-hook 'find-file-hook #'my-activate-auto-revert)

;; tr-ime: IME パッチモジュールの読み込み 200[ms]
(defun my-activate-tr-ime ()
  (when (and (eq window-system 'w32)
             (string= module-file-suffix ".dll")
             (not (fboundp 'ime-get-mode)))
    (require 'tr-ime nil t))
  (remove-hook 'pre-command-hook #'my-activate-tr-ime))
(add-hook 'pre-command-hook #'my-activate-tr-ime)

;; session
(autoload #'session-initialize "session" "session" t)
(add-hook 'after-init-hook #'session-initialize)

;; recentf
(defun my-activate-recentf ()
  (when (require 'recentf nil t)
    (let ((message-log-max nil))
      (recentf-mode 1)))
  (remove-hook 'pre-command-hook #'my-activate-recentf))
(add-hook 'pre-command-hook #'my-activate-recentf)

;; Ivy, Counsel, Swiper
(autoload #'counsel-M-x "ivy" "ivy,counsel,swiper" t)
(autoload #'counsel-ag "counsel" "ivy,counsel,swiper" t)
(autoload #'counsel-recentf "ivy" "ivy,counsel,swiper" t)
(autoload #'counsel-ibuffer "ivy" "ivy,counsel,swiper" t)
(autoload #'swiper-thing-at-point "ivy" "ivy,counsel,swiper" t)
(autoload #'counsel-osx-app "counsel-osx-app" "Application Launcher" t)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-M-f") 'counsel-ag)
(global-set-key (kbd "C-M-r") 'counsel-recentf)
(global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
(global-set-key (kbd "M-s M-s") 'swiper-thing-at-point)
(global-set-key (kbd "C-M-1") 'counsel-osx-app)

;; bs with bsv.el
(global-set-key (kbd "M-]") 'bs-cycle-next)
(global-set-key (kbd "M-[") 'bs-cycle-previous)

;; moom
(autoload #'moom-move-frame-to-edge-top "moom" "Moom" t)
(autoload #'moom-cycle-frame-height "moom" "Moom" t)
(autoload #'moom-move-frame "moom" "Moom" t)
(autoload #'moom-move-frame-to-center "moom" "Moom" t)
(global-set-key (kbd "C-1") 'moom-move-frame-to-edge-top)
(global-set-key (kbd "C-2") 'moom-cycle-frame-height)
(global-set-key (kbd "M-0") 'moom-move-frame)
(global-set-key (kbd "M-2") 'moom-move-frame-to-center)

;; smartparens
(autoload #'smartparens-global-mode "smartparens" "smartparens" t)
(defun my-activate-smartparens ()
  (when (require 'smartparens nil t)
    (smartparens-global-mode))
  (remove-hook 'find-file-hook #'my-activate-smartparens))
(add-hook 'find-file-hook #'my-activate-smartparens)

;; selected
(autoload #'selected-global-mode "selected" "selected" t)
(defun my-activate-selected ()
  (require 'transient nil t)
  (selected-global-mode 1)
  (selected--on) ;; must call expclitly here
  (remove-hook 'activate-mark-hook #'my-activate-selected))
(add-hook 'activate-mark-hook #'my-activate-selected)

;; hl-todo (depends on compat.el)
(autoload #'global-hl-todo-mode "hl-todo" "hl-todo" t)

;; highlight-symbol
(autoload #'highlight-symbol-mode "highlight-symbol" "highlight-symbol" t)

;; volatile-highlights
(autoload #'volatile-highlights-mode "volatile-highlights" "VHl" t)

;; Postpone: hl-todo.el, highlight-symbol.el, and volatile-highlights.el
(defun my-activate-highlights ()
  (when (require 'hl-todo nil t)
    (my-hl-todo-light-theme))
  (when (require 'highlight-symbol nil t)
    (dolist (hook '(emacs-lisp-mode-hook c-mode-common-hook prog-mode-hook))
      (add-hook hook #'highlight-symbol-mode)))
  (when (require 'volatile-highlights nil t)
    (dolist (hook '(org-mode-hook emacs-lisp-mode-hook))
      (add-hook hook #'volatile-highlights-mode)))
  (remove-hook 'pre-command-hook #'my-activate-highlights))
(add-hook 'pre-command-hook #'my-activate-highlights)

;; eldoc
(defun my-activate-eldoc ()
  (when (require 'eldoc nil t)
    (dolist (hook '(emacs-lisp-mode-hook org-mode-hook c-mode-common-hook))
      (add-hook hook #'turn-on-eldoc-mode)))
  (remove-hook 'find-file-hook #'my-activate-eldoc))
(add-hook 'find-file-hook #'my-activate-eldoc)
(add-hook 'org-mode-hook #'my-load-echo-org-link)

;; bm: ファイルオープン時にブックマークを復帰
(autoload #'my-toggle-bm "bm" "bm" t)
(autoload #'my-bm-next "bm" "bm" t)
(autoload #'bm-buffer-restore "bm" "bm" t)
(autoload #'counsel-bm "bm" "bm" t)
(global-set-key (kbd "<f10>") 'my-toggle-bm)
(global-set-key (kbd "<C-f10>") 'my-bm-next)
(global-set-key (kbd "<S-f10>") 'bm-show-all)
(add-hook 'find-file-hook #'bm-buffer-restore)

;; google-this
(autoload #'google-this "google-this" "google-this" t)
(autoload #'my-google-this "google-this" "google-this" t)

;; Tree Sitter
(let* ((elp (expand-file-name (concat "~/.emacs.d/lisp/")))
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

;; replace-from-region
(autoload #'query-replace-from-region "replace-from-region" nil t)
(global-set-key (kbd "M-%") 'query-replace-from-region)

;; Undo-fu
(autoload #'undo-fu-only-undo "undo-fu" "Undo" t)
(autoload #'undo-fu-only-redo "undo-fu" "Undo" t)
(global-set-key (kbd "C-/") 'undo-fu-only-undo)
(global-set-key (kbd "C-M-/") 'undo-fu-only-redo)

;; view
(autoload #'my-auto-view "view" "view mode" t)
(add-hook 'find-file-hook #'my-auto-view)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part B: Configurations for each package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "tr-ime"
  (tr-ime-advanced-initialize)
  ;; IM のデフォルトを IME に設定
  (setq default-input-method "W32-IME")
  ;; IME のモードライン表示設定
  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  ;; IME ON/OFF state を全ウィンドウで1つにする
  (setq w32-ime-buffer-switch-p nil)
  ;; IME 初期化
  (w32-ime-initialize)
  ;; IME 制御（yes/no などの入力の時に IME を OFF にする）
  (wrap-function-to-control-ime 'universal-argument t nil)
  (wrap-function-to-control-ime 'read-string nil nil)
  (wrap-function-to-control-ime 'read-char nil nil)
  (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
  (wrap-function-to-control-ime 'y-or-n-p nil nil)
  (wrap-function-to-control-ime 'yes-or-no-p nil nil)
  (wrap-function-to-control-ime 'map-y-or-n-p nil nil))

(with-eval-after-load "session"
  (add-to-list 'session-globals-exclude 'org-mark-ring)
  (setq session-set-file-name-exclude-regexp
        "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|[/\\]COMMIT_EDITMSG")
  ;; Change save point of session.el
  (setq session-save-file (expand-file-name "u:/emacs.d/.session"))
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 100)
                                  (session-file-alist 100 t)
                                  (file-name-history 200)
                                  ivy-dired-history-variable
                                  search-ring
                                  regexp-search-ring))
  (setq session-undo-check -1))

(with-eval-after-load "recentf"
  (custom-set-variables
   '(recentf-max-saved-items 2000)
   '(recentf-save-file (expand-file-name "u:/.emacs.d/recentf"))
   '(recentf-auto-cleanup 'never)
   '(recentf-exclude
     '(".recentf" "bookmarks" "org-recent-headings.dat" "^/tmp\\.*"
       "^/private\\.*" "^/var/folders\\.*" "/TAGS$")))

  (defun my-recentf-save-list-silence ()
    (interactive)
    (let ((message-log-max nil))
      (recentf-save-list))
    (message ""))
  (defun my-recentf-cleanup-silence ()
    (interactive)
    (let ((message-log-max nil))
      (recentf-cleanup))
    (message ""))
  ;; (run-with-idle-timer 180 t #'my-recentf-cleanup-silence)
  (add-function :before
                after-focus-change-function #'my-recentf-save-list-silence))

(with-eval-after-load "ivy"
  (require 'swiper)
  (require 'counsel)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-,") 'counsel-mark-ring)
  (global-set-key (kbd "C-c i r") 'ivy-resume)
  (global-set-key (kbd "<S-f10>") 'counsel-bm)

  (setq ivy-initial-inputs-alist
        '((org-agenda-refile . "^")
          (org-capture-refile . "^")
          (counsel-describe-function . "^")
          (counsel-describe-variable . "^")
          (Man-completion-table . "^")
          (woman . "^")))

  ;; 順不同化
  (setf (alist-get t ivy-re-builders-alist) #'ivy--regex-ignore-order)

  ;; 履歴を使う
  (when (require 'smex nil t)
    (setq smex-history-length 35)
    (setq smex-completion-method 'ivy))

  ;; 選択候補の先頭に矢印を入れる
  (setq ivy-format-functions-alist '((t . ivy-format-function-arrow)))

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

(with-eval-after-load "counsel-osx-app"
  (defun counsel-win-app-list ()
    ;; NOTE MSYS の場合は，第2引数はフルパスではなく実行ファイル名のみ．
    '(("Mintty" . "C:/cygwin64/bin/mintty.exe")
      ("Edge" . "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
      ("Notepad". "C:/WINDOWS/system32/notepad.exe")
      ("Task manager". "C:/WINDOWS/system32/taskmgr.exe")
      ("Command Prompt". "C:/WINDOWS/system32/cmd.exe")
      ("PowerToys" . "C:/Program Files/PowerToys/PowerToys.exe")
      ("Chrome" . "C:/Program Files/Google/Chrome/Application/chrome.exe")
      ("Word" . "C:/Program Files (x86)/Microsoft Office/Office16/winword.exe")
      ("Excel" . "C:/Program Files (x86)/Microsoft Office/Office16/excel.exe")
      ("PowerPoint" . "C:/Program Files (x86)/Microsoft Office/Office16/powerpnt.exe")
      ("Outlook" . "C:/Program Files (x86)/Microsoft Office/Office16/outlook.exe")))

  (defvar counsel-win-app-launch-cmd
    (lambda (app &optional file)
      (if (bound-and-true-p file)
          (format "%s -a %s" file app)
        (format "%s" app))) "")

  (defun counsel-osx-app-action-default (app)
    "Launch APP using `counsel-win-app-launch-cmd'."
    (let ((arg
           (cond
            ((stringp counsel-win-app-launch-cmd)
             (format "%s %s" counsel-win-app-launch-cmd app))
            ((functionp counsel-win-app-launch-cmd)
             (funcall counsel-win-app-launch-cmd app))
            (t
             (user-error
              "Could not construct cmd from `counsel-win-app-launch-cmd'")))))
      (message "%s" (concat "open " "\"" arg "\""))
      ;; MSYS2 の場合はファイルパスではなくアプリ名で判定される
      ;; TODO file check only in Cygwin
      (w32-shell-execute "open" arg)
      ;; (if (file-exists-p arg)
      ;;     (call-process-shell-command (concat "open " "\"" arg "\""))
      ;;   (user-error (format "Could not find \"%s\"" arg)))
      ))

  (defun counsel-osx-app ()
    "Launch an application via ivy interface."
    (interactive)
    (ivy-read "Run application: " (counsel-win-app-list)
              :action (counsel-osx-app--use-cdr
                       counsel-osx-app-action-default)
              :caller 'counsel-app)))

(with-eval-after-load "bs"
  (when (require 'bsv nil t)
    (setq bsv-max-height 5)
    (setq bsv-message-timeout 9)))

(with-eval-after-load "moom"
  (moom-recommended-keybindings 'all)
  (setq moom-lighter "M")
  (setq moom-verbose t)
  (setq moom-font-ja-scale 1.0)
  (moom-mode 1)

  ;; Font setting with `moom-font'
  (when (require 'moom-font nil t)
    (moom-font-ja "Migu 2M")
    (moom-font-ascii "Inconsolata")
    (moom-font-resize 24)))

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
  (sp-local-pair 'yatex-mode "$" "$"))

(with-eval-after-load "selected"
  (defvar my-eval-result "*eval-result*")
  (defun my-eval-region ()
    (interactive)
    (when (use-region-p)
      (eval-region (region-beginning) (region-end)
                   (get-buffer-create my-eval-result))
      ;; Copy the result to kill-ring and print it
      (with-current-buffer (get-buffer-create my-eval-result)
        (delete-char -1)
        (goto-char (point-min))
        (delete-blank-lines)
        (mark-whole-buffer)
        (kill-ring-save (point-min) (point-max))
        (message "%s" (car kill-ring))
        (erase-buffer))
      ;; Jump to the end of the region
      (goto-char (max (mark) (point)))
      (deactivate-mark)))
  (setq selected-org-mode-map (make-sparse-keymap))
  (define-key selected-org-mode-map (kbd "t") #'org-toggle-checkbox)
  (define-key selected-org-mode-map (kbd "-") #'my-org-bullet-and-checkbox)
  (define-key selected-keymap (kbd "5") #'query-replace-from-region)
  (define-key selected-keymap (kbd ";") #'comment-dwim)
  (define-key selected-keymap (kbd "e") #'my-eval-region)
  (define-key selected-keymap (kbd "g") #'my-google-this)
  (when (require 'expand-region nil t)
    (define-key selected-keymap (kbd "SPC") #'er/expand-region)))

(with-eval-after-load "hl-todo"
  (defun my-hl-todo-reload ()
    (interactive)
    (global-hl-todo-mode -1)
    (global-hl-todo-mode))

  (defun my-hl-todo-light-theme ()
    (setq hl-todo-keyword-faces
          '(("HOLD" . "#d0bf8f")
            ("TODO" . "#FF0000")
            ("NEXT" . "#dca3a3")
            ("THEM" . "#dc8cc3")
            ("PROG" . "#7cb8bb")
            ("OKAY" . "#7cb8bb")
            ("DONT" . "#5f7f5f")
            ("FAIL" . "#8c5353")
            ("DONE" . "SeaGreen")
            ("NOTE"   . "#d0bf8f")
            ("KLUDGE" . "#d0bf8f")
            ("HACK"   . "#d0bf8f")
            ("TEMP"   . "#d0bf8f")
            ("FIXME"  . "#3030FF")
            ("XXX+"   . "#cc9393")
            ("\\?\\?\\?+" . "#cc9393")))
    (my-hl-todo-reload)))

(with-eval-after-load "highlight-symbol"
  (custom-set-variables
   '(highlight-symbol-idle-delay 0.5)))

(with-eval-after-load "volatile-highlights"
  (set-face-attribute
   'vhl/default-face nil :foreground "#FF3333" :background "#FFCDCD")
  (volatile-highlights-mode t)

  ;; ふわっとエフェクトの追加（ペースト時の色 => カーソル色 => 本来色）
  (defun my-vhl-change-color ()
    (interactive)
    (let ((next 0.2)
          (reset 0.5)
          (colors '("#F8D3D7" "#F2DAE1" "#EBE0EB" "#E5E7F5" "#DEEDFF")))
      (dolist (color colors)
        (run-at-time next nil
                     'set-face-attribute
                     'vhl/default-face
                     nil :foreground "#FF3333" :background color)
        (setq next (+ 0.05 next)))
      (run-at-time reset nil 'vhl/clear-all))
    (set-face-attribute 'vhl/default-face
                        nil :foreground "#FF3333"
                        :background "#FFCDCD"))

  (defun my-yank (&optional ARG)
    (interactive)
    (yank ARG)
    (when window-system
      (my-vhl-change-color)))
  (global-set-key (kbd "M-v") 'my-yank)
  (global-set-key (kbd "C-y") 'my-yank)

  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-y") 'my-org-yank)
    (defun my-org-yank ()
      (interactive)
      (org-yank)
      (when window-system
        (my-vhl-change-color)))))

(with-eval-after-load "eldoc"
  ;; for ivy-mode
  (defun ad:eldoc-message (f &optional string)
    (unless (active-minibuffer-window)
      (funcall f string)))
  (advice-add 'eldoc-message :around #'ad:eldoc-message)

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
    )

  (custom-set-variables
   '(eldoc-idle-delay 1.0)))

(with-eval-after-load "bm"
  ;; (setq bm-annotation-width 30)
  (setq-default bm-buffer-persistence t)
  (setq bm-restore-repository-on-load t)
  (setq bm-cycle-all-buffers t)
  ;; (setq bm-toggle-buffer-persistence t)
  (setq bm-buffer-persistence t)
  (setq bm-persistent-face 'bm-face)
  (setq bm-repository-file (expand-file-name "u:/emacs.d/.bm-repository"))
  ;; ビルトイン bookmark の配色を無効にする(as of 28.1)
  (setq bookmark-fontify nil)
  ;; ビルトイン bookmark がfringeに出すマークを無効にする(as of 28.1)
  (setq bookmark-set-fringe-mark nil)

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

  (defun my-bm-save-all ()
    (bm-buffer-save-all)
    (bm-repository-save))

  (defun my-toggle-bm ()
    "bm-toggle with updating history"
    (interactive)
    (let ((bm (concat
               (buffer-name) "::"
               (if (and (equal major-mode 'org-mode)
                        (not (org-before-first-heading-p)))
                   (nth 4 (org-heading-components))
                 (format "%s" (line-number-at-pos))))))
      (if (bm-bookmark-at (point))
          (bookmark-delete bm)
        (bookmark-set bm)))
    (bm-toggle)
    (bm-buffer-save-all)
    (bm-repository-save))

  (defun my-bm-next ()
    "bm-next with org-mode"
    (interactive)
    (bm-next)
    (when (and (equal major-mode 'org-mode)
               (not (org-before-first-heading-p)))
      (widen)
      (org-overview)
      (org-reveal)
      (org-cycle-hide-drawers 'all)
      (org-show-entry)
      (show-children)
      (org-show-siblings)))

  (when (require 'ivy nil t)
    (defun counsel-bm-get-list (bookmark-overlays)
      (-map (lambda (bm)
              (with-current-buffer (overlay-buffer bm)
                (let* ((line (replace-regexp-in-string
                              "\n$" ""
                              (buffer-substring (overlay-start bm)
                                                (overlay-end bm))))
                       ;; line numbers start on 1
                       (line-num
                        (+ 1 (count-lines (point-min) (overlay-start bm))))
                       (name (format "%s:%d - %s" (buffer-name) line-num line)))
                  `(,name . ,bm))))
            bookmark-overlays))

    (defun counsel-bm ()
      (interactive)
      (let* ((bm-list (counsel-bm-get-list (bm-overlays-lifo-order t)))
             (bm-hash-table (make-hash-table :test 'equal))
             (search-list (-map (lambda (bm) (car bm)) bm-list)))
        (-each bm-list (lambda (bm)
                         (puthash (car bm) (cdr bm) bm-hash-table)
                         ))
        (ivy-read "Find bookmark(bm.el): "
                  search-list
                  :require-match t
                  :keymap counsel-describe-map
                  :action (lambda (chosen)
                            (let ((bookmark (gethash chosen bm-hash-table)))
                              (switch-to-buffer (overlay-buffer bookmark))
                              (bm-goto bookmark)
                              ))
                  :sort t)))))

(with-eval-after-load "google-this"
  (defun my-google-this ()
    (interactive)
    (google-this (current-word) t)))

(with-eval-after-load "expand-region"
  (defun ad:er:mark-sexp (f &optional arg allow-extend)
    "If the cursor is on a symbol, expand the region along the symbol."
    (interactive "P\np")
    (if (and (not (use-region-p))
             (symbol-at-point))
        (er/mark-symbol)
      (funcall f arg allow-extend)))
  (advice-add 'mark-sexp :around #'ad:er:mark-sexp))

(with-eval-after-load "epa"
  (setq epg-pinentry-mode 'loopback))

(with-eval-after-load "calendar"
  (when (require 'japanese-holidays nil t)
    (setq calendar-holidays
          (append japanese-holidays
                  holiday-local-holidays holiday-other-holidays))
    (setq mark-holidays-in-calendar t)
    (setq calendar-mark-holidays-flag t)
    (setq japanese-holiday-weekend-marker
          '(holiday nil nil nil nil nil japanese-holiday-saturday))
    (setq japanese-holiday-weekend '(0 6))
    (add-hook 'calendar-today-visible-hook #'japanese-holiday-mark-weekend)
    (add-hook 'calendar-today-invisible-hook #'japanese-holiday-mark-weekend))

  (setq calendar-week-start-day 1)
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)
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
          'font-lock-face 'calendar-iso-week-face))

  (defun my-get-week-number ()
    "Return the current week number."
    (format "%02d"
            (car
             (calendar-iso-from-absolute
              (calendar-absolute-from-gregorian
               (list (string-to-number (format-time-string "%m"))
                     (string-to-number (format-time-string "%d"))
                     (string-to-number (format-time-string "%y")))))))))

(with-eval-after-load "transient"
  (defvar my-org-bullet-re
    "\\(^[ \t]*[-\\+\\*][ \t]\\|^[ \t]*[a-z0-9A-Z]*[\\.)][ \t]\\)")

  (defvar my-org-bullet-with-checkbox-re
    (concat my-org-bullet-re "\\[.\\][ \t]+"))

  (defun my-org-insert-bullet (begin end)
    (interactive "r")
    (unless mark-active
      (setq begin (line-beginning-position))
      (setq end (line-end-position)))
    (let* ((bullet "- ")
           (len (string-width bullet)))
      (goto-char begin)
      (while (and (re-search-forward (concat "\\(^[ \t]*\\)") end t)
                  (not (looking-at "[-\\+\\*][ \t]\\|[a-z0-9A-Z]*[\\.)][ \t]"))
                  (not (equal (point) end)))
        (replace-match (concat "\\1" bullet) nil nil)
        (setq end (+ end len)))
      (goto-char begin)))

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
         my-org-bullet-with-checkbox-re (point-at-eol) t)
        (my-org-delete-checkbox-from-bullet begin end)
      (my-org-insert-checkbox-into-bullet begin end)))

  (defun my-org-insert-checkbox-into-bullet (begin end)
    (interactive "r")
    (unless mark-active
      (setq begin (line-beginning-position))
      (setq end (line-end-position)))
    (let* ((checkbox "[ ] ")
           (len (string-width checkbox)))
      (goto-char begin)
      (while (and (re-search-forward my-org-bullet-re end t)
                  (not (looking-at "\\[.\\][ \t]+")))
        (replace-match (concat "\\1" checkbox) nil nil)
        (setq end (+ end len)))
      (goto-char begin)))

  (defun my-org-delete-checkbox-from-bullet (begin end)
    (interactive "r")
    (unless mark-active
      (setq begin (line-beginning-position))
      (setq end (line-end-position)))
    (let ((len (string-width "[ ] ")))
      (goto-char begin)
      (while (re-search-forward my-org-bullet-with-checkbox-re end t)
        (replace-match "\\1" nil nil)
        (setq end (- end len)))
      (goto-char begin)))

  (defun my-org-insert-bullet-and-checkbox (begin end)
    (interactive "r")
    (unless mark-active
      (setq begin (line-beginning-position))
      (setq end (line-end-position)))
    (let* ((bullet "- ")
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

  (defun my-org-delete-bullet-and-checkbox (begin end)
    (interactive "r")
    (unless mark-active
      (setq begin (line-beginning-position))
      (setq end (line-end-position)))
    (goto-char begin)
    (while (re-search-forward my-org-bullet-with-checkbox-re end t)
      (let ((len (- (match-end 0) (match-beginning 0))))
        (replace-match "" nil nil)
        (setq end (- end len))))
    (goto-char begin))

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

(with-eval-after-load "view"
  ;; 特定の拡張子・ディレクトリ
  (defvar my-auto-view-regexp "\\.el.gz$\\|\\.patch$\\|\\.xml$\\|\\.csv$\\|\\.emacs.d/[^/]+/el-get\\|config")
  ;; 特定のディレクトリ（絶対パス・ホームディレクトリ以下）
  (defvar my-auto-view-dirs nil)
  (add-to-list 'my-auto-view-dirs (expand-file-name "~/.emacs.d/lisp/"))

  (define-key view-mode-map (kbd "i") 'View-exit-and-edit)
  (define-key view-mode-map (kbd "<SPC>") 'ignore)
  (define-key view-mode-map (kbd "<DEL>") 'ignore)
  (define-key view-mode-map (kbd "S-SPC") 'mac-ime-toggle)
  (define-key view-mode-map (kbd "f") 'forward-char)
  (define-key view-mode-map (kbd "b") 'backward-char)
  (define-key view-mode-map (kbd "n") 'my-org-view-next-heading)
  (define-key view-mode-map (kbd "p") 'my-org-view-previous-heading)
  (define-key view-mode-map (kbd "g") #'my-google-this)
  (define-key view-mode-map (kbd "<tab>") 'my-view-tab)
  (define-key view-mode-map (kbd "S-<tab>") 'my-view-shifttab)

  (defun my-auto-view ()
    "Open a file with `view-mode'."
    (when (file-exists-p buffer-file-name)
      (when (and my-auto-view-regexp
	               (string-match my-auto-view-regexp buffer-file-name))
        (view-mode 1))
      (dolist (dir my-auto-view-dirs)
        (when (eq 0 (string-match (expand-file-name dir) buffer-file-name))
          (view-mode 1)))))

  (defun my-org-view-next-heading ()
    (interactive)
    (if (and (derived-mode-p 'org-mode)
             (org-at-heading-p))
        (org-next-visible-heading 1)
      (next-line)))

  (defun my-org-view-previous-heading ()
    (interactive)
    (if (and (derived-mode-p 'org-mode)
             (org-at-heading-p))
        (org-previous-visible-heading 1)
      (previous-line)))

  (defun my-view-tab ()
    (interactive)
    (if (and (derived-mode-p 'org-mode)
             (or (org-at-heading-p)
                 (org-at-property-drawer-p)))
        (let ((view-mode nil))
          (org-cycle))))

  (defun my-view-shifttab ()
    (interactive)
    (if (derived-mode-p 'org-mode)
        (let ((view-mode nil))
          (org-shifttab))))

  (defun my-unlock-view-mode ()
    (when view-mode
      (View-exit-and-edit))))

;; org mode
(with-eval-after-load "org"
  (add-hook 'org-mode-hook #'turn-on-font-lock)
  (custom-set-faces '(org-drawer ((t (:foreground "#999999"))))
                    '(org-verbatim ((t (:foreground "#FF0000")))))

  ;; プロパティ等を自動的閉じる．
  (defun my-org-hide-drawers ()
    "Hide all drawers in an org tree."
    (interactive)
    (save-excursion
      (beginning-of-line)
      (unless (looking-at-p org-drawer-regexp)
        (org-cycle-hide-drawers 'subtree))))
  (add-hook 'org-tab-first-hook 'my-org-hide-drawers)

  (setq org-startup-truncated nil
        org-hide-leading-stars t
        org-use-speed-commands t
        org-adapt-indentation nil
        org-tags-column -76
        org-list-allow-alphabetical t)
  (define-key org-mode-map (kbd "C-M-t") 'beginning-of-buffer)

  (when (version< (org-version) "9.4.6")
    (defvaralias 'org-speed-commands 'org-speed-commands-user))
  (add-to-list 'org-speed-commands '("d" org-todo "DONE"))
  (add-to-list 'org-speed-commands
               '("D" my-org-todo-complete-no-repeat "DONE"))
  (add-to-list 'org-speed-commands
               '("$" call-interactively 'org-archive-subtree))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (add-to-list 'org-modules 'org-id)
  (delq 'ol-gnus org-modules)

  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-headline-done prepend))
   'append)

  ;; 周期タクスを終了させます．
  (defun my-org-todo-complete-no-repeat (&optional ARG)
    (interactive "P")
    (when (org-get-repeat)
      (org-cancel-repeater))
    (org-todo ARG))

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
  (advice-add 'org-insert-todo-heading :after #'ad:org-insert-todo-heading)
  (add-hook 'org-capture-before-finalize-hook #'my-org-set-created-property)

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

  (defvar my-org-bullet-with-checkbox-regexp
    (concat "\\(^[ \t]*[-\\+\\*][ \t]\\|^[ \t]*[a-z0-9A-Z]*[\\.)][ \t]\\)"
            "\\[.\\][ \t]+"))
  (defun my-cycle-bullet-at-heading (arg)
    "Add a bullet of \"- \" if the line is NOT a bullet line."
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
          (replace-match (concat "\\1" bullet) nil nil))
         (t nil)))))
  (global-set-key (kbd "C-M--") 'my-cycle-bullet-at-heading)

  (defun ad:org-return (f &optional arg)
    "An extension for checking invisible editing when you hit the enter."
    (interactive "P")
    (org-check-before-invisible-edit 'insert)
    (apply f arg))
  (advice-add 'org-return :around #'ad:org-return)

  (let ((dir (expand-file-name org-directory)))
    (setq org-refile-targets
          `((,(concat dir "next.org") :level . 1)
            (,(concat dir "patent.org") :level . 1)
            (,(concat dir "reports.org") :level . 1))))

  (defun do-org-update-statistics-cookies ()
    (interactive)
    (org-update-statistics-cookies 'all))

  (defun my-do-org-update-staistics-cookies ()
    (interactive)
    (message "Update statistics...")
    (do-org-update-statistics-cookies)
    (message "Update statistics...done"))

  (define-key org-mode-map (kbd "C-c f 2")
    'my-do-org-update-staistics-cookies)

  ;; M-x calendar
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

  (with-eval-after-load "ob-core"
    (setq org-edit-src-content-indentation 0)
    (setq org-src-fontify-natively t)
    (setq org-src-tab-acts-natively t)
    (setq org-confirm-babel-evaluate nil)
    (setq org-src-window-setup 'current-window)
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
       (python . t))))

  (defun my-org-heading-auto-ascii ()
    "IME off, when the cursor on org headings."
    (when (and (fboundp 'frame-focus-state)
		           (frame-focus-state)
               (eq major-mode 'org-mode)
               (boundp 'org-agenda-buffer-name)
               (or (looking-at org-heading-regexp)
                   (equal (buffer-name) org-agenda-buffer-name))
               (my-ime-active-p))
      (my-ime-off)))
  (run-with-idle-timer 0.4 t #'my-org-heading-auto-ascii)

  (defun my-insert-empty-pgp-tree ()
    (interactive)
    (insert "** TODO\n")
    (insert "-----BEGIN PGP MESSAGE-----\n\n-----END PGP MESSAGE-----\n"))

  (defun my-insert-enc2me-tree ()
    (interactive)
    (insert "** TODO share with me\n")
    (insert "   :PROPERTIES:\n")
    (insert "   :CRYPTKEY: takaxp@ieee.org\n")
    (insert "   :END:\n")
    (insert "\n")
    (forward-line -1))

  (remove-hook 'org-tab-first-hook 'my-org-hide-drawers) ;; error on v9.4

  (org-defkey org-mode-map (kbd "M-p") #'my-org-meta-next)
  (org-defkey org-mode-map (kbd "M-n") #'my-org-meta-previous)
  (org-defkey org-mode-map (kbd "M-b") #'my-org-meta-backward)
  (org-defkey org-mode-map (kbd "M-f") #'my-org-meta-forward)

  (defun my-org-item-has-child-p ()
    "Return t, if the item has at least a child item."
    (save-excursion
      (beginning-of-line)
      (org-list-has-child-p (point) (org-list-struct))))

  (defun my-org-heading-has-child-p ()
    "Return t, if the heading has at least a child heading."
    (save-excursion
      (org-goto-first-child)))

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
          (t (call-interactively 'scroll-up))))

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
          (t (call-interactively 'scroll-down))))

  (defvar my-org-promote-demote-independently nil)
  (defun my-inherit-struct-p ()
    (and (not my-org-promote-demote-independently)
         (or (my-org-item-has-child-p) (my-org-heading-has-child-p))))

  (defun my-org-at-meta-fb-p ()
    "Return t, if the cursor stay at item, heading, or table."
    (or (org-at-item-p)
        (looking-at org-heading-regexp)
        (and (org-at-heading-p) (eolp))
        (org-at-table-p)))

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

  (defun my-org-meta-backward ()
    (interactive)
    (if (my-org-at-meta-fb-p)
        (if (my-inherit-struct-p)
            (org-shiftmetaleft)
          (org-metaleft)) ;; FIXME similar check to my-org-at-meta-fb-p
      (if (and (fboundp 'syntax-subword-mode)
               syntax-subword-mode)
          (call-interactively 'syntax-subword-backward)
        (backward-word)))))

(with-eval-after-load "org"
  (when (version< "9.1.4" (org-version))
    (add-to-list 'org-modules 'org-tempo)))

(with-eval-after-load "org"
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

  (setq org-tag-faces
        '(("Achievement" :foreground "#66CC66")
          ("Bug"         :foreground "#FF0000")
          ("Background"  :foreground "#66CC99")
          ("Chore"       :foreground "#6699CC")
          ("book"        :foreground "#6666CC")
          ("Doing"       :foreground "#FF0000")
          ("Draft"       :foreground "#9933CC") ;; Draft(r1,r2,r3)->Review(1,2)
          ("Mag"         :foreground "#9966CC")
          ("emacs"       :foreground "#6633CC")
          ("Ongoing"     :foreground "#CC6666") ; for non scheduled/reminder
          ("Repeat"      :foreground "#CC9999") ; for interval tasks
          ("buy"         :foreground "#9966CC")
          ("pay"         :foreground "#CC6699")
          ("try"         :foreground "#FF3366")
          ("secret"      :foreground "#FF0000")
          ("study"       :foreground "#6666CC")
          ("Open"        :foreground "#CC9999" :weight bold)
          ("plan"        :foreground "#FF7D7D")
          ("Test"        :foreground "#FF0000" :weight bold)
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
  (my-org-src-block-face)

  (custom-set-faces
   '(fixed-pitch ((t (:family inconsolata))))
   '(org-block-begin-line
     ((((background dark))
       (:foreground "#669966" :weight bold)) ;; :background "#444444"
      (t (:foreground "#CC3333" :weight bold)))) ;; :background "#EFEFEF"
   '(org-block-end-line
     ((((background dark)) (:foreground "#CC3333" :weight bold))
      (t (:foreground "#669966" :weight bold))))))

(with-eval-after-load "org"
  (defun my-org-agenda-prepare-buffers ()
    (unless (featurep 'org-agenda)
      (when (require 'org-agenda nil t)
        (message "Building agenda buffers...")
        (org-agenda-prepare-buffers org-agenda-files)
        (message "Building agenda buffers...done"))))
  (run-with-idle-timer 10 nil #'my-org-agenda-prepare-buffers))

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

  ;; org-agenda でも "d" 押下で "DONE" にする
  (defun my-org-agenda-done ()
    (interactive)
    (org-agenda-todo "DONE")) ;; call with async
  (org-defkey org-agenda-mode-map "d" 'my-org-agenda-done)

  ;; org-agenda の表示高さを 50% に固定する
  (setq org-agenda-window-frame-fractions '(0.5 . 0.5)))

(with-eval-after-load "org-tempo"
  ;; 空行のとき "<" をインデントさせない
  (defun ad:org-tempo-complete-tag (f &rest arg)
    (if (save-excursion
          (beginning-of-line)
          (looking-at "<"))
        (let ((indent-line-function 'ignore))
          (apply f arg))
      (apply f arg)))
  (advice-add 'org-tempo-complete-tag :around #'ad:org-tempo-complete-tag)

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

(when do-profile (profiler-stop))
;; End of init-win.el
