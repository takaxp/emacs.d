;; The initial file that will be loaded first in w32 environment .
;; each setting will be updated in .emacs, see AppData/Roming/.emacs.
(when (eq system-type 'windows-nt)
  (defvar do-profile nil)
  (when do-profile (profiler-start 'cpu))

  (setq byte-compile-warnings '(obsolete))
  (setq system-time-locale "C") ;; format-time-string %a, not 日 but Sun
  (setq make-backup-files nil)
  (setq ring-bell-function 'ignore)
  (setq default-directory "~/")
  (setq initial-buffer-choice t) ;; Starting from *scratch* buffer
  (setq confirm-kill-emacs 'yes-or-no-p)
  (setq truncate-line nil
        truncate-partial-width-windows nil
        mouse-drag-copy-region t)
  (setq-default tab-width 2)
  (setq-default indent-tabs-mode nil)
  (setq indent-line-function 'insert-tab)

  ;; Language, will override default-input-method
  (set-language-environment "Japanese")
  (set-clipboard-coding-system 'utf-16le) ;; enable copy-and-paste correctly
  (global-auto-revert-mode 1)
  (global-font-lock-mode 1)

  ;; モードラインの配色
  (custom-set-faces
   '(mode-line
     ((t (:background "#7D60AF" :foreground "#FFFFFF" :box nil :height 1.0))))
   '(mode-line-inactive
     ((t (:background "#CCCCCC" :foreground "#FFFFFF" :box nil :height 1.0)))))

  ;; Home directory
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

  ;; AppData\Roaming\.emacs.d 以下に各追加パッケージを配置すること
  ;; smartparens requires dash.el.
  (let ((default-directory (expand-file-name "~/.emacs.d/lisp")))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-to-load-path
     '("moom" "swiper" "selected" "expand-region" "counsel-osx-app" "dash.el"
       "smartparens" "emacs-htmlize" "emacs-undo-fu" "transient" "bsv")))

  (unless noninteractive
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (display-time-mode 1)
    (set-face-background 'fringe (face-background 'default))

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

    ;; Scroll window on a line-by-line basis
    (setq scroll-conservatively 1000)
    (setq scroll-step 1)
    (setq scroll-preserve-screen-position t) ;; スクロール時にスクリーン内で固定

    (defun my-linespacing ()
      (unless (minibufferp)
        (setq-local line-spacing 0)))
    (add-hook 'buffer-list-update-hook #'my-linespacing)
    (add-hook 'org-src-mode-hook #'my-linespacing)
    (add-hook 'debugger-mode-hook #'my-linespacing)

    ;; load-path の追加
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/tr-ime"))

    ;; IME パッチモジュールの読み込み 200[ms]
    (when (and (eq window-system 'w32)
               (string= module-file-suffix ".dll")
               (not (fboundp 'ime-get-mode))
               (require 'tr-ime nil t))

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

    ;; for init setup
    (if (my-ime-active-p) (my-ime-on-hline) (my-ime-off-hline))
    (if (my-ime-active-p) (my-ime-on-cursor) (my-ime-off-cursor))

    (defun my-open-w32-explore ()
      (interactive)
      (shell-command-to-string "open ."))
    (global-set-key (kbd "C-M-<return>") #'my-open-w32-explore)

    (defun my-open-hoge ()
      (interactive)
      (find-file "U://org/remote.org"))
    (global-set-key (kbd "C-M-o") 'my-open-hoge)

    (defun my-open-scratch ()
      "Switch the current buffer to \*scratch\* buffer."
      (interactive)
      (switch-to-buffer "*scratch*"))
    (global-set-key (kbd "C-M-s") #'my-open-scratch)

    (defun insert-formatted-current-date ()
      "Insert a timestamp at the cursor position. C-u will add [] brackets."
      (interactive)
      (insert (format-time-string "%Y-%m-%d")))
    (global-set-key (kbd "C-c 0") 'insert-formatted-current-date)

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; recentf
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
    (when (file-exists-p "/Volumes/orzHDn")
      (if shutup-p
          (shut-up (recentf-cleanup))
        (let ((message-log-max nil))
          (recentf-cleanup)))
      (message "")))
  (add-function :before
                after-focus-change-function #'my-recentf-save-list-silence)
  (add-function :before
                after-focus-change-function #'my-recentf-cleanup-silence)

  (unless noninteractive
    (let ((message-log-max nil))
      (recentf-mode 1)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Ivy, Counsel, Swiper
  (autoload #'counsel-M-x "ivy" "ivy,counsel,swiper" nil t)
  (autoload #'counsel-ibuffer "ivy" "ivy,counsel,swiper" nil t)
  (autoload #'counsel-recentf "ivy" "ivy,counsel,swiper" nil t)
  (autoload #'swiper-thing-at-point "ivy" "ivy,counsel,swiper" nil t)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-M-f") 'counsel-ag)
  (global-set-key (kbd "C-M-r") 'counsel-recentf)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
  (global-set-key (kbd "M-s M-s") 'swiper-thing-at-point)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; bs with bsv.el
  (global-set-key (kbd "M-]") 'bs-cycle-next)
  (global-set-key (kbd "M-[") 'bs-cycle-previous)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; moom
  (autoload #'moom-move-frame-to-edge-top "moom" "Moom" nil t)
  (autoload #'moom-cycle-frame-height "moom" "Moom" nil t)
  (autoload #'moom-move-frame-to-center "moom" "Moom" nil t)
  (autoload #'moom-move-frame "moom" "Moom" nil t)
  (global-set-key (kbd "C-1") 'moom-move-frame-to-edge-top)
  (global-set-key (kbd "C-2") 'moom-cycle-frame-height)
  (global-set-key (kbd "M-0") 'moom-move-frame)
  (global-set-key (kbd "M-2") 'moom-move-frame-to-center)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Counsel (apps)
  (autoload #'counsel-osx-app "counsel-osx-app" "Application Launcher" nil t)
  (global-set-key (kbd "C-M-1") 'counsel-osx-app)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Undo-fu
  (autoload #'undo-fu-only-undo "undo-fu" "Undo" nil t)
  (autoload #'undo-fu-only-redo "undo-fu" "Undo" nil t)
  (global-set-key (kbd "C-/") 'undo-fu-only-undo)
  (global-set-key (kbd "C-M-/") 'undo-fu-only-redo)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; smartparens
  (autoload #'smartparens-global-mode "smartparens" "smartparens" nil t)
  (defun my-smartparens-mode ()
    (smartparens-global-mode)
    (remove-hook 'yatex-mode-hook #'my-smartparens-mode)
    (remove-hook 'org-mode-hook #'my-smartparens-mode))
  (add-hook 'yatex-mode-hook #'my-smartparens-mode)
  (add-hook 'org-mode-hook #'my-smartparens-mode)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; selected
  (autoload #'selected-global-mode "selected" "selected" nil t)
  (defun my-activate-selected ()
    (require 'transient nil t)
    (selected-global-mode 1)
    (selected--on) ;; must call expclitly here
    (remove-hook 'activate-mark-hook #'my-activate-selected))
  (add-hook 'activate-mark-hook #'my-activate-selected)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; replace-from-region
  (autoload #'query-replace-from-region "replace-from-region"
    "replace-from-region" nil t)
  (global-set-key (kbd "M-%") 'query-replace-from-region)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Tree Sitter
  (let* ((elp (expand-file-name
               ;; (concat "~/.emacs.d/" (format "%s" emacs-version) "/el-get/")
               (concat "~/.emacs.d/lisp/")
               ))
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


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Package config

  (with-eval-after-load "epa"
    (setq epg-pinentry-mode 'loopback))

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

  (with-eval-after-load "ivy"
    (require 'swiper)
    (require 'counsel)
    (global-set-key (kbd "M-y") 'counsel-yank-pop)
    (global-set-key (kbd "C-,") 'counsel-mark-ring)
    (global-set-key (kbd "C-c i r") 'ivy-resume)

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

    (require 'org-agenda nil t)
    (defun my-ns-org-heading-auto-ascii ()
      "IME off, when the cursor on org headings."
      (when (and (frame-focus-state)
                 (eq major-mode 'org-mode)
                 (or (looking-at org-heading-regexp)
                     (equal (buffer-name) org-agenda-buffer-name))
                 (my-ime-active-p))
        (my-ime-off)))
    (run-with-idle-timer 0.4 t #'my-ns-org-heading-auto-ascii)

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
          (backward-word))))
    )

  (with-eval-after-load "org"
    (when (version< "9.1.4" (org-version))
      (add-to-list 'org-modules 'org-tempo)))

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
                    #'my-echo-org-link)))

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

  (with-eval-after-load "counsel-osx-app"
    ;; under experimental implementation
    (defun counsel-win-app-list ()
      ;; NOTE MSYS の場合は，第2引数はフルパスではなく実行ファイル名のみ．
      '(("Emacs" . "C:/Users/takaxp/share/emacs-27.1-x86_64/bin/runemacs.exe")
        ("Mintty" . "C:/cygwin64/bin/mintty.exe")
        ("Edge" . "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
        ("Notepad". "C:/WINDOWS/system32/notepad.exe")
        ("Task manager". "C:/WINDOWS/system32/taskmgr.exe")
        ("Command Prompt". "C:/WINDOWS/system32/cmd.exe")
        ("PowerToys" . "C:/Program Files/PowerToys/PowerToys.exe")
        ("Chrome" . "C:/Program Files/Google/Chrome/Application/chrome.exe")
        ("Word" . "C:/Program Files (x86)/Microsoft Office/Office16/winword.exe")
        ("Excel" . "C:/Program Files (x86)/Microsoft Office/Office16/excel.exe")
        ("PowerPoint" . "C:/Program Files (x86)/Microsoft Office/Office16/powerpnt.exe")
        ("Outlook" . "C:/Program Files (x86)/Microsoft Office/Office16/outlook.exe")
        ("Visio" . "C:/Program Files (x86)/Microsoft Office/Office16/visio.exe")))

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
    (when (require 'expand-region nil t)
      (define-key selected-keymap (kbd "SPC") #'er/expand-region)))

  (with-eval-after-load "expand-region"
    (defun ad:er:mark-sexp (f &optional arg allow-extend)
      "If the cursor is on a symbol, expand the region along the symbol."
      (interactive "P\np")
      (if (and (not (use-region-p))
               (symbol-at-point))
          (er/mark-symbol)
        (funcall f arg allow-extend)))
    (advice-add 'mark-sexp :around #'ad:er:mark-sexp))

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

  (with-eval-after-load "calendar"
    (when (require 'japanese-holidays nil t)
      (setq calendar-holidays
            (append japanese-holidays
                    holiday-local-holidays holiday-other-holidays))
      (setq mark-holidays-in-calendar t)
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

  (when do-profile (profiler-stop))
  ) ;; End of init-win.el
