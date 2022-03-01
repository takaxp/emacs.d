;; The initial file that will be loaded first in w32 environment .
(when (eq system-type 'windows-nt)
  (setq byte-compile-warnings '(obsolete))
  (setq system-time-locale "C") ;; format-time-string %a, not 日 but Sun
  (setq make-backup-files nil)
  (setq ring-bell-function 'ignore)
  (setq command-line-default-directory "~/") ;; (setq default-directory "~/")
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
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/moom"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/swiper"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/counsel-osx-app"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-htmlize"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-undo-fu"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/bsv"))

  (unless noninteractive
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
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
    (global-set-key (kbd "M-p") 'scroll-down)
    (global-set-key (kbd "M-n") 'scroll-up)
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
      "IME OFN."
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
      (my-hl-line-disable)
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
      (find-file (expand-file-name "~/hoge.org")))
    (global-set-key (kbd "C-M-o") 'my-open-hoge)

    (defun my-open-scratch ()
      "Switch the current buffer to \*scratch\* buffer."
      (interactive)
      (switch-to-buffer "*scratch*"))
    (global-set-key (kbd "C-M-s") #'my-open-scratch)

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
        ad-do-it)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; recentf
	(custom-set-variables
	 '(recentf-max-saved-items 2000)
	 '(recentf-save-file (expand-file-name "~/.emacs.d/recentf"))
	 '(recentf-auto-cleanup 'never)
	 '(recentf-exclude
		 '(".recentf" "bookmarks" "org-recent-headings.dat" "^/tmp\\.*"
			 "^/private\\.*" "^/var/folders\\.*" "/TAGS$")))

	(defun my-recentf-save-list-silence ()
		(interactive)
    (let ((message-log-max nil))
      (recentf-save-list))
		(message ""))
	(add-hook 'focus-out-hook #'my-recentf-save-list-silence)

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

  ;; Counsel (apps)
  (autoload #'counsel-osx-app "counsel-osx-app" "Application Launcher" nil t)
  (global-set-key (kbd "C-M-1") 'counsel-osx-app)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Undo-fu
  (autoload #'undo-fu-only-undo "undo-fu" "Undo" nil t)
  (autoload #'undo-fu-only-redo "undo-fu" "Undo" nil t)
  (global-set-key (kbd "C-/") 'undo-fu-only-undo)
  (global-set-key (kbd "C-M-/") 'undo-fu-only-redo)

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

    (custom-set-faces
     '(ivy-current-match
       ((((class color) (background light))
         :background "#FFF3F3" :distant-foreground "#000000")
        (((class color) (background dark))
         :background "#404040" :distant-foreground "#abb2bf")))
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

  (add-hook 'org-mode-hook #'turn-on-font-lock)
  (with-eval-after-load "org"
    (setq org-startup-truncated nil
		      org-hide-leading-stars t
          org-use-speed-commands t
          org-adapt-indentation nil
          org-list-allow-alphabetical t)
    (define-key org-mode-map (kbd "C-M-t") 'beginning-of-buffer)

	  (when (version< (org-version) "9.4.6")
		  (defvaralias 'org-speed-commands 'org-speed-commands-user))
	  (add-to-list 'org-speed-commands '("d" org-todo "DONE"))
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
		  "Add a bullet of \" - \" if the line is NOT a bullet line."
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
					  (replace-match (concat "\\1 " bullet) nil nil))
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


  (with-eval-after-load "counsel-osx-app"
    ;; under experimental implementation
    (defun counsel-win-app-list ()
      ;; NOTE MSYS の場合は，第2引数はフルパスではなく実行ファイル名のみ．
      '(("Emacs" . "C:/Users/takaxp/share/emacs-27.1-x86_64/bin/runemacs.exe")
        ("Chrome" . "C:/Program Files/Google/Chrome/Application/chrome.exe")
        ("Internet Explorer" . "C:/Program Files/Internet Explorer/iexplore.exe")
        ("Edge" . "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
        ("Notepad". "C:/WINDOWS/system32/notepad.exe")
        ("Task manager". "C:/WINDOWS/system32/taskmgr.exe")
        ("Command Prompt". "C:/WINDOWS/system32/cmd.exe")
        ("Mintty" . "C:/cygwin64/bin/mintty.exe")
        ("Word" . "C:/Program Files (x86)/Microsoft Office/Office16/winword.exe")
        ("Excel" . "C:/Program Files (x86)/Microsoft Office/Office16/excel.exe")
        ("PowerPoint" . "C:/Program Files (x86)/Microsoft Office/Office16/powerpnt.exe")
        ("Outlook" . "C:/Program Files (x86)/Microsoft Office/Office16/outlook.exe")
        ("Visio" . "C:/Program Files (x86)/Microsoft Office/Office16/visio.exe")
        ))

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
        ;; MSYS2 の場合はファイルパスではなくアプリ名で判定されるの
        ;; TODO file check only in Cygwin
        (if (file-exists-p arg)
            (call-process-shell-command (concat "open " "\"" arg "\""))
          (user-error (format "Could not find \"%s\"" arg)))))

    (defun counsel-osx-app ()
      "Launch an application via ivy interface."
      (interactive)
      (ivy-read "Run application: " (counsel-win-app-list)
                :action (counsel-osx-app--use-cdr
                         counsel-osx-app-action-default)
                :caller 'counsel-app)))

  ) ;; End of init-win-min.el
