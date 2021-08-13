;; The initial file that will be loaded first in w32 environment .
(when (eq system-type 'windows-nt)
  (setq byte-compile-warnings '(obsolete))
  (set-clipboard-coding-system 'utf-16le) ;; enable copy-and-paste correctly
  (setq system-time-locale "C") ;; format-time-string %a, not 日 but Sun

  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq make-backup-files nil)

  ;; Home directory
  ;; (setenv "HOME" "c:/cygwin64/home/********")
  ;; Proxy
  ;; (setq url-proxy-services
  ;;       '(("http" . "hoge@hoge.org:8888")
  ;;         ("https" . "hoge@hoge.org:8888")))
  (setenv "PATH" (concat (getenv "PATH") ";C:\\cygwin64\\bin"))
  (load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
  (setq shell-file-name "C:/cygwin64/bin/bash")
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

  ;; AppData\Roaming\.emacs.d 以下に各追加パッケージを配置すること（e.g. bsv, moom）
  ;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
  ;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/moom"))
  ;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/swiper"))
  ;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/htmlize"))
  ;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/counsel-osx-app"))
  ;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/bsv"))

  (global-set-key (kbd "C-M-t") 'beginning-of-buffer)
  (global-set-key (kbd "C-M-b") 'end-of-buffer)
  (global-set-key (kbd "M-v") 'yank)
  (global-set-key (kbd "M-p") 'scroll-down)
  (global-set-key (kbd "M-n") 'scroll-up)
  (global-set-key (kbd "C-c g") 'goto-line)
  (global-set-key (kbd "C-M-p") (lambda () (interactive) (other-window -1)))
  (global-set-key (kbd "C-M-n") (lambda () (interactive) (other-window 1)))
  (global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)
  (global-set-key (kbd "M-=") 'count-words)

  ;; For IME module (do not load under postpone.el)
  (unless noninteractive
    (setq truncate-line nil
          truncate-partial-width-windows nil
          mouse-drag-copy-region t)
    (setq-default tab-width 2)
    (setq-default indent-tabls-mode nil)
    (setq indent-line-function 'insert-tab)
    (global-auto-revert-mode 1)

		(defun my-linespacing ()
			(unless (minibufferp)
				(setq-local line-spacing 0)))
		(add-hook 'buffer-list-update-hook #'my-linespacing)
		(add-hook 'org-src-mode-hook #'my-linespacing)
		(add-hook 'debugger-mode-hook #'my-linespacing)

    ;; Language, will override default-input-method
    (set-language-environment "Japanese")

    ;; load-path の追加
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/tr-ime"))

    ;; IME パッチモジュールの読み込み
    (when (and (eq window-system 'w32)
               (not (fboundp 'ime-get-mode))
               (string= module-file-suffix ".dll"))
      (when (require 'tr-ime nil t)
        (tr-ime-advanced-initialize)))

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
    (wrap-function-to-control-ime 'map-y-or-n-p nil nil)

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
    (global-set-key (kbd "S-SPC") 'my-toggle-ime))

  (defun my-open-w32-explore ()
    (interactive)
    (shell-command-to-string "open ."))
  (global-set-key (kbd "C-M-<return>") #'my-open-w32-explore)

  (defun my-open-hoge ()
    (interactive)
    (find-file (expand-file-name "~/hoge.org")))
  (global-set-key (kbd "C-M-o") 'my-open-hoge)

  ;; Cursor color
  (set-cursor-color (plist-get my-cur-color-ime :off))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; recentf
  (when (require 'recentf nil t)
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
	  (global-set-key (kbd "C-M-r") 'counsel-recentf))

  (when (and (require 'swiper nil t)
	           (require 'ivy nil t)
	           (require 'counsel nil t))
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "M-y") 'counsel-yank-pop)
    (global-set-key (kbd "C-,") 'counsel-mark-ring)
    (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
    (global-set-key (kbd "C-c i r") 'ivy-resume)
    (global-set-key (kbd "M-s M-s") 'swiper-thing-at-point))

  (when (require 'bs nil t)
    (global-set-key (kbd "M-]") 'bs-cycle-next)
    (global-set-key (kbd "M-[") 'bs-cycle-previous)
    (when (require 'bsv nil t)
		  (setq bsv-max-height 5)
		  (setq bsv-message-timeout 9)))

  ;; moom
  (with-eval-after-load "moom"
    (global-set-key (kbd "C-1") 'moom-move-frame-to-edge-top)
    (global-set-key (kbd "C-2") 'moom-cycle-frame-height)
    (global-set-key (kbd "<f1>") 'moom-move-frame-to-edge-top)
    (global-set-key (kbd "<f2>") 'moom-cycle-frame-height)
    (global-set-key (kbd "C-c C-<") 'moom-move-frame-to-edge-left)
    (global-set-key (kbd "C-c C->") 'moom-move-frame-to-edge-right)
    (global-set-key (kbd "M-2") 'moom-move-frame-to-center)
    (global-set-key (kbd "M-9") 'moom-cycle-monitors)
    (global-set-key (kbd "M-<f2>") 'moom-toggle-frame-maximized)
    (moom-recommended-keybindings 'all)
    (setq moom-font-ja-scale 1.0)
    (moom-reset)
    (moom-font-resize 20))

  ;; org-agenda
  (with-eval-after-load "org-agenda"
    (setq org-agenda-files
          (mapcar (lambda (arg)
                    (concat (getenv "SYNCROOT") "/org/" arg))
                  '("next.org" "patent.org" "vvc.org" "video.org"
                    "reports.org"))))

  (with-eval-after-load "org"
    (setq org-startup-truncated nil
		      org-hide-leading-stars t
          org-use-speed-commands t
          org-adapt-indentation t
          org-list-allow-alphabetical t)
    (define-key org-mode-map (kbd "C-M-t") 'beginning-of-buffer)

		(when (version< (org-version) "9.4.6")
			(defvaralias 'org-speed-commands 'org-speed-commands-user))
		(add-to-list 'org-speed-commands '("d" org-todo "DONE"))
		(add-to-list 'org-speed-commands
								 '("$" call-interactively 'org-archive-subtree))
		(setq org-log-done 'time)
		(add-to-list 'org-modules 'org-id)
		(delq 'ol-gnus org-modules)

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
    (add-hook 'org-mode-hook #'turn-on-font-lock))

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
  )
