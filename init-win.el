;; The initial file that will be loaded first in w32 environment .
(when (eq system-type 'windows-nt)
  (setq byte-compile-warnings '(obsolete))
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

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

  ;; For IME module (do not load under postpone.el)
  (unless noninteractive
    ;; Language, will override default-input-method
    (set-language-environment "Japanese")

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
    (wrap-function-to-control-ime 'map-y-or-n-p nil nil)))

(with-eval-after-load "postpone"
  (defun my-open-w32-explore ()
    (interactive)
    (shell-command-to-string "open ."))
  (global-set-key (kbd "C-M-<return>") #'my-open-w32-explore)

  ;; Cursor color
  (set-cursor-color (plist-get my-cur-color-ime :off))

  ;; Local setting
  (with-eval-after-load "moom"
    (global-set-key (kbd "C-1") 'moom-move-frame-to-edge-top)
    (global-set-key (kbd "C-2") 'moom-cycle-frame-height)
    ;;(moom-font-resize 16)
    ;;(setq moom-user-margin '(0 -6 0 0))
    (setq moom-font-ja-scale 1.0)
    (moom-reset))

  ;; org-agenda
  (with-eval-after-load "org-agenda"
    (setq org-agenda-files
          (mapcar (lambda (arg)
                    (concat (getenv "SYNCROOT") "/org/" arg))
                  '("next.org" "patent.org" "vvc.org" "video.org"
                    "reports.org"))))

  (with-eval-after-load "org"
    (remove-hook 'org-tab-first-hook 'my-org-hide-drawers) ;; error on v9.4
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
    (run-with-idle-timer 0.4 t #'my-ns-org-heading-auto-ascii))

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
