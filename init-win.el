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
  (defun my-open-ie ()
    (interactive)
    (shell-command-to-string "open iexplore"))
  (global-set-key (kbd "C-M-1") #'my-open-ie)

  ;; Cursor color
  (set-cursor-color (plist-get my-cur-color-ime :off))

  ;; Local setting
  (with-eval-after-load "moom"
    (global-set-key (kbd "C-1") 'moom-move-frame-to-edge-top)
    (global-set-key (kbd "C-2") 'moom-cycle-frame-height)
    (moom-font-resize 16)
    (setq moom-font-ja-scale 1.0))

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
  (global-set-key (kbd "S-SPC") 'my-toggle-ime))
