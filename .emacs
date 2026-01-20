;; -*- lexical-binding: t; -*-
;;					    Takaaki ISHIKAWA <takaxp@ieee.org>
;;					    https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                             TODO/DONE/FIXME

(with-eval-after-load "org"
  (keymap-set org-mode-map "C-c c" 'ignore)

  ;; org-agenda	に表示される deadline の配色
  ;; org-deadline-warning-days =8 の時，{1.0, 0.75, 0.0}ならば，
  ;; In 1-2 day が紫で，In 3-8 day が白
  ;;
  (setq org-agenda-deadline-faces
	'((1.0 . org-imminent-deadline) ;; '((t :inherit org-warning))
	  (0.75 . org-upcoming-deadline) ;; (:foreground "red")
	  (0.0 . org-upcoming-distant-deadline))) ;; '((t :inherit org-default))


  ;; (advice-add 'org-assert-version :override #'ignore)
  ;; (require 'org-phscroll nil t)

  ;; M-x my-toggle-org-pin-subtree
  (defun my-revisit-current-file ()
    ;; プロパティにフラグ設定がある場合に常にコンテンツ表示する
    (interactive)
    (let ((cb buffer-file-name))
      (kill-buffer (current-buffer))
      ;; (sleep-for 1)
      (find-file cb)))

  )

(with-eval-after-load "recentf"
  (defun my-recentf-cleanup-silence ()
    (interactive)
    (my-print-message `("<recentf> i/m" ,(length recentf-list)
			"/" ,recentf-max-saved-items))
    (let ((message-log-max nil))
      (recentf-cleanup))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boot mode selection
;; Note: `load-path' and `exec-path' are both configured in early-init.el
(cond
 ;; minimal boot or DOOM Emacs (use toggle-doom.sh to switch)
 (nil
  (when (boundp 'ns-command-modifier) (setq ns-command-modifier 'meta))
  (when (and (memq window-system '(ns mac))
	     (fboundp 'mac-get-current-input-source))
    ;; "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese" for Big Sur
    (custom-set-variables
     '(mac-default-input-source "com.google.inputmethod.Japanese.base"))
    (mac-input-method-mode 1)
    (global-set-key (kbd "M-SPC") 'mac-ime-toggle)
    (global-set-key (kbd "S-SPC") 'mac-ime-toggle)))

 ;; To test the latest org
 (nil
  (add-to-list 'load-path (expand-file-name "~/devel/git/org-mode/lisp"))
  (add-to-list 'load-path (expand-file-name "~/devel/git/org-tree-slide"))
  (setq org-agenda-files '("~/Desktop/test/hoge.org")))

 ;; Debug
 (nil
  (add-to-list 'load-path (expand-file-name "~/.local/config"))
  (add-to-list 'load-path (expand-file-name "~/devel/git/org-mode/lisp"))
  (add-to-list 'load-path
	       (expand-file-name "~/devel/git/org-mode/contrib/lisp"))
  (add-to-list 'load-path my-package-dir) ;; defined in early-init.el
  (require 'my-debug))

 ;; minimum
 (nil (load (concat user-emacs-directory "min/init.el")))

 ;; configured with use-package (TRIAL)
 (nil (load (concat user-emacs-directory "use-init.el")))

 ;; Spacemacs
 (nil (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el")))

 ;; Normal mode. see also init-eval.el
 (t
  (setq debug-on-error nil
	postpone-verbose nil
	my-toggle-modeline-global t ;; 'doom ;; {nil, t, 'doom}
	my-frame-appearance nil     ;; {nil, 'dark, 'light}
	my-skip-check-autoload-file t)

  (defvar my-disabled-packages nil) ;; '(("web-mode" . nil)("org" . nil))
  (defvar my-ad-require-p nil
    "If non-nil, override `require' and `load' to show loading times.")
  (defvar my-profiler-p nil
    "If non-nil, use built-in profiler.el.")
  (defvar my-loading-profile-p nil
    "If non-nil, show ticks while booting.")
  (defvar my-secure-boot nil
    "Ensure to start Emacs.  If non-nil, postpone and session are disabled.")

  (setq measure-exec-time-list '(
				 my-private-conf-activate
				 my-org-babel-load-activate
				 my-org-modules-activate
				 my-org-agenda-prepare-buffers
				 ;; my-show-org-buffer
				 ))
  ;; (require 'my-eshell nil t)
  (require 'init nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (package-initialize) ;; do not delete this line here for previous versions
