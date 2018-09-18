;; recentf (C-M-r) / helm-swoop (M-s M-s) / isearch (C-s) / bm <f10>, C-<f10>
;; helm-locate (C-M-l) / org-grep (C-M-g) / ag (C-M-f) / google-this (C-c f g)
;; fullscreen <f11> / double-width (C-c f d)
;; helm-projectile: (C-c p f), (C-c p p), (C-c p h)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst my-before-load-init-time (current-time)
  "Starting point to calculate Emacs booting time.  see `my-load-init-time'.")
(defconst my-ad-require-p nil
  "If non-nil, override `require' and `load' to show loading times.")
(defconst my-profiler-p nil
  "If non-nil, use built-in profiler.el.")
(defconst my-boot-type 'default
  "Boot menu selection: {debug, test, default}.")
(defconst my-loading-packages nil) ;; `my-autoload-file-check' shall be nil.
;; (setq my-loading-packages '(("moom" . nil) ("moom-font" . nil)))
(setq debug-on-error nil
      my-skip-autoload-file-check t
      postpone-verbose nil) ;; see postpone.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when my-profiler-p
  (profiler-start 'cpu+mem))
;; Suppress exporting of custom-set-variables (25.1 or later)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(defun my-path-setter (path-list target-path)
  "Utility function to set PATH-LIST to TARGET-PATH."
  (dolist (x path-list)
    (add-to-list target-path (file-name-as-directory x))))

;; (1) theme-path
(defconst my-cask-package-dir
  (format "~/.emacs.d/.cask/package/%s" emacs-version))
(my-path-setter
 `(,my-cask-package-dir)
 'custom-theme-load-path)

;; (2) exec-path
(my-path-setter
 `("/usr/bin" "/usr/local/bin"
   ,(expand-file-name ".cask/bin")
   ,(expand-file-name "devel/git/tern/bin")
   ,(expand-file-name "Dropbox/emacs.d/bin")
   "/usr/local/opt/llvm/bin"
   "/Applications/UpTex.app/teTeX/bin"
   "/Applications/LibreOffice.app/Contents/MacOS/"
   "/Applications/qt_color_picker.app/Contents/MacOS/"
   "/usr/local/opt/imagemagick@6/bin")
 'exec-path)
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
;; you may want to use exec-path-from-shell.el.

;; (3) load-path for { nil | debug | test } booting
;; M-x list-load-path-shadows
(defvar my-default-load-path nil)
(defvar my-batch-build nil) ;; see also init-eval.el
(cond
 ((equal my-boot-type 'default)
  (let* ((g "~/devel/git/")
         (od "org-mode")
         (l `("~/Dropbox/config"
              "~/.emacs.d/lisp"
              ,(concat g od "/lisp")
              ,(concat g od "/contrib/lisp")
              ,my-cask-package-dir)))
    (my-path-setter l 'load-path)
    (setq my-default-load-path load-path))
  (when my-ad-require-p
    (load "~/Dropbox/emacs.d/config/init-ad.el" nil t))
  (require 'init nil t)
  (require 'my-eshell nil t)
  (require 'my-mail nil t)
  (require 'private nil t)
  (when my-profiler-p
    (profiler-report)))
 ((equal my-boot-type 'debug)
  (my-path-setter
   '("~/Dropbox/config")
   'load-path)
  (require 'my-debug))
 ((equal my-boot-type 'test)
  (my-path-setter
   `("~/Dropbox/config"
     "~/devel/git/org-mode/lisp"
     "~/devel/git/org-mode/contrib/lisp"
     ,my-cask-package-dir)
   'load-path)
  (require 'my-debug))
 (t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities to manage packages
(defun my-list-packages ()
  "Call \\[paradox-list-packages]' if available instead of \\[list-packages]."
  (interactive)
  (my-setup-cask)
  (if (fboundp 'paradox-list-packages)
      (paradox-list-packages nil)
    (list-packages nil)))

(defun my-setup-cask ()
  "Override `load-path' to use cask."
  (when (or (require 'cask "/usr/local/opt/cask/cask.el" t)
            (require 'cask "~/.cask/cask.el" t))
    (setq load-path (cask-load-path (cask-initialize)))))

(defun my-setup-package-el ()
  "Setting up for installing packages via built-in package.el.
Downloaded packages will be stored under ~/.eamcs.d/elpa."
  (when (require 'package nil t)
    (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                        (not (gnutls-available-p))))
           (proto (if no-ssl "http" "https")))
      (add-to-list 'package-archives
                   (cons "melpa" (concat proto "://melpa.org/packages/")) t)
      (add-to-list 'package-archives
                   (cons "takaxp" "~/devel/git/melpa/packages/") t))
    (package-initialize)))

(defun my-reset-load-path ()
  "Revert `load-path' to `my-default-load-path'."
  (shell-command-to-string "~/Dropbox/emacs.d/bin/update-cask.sh link")
  (setq load-path my-default-load-path)
  (message "--- Reverted to the original `load-path'."))

(with-eval-after-load "paradox"
  (defun advice:paradox-quit-and-close (kill)
    (my-reset-load-path))
  (advice-add 'paradox-quit-and-close :after #'advice:paradox-quit-and-close))

(defun my-kill-emacs-hook-show ()
  "Test Emacs killing sequence."
  (add-hook 'after-init-hook
            '(lambda () (message "1: %s" kill-emacs-hook)) t)
  (with-eval-after-load "postpone"
    (message "2: %s" kill-emacs-hook))
  (defun my-kill-emacs ()
    (switch-to-buffer "*Messages*")
    (message "3: %s" kill-emacs-hook)
    (y-or-n-p "Sure? "))
  (add-hook 'kill-emacs-hook 'my-kill-emacs))
;; (my-kill-emacs-hook-show)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff を取る前に tabify スペースをタブに変換する．今は全てスペース置換中．
;; この設定でファイルを一度編集後に，M-x tabify しないとだめ．
;; (add-hook 'emacs-lisp-mode-hook
;;           '(lambda ()
;;              (setq indent-tabs-mode t)
;;              (setq tab-width 8)
;;              (setq indent-line-function 'lisp-indent-line)))
