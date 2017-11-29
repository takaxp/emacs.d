;; recentf (C-M-r) / helm-swoop (M-s M-s) / isearch (C-s) / bm <f10>, C-<f10>
;; helm-locate (C-M-l) / org-grep (C-M-g) / ag (C-M-f) / google-this (C-c f g)
;; fullscreen <f11> / double-width (C-c f d)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst before-load-init-time (current-time)) ;; see my:load-init-time in init
(defconst my:profiler nil)
(defconst ad-require nil)
(defconst loading-packages nil)
;; (setq loading-packages '(("org" . nil)))
(defconst my:debug nil)
(setq debug-on-error nil)
(defvar batch-build nil) ;; see also init-eval.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when my:profiler (profiler-start 'cpu+mem))
;; Suppress exporting of custom-set-variables (from 25.1)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; or you can use `exec-path-from-shell'.
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; Utility // M-x list-load-path-shadows ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-path-setter (path-list target-path)
  (dolist (x path-list) (add-to-list target-path (file-name-as-directory x))))

;; (1) theme-path
(defconst cask-package-dir (format "~/.emacs.d/.cask/package/%s" emacs-version))
(load-path-setter `(,cask-package-dir) 'custom-theme-load-path)

;; (2) exec-path
(load-path-setter
 `("/usr/bin" "/usr/local/bin"
   ,(expand-file-name ".cask/bin")
   ,(expand-file-name "devel/git/tern/bin")
   ,(expand-file-name "Dropbox/emacs.d/bin")
   "/Applications/UpTex.app/teTeX/bin"
   "/Applications/LibreOffice.app/Contents/MacOS/"
   "/Applications/qt_color_picker.app/Contents/MacOS/"
   "/usr/local/opt/imagemagick@6/bin")
 'exec-path)

;; (3) load-path for { debug | normal } booting
(defconst init-load-path nil)
(cond
 (my:debug
  (load-path-setter '("~/Dropbox/config") 'load-path)
  (require 'my-debug))
 (t
  (let* ((p "~/Dropbox/emacs.d/")
         (g "~/devel/git/")
         (od "org-mode")
         ;; (od "org-9.0")
         (l `("~/Dropbox/emacs.d/config" "~/Dropbox/config" ,p
              ;; ,(concat p "livemirror") ,(concat g "git-complete")
              ,(concat g od "/lisp") ,(concat g od "/contrib/lisp"))))

    (load-path-setter l 'load-path)
    (load-path-setter `(,cask-package-dir) 'load-path)
    (setq init-load-path load-path))

  (when ad-require (require 'init-ad nil t))
  (require 'init nil t)
  ;; (require 'init-test nil t)
  (require 'my-eshell nil t)
  (require 'my-mail nil t)
  (require 'private nil t)
  (when my:profiler (profiler-report))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my:list-packages ()
  (interactive)
  (my:setup-cask)
  (if (fboundp 'paradox-list-packages)
      (paradox-list-packages nil)
    (list-packages nil)))

(defun my:setup-cask ()
  (interactive)
  (when (or (require 'cask "/usr/local/opt/cask/cask.el" t)
            (require 'cask "~/.cask/cask.el" t))
    (setq load-path (cask-load-path (cask-initialize)))))

(defun my:reset-load-path ()
  (interactive)
  (shell-command-to-string "~/Dropbox/emacs.d/bin/update-cask.sh link")
  (setq load-path init-load-path)
  (message "--- Reverted to the original `load-path'."))

(with-eval-after-load "paradox"
  (defun advice:paradox-quit-and-close (kill)
    (my:reset-load-path))
  (advice-add 'paradox-quit-and-close :after #'advice:paradox-quit-and-close))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff を取る前に tabify スペースをタブに変換する．今は全てスペース置換中．
;; この設定でファイルを一度編集後に，M-x tabify しないとだめ．
;; (add-hook 'emacs-lisp-mode-hook
;;           '(lambda ()
;;              (setq indent-tabs-mode t)
;;              (setq tab-width 8)
;;              (setq indent-line-function 'lisp-indent-line)))
