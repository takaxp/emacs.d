;; helm-swoop(M-s M-s), bm(<f10>,C-<f10>), helm-locate(C-M-l), org-grep(C-M-g)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst my-before-load-init-time (current-time)
  "Starting point to calculate Emacs booting time.  see `my-load-init-time'.")
(defconst my-ad-require-p nil
  "If non-nil, override `require' and `load' to show loading times.")
(defconst my-profiler-p nil
  "If non-nil, use built-in profiler.el.")
(defconst my-loading-profile-p nil
  "If non-nil, show tick while booting.  Do not use `my-profiler-p' with this.")
(defconst my-boot-type 'default
  "Boot menu selection: {debug, test, default, spacemacs}.")
(defconst my-loading-packages nil)
(defvar my-use-el-get emacs-version ;; nil
  "If version number is provided, use packages installed via el-get.")
(setq postpone-verbose nil
      my-toggle-modeline-global 'doom ;; 'doom ;; {nil, t, 'doom}
      my-frame-appearance nil ;; {nil, 'dark, 'light}
      my-skip-check-autoload-file t
      debug-on-error nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when my-profiler-p
  (profiler-start 'cpu+mem))
;; Suppress exporting of custom-set-variables (25.1 or later)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(defun my-path-setter (path-list target-path)
  "Utility function to set PATH-LIST to TARGET-PATH."
  (dolist (x path-list)
    (add-to-list target-path (file-name-as-directory x))))
(defvar my-package-dir nil)
(defvar my-cask-package-dir
  (format "~/.emacs.d/.cask/package/%s" emacs-version))
(defvar my-elget-package-dir
  (concat "~/.emacs.d/"
          (if my-use-el-get my-use-el-get emacs-version) "/packages"))

;; Build and check `my-package-dir'
(setq my-package-dir my-cask-package-dir)
(when my-use-el-get
  (setq my-package-dir my-elget-package-dir))
(unless (file-directory-p my-package-dir)
  (user-error "%s does NOT exist" my-package-dir))

;; (1) theme-path
(my-path-setter
 `(,my-package-dir "~/.emacs.d/lisp") 'custom-theme-load-path)

;; (2) exec-path
(my-path-setter
 `("/usr/bin" "/usr/local/bin"
   ,(expand-file-name ".cask/bin")
   ,(expand-file-name "devel/git/tern/bin")
   ,(expand-file-name "Dropbox/emacs.d/bin")
   "/usr/local/opt/llvm/bin"
   "/Applications/UpTex.app/teTeX/bin"
   "/Applications/UpTeX.app/Contents/Resources/TEX/texbin"
   "/Applications/LibreOffice.app/Contents/MacOS/"
   "/Applications/qt_color_picker.app/Contents/MacOS/"
   "/usr/local/opt/imagemagick@6/bin")
 'exec-path)
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
;; you may want to use exec-path-from-shell.el.

;; (3) load-path for { nil | debug | test } booting
;; M-x list-load-path-shadows
(cond
 ((eq my-boot-type 'default)
  (let* ((g "~/devel/git/")
         (od "org-mode")
         (l `("~/Dropbox/config"
              "~/.emacs.d/lisp"
              ,(concat g od "/lisp")
              ,(concat g od "/contrib/lisp")
              ,my-package-dir)))
    (my-path-setter l 'load-path))

  (when my-ad-require-p
    (load "~/Dropbox/emacs.d/config/init-ad.el" nil t))
  ;; (load "~/Dropbox/emacs.d/config/init-chart.el" nil t)
  ;; (require 'init-eval nil t)
  ;; (require 'utility nil t) ;; 5[ms]
  (require 'utility-autoloads nil t) ;; 2[ms]
  (require 'init nil t)
  ;; (require 'init-org nil t)
  (require 'my-eshell nil t)
  (require 'my-mail nil t)
  (require 'private nil t)
  (when my-profiler-p
    (profiler-report)))
 ((eq my-boot-type 'debug)
  (my-path-setter
   '("~/Dropbox/config")
   'load-path)
  (require 'my-debug))
 ((eq my-boot-type 'test)
  (my-path-setter
   `("~/Dropbox/config"
     "~/devel/git/org-mode/lisp"
     "~/devel/git/org-mode/contrib/lisp"
     ,my-package-dir)
   'load-path)
  (require 'my-debug))
 ((eq my-boot-type 'spacemacs) ;; FIXME
  (my-path-setter
   '("~/.spacemacs.d")
   'load-path)
  (load "~/.spacemacs.d/init.el" nil t))
 (t nil))

;; Hide package.el, could be removed
(when (> 27 emacs-major-version)
  (defun ad:package--ensure-init-file ()
    (setq package--init-file-ensured t))
  (advice-add 'package--ensure-init-file :override
              #'ad:package--ensure-init-file))

;; (my-kill-emacs-hook-show)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff を取る前に tabify スペースをタブに変換する．今は全てスペース置換中．
;; この設定でファイルを一度編集後に，M-x tabify しないとだめ．
;; (add-hook 'emacs-lisp-mode-hook
;;           '(lambda ()
;;              (setq indent-tabs-mode t)
;;              (setq tab-width 8)
;;              (setq indent-line-function 'lisp-indent-line)))
