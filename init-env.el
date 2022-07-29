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
(setq debug-on-error nil
      postpone-verbose nil
      my-toggle-modeline-global t ;; 'doom ;; {nil, t, 'doom}
      my-frame-appearance nil ;; {nil, 'dark, 'light}
      my-skip-check-autoload-file t)
(unless (getenv "SYNCROOT")
  (setenv "SYNCROOT" (concat (getenv "HOME") "/Dropbox" )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; swiper(M-s M-s), bm(<f10>,C-<f10>), helm-locate(C-M-l), org-grep(C-M-g)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when my-profiler-p (profiler-start 'cpu+mem))
;; Suppress exporting of custom-set-variables (25.1 or later)
(setq custom-file (locate-user-emacs-file "custom.el"))
(defun my-path-setter (path-list target-path)
  "Utility function to set PATH-LIST to TARGET-PATH."
  (dolist (x path-list)
    (add-to-list target-path (file-name-as-directory x))))
(defvar my-cask-package-dir
  (format "~/.emacs.d/.cask/package/%s" emacs-version))
(defvar my-elget-package-dir
  (format "~/.emacs.d/%s/packages"
          (if my-use-el-get my-use-el-get emacs-version)))

;; Build and check `my-package-dir'
(defvar my-package-dir my-cask-package-dir)
(when my-use-el-get
  (setq my-package-dir my-elget-package-dir))
(unless (file-directory-p my-package-dir)
  (user-error "%s does NOT exist. Run setup script first" my-package-dir))

;; (1) theme-path
(my-path-setter
 `(,my-package-dir "~/.emacs.d/lisp") 'custom-theme-load-path)

;; (2) exec-path
(my-path-setter
 `("/usr/bin" "/usr/local/bin" "/opt/homebrew/bin"
   ,(expand-file-name "~/.cask/bin")
   ,(expand-file-name "~/devel/git/tern/bin")
   ,(expand-file-name "~/.go/bin")
   ,(expand-file-name "~/Dropbox/emacs.d/bin")
   ,(expand-file-name "~/Dropbox/scripts")
   "/usr/local/opt/llvm/bin"
   "C:/cygwin64/bin" "C:/msys64/usr/bin" "C:/msys64/mingw64/bin"
   "/Applications/UpTex.app/teTeX/bin"
   "/Applications/UpTeX.app/Contents/Resources/TEX/texbin"
   "/Applications/LibreOffice.app/Contents/MacOS/"
   "/Applications/qt_color_picker.app/Contents/MacOS/"
   "/usr/local/opt/imagemagick@6/bin")
 'exec-path)
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "GOPATH" (concat (getenv "HOME") "/.go"))
;; you may want to use exec-path-from-shell.el.

;; (3) load-path for { nil | debug | test } booting
;; M-x list-load-path-shadows
(defvar my-required-libraries nil) ;; will be used in `init' and `init-async'.
(cond
 ((eq my-boot-type 'default)
  (let* ((g "~/devel/git/")
         (od "org-mode")
         (l `("~/Dropbox/config"
              "~/.emacs.d/lisp"
              ,my-package-dir ;; may include a path to org
              ,(concat g od "/lisp") ;; override the path to org
              ,(concat g od "/contrib/lisp")
              )))
    (my-path-setter l 'load-path))

  (when my-ad-require-p
    (load "~/Dropbox/emacs.d/config/init-ad.el" nil t))
  ;; (load "~/Dropbox/emacs.d/config/init-chart.el" nil t)
  ;; (require 'use-package nil t) ;; 24[ms]
  ;; (require 'leaf nil t) ;; 2[ms]
  ;; (when (require 'benchmark-init nil t)
  ;;   (add-hook 'after-init-hook #'benchmark-init/deactivate))

  (require 'init nil t)
  (unless noninteractive
    (with-eval-after-load "postpone"
      (when (and window-system
                 (require 'init-async nil t))
        (my-delete-old-backup 5)
        (when my-skip-check-autoload-file
          (my-find-missing-packages 10)))
      (require 'my-eshell nil t)
      (require 'my-mail nil t))))
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

(when (version< "28.0" emacs-version)
  ;; Native Compiling の最終のワーニング等をウィンドウに出さない
  (setq native-comp-async-report-warnings-errors nil)
  ;; define-obsolete-variable-alias の上書き補正
  (defmacro define-obsolete-variable-alias (obsolete-name
                                            current-name
					                                  &optional when docstring)
    ""
    (declare (doc-string 4)
             (advertised-calling-convention
              (obsolete-name current-name when &optional docstring) "23.1"))
    `(progn
       (defvaralias ,obsolete-name ,current-name ,docstring)
       (dolist (prop '(saved-value saved-variable-comment))
         (and (get ,obsolete-name prop)
              (null (get ,current-name prop))
              (put ,current-name prop (get ,obsolete-name prop))))
       (make-obsolete-variable ,obsolete-name ,current-name ,when)))
  ;; define-obsolete-function-alias の上書き補正
  (defmacro define-obsolete-function-alias (obsolete-name
                                            current-name
					                                  &optional when docstring)
    ""
    (declare (doc-string 4)
             (advertised-calling-convention
              (obsolete-name current-name when &optional docstring) "23.1"))
    `(progn
       (defalias ,obsolete-name ,current-name ,docstring)
       (make-obsolete ,obsolete-name ,current-name ,when))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hide package.el, could be removed
;; (unless (version< "27.0" (format "%s" emacs-major-version))
;;   (setq byte-compile-warnings '(cl-functions)))

;; (defun ad:package--ensure-init-file ()
;;   (setq package--init-file-ensured t))
;; (advice-add 'package--ensure-init-file :override
;;             #'ad:package--ensure-init-file))

;; (my-kill-emacs-hook-show)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff を取る前に tabify スペースをタブに変換する．今は全てスペース置換中．
;; この設定でファイルを一度編集後に，M-x tabify しないとだめ．
(when nil
  (add-hook 'emacs-lisp-mode-hook
            #'(lambda ()
                (setq indent-tabs-mode t)
                (setq tab-width 8)
                (setq indent-line-function 'lisp-indent-line)
                )))

(when my-profiler-p (profiler-report))
;; end of init-env.el
