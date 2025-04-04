;; (message "--- Window system (ns mac) %s, display-graphic-p %s, File %s" window-system (display-graphic-p) early-init-file)
;; References:
;; https://raw.githubusercontent.com/hlissner/doom-emacs/develop/early-init.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (unless (getenv "LIBRARY_PATH")
;;   (setenv "LIBRARY_PATH"
;;           (string-join
;;            '("/opt/homebrew/opt/gcc/lib/gcc/14"
;;              "/opt/homebrew/opt/libgccjit/lib/gcc/14"
;;              "/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin24/14")
;;            ":")))
;; for Intel mac user, replace "aarch64-apple-darwin24" with "x86_64-apple-darwin24".

(unless noninteractive
  (defvar my-early-start (current-time))
  (defvar my-early-init
    (format "%searly-init.el" (expand-file-name user-emacs-directory)))

  (message "Loading %s..." my-early-init)

  (setq package-enable-at-startup nil
        frame-inhibit-implied-resize t)
  (with-eval-after-load "moom"
    (setq frame-inhibit-implied-resize nil))

  (set-scroll-bar-mode nil)
  (menu-bar-mode -1)
  (tab-bar-mode -1)
  (tool-bar-mode -1))

;; setenv "SYNCROOT"
(setenv "SYNCROOT" (concat (getenv "HOME") "/Dropbox" ))

(setq gc-cons-threshold (* 16 1024 1024)) ;; [MB]
;; (setq garbage-collection-messages t)
;; (defvar my-gc-last 0.0)
;; (add-hook 'post-gc-hook
;;           #'(lambda ()
;;               (message "GC! > %.4f[sec]" (- gc-elapsed my-gc-last))
;;               (setq my-gc-last gc-elapsed)))

;; Build and check `my-package-dir'
(defvar my-package-dir nil)
(defvar my-use-el-get emacs-version ;; nil
  "If version number is provided, Emacs uses packages installed via el-get.")
(defvar my-elget-package-dir
  (format (expand-file-name "~/.emacs.d/%s/packages") my-use-el-get))
(when my-use-el-get
  (setq my-package-dir my-elget-package-dir))
(unless (file-directory-p my-package-dir)
  (user-error "%s does NOT exist. Run setup script first" my-package-dir))

(defun my-path-setter (path-list target-path)
  "Utility function to set PATH-LIST to TARGET-PATH."
  (dolist (x path-list)
    (add-to-list target-path (file-name-as-directory x))))

;; (1) theme-path
(my-path-setter
 `(,my-package-dir ,(expand-file-name "~/.emacs.d/lisp"))
 'custom-theme-load-path)

;; (2) exec-path
(my-path-setter
 `("/usr/bin" "/usr/local/bin" "/opt/homebrew/bin"
   ,(expand-file-name "~/.cask/bin")
   ,(expand-file-name "~/devel/git/tern/bin")
   ,(expand-file-name "~/.go/bin")
   ,(expand-file-name "~/Dropbox/emacs.d/bin")
   ,(expand-file-name "~/Dropbox/local/scripts")
   "/usr/local/opt/llvm/bin"
   "C:/cygwin64/bin" "C:/msys64/usr/bin" "C:/msys64/mingw64/bin"
   "/Applications/UpTex.app/teTeX/bin"
   "/Applications/UpTeX.app/Contents/Resources/TEX/texbin"
   "/Applications/LibreOffice.app/Contents/MacOS/"
   "/Applications/qt_color_picker.app/Contents/MacOS/"
   "/opt/homebrew/opt/imagemagick@6/bin")
 'exec-path)

(setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "GOPATH" (concat (getenv "HOME") "/.go"))
;; you may want to use exec-path-from-shell.el.

;; (3) load-path, 拡張パッケージにパスを通す
;; M-x list-load-path-shadows
(let* ((git-path (expand-file-name "~/devel/git/"))
       (org-path "org-mode")
       (pl `(,(expand-file-name "~/.emacs.d/lisp")
             ,my-package-dir ;; may include a path to org
             ,(concat git-path org-path "/lisp") ;; override the path to org
             ,(concat git-path org-path "/contrib/lisp"))))
  (my-path-setter pl 'load-path))

(unless noninteractive
  (defvar my-early-end (current-time))
  (message "Loading %s...done" my-early-init))
