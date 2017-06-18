;; recentf (C-M-r) / helm-swoop (M-s M-s) / isearch (C-s) / bm <f10>, C-<f10>
;; helm-locate (C-c f l) / org-grep (C-M-g) / ag (C-M-f) / google-this (C-c f g)
;; fullscreen <f11> / double-width (C-c f d)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suppress exporting of custom-set-variables (from 25.1)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; or you can use `exec-path-from-shell'.
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
;; Utility // M-x list-load-path-shadows ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-path-setter (path-list target-path)
  (dolist (x path-list) (add-to-list target-path (file-name-as-directory x))))
(defconst cask-package-dir
  (format "~/.emacs.d/.cask/package/%s" emacs-version))
;; (format "%s.%s" emacs-major-version emacs-minor-version)))

;; (1) theme-path
(load-path-setter `(,cask-package-dir) 'custom-theme-load-path)
;; (2) exec-path
(load-path-setter
 `("/usr/bin" "/usr/local/bin"
   ,(expand-file-name ".cask/bin")
   ,(expand-file-name "devel/git/tern/bin")
   ,(expand-file-name "Dropbox/emacs.d/bin")
   "/Applications/UpTex.app/teTeX/bin"
   "/Applications/LibreOffice.app/Contents/MacOS/") 'exec-path)
;; (3) load-path for { debug | normal } booting
(cond
 (debug (load-path-setter '("~/Dropbox/config") 'load-path)
        (require 'my-debug))
 (t (let*
        ((p "~/Dropbox/emacs.d/") (g "~/devel/git/") (mg "~/devel/mygit")
         (od "org-mode")
         ;; (od "org-9.0")
         (l `("~/Dropbox/emacs.d/config" "~/Dropbox/config" ,p
              ,(concat p "livemirror")
              ,(concat g od "/lisp") ,(concat g od "/contrib/lisp"))))
      (load-path-setter l 'load-path))
    (if (not use-cask)
        (load-path-setter `(,cask-package-dir) 'load-path)
      (when (or (require 'cask "~/.cask/cask.el" t)
                (require 'cask "/usr/local/opt/cask/cask.el" t)) ;; Homebrew
        (when (fboundp 'cask-initialize) (cask-initialize))) ;; 800[ms]
      (when (require 'pallet nil t)
        (when (fboundp 'pallet-mode) (pallet-mode t))))       ;; 30[ms]
    (require 'init nil t)         ;; Less than 500[ms], Cocoa: 1000[ms]
    ;; (require 'init-org nil t)
    (require 'my-eshell nil t)    ;; 0[ms]
    (require 'my-mail nil t)      ;; 0[ms]
    (require 'private nil t)))    ;; 0[ms] This package depends on init.el

