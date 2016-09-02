;; recentf (C-M-r) / helm-swoop (M-s M-s) / isearch (C-s) / bm <f10>, C-<f10>
;; helm-locate (C-c f l) / org-grep (C-M-g) / ag (C-M-f) / google-this (C-c f g)
;; fullscreen <f11> / double-width (C-c f d)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; or you can use `exec-path-from-shell'.
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; Utility // M-x list-load-path-shadows ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-path-setter (path-list target-path)
  (dolist (x path-list) (add-to-list target-path (file-name-as-directory x))))

;; (1) theme-path
(load-path-setter
 '("~/.emacs.d/.cask/package") 'custom-theme-load-path)

;; (2) exec-path
(load-path-setter
 '("/usr/bin" "/usr/local/bin" "/Users/taka/.cask/bin"
   "/Users/taka/Dropbox/emacs.d/bin" "/Applications/UpTex.app/teTeX/bin"
   "/Applications/LibreOffice.app/Contents/MacOS/" 
   "/Users/taka/devel/git/tern/bin") 'exec-path)

;; (3) load-path for { debug | normal } booting
(cond
 (debug (load-path-setter '("~/Dropbox/config") 'load-path)
        (require 'my-debug))
 (t (let*
        ((p "~/Dropbox/emacs.d/") (g "~/devel/git/")
         (od "org-mode")
         ;; (od "org-stable")
         (l `("~/Dropbox/emacs.d/config" "~/Dropbox/config" ,p
              ,(concat g od "/lisp") ,(concat g od "/contrib/lisp"))))
      (load-path-setter l 'load-path))
    (if (not use-cask)
        (load-path-setter '("~/.emacs.d/.cask/package") 'load-path)
      (when (require 'cask "~/.cask/cask.el" t)
        (when (fboundp 'cask-initialize) (cask-initialize))) ;; 800[ms]
      (when (require 'pallet nil t)
        (when (fboundp 'pallet-mode) (pallet-mode t)))) ;; 30[ms]

    (require 'init nil t)      ;; Less than 500[ms], Cocoa: 1000[ms]
    (require 'my-eshell nil t) ;; 0[ms]
    (require 'my-mail nil t)   ;; 0[ms]
    (require 'private nil t)   ;; 0[ms] This package depends on init.el
    ))
