;; (message "--- Window system (ns mac) %s, display-graphic-p %s, File %s" window-system (display-graphic-p) early-init-file)
;; References:
;; https://raw.githubusercontent.com/hlissner/doom-emacs/develop/early-init.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(tool-bar-mode -1)

(setq gc-cons-threshold (* 16 1024 1024)) ;; [MB]
(with-eval-after-load "postpone"
  (setq gc-cons-threshold (* 256 1024 1024)))
;; (setq garbage-collection-messages t)
(defvar my-gc-last 0.0)
(add-hook 'post-gc-hook
          #'(lambda ()
              (message "GC! > %.4f[sec]" (- gc-elapsed my-gc-last))
              (setq my-gc-last gc-elapsed)))

(message "Loading %s...done" my-early-init)
