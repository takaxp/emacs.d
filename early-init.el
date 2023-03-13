;; (message "--- Window system (ns mac) %s, display-graphic-p %s, File %s" window-system (display-graphic-p) early-init-file)
;; References:
;; https://raw.githubusercontent.com/hlissner/doom-emacs/develop/early-init.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-early-init
  (format "%searly-init.el" (expand-file-name user-emacs-directory)))

(message "Loading %s..." my-early-init)
;; (unless (getenv "LIBRARY_PATH")
;;   (setenv "LIBRARY_PATH"
;;           (string-join
;;            '("/opt/homebrew/opt/gcc/lib/gcc/12"
;;              "/opt/homebrew/opt/libgccjit/lib/gcc/12"
;;              "/opt/homebrew/opt/gcc/lib/gcc/12/gcc/aarch64-apple-darwin22/12")
;;            ":")))

(setq package-enable-at-startup nil
      frame-inhibit-implied-resize t)
(with-eval-after-load "moom"
  (setq frame-inhibit-implied-resize nil))

(set-scroll-bar-mode nil)
(menu-bar-mode -1)
(tab-bar-mode -1)
(tool-bar-mode -1)

(setq gc-cons-threshold (* 16 1024 1024)) ;; [MB]
;; (setq garbage-collection-messages t)
;; (defvar my-gc-last 0.0)
;; (add-hook 'post-gc-hook
;;           #'(lambda ()
;;               (message "GC! > %.4f[sec]" (- gc-elapsed my-gc-last))
;;               (setq my-gc-last gc-elapsed)))

(message "Loading %s...done" my-early-init)
