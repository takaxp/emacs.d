;; (message "--- Window system (ns mac) %s, display-graphic-p %s, File %s" window-system (display-graphic-p) early-init-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((early-init (format "%searly-init.el"
                          (expand-file-name user-emacs-directory))))
  (message "Loading %s..." early-init)
  (setq package-enable-at-startup nil
        frame-inhibit-implied-resize t)
  (with-eval-after-load "moom"
    (setq frame-inhibit-implied-resize nil))
  (set-scroll-bar-mode nil)
  (menu-bar-mode -1)
  (tab-bar-mode -1)
  (tool-bar-mode -1)
  (message "Loading %s...done" early-init))

;; References:
;; https://raw.githubusercontent.com/hlissner/doom-emacs/develop/early-init.el
