;; (package-initialize) ;; for Emacs 25     Validated in Emacs 25.1
;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst before-load-init-time (current-time)) ;; see my:load-init-time
(defvar debug nil)
(defvar use-cask nil)
(defvar loading-packages nil) ;; '(("org" . t) ("markdown-mode" . nil))
(load "~/Dropbox/emacs.d/config/init-env.el" nil t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "org-grep"
  (setq org-grep-extensions '(".org")))

