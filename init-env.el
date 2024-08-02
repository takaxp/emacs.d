(defconst my-default-loading-delay 6) ;; [s]
(defconst my-ad-require-p nil
  "If non-nil, override `require' and `load' to show loading times.")
(defconst my-profiler-p nil
  "If non-nil, use built-in profiler.el.")
(defconst my-loading-profile-p nil
  "If non-nil, show tick while booting.  Do not use `my-profiler-p' with this.")
(defconst my-loading-packages nil) ;; '(("web-mode" . nil)("org" . nil))
(defvar my-required-libraries nil) ;; will be used in `init'.
(defvar my-secure-boot nil
  "Ensure to start Emacs.") ;; If non-nil, postpone and session are disabled.
(setq debug-on-error nil
      postpone-verbose nil
      my-toggle-modeline-global t ;; 'doom ;; {nil, t, 'doom}
      my-frame-appearance nil ;; {nil, 'dark, 'light}
      my-skip-check-autoload-file t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when my-ad-require-p (load "~/Dropbox/emacs.d/config/init-ad.el" nil t))
(require 'init nil t)
;; (setq measure-exec-time-list '(my-show-org-buffer
;; 			       my-private-conf-activate
;; 			       my-org-babel-load-activate
;; 			       my-org-modules-activate
;; 			       my-org-agenda-prepare-buffers
;; 			       ))
;; (setq my-suppress-message-p nil)
;; (require 'my-eshell nil t)

;; end of init-env.el
