(defconst my-default-loading-delay 6)
(defconst my-ad-require-p nil
  "If non-nil, override `require' and `load' to show loading times.")
(defconst my-profiler-p nil
  "If non-nil, use built-in profiler.el.")
(defconst my-loading-profile-p nil
  "If non-nil, show tick while booting.  Do not use `my-profiler-p' with this.")
(defconst my-loading-packages nil)
(defvar my-secure-boot nil
  "Ensure to start Emacs.") ;; If non-nil, postpone and session are disabled.
(setq debug-on-error nil
      postpone-verbose nil
      my-toggle-modeline-global t ;; 'doom ;; {nil, t, 'doom}
      my-frame-appearance nil ;; {nil, 'dark, 'light}
      my-skip-check-autoload-file t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq my-suppress-message-p nil)
;; (setq measure-exec-time-list '(my-show-org-buffer
;;                                my-private-conf-activate
;;                                my-org-babel-load-activate
;;                                my-org-modules-activate
;;                                my-org-agenda-prepare-buffers
;;                                ))
(when my-profiler-p (profiler-start 'cpu+mem))
;; Suppress exporting of custom-set-variables (25.1 or later)
(setq custom-file (locate-user-emacs-file "custom.el"))

;; M-x list-load-path-shadows
(defvar my-required-libraries nil) ;; will be used in `init'.
(when my-ad-require-p
  (load "~/Dropbox/emacs.d/config/init-ad.el" nil t))
(require 'init nil t)
(with-eval-after-load "postpone"
  (autoload 'mail "~/Dropbox/config/my-mail.el.gpg" nil t)
  (require 'my-eshell nil t))

(when my-profiler-p (profiler-report))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hide package.el, could be removed
;; (unless (version< "27.0" (format "%s" emacs-major-version))
;;   (setq byte-compile-warnings '(cl-functions)))

;; (defun ad:package--ensure-init-file ()
;;   (setq package--init-file-ensured t))
;; (advice-add 'package--ensure-init-file :override
;;             #'ad:package--ensure-init-file))

;; To check closing sequence
;; (when (require 'init-autoloads nil t)
;;   (my-kill-emacs-hook-show))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff を取る前に tabify スペースをタブに変換する．今は全てスペース置換中．
;; この設定でファイルを一度編集後に，M-x tabify しないとだめ．
(when nil
  (add-hook 'emacs-lisp-mode-hook
            #'(lambda ()
                (setq indent-tabs-mode t)
                (setq tab-width 8)
                (setq indent-line-function 'lisp-indent-line))))

;; end of init-env.el
