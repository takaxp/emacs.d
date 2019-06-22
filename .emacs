;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
(load "~/Dropbox/emacs.d/config/init-ivy.el" nil t) ;; will be deleted
;; (load "~/Dropbox/emacs.d/config/el-get-setup.el" nil t)
;; (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el"))
;; ln -s ~/Dropbox/emacs.d/config/.spacemacs ~/
;; git clone https://github.com/syl20bnr/spacemacs ~/.spacemacs.d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME
;; https://github.com/akirak/ivy-omni-org
;; https://github.com/Kungsgeten/ivy-todo
;; https://github.com/mkcms/ivy-yasnippet
;; https://github.com/squiter/ivy-youtube






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy-posframe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (autoload-if-found
       '(ivy-posframe-mode my-toggle-ivy-posframe)
       "ivy-posframe" nil t)

  (defun my-toggle-ivy-posframe ()
    "Toggle `ivy-posframe'."
    (interactive)
    (ivy-posframe-mode (if ivy-posframe-mode -1 1)))

  (with-eval-after-load "ivy-posframe"
    (setq ivy-posframe-display-functions-alist
          '((counsel-M-x . ivy-posframe-display-at-point)
            (t           . ivy-posframe-display)))
    (ivy-posframe-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (with-eval-after-load "company"
;;   (when (and (require 'all-the-icons nil t)
;;              (require 'company-box nil t))
;;     (add-hook 'company-mode-hook 'company-box-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trying LSP
;; https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
(when (and (fboundp 'lsp)
           (autoload-if-found '(lsp) "lsp-mode" nil t))

  (add-hook 'c-mode-common-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)

  (with-eval-after-load "lsp"
    (setq lsp-prefer-flymake nil)
    (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error")))

  (with-eval-after-load "lsp-ui"
    (setq lsp-ui-doc-enable t
          lsp-ui-doc-use-childframe t
          lsp-ui-doc-position 'top
          lsp-ui-doc-include-signature t
          lsp-ui-sideline-enable nil
          lsp-ui-flycheck-enable t
          lsp-ui-flycheck-list-position 'right
          lsp-ui-flycheck-live-reporting t
          lsp-ui-peek-enable t
          lsp-ui-peek-list-width 60
          lsp-ui-peek-peek-height 25)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;; (advice-add 'epg--check-error-for-decrypt :override 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun ad:message (f FORMAT-STRING &rest ARGS)
;;   (let ((str (concat (make-string (frame-width) ?\x5F) "\n" FORMAT-STRING)))
;;     (apply f str ARGS)))
;; (advice-add 'message :around #'ad:message)

;; .emacs ends here
