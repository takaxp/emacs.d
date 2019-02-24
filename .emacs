;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
;; (load "~/Dropbox/emacs.d/config/el-get-setup.el" nil t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "postpone"
  (add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))
  (defun my-activate-org-trello ()
    (let ((filename (buffer-file-name (current-buffer))))
      (when (and filename
                 (string= "trello" (file-name-extension filename))
                 (require 'org-trello nil t))
        (org-trello-mode))))
  (add-hook 'org-mode-hook #'my-activate-org-trello))

;; To decrypt old sub trees
;; (advice-add 'epg--check-error-for-decrypt :override 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs ends here
