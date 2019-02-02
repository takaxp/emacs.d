;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t);; see also init-eval.el
;; (load "~/Dropbox/emacs.d/config/el-get-setup.el" nil t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For Mojave to open external link quickly
(when (and (boundp 'ns-version-string)
           (< 1600 (string-to-number
                    (let ((str ns-version-string))
                      (string-match "^appkit-\\([0-9\.]+\\) .+$" str)
                      (match-string 1 str)))))
  (defun ad:browse-url (url &rest _args)
    (do-applescript
     (concat "tell application \"Chrome\" to open location \"" url "\"")))
  (advice-add 'browse-url :override #'ad:browse-url))

(with-eval-after-load "org-agenda"
  (defun my-org-agenda-done (&optional _arg)
    (interactive "P")
    (org-agenda-todo 'done)) ;; FIXME "d" 以外も対応したい．
  (org-defkey org-agenda-mode-map "d" 'my-org-agenda-done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs ends here
