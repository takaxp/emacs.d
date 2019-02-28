;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
;; (load "~/Dropbox/emacs.d/config/el-get-setup.el" nil t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (when (require 'disk-usage nil t))

(with-eval-after-load "postpone"
  (defvar org-trello-current-prefix-keybinding nil) ;; To avoid an error
  ;; 1. TODO/DOING/DONE に trello 側のカードを変えておく．
  ;; 2. M-x org-trello-install-key-and-token
  ;; ~/.emacs.d/.trello/<account>.el が作られる
  ;; 3. M-x org-trello-install-board-metadata
  ;; Trello 側の情報を基にして current-buffer にプロパティブロックが挿入される
  ;; 4. C-u M-x org-trello-sync-buffer で pull
  ;; 5. M-x org-trello-sync-buffer で push
  (add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))
  (defun my-activate-org-trello ()
    (let ((filename (buffer-file-name (current-buffer))))
      (when (and filename
                 (string= "trello" (file-name-extension filename))
                 (require 'org-trello nil t))
        (org-trello-mode))))
  (add-hook 'org-mode-hook #'my-activate-org-trello))

;; FIXME 同期するとカード記述の情報が繰り返しインデントされてしまう．
(with-eval-after-load "org-trello"
  (defun my-push-trello-card () (org-trello-sync-card))
  (defun my-pull-trello-card () (org-trello-sync-card t))
  (defun my-push-trello () (org-trello-sync-buffer))
  (defun my-pull-trello () (org-trello-sync-buffer t)))

;; To decrypt old sub trees
;; (advice-add 'epg--check-error-for-decrypt :override 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs ends here
