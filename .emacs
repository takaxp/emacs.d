;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
;; (load "~/Dropbox/emacs.d/config/el-get-setup.el" nil t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load "org"
  (set-locale-environment "ja_JP.UTF-8") ; "ja_JP.UTF-8"
  (require 'org-recur nil t))

;; テクニック
;; 1. 1回だけ呼ぶ
;; 2. 非同期で処理させる

;; Fontawesome 拡張
(with-eval-after-load "postpone"
  ;; 以下を関数化して，任意の文字コードに対応させる．
  (defface my-face-f0a4 '((t (:foreground "orange")))
    nil :group 'font-lock-highlighting-faces)
  (defface my-face-f088 '((t (:foreground "red")))
    nil :group 'font-lock-highlighting-faces)
  (defface my-face-f087 '((t (:foreground "Seagreen3")))
    nil :group 'font-lock-highlighting-faces)
  (defvar my-face-f0a4 'my-face-f0a4)
  (defvar my-face-f088 'my-face-f088)
  (defvar my-face-f087 'my-face-f087)
  (defadvice font-lock-mode (before my-font-lock-mode1 ())
    (font-lock-add-keywords
     major-mode
     '(("" 0 my-face-f0a4 append)
       ("" 0 my-face-f088 append)
       ("" 0 my-face-f087 append))))
  (ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode1)
  (ad-activate 'font-lock-mode))

;; disk-usage.el
(when (autoload-if-found '(disk-usage) "disk-usage" nil t)
  (with-eval-after-load "disk-usage"
    (when (eq system-type 'darwin)
      (setq disk-usage-du-command "du"))))

;; org-trello
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
  (defun my-push-trello-card () (interactive) (org-trello-sync-card))
  (defun my-pull-trello-card () (interactive) (org-trello-sync-card t))
  (defun my-push-trello () (interactive) (org-trello-sync-buffer))
  (defun my-pull-trello () (interactive) (org-trello-sync-buffer t)))

;; To decrypt old sub trees
;; (advice-add 'epg--check-error-for-decrypt :override 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs ends here
