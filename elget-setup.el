(defvar my-elget-threads 1)
(defvar my-elget-initialize nil)
(defvar my-elget--verbose nil)
(defvar my-elget--private-recipes nil "Used for mainly non shallow recipes")
(defvar my-elget--private-recipe-dirs '("~/.emacs.d/recipes"))

(defun my-elget-setup ()
  ;; Install all packages to this directory
  (setq el-get-dir
        (expand-file-name "el-get" (locate-user-emacs-file emacs-version)))
  (add-to-list 'load-path (concat el-get-dir "/el-get"))
  (add-to-list 'load-path (concat el-get-dir "/postpone"))
  (setq el-get-verbose nil)
  (with-eval-after-load "el-get-notify"
    ;;(remove-hook 'el-get-post-init-hooks #'el-get-post-init-message)
    (remove-hook 'el-get-post-update-hooks #'el-get-post-update-message)
    (remove-hook 'el-get-post-update-hooks #'el-get-post-update-notification)
    (remove-hook 'el-get-post-install-hooks #'el-get-post-install-notification)
    (remove-hook 'el-get-post-remove-hooks #'el-get-post-remove-notification))
  (unless (file-directory-p el-get-dir)
    (user-error (format "!! Before use this, be sure %s exists !!" el-get-dir)))

  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        ;; `el-get-silent-update' が使えるカスタマイズパッケージを使う．
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/takaxp/el-get/master/el-get-install.el")
      ;; オリジナルはこっち
      ;;"https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))

  ;; use private recipes
  (dolist (dir my-elget--private-recipe-dirs)
    (add-to-list 'el-get-recipe-path dir))

  ;; Non shallow packages, requires private recipe FIXME: naming issue
  (dolist (dir my-elget--private-recipe-dirs)
    (dolist (file (directory-files dir))
      (when (string-match "^\\(.+\\).rcp$" file)
        (add-to-list 'my-elget--private-recipes (match-string 1 file)))))

  (setq el-get-git-shallow-clone t ;; "--depth 1"
        el-get-verbose nil ;; just for sure
        el-get-silent-update t ;; 出力されるメッセージの抑制
        gc-cons-threshold (* 512 1024 1024) ;; 512MB
        el-get-default-process-sync t ;; 常にシングルスレッドで動かす
        garbage-collection-messages t))

(provide 'elget-setup)
;;; elget-setup.el ends here
