;; elpaca-install.el --- -*- lexical-binding: t; -*-

;; Constructing load-path for normal emacs session
(load (concat user-emacs-directory "early-init.el"))

;; Changing the frame width to view elpaca-log window
(when (display-graphic-p)
  (set-frame-width nil 155)
  (set-frame-position nil 0 0)
  (raise-frame))

;; kill-emacs when the process is completed. see elpaca-config.el
;; (add-hook 'elpaca-post-queue-hook #'my-elpaca-post-process)
(add-hook 'kill-emacs-hook #'my-elpaca-post-process)

(require 'elpaca-config)
