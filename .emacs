;; (package-initialize) ;; for Emacs 25
;;                                          Validated for Emacs 24.5.1
;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar use-cask
  nil)
(load "~/Dropbox/emacs.d/config/init-setup.el" nil t)













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    | recentf     | C-M-r   || helm-swoop  | M-s     |
;;    | helm-locate | C-c f f || isearch     | C-s     |
;;    | org-grep    | C-M-g   || bm          | <f10>   |
;;    | ag          | C-M-f   || my:bm-next  | C-<f10> |
;; >> milk web&
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'kurecolor nil t)
;; (require 'hydra nil t)
;; (when (require 'pinenty nil t)
;;   (defun pinentry-emacs (desc prompt ok error)
;;     (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
;;       str)))
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read http://doc.norang.ca/org-mode.html
;; <f11>  is fullscreen mode
;; hydra is available now.
;; Total: (940 + 310)[ms] @2015-05-25

