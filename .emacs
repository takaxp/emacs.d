;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
;; (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el"))
;;  1. ln -s ~/Dropbox/emacs.d/config/.spacemacs ~/
;;  2. git clone https://github.com/syl20bnr/spacemacs ~/.spacemacs.d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME

(with-eval-after-load "postpone"
  (setq ns-alerter-command nil)) ;; due to broken of alerter command



























;; (with-eval-after-load "org-num"
;;   (defun my-org-num-format (numbering)
;;     (concat (mapconcat #'number-to-string numbering ".") " "))
;;   (setq org-num-face
;;         (funcall #'(lambda ()
;;                      (when )
;;                      '((t (:bold t :background "#DEEDFF"))))))
;;   ;; line-number-current-line ((t (:bold t :background "#DEEDFF")))
;;   (funcall #'(lambda () '((t (:bold t :background "#DEEDFF")))))
;;   )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; posframe - testing

;; (set-frame-parameter (selected-frame) 'internal-border-width 10)
;; (set-face-background 'internal-border "#FF00FF")
;; Could be useful.
;; (when (require 'mini-modeline nil t)
;;   (mini-modeline-mode 1))

(with-eval-after-load "posframe"
  (custom-set-faces
   '(internal-border
     ((((background dark)) :background "#FF0000")
      (t (:background "#FF0000")))))

  (with-eval-after-load "frame"
    (advice-remove 'make-frame #'ad:make-frame))

  (setq my-string "hoge.
hoge.")

  (when (and nil
             (require 'posframe nil t)
             (posframe-workable-p))
    (posframe-show
     "test"
     :string my-string
     :background-color "black"
     :foreground-color "green"
     :internal-border-width 1
     :internal-border-color "red"))
  )


;; ivy-posframe
(when (and nil
           (require 'ivy-posframe nil t))
  (defun my-toggle-ivy-posframe ()
    "Toggle `ivy-posframe'."
    (interactive)
    (ivy-posframe-mode (if ivy-posframe-mode -1 1)))
  (setq ivy-posframe-border-width
        (* 2 (cdr (assoc 'internal-border-width (frame-geometry)))))
  (setq ivy-posframe-hide-minibuffer nil)
  ;;  (setq ivy-posframe-border ((t (:background "#6272a4"))))
  (setq ivy-posframe-parameters
        '((left-fringe . (frame-parameter nil 'left-fringe))
          (right-fringe . (frame-parameter nil 'right-fringe))))
  (setq ivy-posframe-display-functions-alist
        '((counsel-M-x . ivy-posframe-display-at-point)
          (t           . ivy-posframe-display)))
  (set-face-background 'internal-border "#FF0000")
  (set-frame-parameter (selected-frame) 'internal-border-width 10)
  (ivy-posframe-mode 1)
  (set-face-background 'internal-border "#FF0000")
  (set-frame-parameter (selected-frame) 'internal-border-width 10))

(when nil
  (set-face-background 'fringe "green")
  (set-face-background 'default "#999999")
  (set-face-foreground 'default "#FFFFFF")
  (frame-parameter (selected-frame) 'foreground-color)
  (frame-parameter (selected-frame) 'background-color)

  (set-frame-parameter (selected-frame) 'border-width '20)
  (frame-parameter (selected-frame) 'border-width)

  (set-frame-parameter (selected-frame) 'outer-border-width '30)
  (frame-parameter (selected-frame) 'outer-border-width)
  (frame-parameter (selected-frame) 'frame-width)

  (set-frame-parameter (selected-frame) 'border-color "#FF0000")
  (frame-parameter (selected-frame) 'border-color)

  (frame-parameter (selected-frame) 'internal-border-width)
  (frame-parameter (selected-frame) 'internal-frame-width)

  (modify-frame-parameters (selected-frame) '((border-width . 20)))

  (insert (format "%s" (frame-parameters))))


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

;; Trying... but...
;; (when (require 'dumb-jump nil t)
;;   (dumb-jump-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;; (advice-add 'epg--check-error-for-decrypt :override 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load "postpone"
  (defun ad:message (f FORMAT-STRING &rest ARGS)
    (let ((str (concat (make-string (frame-width) ?\x5F) "\n" FORMAT-STRING)))
      (apply f str ARGS)))
  ;; (advice-add 'message :around #'ad:message)
  ;; (advice-remove 'message #'ad:message)
  )

;; .emacs ends here
;; (put 'narrow-to-region 'disabled nil)
