;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
;; (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME
(when (>= emacs-major-version 27)
  (with-eval-after-load "postpone"
    (setq ns-alerter-command nil)) ;; due to broken of alerter command

  (custom-set-variables
   '(mac-default-input-source "com.google.inputmethod.Japanese.base"))
  ;; mac-get-cursor-color を設定するとどうなる？
  (mac-input-method-mode 1)


  ;; Shiftを使って大文字を入力する時，IME的にはASCIIにHLINEがかわるから
  ;; 表示が乱れる．Shiftモードに入る時のフックと抜ける時のフックが必要
  ;; hook があれば，そこに custom-set-faces を当てられる．
  ;; それか，let で回避するか．
  ;; それか，ShiftでIME入力を継続させる時は，IMEONを継続させるとか？
  ;; /usr/local/bin/gpg へ gpg2 からシンボリックリンクを貼るでOK．OK
  ;; %04y を %Y にしろと怒られた


  (with-eval-after-load "hl-line"
    ;; isearchも同様の設定が必要
    (defun my-working-text-face-on ()
      (if (or isearch-mode
              (minibufferp))
          (custom-set-faces
           '(ns-working-text-face nil))
        (custom-set-faces
         '(ns-working-text-face
           ((((background dark)) :background "#594d5d" :underline "white")
            (t (:background "#fff0de" :underline "black")))))))
    (defun my-working-text-face-off ()
      (if (or isearch-mode
              (minibufferp))
          (custom-set-faces
           '(ns-working-text-face nil))
        (custom-set-faces
         '(ns-working-text-face
           ((((background dark)) :background "#484c5c" :underline "white")
            (t (:background "#DEEDFF" :underline "black")))))))

    (add-hook 'input-method-activate-hook #'my-working-text-face-on)
    (add-hook 'input-method-deactivate-hook #'my-working-text-face-off)

    ;; input-method-activate-hook, input-method-deactivate-hook
    ;; activate-mark-hook, deactivate-mark-hook
    ;; minibuffer-setup-hook, minibuffer-exit-hook
    ;; isearch-mode-hook, isearch-mode-end-hook

    (when (require 'migemo nil t)
      ;; FIXME Conflict with migemo...
      ;; To fix this issue, you should update migemo.el because it is not minor mode
      (defun my-isearch-ime-deactivate-sticky ()
        (unless (region-active-p)
          (mac-ime-deactivate-sticky)))
      ;; see also activate-mark-hook, deactivate-mark-hook
      (add-hook 'isearch-mode-hook #'my-isearch-ime-deactivate-sticky)
      (add-hook 'isearch-mode-end-hook #'mac-ime-activate-sticky))
    ))

(with-eval-after-load "org"
  (when (require 'backline nil t)
    (advice-add 'outline-flag-region :after 'backline-update)))











;; TODO eldocの確認時に narrowing/widen している可能性がある．

;; これは，async で切り離したプロセス(emacs -Q)内部で動かせばよいのでは？
;; 結果はバッファで受け取る

(defun my-test-release-build ()
  (interactive)
  (unless (require 'async nil t)
    (user-error "Async is NOT installed"))
  (async-start
   (lambda ()
     (add-to-list 'load-path "~/devel/git/org-mode/lisp")
     (require 'org)
     (defvar result `(:org ,(org-version)))
     (load "~/.emacs.d/26.3.50/el-get/package-lint/package-lint.el" nil t)
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Build a new package here for release
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (byte-compile-file "~/devel/git/org-onit/org-onit.el")
     ;; (with-current-buffer (get-file-buffer "~/devel/git/org-onit/org-onit.el")
     ;;   (package-lint-current-buffer))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; (load "~/devel/git/org-onit/org-onit.el" nil t)
     ;; (require 'org-onit)
     ;; (org-onit-get-sign)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (switch-to-buffer "*Compile-Log*") ;; FIXME
     (message "%s"
              (buffer-substring-no-properties (point-min) (point-max)))
     ;;     (message "%s" (buffer-list))
     )
   (lambda (result)
     (message "%s" result))))


;; (require 'package)
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/") t)
;; (package-initialize)
;;
;; (require 'package-lint)


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
