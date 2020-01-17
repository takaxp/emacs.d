;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
;; (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME

(when nil
  ;; https://github.com/chuntaro/emacs-keycaster/issues/4
  (load "~/.emacs.d/27.0.60/el-get/keypression/keypression.el")
  ;; (custom-set-variables
  ;;  '(keycaster-x-offset (+ (frame-pixel-width) 10)))
  (custom-set-variables
   '(keycaster-use-child-frame t)
   '(keycaster-x-offset (+ 576 10)))
  ;; (custom-set-variables
  ;;  '(keycaster-x-offset (+ 476 10)))
  (keycaster-mode))

(with-eval-after-load "magit"
  ;; To suppress warnings
  (remove-hook 'magit-status-sections-hook 'magit-insert-merge-log)
  (remove-hook 'magit-status-sections-hook 'magit-insert-rebase-sequence)
  (remove-hook 'magit-status-sections-hook 'magit-insert-am-sequence)
  (remove-hook 'magit-status-sections-hook 'magit-insert-sequencer-sequence)
  (remove-hook 'magit-status-sections-hook 'magit-insert-bisect-output)
  (remove-hook 'magit-status-sections-hook 'magit-insert-bisect-rest)
  (remove-hook 'magit-status-sections-hook 'magit-insert-bisect-log)
  (remove-hook 'magit-status-sections-hook 'magit-insert-stashes))

(when t
  ;; Shall be updated for Kotoeri
  (setq mac-ime-cursor-type nil)
  (defun ns-insert-marked-text (pos len)
    "Insert contents of `ns-working-text' as UTF-8 string and mark with
  `ns-working-overlay' and `ns-marked-overlay'.  Any previously existing
  working text is cleared first. The overlay is assigned the faces
  `ns-working-text-face' and `ns-marked-text-face'."
    (ns-delete-working-text)
    (let ((start (point)))
      (when (<= pos (length ns-working-text))
        ;; (put-text-property pos len 'face 'ns-working-text-face ns-working-text)
        ;; (insert ns-working-text)
        ;; (if (= len 0)
        ;;     (overlay-put (setq ns-working-overlay
        ;;                        (make-overlay start (point) (current-buffer) nil t))
        ;;                  'face 'ns-working-text-face)
        ;;   (overlay-put (setq ns-working-overlay
        ;;                      (make-overlay start (point) (current-buffer) nil t))
        ;;                'face 'ns-unmarked-text-face)
        ;;   (overlay-put (setq ns-marked-overlay
        ;;                      (make-overlay (+ start pos) (+ start pos len)
        ;;                                    (current-buffer) nil t))
        ;;                'face 'ns-marked-text-face))
        ;; (goto-char (+ start pos))

        (if (= len 0)
            (overlay-put (setq ns-working-overlay
                               (make-overlay start (point) (current-buffer) nil t))
                         'after-string
                         (propertize ns-working-text 'face 'ns-working-text-face))
          (overlay-put (setq ns-working-overlay
                             (make-overlay start (point) (current-buffer) nil t))
                       'after-string
                       (propertize (substring ns-working-text 0 len)
                                   'face 'ns-marked-text-face))
          (overlay-put (setq ns-marked-overlay
                             (make-overlay (point) (+ (point) pos len)
                                           (current-buffer) nil t))
                       'before-string
                       (propertize (substring ns-working-text len
                                              (length ns-working-text))
                                   'face 'ns-unmarked-text-face))
          (goto-char (+ start pos)))
        ))))

(when nil
  (unless (version< emacs-version "27.0")
    (when (autoload-if-found
           '(keycaster-mode)
           "keycaster" nil t)

      (add-hook 'keycaster-mode-hook #'dimmer-permanent-off)
      (with-eval-after-load "keycaster"

        ;; 日本語入力のガード
        ;; 表示位置の調整
        (defun keycaster--set-frame-string (i string)
          (aset keycaster--strings i string)
          (with-current-buffer (aref keycaster--buffers i)
            (erase-buffer)
            (insert string))
          (let ((window-resize-pixelwise t)
                (frame-resize-pixelwise t)
                (window-min-width 0)
                (window-min-height 0)
                (frame (aref keycaster--frames i)))
            (if keycaster-use-child-frame
                (fit-frame-to-buffer frame nil 0 nil 0)
              (set-frame-width frame (string-width string)))))

        (setq keycaster-frames-maxnum 8
              ;; keycaster-frame-justify 'keycaster-left-justified
              keycaster-use-child-frame nil
              keycaster-x-offset 1
              keycaster-font "Monaco"
              keycaster-fade-out-delay 2.0
              keycaster-y-offset 20))
      )))

(with-eval-after-load "postpone"
  (add-hook 'focus-in-hook 'mac-ime-update-title))

(with-eval-after-load "postpone"
  (setq ns-alerter-command nil)) ;; due to broken of alerter command

;; Shiftを使って大文字を入力する時，IME的にはASCIIにHLINEがかわるから
;; 表示が乱れる．Shiftモードに入る時のフックと抜ける時のフックが必要
;; hook があれば，そこに custom-set-faces を当てられる．
;; それか，let で回避するか．
;; それか，ShiftでIME入力を継続させる時は，IMEONを継続させるとか？
;; /usr/local/bin/gpg へ gpg2 からシンボリックリンクを貼るでOK．OK
;; %04y を %Y にしろと怒られた

;; input-method-activate-hook, input-method-deactivate-hook
;; activate-mark-hook, deactivate-mark-hook
;; minibuffer-setup-hook, minibuffer-exit-hook
;; isearch-mode-hook, isearch-mode-end-hook

(with-eval-after-load "org"
  (when (require 'backline nil t)
    (advice-add 'outline-flag-region :after 'backline-update)))

(custom-set-faces
 '(ns-marked-text-face
   ((t (:background "light pink" :underline "OrangeRed2"))))
 '(ns-unmarked-text-face
   ((t (:background "light sky blue" :underline "royal blue")))))

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
