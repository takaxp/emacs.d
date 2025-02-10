;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME

(with-eval-after-load "nerd-icons"
  ;; ;;  
  ;; (nerd-icons-octicon "nf-oct-fold")

  ;; (setq mode-line-modes
  ;;       (mapcar
  ;;        (lambda (entry)
  ;;          (if (equal entry "%n")
  ;;              '(:eval (progn
  ;;                        ;; org が widen を乱発するのでこちらをトリガーにする．
  ;;                        ;; 色の変更
  ;;                        (my-update-modeline-color)
  ;;                        ;; "Narrow" を "N" に短縮表示
  ;;                        (if (and (buffer-narrowed-p)
  ;;                                 (fboundp 'nerd-icons-octicon))
  ;;                            (concat " " (nerd-icons-octicon
  ;;                                         "nf-oct-fold" :v-adjust 0.0)) "")))
  ;;            entry))
  ;;        mode-line-modes))
  ;; (setq mode-line-position-line-format
  ;;       `(,(nerd-icons-mdicon "nf-md-square_edit_outline") "%3l"))
  ;; ;;  󰤌
  )

(with-eval-after-load "icons-in-terminal"
  ;;  
  )

(with-eval-after-load "org"
  ;; (advice-add 'org-assert-version :override #'ignore)
  ;; (require 'org-phscroll nil t)

  (defun my-org-pin-subtree ()
    "Pin the subtree with \"pin\" tag."
    (interactive)
    (save-excursion
      (save-restriction
        (unless (org-at-heading-p)
          (org-previous-visible-heading 1))
        (unless (org-before-first-heading-p)
          (org-set-property "VISIBILITY" "children")
          (org-toggle-tag "pin" 'on)
          (message "Pinned")))))

  (defun my-org-unpin-subtree ()
    "Unpin the subtree with \"pin\" tag."
    (interactive)
    (save-excursion
      (save-restriction
        (unless (org-at-heading-p)
          (org-previous-visible-heading 1))
        (unless (org-before-first-heading-p)
          (when (org-element-property :VISIBILITY (org-element-at-point))
            (org-delete-property "VISIBILITY")
            (org-toggle-tag "pin" 'off)
            (message "Unpinned"))))))

  (defun my-toggle-org-pin-subtree ()
    "Toggle \"VISIBILITY\" of the current tree."
    (interactive)
    (save-excursion
      (save-restriction
        (unless (org-at-heading-p)
          (org-previous-visible-heading 1))
        (unless (org-before-first-heading-p)
          (let ((element (org-element-at-point)))
            (cond ((org-element-property :VISIBILITY element)
                   (org-delete-property "VISIBILITY")
                   (org-toggle-tag "pin" 'off)
                   (message "Unpinned"))
                  (t
                   (org-set-property "VISIBILITY" "children")
                   (org-toggle-tag "pin" 'on)
                   (message "Pinned"))))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boot mode selection
;; Note: `load-path' and `exec-path' are both configured in early-init.el
(cond
 ;; minimal boot or DOOM Emacs (use toggle-doom.sh to switch)
 (nil
  (when (boundp 'ns-command-modifier) (setq ns-command-modifier 'meta))
  (when (and (memq window-system '(ns mac))
             (fboundp 'mac-get-current-input-source))
    ;; "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese" for Big Sur
    (custom-set-variables
     '(mac-default-input-source "com.google.inputmethod.Japanese.base"))
    (mac-input-method-mode 1)
    (global-set-key (kbd "M-SPC") 'mac-ime-toggle)
    (global-set-key (kbd "S-SPC") 'mac-ime-toggle)))

 ;; To test the latest org
 (nil
  (add-to-list 'load-path (expand-file-name "~/devel/git/org-mode/lisp"))
  (add-to-list 'load-path (expand-file-name "~/devel/git/org-tree-slide"))
  (setq org-agenda-files '("~/Desktop/test/hoge.org")))

 ;; Debug
 (nil
  (add-to-list 'load-path (expand-file-name "~/Dropbox/config"))
  (add-to-list 'load-path (expand-file-name "~/devel/git/org-mode/lisp"))
  (add-to-list 'load-path
               (expand-file-name "~/devel/git/org-mode/contrib/lisp"))
  (add-to-list 'load-path my-package-dir) ;; defined in early-init.el
  (require 'my-debug))

 ;; minimum
 (nil (load (concat user-emacs-directory "min/init.el")))

 ;; configured with use-package (TRIAL)
 (nil (load (concat user-emacs-directory "use-init.el")))

 ;; Spacemacs
 (nil (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el")))

 ;; Normal mode. see also init-eval.el
 (t
  (setq debug-on-error nil
	postpone-verbose nil
	my-toggle-modeline-global t ;; 'doom ;; {nil, t, 'doom}
	my-frame-appearance nil     ;; {nil, 'dark, 'light}
	my-skip-check-autoload-file t)

  (defvar my-disabled-packages nil) ;; '(("web-mode" . nil)("org" . nil))
  (defvar my-ad-require-p nil
    "If non-nil, override `require' and `load' to show loading times.")
  (defvar my-profiler-p nil
    "If non-nil, use built-in profiler.el.")
  (defvar my-loading-profile-p nil
    "If non-nil, show ticks while booting.")
  (defvar my-secure-boot nil
    "Ensure to start Emacs.  If non-nil, postpone and session are disabled.")

  ;; (setq measure-exec-time-list '(my-show-org-buffer
  ;; 			       my-private-conf-activate
  ;; 			       my-org-babel-load-activate
  ;; 			       my-org-modules-activate
  ;; 			       my-org-agenda-prepare-buffers
  ;; 			       ))
  ;; (require 'my-eshell nil t)

  (require 'init nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (package-initialize) ;; do not delete this line here for previous versions
