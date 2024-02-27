;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME
;;;###autoload
(defun my-corfu-disable-in-minibuffer ()
  (corfu-mode -1))

(with-eval-after-load "late-init"

  (when (autoload-if-found '(corfu-mode) "corfu" nil t)
    (add-hook 'emacs-lisp-mode-hook #'corfu-mode))

  (with-eval-after-load "corfu"
    (require 'company)
    (company-prescient-mode -1)
    (global-company-mode -1)

    (add-hook 'find-file-hook #'my-corfu-disable-in-minibuffer)

    (custom-set-variables
     '(corfu-count 5)
     '(corfu-on-exact-match nil)
     '(corfu-auto-prefix 2)
     '(corfu-auto-delay 0.2)
     '(corfu-auto t))

    (when (require 'corfu-prescient nil t)
      (corfu-prescient-mode 1))

    (when (require 'org-block-capf nil t)
      (setq org-block-capf-edit-style 'inline)
      (setq org-block-capf-auto-indent nil)
      (add-hook 'org-mode-hook
                #'org-block-capf-add-to-completion-at-point-functions))

    (when (require 'kind-icon nil t)
      (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

    ))

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

;; Boot mode selection
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
 ;; minimum
 (nil (load (concat user-emacs-directory "min/init.el")))
 ;; Spacemacs
 (nil (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el")))
 ;; Normal mode. see also init-eval.el
 (t (load "~/Dropbox/emacs.d/config/init-env.el" nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;; (advice-add 'epg--check-error-for-decrypt :override 'ignore)
;; (package-initialize) ;; keep this line here for previous versions
