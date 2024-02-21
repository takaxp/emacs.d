;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME
(with-eval-after-load "org"
  (advice-add 'org-assert-version :override #'ignore)
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
 (nil ;; minimal boot or DOOM Emacs (use toggle-doom.sh to switch)
  (when (boundp 'ns-command-modifier) (setq ns-command-modifier 'meta))
  (when (and (memq window-system '(ns mac))
             (fboundp 'mac-get-current-input-source))
    ;; "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese" for Big Sur
    (custom-set-variables
     '(mac-default-input-source "com.google.inputmethod.Japanese.base"))
    (mac-input-method-mode 1)
    (global-set-key (kbd "M-SPC") 'mac-ime-toggle)
    (global-set-key (kbd "S-SPC") 'mac-ime-toggle)))
 (nil ;; To test the latest org
  (add-to-list 'load-path (expand-file-name "~/devel/git/org-mode/lisp"))
  (add-to-list 'load-path (expand-file-name "~/devel/git/org-tree-slide"))
  (setq org-agenda-files '("~/Desktop/test/hoge.org")))
 (nil ;; minimum
  (load (concat user-emacs-directory "min/init.el")))
 (nil ;; Spacemacs
  (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el")))
 (t ;; Normal mode. see also init-eval.el
  (load "~/Dropbox/emacs.d/config/init-env.el" nil t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load "postpone"
  ;; At least in Big Sur, this setting shall be used with side car for moom.el.
  ;; Without side car in Big Sur, the following setting is also correct.
  ;; Then what about other macOSs?

  ;; (when (string= "Big Sur" (macos-name (macos-version)))
  ;;   (setq moom--common-margin '(0 0 0 0)))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;; (advice-add 'epg--check-error-for-decrypt :override 'ignore)
;; (package-initialize) ;; keep this line here for previous versions
