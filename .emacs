;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME
(cond (nil ;; To test the latest org
       (add-to-list 'load-path (expand-file-name "~/devel/git/org-mode/lisp"))
       (setq org-agenda-files '("~/Desktop/hoge.org")))
      (t ;; Normal mode. see also init-eval.el
       (load "~/Dropbox/emacs.d/config/init-env.el" nil t)))
;; (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "postpone"
  ;; At least in Big Sur, this setting shall be used with side car for moom.el.
  ;; Without side car in Big Sur, the following setting is also correct.
  ;; Then what about other macOSs?
  (when (string= "Big Sur" (macos-name (macos-version)))
    (setq moom--common-margin '(0 0 0 0))))

(with-eval-after-load "org-tree-slide"
  (defvar my-hide-org-meta-line-p nil)
  (defun my-hide-org-meta-line ()
    (interactive)
    (setq my-hide-org-meta-line-p t)
    (set-face-attribute 'org-meta-line nil
			                  :foreground (face-attribute 'default :background)))
  (defun my-show-org-meta-line ()
    (interactive)
    (setq my-hide-org-meta-line-p nil)
    (set-face-attribute 'org-meta-line nil :foreground nil))

  (defun my-toggle-org-meta-line ()
    (interactive)
    (if my-hide-org-meta-line-p
	      (my-show-org-meta-line) (my-hide-org-meta-line)))

  (add-hook 'org-tree-slide-play-hook #'my-hide-org-meta-line)
  (add-hook 'org-tree-slide-stop-hook #'my-show-org-meta-line)

  ;; Option
  (defun my-update-org-meta-line ()
    (interactive)
    (when my-hide-org-meta-line-p
      (my-hide-org-meta-line)))
  (add-hook 'ah-after-enable-theme-hook #'my-update-org-meta-line))

(with-eval-after-load "keypression"
  ;; (setq keypression-frame-origin 'keypression-origin-bottom-right)
  (setq keypression-frame-origin 'keypression-origin-top-left)
  (setq keypression-x-offset (- (frame-pixel-width) 100))
  (setq keypression-y-offset 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;;(advice-add 'epg--check-error-for-decrypt :override 'ignore)
