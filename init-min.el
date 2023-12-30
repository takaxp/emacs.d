(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq line-number-display-limit-width 100000)
(setq confirm-kill-emacs 'yes-or-no-p)

(global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)
(global-set-key (kbd "C-M-t") 'beginning-of-buffer)  ;; M-<
(global-set-key (kbd "C-M-b") 'end-of-buffer) ;; M->
(global-set-key (kbd "C-M-p") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-M-n") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "C-;") 'comment-dwim) ;; M-; is the default
(global-set-key (kbd "M-=") 'count-words)
(global-set-key (kbd "M-]") 'bs-cycle-next)
(global-set-key (kbd "M-[") 'bs-cycle-previous)
(global-set-key (kbd "C-c g") 'goto-line)

(defun my-open-scratch ()
  "Switch the current buffer to \*scratch\* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))
(global-set-key (kbd "C-M-s") #'my-open-scratch)

;; markdown-mode
;; cd ${HOME}/.emacs.d
;; git clone --depth 1 https://github.com/jrblevin/markdown-mode.git
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(require 'markdown-mode nil t)

;; view-mode
(add-hook 'find-file-hook #'view-mode)

(with-eval-after-load "view"
  (define-key view-mode-map (kbd "i") 'View-exit-and-edit)
  (define-key view-mode-map (kbd "<SPC>") 'ignore)
  (define-key view-mode-map (kbd "<DEL>") 'ignore)
  (define-key view-mode-map (kbd "f") 'forward-char)
  (define-key view-mode-map (kbd "b") 'backward-char)
  (define-key view-mode-map (kbd "n") 'my-org-view-next-heading)
  (define-key view-mode-map (kbd "p") 'my-org-view-previous-heading)
  (define-key view-mode-map (kbd "<tab>") 'my-view-tab)
  (define-key view-mode-map (kbd "S-<tab>") 'my-view-shifttab)

  (defun my-org-view-next-heading ()
    (interactive)
    (if (and (derived-mode-p 'org-mode)
             (org-at-heading-p))
        (org-next-visible-heading 1)
      (next-line)))

  (defun my-org-view-previous-heading ()
    (interactive)
    (if (and (derived-mode-p 'org-mode)
             (org-at-heading-p))
        (org-previous-visible-heading 1)
      (previous-line)))

  (defun my-view-tab ()
    (interactive)
    (when (and (derived-mode-p 'org-mode)
               (or (org-at-heading-p)
                   (org-at-property-drawer-p)))
      (let ((view-mode nil))
        (org-cycle))))

  (defun my-view-shifttab ()
    (interactive)
    (when (derived-mode-p 'org-mode)
      (let ((view-mode nil))
        (org-shifttab)))))
