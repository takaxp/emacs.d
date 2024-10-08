(add-to-list 'load-path (concat user-emacs-directory "min"))
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq line-number-display-limit-width 100000)
(setq vc-follow-symlinks t)
(setq confirm-kill-emacs 'y-or-n-p)
(keymap-global-set "RET" 'electric-newline-and-maybe-indent)
(keymap-global-set "C-M-t" 'beginning-of-buffer) ;; M-<
(keymap-global-set "C-M-b" 'end-of-buffer) ;; M->
(keymap-global-set "C-M-p" (lambda () (interactive) (other-window -1)))
(keymap-global-set "C-M-n" (lambda () (interactive) (other-window 1)))
(keymap-global-set "C-;" 'comment-dwim) ;; M-; is the defualt
(keymap-global-set "M-=" 'count-words)
(keymap-global-set "M-]" 'bs-cycle-next)
(keymap-global-set "M-[" 'bs-cycle-previous)
(keymap-global-set "C-c g" 'goto-line)
(defun my-open-scratch ()
  "Switch the current buffer to \*scratch\* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))
(keymap-global-set "C-M-s" #'my-open-scratch)
(when (eq system-type 'darwin)
  (when (boundp 'ns-command-modifier) (setq ns-command-modifier 'meta))
  (when (boundp 'ns-alternate-modifier) (setq ns-alternate-modifier 'super))
  (when (boundp 'ns-pop-up-frames) (setq ns-pop-up-frames nil))
  (keymap-global-set (kbd "M-v") 'yank)
  (keymap-global-set "<ns-drag-file>" 'ns-find-file))
(keymap-global-set "<delete>" 'delete-char)
(keymap-global-set "<kp-delete>" 'delete-char)
