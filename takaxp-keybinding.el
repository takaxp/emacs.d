;;;; Global keybindings
;;;;                                      Last Update: 2011-12-17@23:31
;;;                                       Takaaki ISHIKAWA  <takaxp@ieee.org>
;;; Cite: http://www.gnu.org/software/emacs/manual/html_node/emacs/Key-Bindings.html#Key-Bindings
;;; Cite: http://www.uranus.dti.ne.jp/~shiro-/soft/xyzzy/keybind.html
;;; Note: M-x descbinds-anything, M-x describe-bindings

(message "* --[ Loading an init file, takaxp-keybinding.el ] --")

;; Use command key as meta key under Emacs 23
(when (eq window-system 'ns)
  (setq ns-command-modifier (quote meta))
  (set ns-alternate-modifier (quote super))
  ;; Drag and Drop config. for Emacs 23 (2010-07-20)
  (global-set-key [ns-drag-file] 'ns-find-file))

;; Use command key for Emacs 24
;(when (= emacs-major-version 24)
;  (global-set-key (kbd "S-SPC") '(lambda () (message "toggle-input-method"))))

;;; Single key
(autoload 'point-undo "point-undo" nil t)
;; [point-undo.el] Move the cursor to the previous position
(global-set-key (kbd "<f7>") 'point-undo)
;; [point-undo.el] Redo of point-undo
(global-set-key (kbd "S-<f7>") 'point-redo)
;; delete key means C-d
(global-set-key [delete] 'delete-char)
;; Backspace key means C-d
(global-set-key [kp-delete] 'delete-char)
;; Move the current frame to the top of the window display
(global-set-key (kbd "<f1>") 'move-frame-to-edge-top)
;; [point-undo.el] Move the cursor to the previous position
(global-set-key (kbd "S-<f1>") 'move-frame-to-edge-bottom)
;; Cycle buffers user specified
(global-set-key (kbd "<f12>") 'my-open-file-ring)

;;; C-<key>
(global-set-key (kbd "C-0") 'insert-formatted-current-date)
(global-set-key (kbd "C--") 'insert-formatted-current-time)
(global-set-key (kbd "C-=") 'insert-formatted-signature)
;;; Hide or show current block of souces
(global-set-key (kbd "C-(") 'hs-hide-block)
(global-set-key (kbd "C-)") 'hs-show-block)

;;; C-c <key>
;; Spell checking within a specified region
(global-set-key (kbd "C-c 0") 'ispell-region)
; C-c 1 is assigned to org-mode
; C-c 2 is assigned to org-mode
(global-set-key (kbd "C-c 3") 'do-test-applescript)
(global-set-key (kbd "C-c w") 'sdic-describe-word)
;; Move cursor to a specific line
(global-set-key (kbd "C-c g") 'goto-line)
;; Call make command
(global-set-key (kbd "C-c c") 'compile)
;; Call org-capture (replace the org-remember)
(global-set-key (kbd "C-c r") 'org-capture)
;; Show speedbar
(global-set-key (kbd "C-c b") 'speedbar)
;; occur
(global-set-key (kbd "C-c o") 'anything-c-moccur-occur-by-moccur)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-;") 'comment-dwim) ;; M-; is the defualt

;;; C-x <key>
(global-set-key (kbd "C-x -") 'change-frame-width-single)
(global-set-key (kbd "C-x =") 'change-frame-width-double)

;;; C-<key> C-<key>
;; Show ibuffer powered by anything
(global-set-key (kbd "C-x C-b") 'my-anything-buffer)
;; Backup the buffer whenever the buffer is saved
(global-set-key (kbd "C-x C-s") '(lambda () (interactive) (save-buffer 16)))
;; Show yes or no when you try to kill Emacs
(global-set-key (kbd "C-x C-c") 'confirm-save-buffers-kill-emacs)
;; Move focused region to the end of buffer
;(global-set-key (kbd "C-x C-e") 'forward-region-to-tail)
;(global-set-key (kbd "C-c C-o") 'org-open-at-point-with-e2wm)

;;; M-<key>
(global-set-key (kbd "M-+") 'word-count-mode)
;; Paste command by Command+v 
(global-set-key (kbd "M-v") 'yank)
;; Backward page scrolling instead of M-v
(global-set-key (kbd "M-p") 'scroll-down)
;; Frontward page scrolling instead of C-v
(global-set-key (kbd "M-n") 'scroll-up)
;; Move the frame to somewhere (default: 0,0)
(global-set-key (kbd "M-0") 'move-frame-with-user-specify)
;; Move the frame to left side of the current position (require 'frame-cmds)
(global-set-key (kbd "M-1") '(lambda () (interactive) (move-frame-left 200)))
;; Move the frame to the center of the window display (require 'takaxp-utility)
(global-set-key (kbd "M-2") 'move-frame-to-center)
;; Move the frame to right side of the current position (require 'frame-cmds)
(global-set-key (kbd "M-3") '(lambda () (interactive) (move-frame-right 200)))
;; [ElScreen] move to right tab
;(global-set-key (kbd "M-3") 'elscreen-previous)
;; [Elscreen] move to left tab
;(global-set-key (kbd "M-4") 'elscreen-next)
;; The previous buffer put on the right side of me.
(global-set-key (kbd "M-]") 'cycle-buffer)
(global-set-key (kbd "M-[") 'cycle-buffer-backward)

;;; C-M-<key>: Personal setting for fast action
(global-set-key (kbd "C-M-o") '(lambda () (interactive)
				 (show-org-buffer "next.org")))
(global-set-key (kbd "C-M-9") '(lambda () (interactive)
				 (show-org-buffer "buffer.org")))
(global-set-key (kbd "C-M-0") '(lambda () (interactive)
				 (show-org-buffer "today.org")))
;(global-set-key (kbd "C-M-r") 'recentf-open-files)
(global-set-key (kbd "C-M-r") 'my-anything)
(global-set-key (kbd "C-M-s") 'anything-spotlight)
(global-set-key (kbd "C-M--") 'add-itemize-head)
(global-set-key (kbd "C-M-w") 'sdic-describe-word-at-point)
(global-set-key (kbd "C-M-c") 'lookup-word)
;(global-set-key (kbd "C-M-i") '(lambda () (interactive)
;				 (shell-command "open -a iTerm2.app")))
;; Focus on the previous split window (oppose to C-x o)
(global-set-key (kbd "C-M-p") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-M-n") '(lambda () (interactive) (other-window 1)))
(global-set-key (kbd "C-M-t") 'beginning-of-buffer)
(global-set-key (kbd "C-M-b") 'end-of-buffer)

;;; Multiple combination
;; Editing with a rectangle region
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)

(provide 'takaxp-keybinding)

;;; Note:
; C-<prior> = Ctrl+PageDown
; C-<next>  = Ctrl+PageUp
