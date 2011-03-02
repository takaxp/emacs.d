;;; Global keybindings
;;;                                       Takaaki ISHIKAWA  <takaxp@ieee.org>
;; Cite: http://www.uranus.dti.ne.jp/~shiro-/soft/xyzzy/keybind.html
;; M-x describe-bindings

(global-set-key (kbd "C-c 1") 'reload-ical-export)
(global-set-key (kbd "C-c 2") 'do-org-update-statistics-cookies)
(global-set-key (kbd "C-c 3") 'do-test-applescript)

(global-set-key (kbd "C-0") 'insert-formatted-current-date)
(global-set-key (kbd "C--") 'insert-formatted-current-time)
(global-set-key (kbd "C-:") 'my-anything)

(global-set-key (kbd "M-s") 'anything-c-moccur-occur-by-moccur)
(define-key isearch-mode-map (kbd "C-o") 'anything-c-moccur-from-isearch)
(define-key isearch-mode-map (kbd "C-M-o") 'isearch-occur)

(global-set-key (kbd "C-M-t") '(lambda () (interactive)
				 (show-org-buffer "next.org")))
(global-set-key (kbd "C-M-h") 'recentf-open-files)
(global-set-key (kbd "C-M--") 'add-itemize-head)
(global-set-key (kbd "C-M-s") 'anything-spotlight)
(global-set-key (kbd "C-M-w") 'lookup-word)
;(global-set-key (kbd "C-M-i") 'eshell)

(global-set-key (kbd "M-+") 'word-count-mode)
;; Paste command by Command+v 
(global-set-key (kbd "M-v") 'yank)
;; Backward page scrolling instead of M-v
(global-set-key (kbd "M-p") 'scroll-down)
;; Frontward page scrolling instead of C-v
(global-set-key (kbd "M-n") 'scroll-up)
;; Focus on the previous split window (oppose to C-x o)
(global-set-key (kbd "C-M-p") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-M-n") '(lambda () (interactive) (other-window 1)))
(global-set-key (kbd "C-x p") '(lambda () (interactive) (other-window -1)))
(global-set-key [C-M-up] '(lambda () (interactive) (other-window -1)))
(global-set-key [C-M-down] '(lambda () (interactive) (other-window 1)))
;; Backup the buffer whenever the buffer is saved
(global-set-key (kbd "C-x C-s") '(lambda () (interactive) (save-buffer 16)))

(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Move cursor to a specific line
(global-set-key (kbd "C-c g") 'goto-line)
;; Call make command
(global-set-key (kbd "C-c c") 'compile)
;; Call org-capture (replace the org-remember)
(global-set-key (kbd "C-c r") 'org-capture)
;; Show a menu to pull or push of org-mobile
(global-set-key (kbd "C-c m") 'org-mobile-sync)
;; Show speedbar
(global-set-key (kbd "C-c b") 'speedbar)
;; occur
(global-set-key (kbd "C-c o") 'occur)
;; Hide or show current block of souces
(global-set-key (kbd "C-;") 'hs-hide-block)
(global-set-key (kbd "C-'") 'hs-show-block)

;; Editing with a rectangle region
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)

;; Show yes or no when you try to kill Emacs
(global-set-key (kbd "C-x C-c") 'confirm-save-buffers-kill-emacs)
;; Move focused region to the end of buffer
(global-set-key (kbd "C-x C-e") 'forward-region-to-tail)
;; Call a test function
(global-set-key (kbd "C-c C-b") 'testfunc)


;; Drag and Drop config. for Emacs 23 (2010-07-20)
(define-key global-map [ns-drag-file] 'ns-find-file)

;; Move the cursor to the previous position
(define-key global-map (kbd "<f7>") 'point-undo)
;; Redo of point-undo
(define-key global-map (kbd "S-<f7>") 'point-redo)
;; Move the frame to the left side of current position
(define-key global-map (kbd "M-1") 'my-move-frame-left)
;; Move the frame to teh right side of current position
(define-key global-map (kbd "M-2") 'my-move-frame-right)
;(define-key global-map [?\C-\M-2] 'my-move-frame-down)
;(define-key global-map [?\C-\M-3] 'my-move-frame-up)

(provide 'takaxp-keybinding)

;; old setting
; (global-set-key [?\C-:] 'my-anything)
; (global-set-key [?\C-\M-S] 'anything-spotlight)
