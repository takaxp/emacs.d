;;;;                                       Validated for Emacs 24.4
;;;;                                       Last Update: 2015-02-18@15:59
;;;;                                       Takaaki ISHIKAWA <takaxp@ieee.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (let ((benchmark-init.el "~/.emacs.d/.cask/package/benchmark-init.el"))
;;  (when (file-exists-p benchmark-init.el)
;;    (load benchmark-init.el)))
(setq byte-compile-warnings '(not obsolete) ;; Suppress warning messages
      ad-redefinition-action 'accept)
(setq gc-cons-threshold 134217728 ;; Expand GC threshold, 67108864
      debug-on-error nil  ;; Show debug error messages
      use-cask-flag t)    ;; Cask or not flag

(defun my-time-lag ()
  (let*
      ((now (current-time)) (min (- (car now) (car my-time-zero)))
       (sec (- (car (cdr now)) (car (cdr my-time-zero))))
       (msec (/ (- (car (cdr (cdr now))) (car (cdr (cdr my-time-zero)))) 1000))
       (lag (+ (* 60000 min) (* 1000 sec) msec)))
    (message "--- .emacs loading time: %d [msec]" lag)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst my-time-zero (current-time))	                       ;; Clock start
;;; Load-path and exec-path // M-x list-load-path-shadows ;;;;;;;;;;;;;;;;;;;;;

(defvar debug nil
  "Flag to start Emacs with debugging")
(defun load-path-setter (path-list target-path)
  (dolist (x path-list) (add-to-list target-path x)))
;; exec-path
(load-path-setter
 '("/opt/local/bin" "/usr/local/bin" "/Applications/pTex.app/teTeX/bin"
   "/Applications/LibreOffice.app/Contents/MacOS/"
   "/Users/taka/Dropbox/emacs.d/bin" "/Users/taka/.cabal/bin"
   "/Users/taka/Applications/Viewer/VLC.app/Contents/MacOS") 'exec-path)
;; load-path
(cond
 (debug (require 'my-debug nil t)
	(load-path-setter '("~/Dropbox/emacs.d") 'load-path)
	(load-path-setter '("~/devel/git/org-8.2/lisp") 'load-path)
	(require 'org-tree-slide)

	(add-hook 'after-init-hook
		  (lambda ()
		    (message "--- Emacs booting time: %.0f [msec]"
			     (* 1000
				(float-time (time-subtract
					     after-init-time
					     before-init-time))))))
	)
 (t
  (let*
      ((p "~/Dropbox/emacs.d/") (g "~/devel/git/") (od "org-8.2") ;; org-mode
       (l `("~/Dropbox/emacs.d/config" "~/Dropbox/config" ,p ,(concat p "yatex")
	    ,(concat g od "/lisp") ,(concat g od "/contrib/lisp"))))
    (load-path-setter l 'load-path))

  (if (not use-cask-flag)
      (load-path-setter '("~/.emacs.d/.cask/package") 'load-path)
    (when (require 'cask "~/.cask/cask.el" t) (cask-initialize)) ;; 800[ms]
    (when (require 'pallet nil t) (pallet-mode t))) ;; 30[ms]
  
  ;; Creating Cocoa window   ;; 1000[ms]
  (require 'init nil t)      ;; 600[ms]
  (require 'my-eshell nil t) ;; 0[ms]
  (require 'private nil t)   ;; 0[ms]
  ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook (lambda () (my-time-lag)) t)          ;; Clock end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'use-package nil t)
  (use-package undo-tree
    ;; 1. C-x u と q に対して，フレームサイズの変更を追加
    ;; 2. ツリーを選んでいる時に auto-save-buffers が発動するのを抑制
    ;; auto-save-buffers が org-files をどんどん記録してしまうので，
    ;; 落ち着いて履歴を辿れるのは非常に助かる．
    ;; unfold した状態でも確認できるのかがキモだな．
    ;; ということは，fisualize の時に shall-all してから入るか．
    :init
    (bind-keys :map undo-tree-visualizer-mode-map
	       ("q" . takaxp:undo-tree-visualizer-quit))
    (bind-keys :map undo-tree-map
	       ("C-x u" . takaxp:undo-tree-visualize))    
    :config
    (progn
      (global-undo-tree-mode)
      (setq undo-tree-mode-lighter nil) ;; モードライン領域を節約
      (defun takaxp:undo-tree-visualizer-quit ()
	(interactive)
	(undo-tree-visualizer-quit)
	(delete-window)
	(set-frame-width (selected-frame) 80))
      (defun takaxp:undo-tree-visualize ()
	(interactive)
	(set-frame-width (selected-frame) 163)
	(undo-tree-visualize))))

  (use-package org
    :defer t
    :bind (("C-c p" . takaxp:proportional-font-toggle))
    :config
    (progn      
      ;; ox-odt.el の 自作パッチの変数（DOCSTRINGが記述されていない）
      (setq org-odt-apply-custom-punctuation t)
      
      (defcustom use-proportional-font nil
	"The status of FONT property"
	:type 'boolean
	:group 'org-mode)

      (set-face-attribute 'variable-pitch nil
			  :family "Verdana"
			  :height 125)

      (defun takaxp:proportional-font-toggle ()
	(interactive)
	(setq use-proportional-font (not use-proportional-font))
	(if use-proportional-font
	    (org-entry-put nil "FONT" "PROPORTIONAL")
	  (org-delete-property "FONT")))

      ;; ツリーにフォーカス時，プロパティにPROPORTIONAL指定があると文字を変える
      ;; ステータスは継承されて，ぶら下がっているツリーにフォーカスする時に
      ;; 影響を受ける
      (add-hook 'org-tree-slide-before-narrow-hook
		'(lambda ()
		   (if (equal "PROPORTIONAL"
			      (org-entry-get-with-inheritance "FONT"))
		       (buffer-face-set 'variable-pitch)
		     (buffer-face-mode 0))))
      
      (add-hook 'org-tree-slide-stop-hook
		'(lambda ()
		   (buffer-face-mode 0)))

      (add-hook 'org-tree-slide-before-narrow-hook
		'(lambda ()
		   (when
		       (and (equal (buffer-name) "work.org")
			    (and (or (eq (org-outline-level) 2)
				     (eq (org-outline-level) 3))
				 (looking-at (concat "^\\*+ "
						     org-not-done-regexp))))
		     (takaxp:org-clock-in))))

      (add-hook 'org-finalize-agenda-hook
		'(lambda () (org-agenda-to-appt t '((headline "TODO")))))

      (defun takaxp:org-clock-in ()
	(setq vc-display-status nil)
	(org-clock-in))
      (defun takaxp:org-clock-out ()
	(setq vc-display-status t)
	(require 'org-clock nil t)
	(when (org-clocking-p) (org-clock-out)))
      (add-hook 'org-tree-slide-before-move-previous-hook 'takaxp:org-clock-out)
      (add-hook 'org-tree-slide-before-move-next-hook 'takaxp:org-clock-out)
      (add-hook 'org-tree-slide-stop-hook 'takaxp:org-clock-out)
      ))

  (use-package org-tree-slide
    :defer t
    :config
    (progn
      ;; org-tre-slide が有効ならタイムスタンプを更新しない （Undo範囲が限定されてしまうため）
      (when (require 'update-stamp nil t)
	(add-hook 'before-save-hook
		  '(lambda () (unless org-tree-slide-mode (update-stamp))))
	(setq update-stamp-start "UPDATE:[ \t]*")
	(setq update-stamp-format "%02H:%02M:%02S")
	(setq update-stamp-end "$")
	(setq update-stamp-line-limit 10))

      (when (require 'time-stamp nil t)
	(add-hook 'before-save-hook
		  '(lambda () (unless org-tree-slide-mode (update-stamp))))
	(setq time-stamp-start "DATE:[ \t]*")
	(setq time-stamp-format "%04y-%02m-%02d")
	(setq time-stamp-end "$")
	(setq time-stamp-line-limit 10)) ; def=8  
      ))

  (use-package org-clock
    :defer t
    :config
    (progn
      ;; cite: http://blog.devnode.pl/blog/2012/01/04/get-notified/
      ;;       https://github.com/p-m/org-notify/blob/master/org-notify.el
      (setq org-show-notification-handler
	    '(lambda (notification)
	        (my-desktop-notify "osx-native" "org-mode notification" 01 01 notification nil)
	       (message notification)))
      ))

  (use-package calfw-org
    :defer t
    :config
    (defun my:org-mark-ring-goto-calfw ()
      (interactive)
      (org-mark-ring-goto))
    (defun my:cfw-open-org-calendar ()
      (interactive)
      (change-frame-width-double)
      (cfw:open-org-calendar))
    (defun my:cfw-burry-buffer ()
      (interactive)
      (bury-buffer)
      (change-frame-width-single))
    (defun cfw:org-goto-date ()
      "Move the cursor to the specified date."
      (interactive)
      (cfw:navi-goto-date
       (cfw:emacs-to-calendar (org-read-date nil 'to-time))))
    ;; (defun cfw:org-goto-date ()
    ;;   "Move the cursor to the specified date."
    ;;   (interactive)
    ;;   (cfw:navi-goto-date 
    ;;    (cfw:org-read-date-command (org-read-date nil 'to-time))))
    (bind-keys :map cfw:calendar-mode-map
	       ("j" . cfw:org-goto-date))
;;    (bind-keys :map cfw:calendar-mode-map
;;	       ("q" . my:cfw-burry-buffer))
    (bind-keys :map org-mode-map
	       ("C-c 4" . my:org-mark-ring-goto-calfw)))

  (defun my:send-reminder (title body)
    (let ((c-time (current-time-string)))
      (mail)
      (mail-to) (insert "takaxp@ieee.org")
      (mail-subject) (insert (concat "[Reminder] " title))
      (mail-text) (insert (concat c-time "\n" body))
      (mail-send-and-exit))
    (message "done"))
  )

;; http://sheephead.homelinux.org/2015/01/10/7220/
;; (when (require 'org-notify nil t)
;;   (org-notify-add 'appt
;; 		  '(:time "5m" :period "2m" :duration 100 :action -notify)
;; 		  '(:time "3d" :actions -email))
;;   (org-notify-start))

(global-set-key (kbd "C-c t") 'my:date)

;; recentf のタイミングに注意
