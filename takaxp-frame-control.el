;;;; Functions to control the frame and window
;;;;                                       Last Update: 2011-12-22@17:13
;;;;                                       Takaaki ISHIKAWA  <takaxp@ieee.org>
;;
;; NOTE: This elisp requires two external elisps.

;; [e2wm] 
;; 1) http://github.com/kiwanami/emacs-window-manager/raw/master/e2wm.el
;; 2) http://github.com/kiwanami/emacs-window-layout/raw/master/window-layout.el
(when (require 'e2wm nil t)
;;; Setting for e2wm.el
  (setq e2wm:c-two-recipe
	'(- (:lower-size 10)
	    (| left right)
	    sub))
  (setq e2wm:c-two-winfo
	'((:name left )
	  (:name right )
	  (:name sub :default-hide t)))
  (setq e2wm:c-two-right-default 'left) ; left, prev
  ;; To avoid rebooting issue when using desktop.el and recentf.el
  (add-hook 'kill-emacs-hook 'e2wm:stop-management))

;; [frame-cmds]
;; 1) http://www.emacswiki.org/emacs/download/frame-cmds.el
;; 2) http://www.emacswiki.org/emacs/download/frame-fns.el
(require 'frame-cmds nil t)

;;
;;  M-0       => Move to the frame to the initial position (0,0)
;;  C-u M-0   => Prompt to set the distination position
;;  M-1       => Move to the frame left side (200px)
;;  M-2       => Move to the frame to the center of the display
;;  M-3       => Move to the frame right side (200px)
;;  C-x =     => Activate e2wm (two perspective), and move to the center
;;  C-x -     => Deactivate e2wm, and move to the center
;;  C-u C-x = => e2wm ON, double size width and height, move to the center
;;  C-u C-x - => e2wm OFF, single size width and double height, move center

;;; Keybindings recommendation
;;
;; Move the frame to somewhere (default: 0,0)
;(global-set-key (kbd "M-0") 'move-frame-with-user-specify)
;; Move the frame to left side of the current position (require 'frame-cmds)
;(global-set-key (kbd "M-1") '(lambda () (interactive) (move-frame-left 200)))
;; Move the frame to the center of the window display (require 'takaxp-utility)
;(global-set-key (kbd "M-2") 'move-frame-to-center)
;; Move the frame to right side of the current position (require 'frame-cmds)
;(global-set-key (kbd "M-3") '(lambda () (interactive) (move-frame-right 200)))
;; Set the frame width single size
;(global-set-key (kbd "C-x -") 'change-frame-width-single)
;; Set the frame width double size
;(global-set-key (kbd "C-x =") 'change-frame-width-double)
;; Move the current frame to the top of the window display
;(global-set-key (kbd "<f1>") 'move-frame-to-edge-top)
;; [point-undo.el] Move the cursor to the previous position
;(global-set-key (kbd "S-<f1>") 'move-frame-to-edge-bottom)

(message "* --[ Loading an init file, takaxp-frame-control.el ] --")

(defcustom frame-width-single 80
  "The width of the current frame as the default value"
  :type 'integer
  :group 'takaxp-frame-control)

(defcustom frame-width-double 163
  "The width of the current frame (double size)"
  :type 'integer
  :group 'takaxp-frame-control)

(defcustom frame-height-small 35
  "The height of the current frame as the default value"
  :type 'integer
  :group 'takaxp-frame-control)

(defcustom frame-height-tall 60
  "The height of the current frame (tall version)"
  :type 'integer
  :group 'takaxp-frame-control)

(defcustom move-frame-pixel-menubar-offset 22
  "Offset of the menubar. The default height is 22 for MacOSX"
  :type 'integer
  :group 'taka-frame-control)

(defcustom move-frame-pixel-offset '(0 . 0)
  "Offset of the center position"
  :type 'sexp
  :group 'takaxp-frame-control)

(defcustom auto-move-frame-to-center nil
  "Toggle status of moving frame to center"
  :type 'boolean
  :group 'takaxp-frame-control)
  
(defun toggle-auto-move-frame-to-center ()
  "Change whether move the frame to center automatically"
  (interactive)
  (cond (auto-move-frame-to-center
	 (setq auto-move-frame-to-center nil)
	 (message "Toggle auto move OFF"))
	(t (setq auto-move-frame-to-center t)
	   (message "Toggle auto move ON"))))

(defun move-frame-to-horizontal-center ()
  "Move the current frame to the horizontal center of the window display."
  (interactive)
  (set-frame-position (selected-frame)
		      (+ (car move-frame-pixel-offset)
			 (/ (- (display-pixel-width) (frame-pixel-width)) 2))
		      (frame-parameter (selected-frame) 'top)))

(defun move-frame-to-vertical-center ()
  "Move the current frame to the vertical center of the window display."
  (interactive)
  (set-frame-position (selected-frame)
		      (frame-parameter (selected-frame) 'left)
		      (+ (cdr move-frame-pixel-offset)
			 (/ (- (display-pixel-height)
			       (frame-pixel-height)) 2))))

(defun move-frame-to-edge-top ()
  "Move the current frame to the top of the window display"
  (interactive)
  (set-frame-position (selected-frame)
		      (frame-parameter (selected-frame) 'left)
		      0))

(defun move-frame-to-edge-bottom ()
  "Move the current frame to the top of the window display
   If you find the frame is NOT moved to the bottom exactly,
   Please set `move-frame-pixel-menubar-offset'.
   22 is the default value for MacOSX"
  (interactive)
  (set-frame-position (selected-frame)
		      (frame-parameter (selected-frame) 'left)		      
		      (- (- (display-pixel-height) (frame-pixel-height))
			 move-frame-pixel-menubar-offset)))

(defun move-frame-to-center ()
  "Move the current frame to the center of the window display."
  (interactive)
  (let
      ((prev-pos-x (frame-parameter (selected-frame) 'left))
       (prev-pos-y (frame-parameter (selected-frame) 'top))
       (center-pos-x
	(+ (car move-frame-pixel-offset)
	   (/ (- (display-pixel-width) (frame-pixel-width)) 2)))
       (center-pos-y
	(+ (cdr move-frame-pixel-offset)
	   (/ (- (display-pixel-height) (frame-pixel-height)) 2))))
    (set-frame-position (selected-frame) center-pos-x center-pos-y)
    (message "Frame move: from (%s, %s) to (%s, %s)"
	     prev-pos-x
	     prev-pos-y
	     (frame-parameter (selected-frame) 'left)
	     (frame-parameter (selected-frame) 'top))))

(defun move-frame-with-user-specify (&optional arg)
  "Move the frame to somewhere (default: 0,0).
   Use prefix to specify the destination position."
  (interactive "P")
  (let ((pos-x 0)
	(pos-y move-frame-pixel-menubar-offset))
    (when arg
      (setq pos-x (string-to-number
		   (read-from-minibuffer
		    (format "X: from %s to "
			    (frame-parameter (selected-frame) 'left)))))
      (setq pos-y (string-to-number
		   (read-from-minibuffer
		    (format "Y: from %s to "
			    (frame-parameter (selected-frame) 'top))))))
    (set-frame-position (selected-frame) pos-x pos-y)
    (message "Frame move: (%s, %s)"
	     (frame-parameter (selected-frame) 'left)
	     (frame-parameter (selected-frame) 'top))))

(defun change-frame-width-single (&optional arg)
  "Change the width of the frame to a single width frame"
  (interactive "P")
  (let 
      ((selected-buffer (current-buffer)))
    (e2wm:stop-management)
    (cond (arg
	   (set-frame-size (selected-frame)
			   frame-width-single frame-height-tall))
	  (t
	   (set-frame-size (selected-frame)
			   frame-width-single frame-height-small)))
    (switch-to-buffer selected-buffer)
    (when auto-move-frame-to-center
      (move-frame-to-center))))

(defun change-frame-width-double (&optional arg)
  "Change the width of the frame to double width frame"
  (interactive "P")
  (cond (arg
	 (set-frame-size (selected-frame)
			 frame-width-double frame-height-tall))
	(t
	 (set-frame-size (selected-frame)
			 frame-width-double frame-height-small)))
  (when auto-move-frame-to-center
    (move-frame-to-center))
  (e2wm:start-management)
  (e2wm:dp-two))

(defun reset-frame-height (new-height)
  "Reset the hight of the current frame."
  (interactive "nNew Height: ")
  (set-frame-height (selected-frame) new-height))

;;; popwin.el
(when (require 'popwin "popwin" t)
  ;; (setq special-display-function 'popwin:special-display-popup-window)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:special-display-config
	(append '(("*Completions*" :height 10 :position bottom :noselect t)
		  ("CAPTURE-next.org" :height 10 :position bottom :noselect t)
		  ("*Calendar*"    :height 10 :position top)
		  ("*wclock*"      :height 10 :position bottom)
		  ("*Org Agenda*"  :height 10 :position bottom)
		  ("*Agenda Commands*"  :height 10 :position bottom)
		  ("*Org Select*"  :height 10 :position bottom)
		  ("*Occur*"       :height 10 :position bottom)
		  ("*sdic*"        :height 10 :position bottom)
;		  ("*anything*"    :height 10 :position bottom)
		  ("*anything complete*"    :height 10 :position bottom)
		  ("*my-anything*" :height 10 :position bottom)
		  ("*my-anything-buffer*"    :height 10 :position bottom)
		  ;;		("*cfw-calendar*" :height 40 :position top)
		  ("*eshell*"      :height 10 :position bottom))
		popwin:special-display-config)))

(provide 'takaxp-frame-control)
