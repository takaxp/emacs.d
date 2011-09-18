;;;; Basic configuration for Emacs
;;;;                                       Last Update: 2011-09-18@11:14
;;;;                                       Takaaki ISHIKAWA  <takaxp@ieee.org>

(message "* --[ Loading an init file, takaxp-face.el ] --")

;;; ElScreen (require apel) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note: change a string in the elscreen.el from "mac" to "ns"
(load "elscreen" "ElScreen" t)

;;; Frame display parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use visible-bell
(setq visible-bell t)

;; Show line number in the mode line.
(line-number-mode t)

;; Show function name in the mode line.
(which-function-mode t)

;; Show clock in in the mode line
;;(display-time-mode t)

;; Show battery information on the mode line.
;;(display-battery-mode t)

;; Show scroll bar or not
(set-scroll-bar-mode nil) ; 'right

;; Disable to show the tool bar.
(tool-bar-mode 0)

;; Scroll window on a line-by-line basis
(setq scroll-conservatively 35)
(setq scroll-margin 0)
(setq scroll-step 1)

;; Scroll window on a page-by-pabe basis with N line overlapping
(setq next-screen-context-lines 1)

;; Disable to show the splash window at startup
(setq inhibit-startup-screen t)

;; Default window position to show a Emacs frame
;; Dynabook UX: top=0, left=0, width=80, height=32
(cond
 ((eq window-system 'ns) ; for Macintosh
  (setq initial-frame-alist
	(append
	 '((vertical-scroll-bars . nil)		
	   (top . 22)  ; Y-pos from (0,0) the height of menu bar is 22pix.
	   (left . 0)  ; X-pos from (0,0) ; 420 is the center for MBP
	   ;; 26 is the setting for Butler's Docklet
	   ;; 837 is the setting for right side for MBP
	   (width . 80) ; Width  : character count
	   (height . 60); Height : character count
	   ) initial-frame-alist)))

 ((eq window-system 'x) ; for Linux
  (setq initial-frame-alist
	(append
	 '((vertical-scroll-bars . nil)
	   (top . 0)
	   (left . 0)
	   (width . 80)
	   (height . 38)
	   ) initial-frame-alist)))

 (t                     ; for Windows
  (setq initial-frame-alist
	(append
	 '((vertical-scroll-bars . nil)		
	   (top . 0)
	   (left . 0)
	   (width . 80)
	   (height . 26)
	   ) initial-frame-alist))))

;; Avoid adding a new line at the end of buffer
(setq next-line-add-newlines nil)

;; Limit the final word to a line break code (automatically correct)
(setq require-final-newline t)

;;; popwin.el
(require 'popwin "popwin")
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:special-display-config
      (append '(("*Completions*" :height 10 :position top)
		("*Org Agenda*"  :height 10)
		("*Occur*"       :height 10)
		("*sdic*"        :height 15)
		("*anything*"    :height 15)
		("*my-anything*" :height 15)
		("*my-anything-buffer*"    :height 45)
		("*eshell*"      :height 15))
	      popwin:special-display-config))

;;; Colors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cursor (see also takaxp-mac.el)
(set-cursor-color "#AAAAAA") ; #91C3FF
;(add-to-list 'default-frame-alist '(cursor-type . (hbar . 5)))
;(add-to-list 'default-frame-alist '(cursor-type . bar))
(when (eq window-system 'ns)
  ;; when Kawasemi is ON
  (mac-set-input-method-parameter
   "jp.monokakido.inputmethod.Kawasemi.Japanese" 'cursor-color "#91C3FF")

  ;; when Kawasemi is OFF
  (mac-set-input-method-parameter
   "jp.monokakido.inputmethod.Kawasemi.Roman" 'cursor-color "#AAAAAA"))

;; Blink cursor
(blink-cursor-mode t)

;; Turn on font-lock mode for Emacs
(global-font-lock-mode t)
(cond ((not running-xemacs)
       (global-font-lock-mode t)))

;; Avoid exceeding of line display
(unless truncate-lines
  (toggle-truncate-lines))

;; Color of the current line
;; Cite: http://murakan.cocolog-nifty.com/blog/2009/01/emacs-tips-1d45.html
;; see also http://www.emacswiki.org/cgi-bin/emacs/highlight-current-line.el
(global-hl-line-mode t)
(set-face-background 'hl-line "#DEEDFF")


;;; Font ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 ;; CocoaEmacs
 ((eq window-system 'ns)
  (when (or (= emacs-major-version 23) (= emacs-major-version 24))
;    (set-face-attribute 'default nil
;			:family "Inconsolata" :height 120)
;    (set-face-attribute 'default nil
;			:family "Menlo" :height 130)
    (set-fontset-font (frame-parameter nil 'font)
		      'japanese-jisx0208
		      '("Hiragino Maru Gothic Pro" . "iso10646-1"))
    (set-fontset-font (frame-parameter nil 'font)
		      'katakana-jisx0201
		      '("Hiragino Maru Gothic Pro" . "iso10646-1"))
    (set-fontset-font (frame-parameter nil 'font)
		      'japanese-jisx0212
		      '("Hiragino Maru Gothic Pro" . "iso10646-1"))
    (set-fontset-font (frame-parameter nil 'font)
		      'mule-unicode-0100-24ff
		      '("Hiragino Kaku Gothic Pro" . "iso10646-1")
    (set-face-attribute 'default nil
			:family "monaco" :height 120))
    ;; Fix ratio provided by set-face-attribute for fonts display
    (setq face-font-rescale-alist '(("^-apple-hiragino.*" . 1.2) ; 1.2
				    (".*osaka-bold.*" . 1.0)     ; 1.2
				    (".*osaka-medium.*" . 1.0)   ; 1.0
				    (".*courier-bold-.*-mac-roman" . 1.0) ; 0.9
				    (".*monaco cy-bold-.*-mac-cyrillic" . 1.0)
				    (".*monaco-bold-.*-mac-roman" . 1.0) ; 0.9
				    ("-cdac$" . 1.0)))           ; 1.3
    ;; Space between lines
    (set-default 'line-spacing 2)
    ;; Anti aliasing with Quartz 2D
    (setq mac-allow-anti-aliasing t)
    ;; Apply the initial setting to default
    (setq default-frame-alist initial-frame-alist)))
 
 ((eq window-system 'x)
  (set-face-attribute 'default nil
;;;		      :family "Monaco" :height 120)
		      :family "Inconsolata" :height 120)
;;;		      :family "VL Gothic" :height 110)
;  (set-frame-font "VL Gothic-12")
;  (set-fontset-font (frame-parameter nil 'font)
;		    'japanese-jisx0208
;		    (font-spec :family "MigMix 1M" :size 12))
  ;; (set-fontset-font (frame-parameter nil 'font)
  ;;  		    'japanese-jisx0208
  ;;  		    '("MigMix 2M-10" . "unicode-bmp"))
  ;; (set-fontset-font (frame-parameter nil 'font)
  ;;  		    'katakana-jisx0201
  ;;  		    '("MigMix 2M-10" . "unicode-bmp"))
  ;; (set-fontset-font (frame-parameter nil 'font)
  ;;  		    'japanese-jisx0212
  ;;  		    '("MigMix 2M-10" . "unicode-bmp"))
  (set-fontset-font (frame-parameter nil 'font)
  		    'japanese-jisx0208
  		    '("IPAゴシック" . "iso10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
  		    'katakana-jisx0201
  		    '("IPAゴシック" . "iso10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
  		    'japanese-jisx0212
  		    '("IPAゴシック" . "iso10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
  		    'mule-unicode-0100-24ff
  		    '("Inconsolata" . "iso10646-1"))
  (setq face-font-rescale-alist '(("*MigMix*" . 1.0)      ; 1.2
				  ("*IPA*" . 1.0)
				  ("*IPAex*" . 1.0)
				  ("*Inconsolata*" . 1.0) ; 0.9
				  ("-cdac$" . 1.0)))      ; 1.3

  (set-default 'line-spacing 0))

 (window-system
  (set-frame-font "VL Gothic-12")
  ;; (set-frame-font "MigMix 1P-12")
  (set-fontset-font (frame-parameter nil 'font)
		    'japanese-jisx0208
		    (font-spec :family "MigMix 1M" :size 12))
  ;; (font-spec :family "M+1M+IPAG" :height 12))
  (set-default 'line-spacing 0)))

(provide 'takaxp-face)