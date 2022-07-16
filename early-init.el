;; (message "--- Window system (ns mac) %s, display-graphic-p %s, File %s" window-system (display-graphic-p) early-init-file)
;; References:
;; https://raw.githubusercontent.com/hlissner/doom-emacs/develop/early-init.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-early-init
  (format "%searly-init.el" (expand-file-name user-emacs-directory)))

(message "Loading %s..." my-early-init)
(setq package-enable-at-startup nil
      frame-inhibit-implied-resize t)
(with-eval-after-load "moom"
  (setq frame-inhibit-implied-resize nil))

(set-scroll-bar-mode nil)
(menu-bar-mode -1)
(tab-bar-mode -1)
(tool-bar-mode -1)

(setq gc-cons-threshold (* 64 1024 1024)) ;; 128MB
;; (setq garbage-collection-messages t)
(defvar my-gc-last 0.0)
(add-hook 'post-gc-hook
          #'(lambda ()
              (message "GC! > %.4f[sec]" (- gc-elapsed my-gc-last))
              (setq my-gc-last gc-elapsed)))

(if (display-graphic-p)
    (progn ;; Terminal
      (set-face-foreground 'mode-line "#96CBFE")
      (set-face-background 'mode-line "#21252B"))

  ;; mode-line
  (set-face-attribute 'mode-line nil
                      :foreground "#FFFFFF"
                      :background "#a46398"
                      ;; :overline "#9d5446"
                      :box nil)
  ;; mode-line-inactive
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#FFFFFF"
                      :background "#c8a1b7"
                      ;; :overline "#FFFFFF"
                      :box nil))

(cond
 ((memq window-system '(mac ns)) ;; for macOS
  (setq initial-frame-alist
	      (append
	       '((top . 23)
	         (left . 0)
	         ;; (alpha . (100 90))
	         ;; (vertical-scroll-bars . nil)
	         ;; (internal-border-width . 20)
	         ;; (outer-border-width . 20)
	         ;; (ns-appearance . nil) ;; 26.1 {light, dark}
	         (ns-transparent-titlebar . t)) ;; 26.1
	       initial-frame-alist)))

 ((eq window-system 'x) ;; for Linux
  (setq initial-frame-alist
	      (append
	       '((vertical-scroll-bars . nil)
	         (top . 0)
	         (left . 0)
	         (width . 80)
	         (height . 38))
	       initial-frame-alist)))

 ((eq window-system nil)
  nil)

 (t ;; for Windows
  (setq initial-frame-alist
	        (append
	         '((vertical-scroll-bars . nil)
	           (top . 0)
	           (left . 0)
	           (width . 80)
	           (height . 26))
	         initial-frame-alist))))

;; Apply the initial setting to default
(setq default-frame-alist initial-frame-alist)
(with-eval-after-load "postpone"
  (set-face-foreground 'vertical-border (face-foreground 'default))
  (set-face-background 'vertical-border (face-background 'default)))
;;(set-face-background 'fringe (face-background 'default)) ;; 10-20[ms]
(set-face-background 'fringe "#FFFFFF") ;; 10-20[ms]

;; 1) Monaco, Hiragino/Migu 2M : font-size=12, -apple-hiragino=1.2
;; 2) Inconsolata, Migu 2M     : font-size=14,
;; 3) Inconsolata, Hiragino    : font-size=14, -apple-hiragino=1.0
(defconst my-font-size 12)
(defconst my-ja-font "Migu 2M") ;; "Hiragino Maru Gothic Pro"
(defconst my-ascii-font "Monaco") ;; "Inconsolata", Monaco
;; (defconst my-ja-font "Hiragino Maru Gothic Pro") ;; "Hiragino Maru Gothic Pro"
;; (defconst my-ascii-font "Inconsolata") ;; "Inconsolata", Menlo, "Ricty Diminished"
(defun my-ja-font-setter (spec)
  (set-fontset-font nil 'japanese-jisx0208 spec)
  (set-fontset-font nil 'katakana-jisx0201 spec)
  (set-fontset-font nil 'japanese-jisx0212 spec)
  (set-fontset-font nil '(#x0080 . #x024F) spec)
  (set-fontset-font nil '(#x0370 . #x03FF) spec)
  (set-fontset-font nil 'mule-unicode-0100-24ff spec))
(defun my-ascii-font-setter (spec)
  (set-fontset-font nil 'ascii spec))
(defun my-unicode-font-setter (spec)
  (set-fontset-font t 'unicode spec nil 'prepend))
(defun my-all-the-icons-setter ()
  (when (require 'icons-in-terminal nil t)
    (my-unicode-font-setter
     (font-spec :family (icons-in-terminal-faicon-family)))
    (my-unicode-font-setter
     (font-spec :family (icons-in-terminal-fileicon-family)))
    (my-unicode-font-setter
     (font-spec :family (icons-in-terminal-material-family)))
    (my-unicode-font-setter
     (font-spec :family (icons-in-terminal-octicon-family)))
    (my-unicode-font-setter
     (font-spec :family (icons-in-terminal-wicon-family)))))
(defun my-font-config (&optional size ascii ja)
  "Font config.
- SIZE: font size for ASCII and Japanese (default: 12)
- ASCII: ascii font family (default: \"Monaco\")
- JA: Japanese font family (default: \"Migu 2M\")
"
  (when (memq window-system '(mac ns))
    (let ((font-size (or size my-font-size))
          (ascii-font (or ascii my-ascii-font))
          (ja-font (or ja my-ja-font)))
      (set-fontset-font t '(#Xe000 . #Xf8ff) "icons-in-terminal")
      ;;(set-fontset-font t '(#Xe0a0 . #Xeea0) "icons-in-terminal")
      (my-ascii-font-setter (font-spec :family ascii-font :size font-size))
      (my-ja-font-setter (font-spec :family ja-font :size font-size)))))

(defun my-setup-font ()
  (cond
   ;; CocoaEmacs
   ((memq window-system '(mac ns))
    (when (>= emacs-major-version 23)

      ;; Fix ratio provided by set-face-attribute for fonts display
      (setq face-font-rescale-alist
            '(("^-apple-hiragino.*" . 1.0) ; 1.2
              (".*Migu.*" . 1.2)
              (".*Ricty.*" . 1.0)
              (".*Inconsolata.*" . 1.0)
              (".*osaka-bold.*" . 1.0)     ; 1.2
              (".*osaka-medium.*" . 1.0)   ; 1.0
              (".*courier-bold-.*-mac-roman" . 1.0) ; 0.9
              ;; (".*monaco cy-bold-.*-mac-cyrillic" . 1.0)
              ;; (".*monaco-bold-.*-mac-roman" . 1.0) ; 0.9
              ("-cdac$" . 1.0))))) ; 1.3
   ;; (my-font-config) ;; see `my-theme'

   ((eq window-system 'ns)
    ;; Anti aliasing with Quartz 2D
    (when (boundp 'mac-allow-anti-aliasing)
      (setq mac-allow-anti-aliasing t)))

   ((eq window-system 'w32) ;; Windows
    (let ((font-size 14)
          (font-height 100)
          (ascii-font "Inconsolata")
          (ja-font "Migu 2M")) ;; Meiryo UI, メイリオ
      (set-fontset-font t '(#Xe000 . #Xf8ff) "icons-in-terminal")
      (my-ascii-font-setter (font-spec :family ascii-font :size font-size))
      (my-ja-font-setter
       (font-spec :family ja-font :size font-size :height font-height))
      (setq face-font-rescale-alist '((".*Inconsolata.*" . 1.0))))) ; 0.9

   ((eq window-system 'x) ; for SuSE Linux 12.1
    (let
        ((font-size 14)
         (font-height 100)
         (ascii-font "Inconsolata")
         ;; (ja-font "MigMix 1M")
         (ja-font "Migu 2M"))
      (set-fontset-font t '(#Xe000 . #Xf8ff) "icons-in-terminal")
      (my-ja-font-setter
       (font-spec :family ja-font :size font-size :height font-height))
      (my-ascii-font-setter (font-spec :family ascii-font :size font-size)))
    (setq face-font-rescale-alist '((".*Migu.*" . 2.0)
                                    (".*MigMix.*" . 2.0)
                                    (".*Inconsolata.*" . 1.0))))) ; 0.9
  )
(my-setup-font)

(message "Loading %s...done" my-early-init)
