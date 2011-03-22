;;;; Configuration for Mac
;;;;                                       Takaaki ISHIKAWA  <takaxp@ieee.org>

(message "* --[ Loading an init file, takaxp-mac.el ] --")

;;; Spotlight search with anything.el
(defun anything-spotlight ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-mac-spotlight)
   " *anything-spotlight*"))

;;; QuickLook can show a file saved by Emacs 
;; Cal xattr setting function through checking exception
(defun set-xattr-mac ()
  (interactive)
  (progn
    ;; Exception for a buffer saved on external devices
    (if (and
	 (not (string-match "^/ssh" (buffer-file-name)))
	 (not (string-match "^/Volumes" (buffer-file-name))))
	(set-xattr))))	
;; Cal xattr command to add metadata
(defun set-xattr ()
  (interactive)
  (progn
    (setq mime-for-xattr
	  (coding-system-get buffer-file-coding-system 'mime-charset))
    (setq encoding-for-xattr nil)
    ;;(setq encoding-for-xattr "SHIFT_JIS;2561")
    ;;(setq encoding-for-xattr "UTF-8;134217984")
    ;;(setq encoding-for-xattr "EUC-JP;2361")
    ;;(setq encoding-for-xattr "MACINTOSH;0")
    ;;(setq encoding-for-xattr "ISO-2022-JP;2080")
    (cond
     ((eq mime-for-xattr 'utf-8) (setq encoding-for-xattr "UTF-8;134217984"))
     ((eq mime-for-xattr 'shift_jis) (setq encoding-for-xattr "SHIFT_JIS;2561"))
     ((eq mime-for-xattr 'euc-jp) (setq encoding-for-xattr "EUC-JP;2361")))
    (if (not (eq encoding-for-xattr nil))
	(shell-command
	 (format "xattr -w com.apple.TextEncoding \"%s\" %s"
		 encoding-for-xattr (buffer-name (current-buffer)))))))
;; Exception for spcific hosts
(if (or
     (string= "mbp.local" (system-name))
     (string= "mini.local" (system-name)))
    (add-hook 'after-save-hook 'set-xattr-mac)) ; add metadata after save

;;; Flag related to inline-patch
;; Set the default input method for Mac
(setq default-input-method "MacOSX")
;; To input two-byte character with Shift key under inline-patch
(mac-add-key-passed-to-system 'shift)
;; Set Kawasemi as a input method
;(mac-set-input-method-parameters "jp.monokakido.inputmethod.Kawasemi" 'cursor-type "red")
;(mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" 'title "具")

;;; ispell / aspell / Flyspell
;; Overwrite ispell-program-name
(setq-default ispell-program-name "/opt/local/bin/aspell")

;;; Testing nextstep only ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These setting will be moved to takaxp-init.el

;;; Shrink or Expand region
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(autoload 'hideshowvis-minor-mode "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)
(dolist (hook (list 'emacs-lisp-mode-hook
		    'c++-mode-hook))
  (add-hook hook 'hideshowvis-enable))

;;; matlab.el
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

;;; auto-complete
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; Use migemo
;(require 'migemo)
;; set off as default
;(setq migemo-isearch-enable-p nil)

;;; Search option
;; igrep (M-x grep Override)
(require 'igrep)
(igrep-define lgrep (igrep-use-zgrep nil) (igrep-regex-option "-n -Ou8"))
(igrep-find-define lgrep (igrep-use-zgrep nil) (igrep-regex-option "-n -Ou8"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'takaxp-mac)
