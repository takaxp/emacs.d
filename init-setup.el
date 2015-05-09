;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-time-lag ()
  (let*
      ((now (current-time)) (min (- (car now) (car my-time-zero)))
       (sec (- (car (cdr now)) (car (cdr my-time-zero))))
       (msec (/ (- (car (cdr (cdr now))) (car (cdr (cdr my-time-zero)))) 1000))
       (lag (+ (* 60000 min) (* 1000 sec) msec)))
    (message "--- .emacs loading time: %d [msec]" lag)))
(defconst my-time-zero (current-time))	                       ;; Clock start

;;; Load-path and exec-path // M-x list-load-path-shadows ;;;;;;;;;;;;;;;;;;;;;
(defun load-path-setter (path-list target-path)
  (dolist (x path-list) (add-to-list target-path x)))

;; exec-path
(load-path-setter
 '("/opt/local/bin" "/usr/local/bin" "/Applications/pTex.app/teTeX/bin"
   "/Applications/LibreOffice.app/Contents/MacOS/"
   "/Users/taka/Dropbox/emacs.d/bin" "/Users/taka/.cabal/bin") 'exec-path)

;; load-path
(cond
 (debug
  (require 'my-debug nil t))
 (t
  (let*
      ((p "~/Dropbox/emacs.d/") (g "~/devel/git/")
       (od "org-8.2")
;;       (od "org-mode")
       (l `("~/Dropbox/emacs.d/config" "~/Dropbox/config" ,p ,(concat p "yatex")
            ,(concat g od "/lisp") ,(concat g od "/contrib/lisp"))))
    (load-path-setter l 'load-path))
  (if (not use-cask-flag)
      (load-path-setter '("~/.emacs.d/.cask/package") 'load-path)
    (when (require 'cask "~/.cask/cask.el" t) (cask-initialize)) ;; 800[ms]
    (when (require 'pallet nil t) (pallet-mode t))) ;; 30[ms]

  (require 'init nil t)      ;; Less than 500[ms], Cocoa: 1000[ms]
  (require 'my-eshell nil t) ;; 0[ms]
  (require 'private nil t)   ;; 0[ms] This package depends on init.el
  ))

;;; config
(setq debug-on-error nil)                    ;; Show debug error messages
(setq gc-cons-threshold 134217728)           ;; Expand GC threshold, 67108864
(setq byte-compile-warnings '(not obsolete)) ;; Suppress warning messages
(setq ad-redefinition-action 'accept)        ;; advice.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook (lambda () (my-time-lag)) t)          ;; Clock end
