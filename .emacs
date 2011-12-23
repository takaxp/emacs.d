;;;;                                              Validated for Emacs 23.3.50
;;;;                                       Last Update: 2011-12-23@19:32
;;;;                                       Takaaki ISHIKAWA <takaxp@ieee.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq byte-compile-warnings
;;       '(free-vars unresolved callargs redefine obsolete noruntime
;; 		  cl-functions interactive-only make-local))
(defun my-time-lag ()
  (let*
      ((now (current-time)) (min (- (car now) (car my-time-zero)))
       (sec (- (car (cdr now)) (car (cdr my-time-zero))))
       (msec (/ (- (car (cdr (cdr now))) (car (cdr (cdr my-time-zero)))) 1000))
       (lag (+ (* 60000 min) (* 1000 sec) msec)))
    (message "'.emacs loading time: %d msec." lag)))
(defconst my-time-zero (current-time))	; Clock start

;;; Load-path and exec-path // M-x list-load-path-shadows ;;;;;;;;;;;;;;;;;;;;;
(defvar debug nil
;(defvar debug t
  "Flag to start Emacs with debugging")
(defun load-path-setter (path-list target-path)
  (dolist (x path-list) (add-to-list target-path x)))
(load-path-setter
 '("/opt/local/bin" "/usr/local/bin" "/Applications/pTex.app/teTeX/bin")
 'exec-path)

(defvar default-path "~/env/config/emacs/")
(load-path-setter `(,default-path "~/.emacs.d"
		     ,(concat default-path "anything")
		     ) 'load-path) ; Common
(cond
 (debug

  (require 'takaxp-init)
  (require 'takaxp-face) ;; no require
  (require 'takaxp-utility)
  (require 'takaxp-keybinding)

  (setq frame-title-format "Emacs Test Now")
  (setq debug-on-error t))
 (t
  (let* ((p default-path)
	 (l `("~/devel/git/orgmode/lisp" ,p ,(concat p "anything")
	      ,(concat p "matlab") ,(concat p "yatex") ,(concat p "sdic")
	      ;; ,(concat p "auctex") ,(concat p "auctex/preview")
	      )))
    (load-path-setter l 'load-path))
  (require 'takaxp-init)
  (require 'takaxp-face) ;; no require
  (when (and (eq window-system 'ns) (= emacs-major-version 23))
    (require 'takaxp-mac))
  (require 'takaxp-org-mode)
  (require 'takaxp-utility)
  (require 'takaxp-frame-control)
  (require 'private) ;; no require
  (require 'takaxp-keybinding)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook (lambda () (my-time-lag)) t) ; Clock end
(message "全部読み込んだよ ﾉ`Д)ﾉ:･'∵:.┻┻")
(require 'play-ground)
