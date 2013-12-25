;;;;                                       Validated for Emacs 24.3
;;;;                                       Last Update: 2013-12-25@16:03
;;;;                                       Take ISHIKAWA <takaxp@ieee.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-time-lag ()
  (let*
      ((now (current-time)) (min (- (car now) (car my-time-zero)))
       (sec (- (car (cdr now)) (car (cdr my-time-zero))))
       (msec (/ (- (car (cdr (cdr now))) (car (cdr (cdr my-time-zero)))) 1000))
       (lag (+ (* 60000 min) (* 1000 sec) msec)))
    (message ".emacs loading time: %d msec." lag)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst my-time-zero (current-time))	                       ;; Clock start
;;; Load-path and exec-path // M-x list-load-path-shadows ;;;;;;;;;;;;;;;;;;;;;
(setq debug-on-error t)

(defvar debug nil
  "Flag to start Emacs with debugging")
(defun load-path-setter (path-list target-path)
  (dolist (x path-list) (add-to-list target-path x)))
;; exec-path
(load-path-setter
 '("/opt/local/bin" "/usr/local/bin" "/Applications/pTex.app/teTeX/bin"
   "/Applications/LibreOffice.app/Contents/MacOS/"
   "/Users/taka/Dropbox/emacs.d/bin"
   "/Users/taka/Applications/Viewer/VLC.app/Contents/MacOS") 'exec-path)
;; load-path
(cond
 (debug (require 'my-debug nil t)
	(load-path-setter '("~/Dropbox/emacs.d") 'load-path)
;	(load-path-setter '("~/devel/git/yasnippet") 'load-path)
	(require 'org-tree-slide)
	)
 (t
  (let*
      ((p "~/Dropbox/emacs.d/") (g "~/devel/git/")
       (l `("~/Dropbox/emacs.d/config" "~/Dropbox/config" ,p
	    "/Applications/LibreOffice.app/Contents/MacOS/"
	    ,(concat g "yasnippet")
;;	    ,(concat g "org-8.0/lisp") ,(concat g "org-8.0/contrib/lisp")
	    ,(concat g "org-mode/lisp") ,(concat g "org-mode/contrib/lisp")
;;	    ,(concat g "org-stable/lisp") ,(concat g "org-stable/contrib/lisp")
	    ,(concat g "emacs-calfw") ,(concat g "org-octopress")
	    ,(concat g "emacs-ctable") ,(concat g "epic") ,(concat g "orglue")
	    ,(concat p "sdic") ,(concat p "yatex")
	    ,(concat p "anything") ,(concat p "matlab")
	    ;; ,(concat g "emacs-powerline")
	    ;; ,(concat g "el-get")
	    ;; ,(concat p "org-sync")
	    ;; ,(concat p "auctex") ,(concat p "auctex/preview")
	    )))
    (load-path-setter l 'load-path))
  (require 'init nil t)
  (require 'my-eshell nil t)
  (require 'private nil t))) ; ~/Dropbox/emacs.d/config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook (lambda () (my-time-lag)) t)          ;; Clock end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'my-test nil t)
;; init.elc ではキックできない．
(when (require 'pomodoro nil t)
  (pomodoro:start nil))
