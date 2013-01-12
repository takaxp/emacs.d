;;;;                                       Validated for Emacs 24.1
;;;;                                       Last Update: 2013-01-12@11:38
;;;;                                       Takaaki ISHIKAWA <takaxp@ieee.org>
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
(defvar debug nil
  "Flag to start Emacs with debugging")
(defun load-path-setter (path-list target-path)
  (dolist (x path-list) (add-to-list target-path x)))
;; exec-path
(load-path-setter
 '("/opt/local/bin" "/usr/local/bin" "/Applications/pTex.app/teTeX/bin"
   "/Applications/LibreOffice.app/Contents/MacOS"
   "/Users/taka/Applications/Viewer/VLC.app/Contents/MacOS") 'exec-path)
;; load-path
(cond 
 (debug (require 'my-debug nil t))
 (t
  (let*
      ((p "~/Dropbox/emacs.d/") (g "~/devel/git/")
       (l `("~/Dropbox/org" "~/Dropbox/config" ,p
	    ,(concat g "org-mode/lisp") ,(concat g "org-mode/contrib/lisp")
	    ,(concat g "emacs-calfw") ,(concat g "el-get") ,(concat p "sdic")
	    ,(concat p "matlab") ,(concat p "yasnippet")
	    ,(concat p "yatex") ;; ,(concat p "org-sync")	     
	    ;; ,(concat p "auctex") ,(concat p "auctex/preview")
	    ,(concat p "anything"))))
    (load-path-setter l 'load-path))
  (require 'init nil t)
  (require 'private nil t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook (lambda () (my-time-lag)) t)          ;; Clock end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'my-test nil t)
(when (require 'pomodoro nil t)
  (pomodoro:start nil))
