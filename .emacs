;;;; Emacs custom configuration
;;;;                                              Validated for Emacs 23.3.50
;;;;                                       Last Update: 2011-09-18@11:39
;;;;                                       Takaaki ISHIKAWA <takaxp@ieee.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To count the duration of loading .emacs files
;; Start clock: (defconst my-time-zero (current-time))
;; Stop  clock: (add-hook 'after-init-hook (lambda () (my-time-lag)) t)
(defun my-time-lag ()
  (let* ((now (current-time))
         (min (- (car now) (car my-time-zero)))
         (sec (- (car (cdr now)) (car (cdr my-time-zero))))
         (msec (/ (- (car (cdr (cdr now)))
                     (car (cdr (cdr my-time-zero))))
		  1000))
         (lag (+ (* 60000 min) (* 1000 sec) msec)))
    (message "'.emacs loading time: %d msec." lag)))
;; Clock start
(defconst my-time-zero (current-time))

;;; Set the path to the home directory
(defvar homedir (getenv "HOME"))
(when (eq window-system 'w32) (setq homedir "C:/Users/taka"))
(when (eq window-system 'ns)  (setq homedir "/Users/taka"))
(when (eq window-system 'x)   (setq homedir "/home/taka"))
(message "Set home dir: %s" homedir)

;;; Load-path and exec-path
;;; Note: M-x list-load-path-shadows
(setq load-path (append '("~/.emacs.d/"                      ; Personal files
			  "~/env/config/emacs"               ; auto-install
			  "~/env/config/emacs/apel"          ; for ElScreen
			  "~/env/config/emacs/ecb"           ; IDE/Code Brower
			  "~/env/config/emacs/lookup"        ; dictionary
			  "~/env/config/emacs/matlab"        ; matlab
			  "~/env/config/emacs/org-mode/lisp" ; org-mode
			  "~/env/config/emacs/sdic"          ; dictionary
			  "~/env/config/emacs/yatex"         ; YaTex
			  "/usr/local/share/emacs/site-lisp" ) load-path))
;;			  "/opt/local/share/emacs/site-lisp"
(setq exec-path (append '("/opt/local/bin" "/usr/local/bin") exec-path))


(require 'takaxp-init)
(require 'takaxp-face)
(require 'takaxp-utility)
(when (eq window-system 'ns)
  (require 'takaxp-mac))
(require 'takaxp-org-mode)
(require 'takaxp-private)
(require 'takaxp-keybinding)

;; init action
(message "%s" "MobileOrg sync ... [pull]")
(org-mobile-pull)
(setq alerm-table "~/Dropbox/org/today.org")
(run-at-time "00:00" nil 'set-alerms-from-file alerm-table)

;; Clock end
(add-hook 'after-init-hook (lambda () (my-time-lag)) t)

