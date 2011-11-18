;;;;                                              Validated for Emacs 23.3.50
;;;;                                       Last Update: 2011-11-19@01:22
;;;;                                       Takaaki ISHIKAWA <takaxp@ieee.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq byte-compile-warnings
;;       '(free-vars unresolved callargs redefine obsolete noruntime
;; 		  cl-functions interactive-only make-local))

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

;;; Load-path and exec-path
;;; Note: M-x list-load-path-shadows
(setq load-path (append '(
			  "~/.emacs.d/"                      ; Personal files
			  "~/env/config/emacs"               ; Auto-install
			  "~/env/config/emacs/anything/"     ; Anything
			  "~/env/config/emacs/matlab"        ; Matlab
			  "~/env/config/emacs/org-mode/lisp" ; Org-mode
			  ;; "~/devel/org-mode/org-7.5/lisp"    ; Org-mode
			  "~/env/config/emacs/sdic"          ; Dictionary
			  "~/env/config/emacs/yatex"         ; YaTex
			  ;; "~/env/config/emacs/auctex/preview"
			  ;; "~/env/config/emacs/auctex"
			  "/usr/local/share/emacs/site-lisp"
			  "/usr/share/emacs/site-lisp" ; preview-latex.el
			  ;; "~/Desktop/"
			  ) load-path))
;;			  "/opt/local/share/emacs/site-lisp"
(setq exec-path
      (append
       '("/opt/local/bin" "/usr/local/bin" "/Applications/pTex.app/teTeX/bin"
	 ) exec-path))

;; Requires ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'takaxp-init)
(require 'takaxp-face) ;; no require
(when (and (eq window-system 'ns) (= emacs-major-version 23))
  (require 'takaxp-mac))
(require 'takaxp-org-mode)
(require 'takaxp-utility)
(require 'takaxp-frame-control)
(require 'private) ;; no require
(require 'takaxp-keybinding) 
;;
;; (require 'e2wm-parallel-pack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clock end
(add-hook 'after-init-hook (lambda () (my-time-lag)) t)
(message "全部読み込んだよ ﾉ`Д)ﾉ:･'∵:.┻┻")

;; http://www.emacswiki.org/emacs/download/org-blog.el
;(require 'org-blog)


;;; test...

(require 'gist nil t)
(setq github-user "takaxp")

;; http://www.emacswiki.org/emacs-zh/CleanBufferList
(require 'midnight)
(setq clean-buffer-list-buffer-names
      (append clean-buffer-list-kill-buffer-names
	      '("note.txt")))
(setq clean-buffer-list-delay-general 1)
(setq clean-buffer-list-delay-special 10)
(when (= emacs-major-version 24)
  (global-set-key (kbd "S-<f12>") '(lambda ()
				     (interactive)
				     (toggle-input-method)
				     (message "toggle-input-method")))
  (global-set-key (kbd "S-SPC") '(lambda ()
				   (interactive)
				   (message "toggle-input-method"))))

;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
;; (setq preview-image-type 'dvipng)

;; Init test for org-tree-slide
;;(setq load-path (append '("~/env/config/emacs"
;;			  "~/env/config/emacs/org-mode"
;;			  ) load-path))
;;(require 'org-tree-slide)


;; https://raw.github.com/gist/1366316/691d8dc792259fb31b4bf5688da0bd0ed82959cf/barusu
;; (require 'barusu)

