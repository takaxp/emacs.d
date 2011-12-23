;;;; Configuration for Mac
;;;;                                       Last Update: 2011-11-30@11:14
;;;;                                       Takaaki ISHIKAWA  <takaxp@ieee.org>

(message "* --[ Loading an init file, takaxp-mac.el ] --")

;;; Testing nextstep only ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; [mode] matlab
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

;;; Shrink or Expand region
;; http://www.emacswiki.org/emacs/hideshowvis.el
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(autoload 'hideshowvis-minor-mode "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe." 'interactive)
(dolist (hook (list 'emacs-lisp-mode-hook
		    'c++-mode-hook))
  (add-hook hook 'hideshowvis-enable))

;;; auto-complete
;; http://cx4a.org/software/auto-complete/manual.ja.html
(when (require 'auto-complete nil t)
  (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/env/config/emacs/ac-dict")
  (ac-config-default)
  ;; ac-modes にあるメジャーモードで有効にする
  ;; lisp, c, c++, java, perl, cperl, python, makefile, sh, fortran, f90
  (global-auto-complete-mode t)
  ;; 追加のメジャーモードを設定
  ;;(add-to-list 'ac-modes 'org-mode)
  (add-to-list 'ac-modes 'objc-mode)
  ;; n文字以上で補完表示する
  (setq ac-auto-start 4)
  ;; n秒後にメニューを表示
  (setq ac-auto-show-menu 0.5)
  ;; ツールチップを表示しない
  (setq ac-use-quick-help nil)
  (setq ac-quick-help-delay 0.5)
  ;; C-n/C-p でメニューをたどる
  (setq ac-use-menu-map t)
  ;; 次の2つは，デフォルトで設定されている
  ;;(define-key ac-menu-map (kbd "C-n") 'ac-next)
  ;;(define-key ac-menu-map (kbd "C-p") 'ac-previous)
  (define-key ac-completing-map "\t" 'ac-complete)
  (define-key ac-completing-map "\r" nil))

;(setq ac-auto-start nil)
;(ac-set-trigger-key "TAB")
;(setq ac-candidate-max 10)

;;; Search option ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; igrep (M-x grep Override)
;(when (require 'igrep nil t)
;  (igrep-define lgrep (igrep-use-zgrep nil) (igrep-regex-option "-n -Ou8"))
;  (igrep-find-define lgrep
;		     (igrep-use-zgrep nil) (igrep-regex-option "-n -Ou8")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom open-current-directory-console-program "iTerm2.app"
  "Specify a console program"
  :type 'string
  :group 'takaxp-mac)

(defun open-current-directory ()
  " Open Current Directory for MacOSX
  0) Put this function in your .emacs
  1) M-x open-current-directory
  2) Terminal will open automatically
  3) Type M-v to paste and move to a path to the current directory in Emacs"
  (interactive)
  (let ((file-path (buffer-file-name (current-buffer))))
    (unless (string= file-path nil)
      (let ((directory
	    (substring file-path 0
		       (-
			(length file-path)
			(length (buffer-name (current-buffer)))))))
	(message "%s" directory)
	(shell-command-to-string (concat "echo cd " directory " |pbcopy"))
	(shell-command-to-string
	 (concat "open -a " open-current-directory-console-program))))))

(provide 'takaxp-mac)
