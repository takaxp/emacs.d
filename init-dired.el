(define-key dired-mode-map (kbd "C-c C-o") 'crux-open-with)

(setq completion-ignored-extensions
      (append completion-ignored-extensions
	      '("./" "../" ".xlsx" ".docx" ".pptx" ".DS_Store")))

;; "dired-mode-map"
;; Use build-in `wdired-mode'.
;; (define-key dired-mode-map (kbd "R") 'wdired-change-to-wdired-mode)
;; http://elpa.gnu.org/packages/gited.html
(when (require 'gited nil t)
  (define-key dired-mode-map (kbd "C-x C-g") 'gited-list-branches))
;; https://github.com/Fuco1/dired-hacks
(when (require 'dired-narrow nil t)
  (define-key dired-mode-map (kbd "/") 'dired-narrow))
(require 'dired-du nil t)
(when (require 'ivy-dired-history nil t)
  ;; ivy-dired-history-variable は，session.el で明示的に管理中．
  ;; check session-globals-include
  (define-key dired-mode-map "," 'dired))
(declare-function dired-extra-startup "dired-x")
(when (require 'dired-x nil t)
  (dired-extra-startup))
(defun my-reveal-in-finder ()
  "Reveal the current buffer in Finder."
  (interactive)
  (shell-command-to-string "open ."))
;; dired-x を読み込んだあとじゃないとだめ
(define-key dired-mode-map (kbd "F") 'my-reveal-in-finder)
;; 上位ディレクトリへの移動
(define-key dired-mode-map (kbd "u") 'dired-up-directory)
;; Finder を使ったファイルオープン
(define-key dired-mode-map (kbd "f") 'ns-open-file-using-panel)

(define-key dired-mode-map
  (kbd "C-M-p") (lambda () (interactive) (other-window -1)))
(define-key dired-mode-map
  (kbd "C-M-n") (lambda () (interactive) (other-window 1)))

;; https://github.com/xuchunyang/emacs.d
;; type "!" or "X" in dired
(when (eq system-type 'darwin)
  (setq dired-guess-shell-alist-user
	(list
	 (list (rx (and "."
			(or
			 ;; Videos
			 "mp4" "avi" "mkv" "rmvb"
			 ;; Torrent
			 "torrent"
			 ;; PDF
			 "pdf"
			 ;; Image
			 "gif" "png" "jpg" "jpeg")
			string-end)) "open"))))

(provide 'init-dired)
