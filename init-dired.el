;; init-dired.el --- config for dired-mode -*- lexical-binding: t -*-

(cond ((require 'nerd-icons-dired nil t)
       (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))
      ((require 'icons-in-terminal nil t)
       (add-hook 'dired-mode-hook #'icons-in-terminal-dired-mode))
      ((require 'all-the-icons nil t)
       (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)))

(keymap-set dired-mode-map "C-c C-o" 'crux-open-with)
(keymap-set dired-mode-map "F" 'my-reveal-in-finder)
;; 上位ディレクトリへの移動
(keymap-set dired-mode-map "u" 'dired-up-directory)
;; Finder を使ったファイルオープン
(keymap-set dired-mode-map "f" 'ns-open-file-using-panel)
(keymap-set dired-mode-map "C-M-p" (lambda () (interactive) (other-window -1)))
(keymap-set dired-mode-map "C-M-n" (lambda () (interactive) (other-window 1)))

;; brew install coreutils
(when (eq system-type 'darwin)
  (setq insert-directory-program "gls"))

(setq dired-listing-switches "-lha")
(setq completion-ignored-extensions
      (append completion-ignored-extensions
              '("./" "../" ".xlsx" ".docx" ".pptx" ".DS_Store")))

;; "dired-mode-map"
;; Use build-in `wdired-mode'.
;; (keymap-set dired-mode-map "R" 'wdired-change-to-wdired-mode)
;; http://elpa.gnu.org/packages/gited.html
(when (require 'gited nil t)
  (keymap-set dired-mode-map "C-x C-g" 'gited-list-branches))
;; https://github.com/Fuco1/dired-hacks
(when (require 'dired-narrow nil t)
  (keymap-set dired-mode-map "/" 'dired-narrow))
(require 'dired-du nil t)
(when (require 'ivy-dired-history nil t)
  ;; ivy-dired-history-variable は，session.el で明示的に管理中．
  ;; check session-globals-include
  (keymap-set dired-mode-map "," 'dired))
(declare-function dired-extra-startup "dired-x")
(when (require 'dired-x nil t)
  (dired-extra-startup))

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

(when (autoload-if-found '(dired-recent-open dired-recent-mode)
                         "dired-recent" nil t)
  (keymap-set dired-mode-map "r" #'dired-recent-open)
  (with-eval-after-load "dired-recent"
    ;; (require 'helm-config nil t)
    (dired-recent-mode 1)))

(provide 'init-dired)
