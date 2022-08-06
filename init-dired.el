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

(define-key dired-mode-map (kbd "C-M-p")
  (lambda () (interactive) (other-window -1)))
(define-key dired-mode-map (kbd "C-M-n")
  (lambda () (interactive) (other-window 1)))

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

(when (and nil
           (require 'hydra nil t)
	         (require 'dired-recent nil t))

  ;; (define-key dired-mode-map "h" 'hydra-dired/body)
  ;; (define-key dired-mode-map "r" 'dired-recent-open)

  ;; https://github.com/abo-abo/hydra/wiki/Dired
  (defhydra hydra-dired (:hint nil :color pink)
    "
_+_ mkdir        _v_iew        _m_ark            _(_ details       _i_nsert-subdir
_C_opy           View _O_ther  _U_nmark all      _)_ omit-mode
_D_elete         _o_pen other  _u_nmark          _l_ redisplay     _w_ kill-subdir
_R_ename         _M_ chmod     _t_oggle          _g_ revert buf
_Y_ rel symlink  _G_ chgrp     _E_xtension mark  _s_ort            _r_ dired-recent-open
_S_ymlink        ^ ^           _F_ind marked     _._ toggle hydra  _?_ summary
_A_ find regexp  _Z_ compress  T - tag prefix
_Q_ repl regexp   [wdired] C-x C-q : edit / C-c C-c : commit / C-c ESC : abort
"
    ;; ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ;; ("=" diredp-ediff)         ;; smart diff
    ("?" dired-summary)
    ;; ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)        ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ;; ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)        ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)   ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-recent-open)
    ;; ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ;; ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))
  )

(provide 'init-dired)
