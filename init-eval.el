;; /Applications/Emacs.app/Contents/MacOS/Emacs -l ~/.emacs -l ~/Dropbox/emacs.d/config/init-eval.el -batch -f batch-byte-compile-if-not-done ~/Dropbox/emacs.d/config/init.el

(require 'postpone nil t) ;; as an entry point
(require 'calfw-org nil t) ;; mode-map
(require 'gnuplot-mode nil t) ;; mode-map
(require 'latex-math-preview nil t) ;; mode-map
(require 'org-tree-slide nil t) ;; mode-map
(require 'ox-icalendar nil t) ;; mode-map
(require 'perl-mode nil t) ;; mode-map
(require 'python-mode nil t) ;; mode-map
(require 'manage-minor-mode nil t) ;; mode-map
(require 'org-grep nil t) ;; defvar
(require 'appt) ;; advice
(require 'checkdoc) ;; advice
(require 'cl-lib nil t)
(require 'doxymacs nil t)
(require 'editorconfig nil t)
(require 'flyspell nil t)
(require 'ggtags nil t)
(require 'js2-mode nil t)
(require 'nxml-mode nil t)
(require 'org nil t)
(require 'org-attach nil t)
(require 'org-attach-screenshot nil t)
(require 'org-capture nil t)
(require 'org-mac-link nil t)
(require 'org-recent-headings nil t)
(require 'orgalist nil t)
(require 'origami nil t)
(require 'pomodoro nil t)
(require 'session nil t)
(require 'tern nil t)
(require 'tern-auto-complete nil t)
(require 'undo-tree nil t)
(require 'neotree nil t)
(require 'web-mode nil t)
(require 'yasnippet nil t)
(require 'time nil t)
(require 'time-stamp nil t)
(require 'update-stamp nil t)
(require 'emmet-mode nil t)
(require 'gif-screencast nil t)
(require 'keyfreq nil t)
(require 'winner nil t)
(require 'cask nil t)
(require 'fringe-helper nil t)

(if (require 'shut-up nil t)
    (progn
      (shut-up (require 'emms-setup nil t))
      (shut-up (require 'yatex nil t)))
  (require 'emms-setup nil t)
  (require 'yatex nil t))

(provide 'init-eval)
;;; init-eval.el ends here
