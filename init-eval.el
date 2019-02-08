;; /Applications/Emacs.app/Contents/MacOS/Emacs -l ~/.emacs -l ~/Dropbox/emacs.d/config/init-eval.el -batch -f batch-byte-compile-if-not-done ~/Dropbox/emacs.d/config/init.el

;; as an entry point
(require 'postpone nil t)
(require 'init-org nil t)
(require 'utility nil t)

;; mode-map
(require 'calfw-org nil t)
(require 'gnuplot-mode nil t)
(require 'latex-math-preview nil t)
(require 'org-tree-slide nil t)
(require 'ox-icalendar nil t)
(require 'perl-mode nil t)
(require 'python-mode nil t)
(require 'manage-minor-mode nil t)
(require 'view nil t)

;; def var
(require 'org-grep nil t)

;; advice
(require 'appt)
(require 'checkdoc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'moom nil t) ;; could be removed
(require 'cask nil t)
(require 'cl-lib nil t)
(require 'doxymacs nil t)
(require 'editorconfig nil t)
(require 'emmet-mode nil t)
(require 'flyspell nil t)
(require 'fringe-helper nil t) ;; for git-gutter-fringe
(require 'ggtags nil t)
(require 'gif-screencast nil t)
(require 'js2-mode nil t)
(require 'keyfreq nil t)
(require 'neotree nil t)
(require 'nxml-mode nil t)
(require 'org nil t)
(require 'org-attach nil t)
(require 'org-attach-screenshot nil t)
(require 'org-capture nil t)
(require 'org-mac-link nil t)
(require 'orgalist nil t)
(require 'origami nil t)
(require 'pomodoro nil t)
(require 'recentf nil t) ;; for org-recent-headings
(require 'session nil t)
(require 'tern nil t)
(require 'tern-auto-complete nil t)
(require 'time nil t)
(require 'time-stamp nil t)
(require 'undo-tree nil t)
(require 'update-stamp nil t)
(require 'web-mode nil t)
(require 'winner nil t)
(require 'yasnippet nil t)
(require 'doom-modeline nil t)

(if (require 'shut-up nil t)
    (progn
      (shut-up (require 'emms-setup nil t)
               (require 'yatex nil t)
               (require 'bm nil t)))
  (require 'emms-setup nil t)
  (require 'yatex nil t)
  (require 'bm nil t))

(provide 'init-eval)
;;; init-eval.el ends here
