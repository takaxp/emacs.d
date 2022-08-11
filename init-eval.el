;; /Applications/Emacs.app/Contents/MacOS/Emacs -l ~/.emacs -l ~/Dropbox/emacs.d/config/init-eval.el -batch -f batch-byte-compile ~/Dropbox/emacs.d/config/init.el

(setq gc-cons-threshold (* 256 1024 1024))
(setq garbage-collection-messages t)
(setq byte-compile-warnings
      '(not free-vars unresolved callargs redefine obsolete noruntime
            cl-functions interactive-only make-local))
;; (setq byte-compile-warnings '(not obsolete))
(setq ad-redefinition-action 'accept)


;; packages used in utility.el for compiling without warning.
(require 'gcmh)
(require 'dash)
(require 'fringe-helper)
(require 'transient)
(require 'org-macs)


(provide 'init-eval)
;;; init-eval.el ends here
