;; (package-initialize) ;; for Emacs 25                Validated in Emacs 25.3
;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Playground
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "org"
  (when (require 'org-edna nil t)

    ))















(with-eval-after-load "org"
  (setq-default org-catch-invisible-edits 'error)

  ;; Settings for the latest Org Mode
  (add-to-list 'org-structure-template-alist '(?S . "src emacs-lisp"))
  (define-skeleton org-skeleton-src-block
    "" nil "#+BEGIN_SRC " _ \n \n "#+END_SRC")
  (define-skeleton org-skeleton-emacslisp-src-block
    "" nil "#+BEGIN_SRC emacs-lisp"\n _ \n "#+END_SRC"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs ends here
