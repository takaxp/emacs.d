;; (package-initialize) ;; for Emacs 25                Validated in Emacs 25.2
;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "org-grep" (setq org-grep-extensions '(".org")))
(with-eval-after-load "org"
  (add-to-list 'org-refile-targets '("money.org" :level . 1)))

;; Let's use easy-hugo!
(when (and nil (require 'easy-hugo nil t))
  (setq easy-hugo-default-ext ".org")
  (setq easy-hugo-basedir "~/Dropbox/org/blog/public")
  (setq easy-hugo-url "https://pxaka.tokyo/blog")
  ;; (setq easy-hugo-sshdomain "~/.ssh/config")
  ;; (setq easy-hugo-root "")

  (defun hugo-org-header-template ()
    (interactive)
    (when (string= major-mode 'org-mode)
      (let ((title "#+TITLE: ")
            (author "#+AUTHOR: Takaaki Ishikawa\n")
            (date (format "#+DATE: %s\n"
                          (format-time-string "%FT%H:%M:%S%:z")))
            (description "#+DESCRIPTION: \n")
            (tags "#+TAGS: \n")
            (draft "#+DRAFT: false\n"))
        (goto-char 0)
        (insert title)
        (save-excursion
          (insert "\n" author date description tags draft))))))

;; Let's try
;; - An emacs extension for displaying gcov data on your code
;;   https://github.com/adamniederer/cov
