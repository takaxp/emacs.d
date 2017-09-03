;; (package-initialize) ;; for Emacs 25                Validated in Emacs 25.2
;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;; projectile, helm-projectile, org-projectile
(with-eval-after-load "helm-config"
  (when (require 'helm-projectile nil t)
    (projectile-global-mode)
    (setq projectile-completion-system 'helm))
  (when (require 'org-projectile nil t))
  (when (require 'org-projectile-helm nil t)))


;; "http://projectile.readthedocs.io/en/latest/"
;; - [ ] projectile-tags-command, gtags との連携
(with-eval-after-load "projectile"
  (when (require 'neotree nil t)
    (setq projectile-switch-project-action 'neotree-projectile-action)
    (defun advice:neotree-dir (path)
      "Extension to change the frame width automatically."
      (unless (neo-global--window-exists-p)
        (neotree-show)))
    (advice-add 'neotree-dir :before #'advice:neotree-dir))
  (setq projectile-use-git-grep t)
  (setq projectile-mode-line
        '(:eval (format " P:%s" (projectile-project-name)))))

;; https://www.emacswiki.org/emacs/eimp.el (emacs de image)
