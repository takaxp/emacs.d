;; (package-initialize) ;; for Emacs 25                Validated in Emacs 25.2
;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "org-grep" (setq org-grep-extensions '(".org")))
(with-eval-after-load "org"
  (add-to-list 'org-refile-targets '("money.org" :level . 1)))

;; Testing: git-complete
;; (when (autoload-if-found '(git-complete) "git-complete" nil t)
;;   (global-set-key (kbd "C-c f <tab>") 'git-complete))

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

(when (executable-find "qt_color_picker")
  (with-eval-after-load "helm-config"
    (require 'edit-color-stamp nil t))
  (global-set-key (kbd "C-c f c p") 'edit-color-stamp))

;; (with-eval-after-load "helm-ag"
;;   (when (executable-find "rg")
;;     (setq helm-ag-base-command "rg --nocolor --nogroup")))

;; (when (require 'focus nil t))

;; (when (require 'beacon nil t)
;;   (setq beacon-lighter nil)
;;   (setq beacon-size 16)
;;   (setq beacon-blink-duration 0.5)
;;   (setq beacon-blink-delay 0.5)
;;   (setq beacon-blink-when-point-moves-vertically 64)
;;   (setq beacon-color (mac-get-cursor-color))
;;   (beacon-mode 1))
