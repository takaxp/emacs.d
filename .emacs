;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME
(cond (nil ;; To test the latest org
       (add-to-list 'load-path (expand-file-name "~/devel/git/org-mode/lisp"))
       (setq org-agenda-files '("~/Desktop/hoge.org")))
      (t ;; Normal mode. see also init-eval.el
       (load "~/Dropbox/emacs.d/config/init-env.el" nil t)))
;; (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "keypression"
  ;; (setq keypression-frame-origin 'keypression-origin-bottom-right)
  (setq keypression-frame-origin 'keypression-origin-top-left)
  (setq keypression-x-offset (- (frame-pixel-width) 100))
  (setq keypression-y-offset 10))

(with-eval-after-load "postpone"
  ;; elfeed, elfeed-org,elfeed-web
  (when (autoload-if-found
         '(elfeed elfeed-web-start)
         "elfeed" nil t)

    (with-eval-after-load "elfeed"
      (when (require 'elfeed-org nil t)
        (elfeed-org)
        (setq rmh-elfeed-org-files (list "~/Dropbox/org/elfeed.org")))
      ;; これで elfeed-feeds が更新される
      ;; その後，M-x elfeed, M-x elfeed-update する
      (when (require 'elfeed-web nil t)
        (setq elfeed-web-data-root (concat my-elget-package-dir "/web"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;;(advice-add 'epg--check-error-for-decrypt :override 'ignore)
