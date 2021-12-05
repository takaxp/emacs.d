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

(with-eval-after-load "postpone"
  ;; At least in Big Sur, this setting shall be used with side car for moom.el.
  ;; Without side car in Big Sur, the following setting is also correct.
  ;; Then what about other macOSs?
  (when (string= "Big Sur" (macos-name (macos-version)))
    (setq moom--common-margin '(0 0 0 0))))

(with-eval-after-load "postpone"
  (autoload-if-found '(vterm) "vterm"  nil t))

(with-eval-after-load "moom"
  (defun my-moom-mixmized-plus ()
    (moom-change-frame-width-single)
    (moom-move-frame-to-center))
  (setq moom-after-fill-screen-hook 'my-moom-mixmized-plus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;;(advice-add 'epg--check-error-for-decrypt :override 'ignore)
