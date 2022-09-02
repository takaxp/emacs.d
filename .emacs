;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME
;; https://github.com/magit/transient/blob/master/docs/transient.org
;; https://github.com/magit/transient/wiki/Developer-Quick-Start-Guide

;; (profiler-start 'cpu)
;; (setq my-suppress-message-p nil)
;; (setq measure-exec-time-list '(my-show-org-buffer
;;                                my-private-conf-activate
;;                                my-org-babel-load-activate
;;                                my-org-modules-activate
;;                                my-org-agenda-prepare-buffers
;;                                ))

(with-eval-after-load "moom-transient"

  )

;; Boot mode selection
(cond
 (nil ;; minimal boot or DOOM Emacs (use toggle-doom.sh to switch)
  (when (boundp 'ns-command-modifier) (setq ns-command-modifier 'meta))
  (when (and (memq window-system '(ns mac))
             (fboundp 'mac-get-current-input-source))
    ;; "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese" for Big Sur
    (custom-set-variables
     '(mac-default-input-source "com.google.inputmethod.Japanese.base"))
    (mac-input-method-mode 1)
    (global-set-key (kbd "M-SPC") 'mac-ime-toggle)
    (global-set-key (kbd "S-SPC") 'mac-ime-toggle)))
 (nil ;; To test the latest org
  (add-to-list 'load-path (expand-file-name "~/devel/git/org-mode/lisp"))
  (setq org-agenda-files '("~/Desktop/test/hoge.org")))
 (nil ;; Spacemacs
  (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el")))
 (t ;; Normal mode. see also init-eval.el
  (load "~/Dropbox/emacs.d/config/init-env.el" nil t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "postpone"
  ;; At least in Big Sur, this setting shall be used with side car for moom.el.
  ;; Without side car in Big Sur, the following setting is also correct.
  ;; Then what about other macOSs?

  ;; (when (string= "Big Sur" (macos-name (macos-version)))
  ;;   (setq moom--common-margin '(0 0 0 0)))

  ;;(profiler-report)
  )

;; (require 'postpone)
;; (my-show-org-buffer "next.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;;(advice-add 'epg--check-error-for-decrypt :override 'ignore)
