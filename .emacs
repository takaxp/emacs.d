;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME

;; https://github.com/qaiviq/echo-bar.el/blob/master/echo-bar.el
;; https://codeberg.org/akib/emacs-why-this

;; https://github.com/magit/transient/blob/master/docs/transient.org
;; https://github.com/magit/transient/wiki/Developer-Quick-Start-Guide

;; (autoload 'moom-transient-undo "transient" nil t)
;; (autoload 'moom-dispatch "transient" nil t)
;; (with-eval-after-load "transient"
;;   ;; "q" で transient を抜ける方法を提供する．
;;   (transient-define-prefix moom-transient-undo ()
;;     "Undo"
;;     :transient-suffix     'transient--do-stay
;;     :transient-non-suffix 'transient--do-warn
;;     ["Undo (type `C-g' to exit)"
;;      [("u" "undo" moom-undo)]]
;;     )

;;   (transient-define-prefix moom-dispatch ()
;;     "Command list of `moom'."
;;     :transient-suffix     'transient--do-stay
;;     :transient-non-suffix 'transient--do-warn
;;     [""
;;      ["Fill screen"
;;       ("f s" "screen" moom-fill-screen)
;;       ("f f w" "width" moom-fill-width)
;;       ("f f h" "height" moom-fill-height)
;;       ("f f m" "band" moom-fill-band)
;;       ("f f t" "top" moom-fill-top)
;;       ("f f b" "bottom" moom-fill-bottom)
;;       ("f f l" "left" moom-fill-left)
;;       ("f f r" "right" moom-fill-right)
;;       ("f f 1" "fill top" moom-fill-top-left)
;;       ("f f 2" "fill bottom" moom-fill-top-right)
;;       ("f f 3" "fill left" moom-fill-bottom-left)
;;       ("f f 4" "fill right" moom-fill-bottom-right)]
;;      ["Fit"
;;       ("M-<f1>" "edge left" moom-move-frame-to-edge-left)
;;       ("M-<f3>" "edge right" moom-move-frame-to-edge-right)
;;       ("<f1>", "edge top" moom-move-frame-to-edge-top)
;;       ("S-<f1>", "edge bottom" moom-move-frame-to-edge-bottom)
;;       ("fcl" "center left" moom-move-frame-to-centerline-from-left)
;;       ("fcr" "center right" moom-move-frame-to-centerline-from-right)
;;       ("fct" "center top" moom-move-frame-to-centerline-from-top)
;;       ("fcb" "center bottom" moom-move-frame-to-centerline-from-bottom)]]
;;     ;; moom-change-frame-width
;;     ;; moom-change-frame-height
;;     ["Filling region of a display"
;;      ["Move the frame"
;;       ("0" "move top-left" moom-move-frame)
;;       ("1" "move left" moom-move-frame-left)
;;       ("2" "move center" moom-move-frame-to-center)
;;       ("3" "move right" moom-move-frame-right)]
;;      ["Expand"
;;       ("cfs" "single" moom-change-frame-width-single) ;; disabled, why?
;;       ("cfd" "double" moom-change-frame-width-double)
;;       ("cfa" "3/2" moom-change-frame-width-half-again)
;;       ("cfh" "height" moom-cycle-frame-height)]
;;      ["Utilities"
;;       ("r" "reset" moom-reset)
;;       ("u" "undo" moom-undo)]]
;;     ))

(cond
 (nil ;; To test the latest org
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

  ;; (when (string= "Big Sur" (macos-name (macos-version)))
  ;;   (setq moom--common-margin '(0 0 0 0)))
  )

(with-eval-after-load "postpone"
  (autoload-if-found '(vterm) "vterm"  nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;;(advice-add 'epg--check-error-for-decrypt :override 'ignore)
