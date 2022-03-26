;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME

;; https://github.com/qaiviq/echo-bar.el/blob/master/echo-bar.el
;; https://codeberg.org/akib/emacs-why-this

;; https://github.com/magit/transient/blob/master/docs/transient.org
;; https://github.com/magit/transient/wiki/Developer-Quick-Start-Guide
(with-eval-after-load "transient"
  (transient-define-prefix moom-transient-undo ()
    "Undo"
    :transient-suffix     'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["Undo (type `C-g' to exit)"
     [("u" "undo" moom-undo)]]
    )
  (transient-define-prefix moom-dispatch ()
    "Command list of `moom'."
    ["Command 1"
     [("r" "reset" moom-reset)]
     [("u" "undo" moom-undo)]]
    ["Filling region of a display"
     [("fft" "fill top" moom-fill-top)]
     [("ffb" "fill bottom" moom-fill-bottom)]
     [("ffl" "fill left" moom-fill-left)]
     [("ffr" "fill right" moom-fill-right)]]
    ["Move the frame"
     [("1" "move left" moom-move-frame-left)]
     [("3" "move right" moom-move-frame-right)]])
  )

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
