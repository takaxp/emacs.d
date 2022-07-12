;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME
;; https://github.com/qaiviq/echo-bar.el/blob/master/echo-bar.el
;; https://codeberg.org/akib/emacs-why-this

;; (repeat-mode 1)

;; https://github.com/magit/transient/blob/master/docs/transient.org
;; https://github.com/magit/transient/wiki/Developer-Quick-Start-Guide
(with-eval-after-load "selected"
  (require 'moom nil t)
  (require 'transient nil t)

  ;; "q" で transient を抜ける方法を提供する．
  (transient-define-prefix moom-transient-undo ()
    "Undo"
    :transient-suffix     'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["Undo (type `C-g' to exit)"
     [("u" "undo" moom-undo)]]
    )

  (transient-define-prefix moom-dispatch ()
    "Command list of `moom'."
    :transient-suffix     'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    [""
     ["Fill screen"
      ;; ("f s" "screen" moom-fill-screen)
      ("f w" "width" moom-fill-width)
      ("f h" "height" moom-fill-height)
      ("f m" "band" moom-fill-band)
      ("f t" "top" moom-fill-top)
      ("f b" "bottom" moom-fill-bottom)
      ("f l" "left" moom-fill-left)
      ("f r" "right" moom-fill-right)
      ("f 1" "fill top" moom-fill-top-left)
      ("f 2" "fill bottom" moom-fill-top-right)
      ("f 3" "fill left" moom-fill-bottom-left)
      ("f 4" "fill right" moom-fill-bottom-right)]
     ["Fit"
      ("M-<f1>" "edge left" moom-move-frame-to-edge-left)
      ("M-<f3>" "edge right" moom-move-frame-to-edge-right)
      ("<f1>", "edge top" moom-move-frame-to-edge-top)
      ("S-<f1>", "edge bottom" moom-move-frame-to-edge-bottom)
      ("cl" "center left" moom-move-frame-to-centerline-from-left)
      ("cr" "center right" moom-move-frame-to-centerline-from-right)
      ("ct" "center top" moom-move-frame-to-centerline-from-top)
      ("cb" "center bottom" moom-move-frame-to-centerline-from-bottom)]]
    ;; moom-change-frame-width
    ;; moom-change-frame-height
    ["Filling region of a display"
     ["Move the frame"
      ("0" "move top-left" moom-move-frame)
      ("1" "move left" moom-move-frame-left)
      ("2" "move center" moom-move-frame-to-center)
      ("3" "move right" moom-move-frame-right)]
     ["Expand"
      ("c f s" "single" moom-change-frame-width-single) ;; disabled, why?
      ("c f d" "double" moom-change-frame-width-double)
      ("c f a" "3/2" moom-change-frame-width-half-again)
      ("c f h" "height" moom-cycle-frame-height)]
     ["Utilities"
      ("r" "reset" moom-reset)
      ("u" "undo" moom-undo)]]
    ))

(with-eval-after-load "transient"
  (transient-define-prefix my-org-bullet-and-checkbox ()
	  "Commands to handle bullet and checkbox"
	  [["Bullet"
		  ("i" "insert" my-org-insert-bullet)
		  ("d" "delete" my-org-delete-bullet)]
		 ["Checkbox"
		  ("[" "insert" my-org-insert-checkbox-into-bullet)
		  ("]" "delete" my-org-delete-checkbox-from-bullet)
		  ;;("a" "toggle checkbox" my-org-toggle-checkbox)
		  ;;("h" "cycle" my-cycle-bullet-at-heading) ;; single line
		  ]
		 ["Bullet and Checkbox"
		  ("I" "insert" my-org-insert-bullet-and-checkbox)
		  ("D" "delete" my-org-delete-bullet-and-checkbox)]]))

(cond
 (nil
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
  ;; M-x vterm-copy-mode (C-c C-t)
  (autoload-if-found '(vterm) "vterm"  nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;;(advice-add 'epg--check-error-for-decrypt :override 'ignore)
