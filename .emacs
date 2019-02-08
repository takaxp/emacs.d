;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t);; see also init-eval.el
;; (load "~/Dropbox/emacs.d/config/el-get-setup.el" nil t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "org-agenda"
  (defun my-org-agenda-done (&optional _arg)
    (interactive "P")
    (org-agenda-todo 'done)) ;; FIXME "d" 以外も対応したい．
  (org-defkey org-agenda-mode-map "d" 'my-org-agenda-done))

(with-eval-after-load "org"
  (unless (require 'hydra nil t)
    (defun hot-expand (str &optional mod)
      "Expand org template."
      (let (text)
        (when (region-active-p)
          (setq text (buffer-substring (region-beginning) (region-end)))
          (delete-region (region-beginning) (region-end)))
        (insert str)
        (org-try-structure-completion)
        (when mod (insert mod) (forward-line))
        (when text (insert text))))

    (defhydra hydra-org-tempo (:color blue :hint nil)
      "
_c_enter  qu_o_te     _e_macs-lisp    _L_aTeX:
_l_atex   _E_xample   p_y_thon        _i_ndex:
_a_scii   _v_erse     ip_Y_thon       _I_NCLUDE:
_s_rc     _g_o        _r_uby          _H_TML:
_h_tml    _S_HELL     _p_erl          _A_SCII:
^ ^       ^ ^         _P_erl tangled  plant_u_ml
"
      ("s" (hot-expand "<s"))
      ("E" (hot-expand "<e"))
      ("o" (hot-expand "<q"))
      ("v" (hot-expand "<v"))
      ("c" (hot-expand "<c"))
      ("l" (hot-expand "<l"))
      ("h" (hot-expand "<h"))
      ("a" (hot-expand "<a"))
      ("L" (hot-expand "<L"))
      ("i" (hot-expand "<i"))
      ("e" (hot-expand "<s" "emacs-lisp"))
      ("y" (hot-expand "<s" "python :results output"))
      ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0"))
      ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)"))
      ("p" (hot-expand "<s" "perl"))
      ("r" (hot-expand "<s" "ruby"))
      ("S" (hot-expand "<s" "sh"))
      ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
      ("P" (progn
             (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
             (hot-expand "<s" "perl")))
      ("I" (hot-expand "<I"))
      ("H" (hot-expand "<H"))
      ("A" (hot-expand "<A"))
      ("<" self-insert-command "ins")
      ("q" nil "quit"))

    (define-key org-mode-map (kbd "<")
      '(lambda () (interactive)
         (if (or (region-active-p) (looking-back "^\s*" 1))
             (hydra-org-tempo/body)
           (self-insert-command 1))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs ends here
