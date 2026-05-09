;; -*- lexical-binding: t; -*-
;;                                          https://takaxp.github.io/init.html
(require 'elpaca-install)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility
(defmacro my-elpaca-github (repo &optional sym)
  "Elpaca wrapper for GitHub OWNER/REPO."
  (let* ((name (file-name-nondirectory repo))
         (pkg-sym  (or sym (intern name))))
    `(elpaca (,pkg-sym :host github :repo ,repo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages to install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Boot
(my-elpaca-github "takaxp/postpone")
(elpaca 'gcmh)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core
(elpaca 'aggressive-indent)
(elpaca 'ws-butler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coursor/Pointer
(my-elpaca-github "takaxp/ah")
(my-elpaca-github "takaxp/bsv")
(elpaca 'bm)
(elpaca 'centered-cursor-mode)
(elpaca 'smart-mark)
(elpaca 'syntax-subword)
(elpaca 'expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing
(elpaca 'modern-cpp-font-lock)
(elpaca 'orgalist) ;; for ChangeLog mode
(elpaca 'yaml-mode)
(elpaca 'nhexl-mode)
(elpaca 'csv-mode)
(elpaca 'es-mode)
(elpaca 'markdown-mode)
(my-elpaca-github "emacsmirror/ascii") ;; (elpaca 'ascii)
(elpaca 'web-mode)
(elpaca 'po-mode)
(elpaca 'go-mode)
(elpaca 'flyspell-correct)
(elpaca 'counsel-world-clock)
(elpaca 'latex-math-preview)
(when (eq system-type 'darwin)
  (elpaca 'osx-dictionary))
(progn ;; describe-number ;; failed on (elpaca 'describe-number)
  (my-elpaca-github "d5884/yabin")
  (my-elpaca-github "netromdk/describe-number"))
(elpaca 'smartparens)
(elpaca 'grugru)
(elpaca 'replace-from-region)
(my-elpaca-github "zk-phi/git-complete") ;; (elpaca 'git-complete)
(progn ;; selected-related
  (my-elpaca-github "takaxp/counsel-selected")
  (my-elpaca-github "takaxp/help-fns-plus" help-fns+)
  (elpaca 'selected)
  (elpaca 'helpful))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display
(elpaca 'mlscroll)
(elpaca 'delight)
(elpaca 'git-gutter-fringe)
(elpaca 'japanese-holidays)
(elpaca 'highlight-symbol)
(progn ;; Nerd-icons
  (elpaca 'nerd-icons)
  (elpaca 'nerd-icons-dired)
  (elpaca 'nerd-icons-corfu)
  (elpaca 'nerd-icons-ivy-rich)
  (my-elpaca-github "seagle0128/icons-in-terminal.el" icons-in-terminal))
(elpaca 'keypression)

(progn ;; ivy
  (elpaca 'smex)
  (elpaca 'ivy-rich)
  (elpaca 'counsel-gtags)
  (elpaca 'counsel-projectile)
  (elpaca 'ivy-omni-org)
  (elpaca 'counsel-osx-app)
  (elpaca 'ivy-pass)
  (elpaca 'ivy-emms))
(my-elpaca-github "takaxp/imenu-list")
(elpaca 'dimmer)
(my-elpaca-github "emacs-vs/rainbow-csv") ;; (elpaca 'rainbow-csv)
(elpaca 'command-log-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Media
(elpaca 'emms) ;; savannah.gnu.org

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File management
(elpaca 'crux)
(elpaca 'backup-each-save)
(elpaca 'dired-du)
(my-elpaca-github "Fuco1/dired-hacks")
(elpaca 'dired-recent)
(my-elpaca-github "jixiuf/ivy-dired-history")
(when (eq system-type 'darwin)
  (elpaca 'osx-trash))
(elpaca 'undo-fu)
(elpaca 'super-save)
(my-elpaca-github "takaxp/session")
(elpaca 'neotree)
(my-elpaca-github "takaxp/facecheck")
(elpaca 'keyfreq)
(elpaca (disk-usage :host gitlab :repo "ambrevar/emacs-disk-usage"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Development
(elpaca 'gist)
(progn ;; flycheck
  (elpaca 'flycheck)
  (elpaca 'flycheck-pos-tip)
  (elpaca 'flycheck-clang-tidy))
(elpaca 'quickrun)
(elpaca 'ggtags)
(elpaca '0xc)
(elpaca 'package-lint)
(elpaca 'projectile)
(elpaca 'relint)
(elpaca 'editorconfig)
(elpaca 'cov)
(progn
  (my-elpaca-github "lassik/emacs-format-all-the-code" format-all)
  (elpaca 'language-id)) ;; safety install format-all
(elpaca 'uuid)
(progn
  (elpaca 'corfu-prescient)
  (elpaca 'corfu)) ;; safety install corfu-prescient
(elpaca 'kind-icon)
(my-elpaca-github "xenodium/org-block-capf")
(elpaca 'vterm)
(elpaca 'slime)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org mode
(elpaca 'ob-async)
(elpaca 'org-download)
(my-elpaca-github "takaxp/org-plist")
(my-elpaca-github "takaxp/org-onit")
(elpaca 'org-clock-today)
(elpaca 'orgbox)
(elpaca 'ob-http)
(elpaca 'ob-go)
(elpaca (org-tree-slide
	 :host github :repo "takaxp/org-tree-slide" :branch "develop"))
(elpaca 'htmlize)

(elpaca 'ox-hugo)
(elpaca 'ox-qmd)
(elpaca 'ox-gfm)
(elpaca 'ox-reveal)
(elpaca 'ox-json)
(elpaca 'org-mac-link)

(elpaca 'orglink)
(elpaca 'org-appear)
(elpaca 'org-recent-headings)
(elpaca 'orgnav)
(elpaca 'toc-org)
(elpaca (org-screenshot :host github :repo "dfeich/org-screenshot"
			:main "org-attach-screenshot.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frame/Window
(elpaca 'moom)
(elpaca 'shackle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font/Face
(elpaca 'hl-todo)
(elpaca 'rainbow-mode)
(elpaca 'edit-color-stamp)
(elpaca 'volatile-highlights)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; async

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility
(elpaca 'google-this)
(elpaca 'gt)
(when (eq system-type 'darwin)
  (elpaca 'osx-lib))
(elpaca 'gif-screencast)
(elpaca 'manage-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Under test
(progn
  (elpaca 'gptel))

;; required at the end of this code to run all items for batch-mode
(when noninteractive
  (elpaca-wait))

;;; ivy/counsel/swiper
(elpaca 'counsel)
(elpaca 'ivy-prescient)
;; (elpaca 'corfu-prescient)

(elpaca (counsel :host github :repo "abo-abo/swiper" :main "counsel.el"))
;; (elpaca (ivy-prescient :host github :repo "radian-software/prescient.el")
;; 	:main "ivy-prescient")
;; (elpaca (corfu-prescient :host github :repo "radian-software/prescient.el")
;; 	:main "corfu-prescient")

;;; magit
(progn
  (elpaca 'magit)
  (elpaca 'transient :inherit nil))

;;; org
(elpaca 'org-contrib)
(elpaca 'org)

;;; async
(elpaca 'async) ;(my-elpaca-github "jwiegley/emacs-async" async)

;; Having issues
;; (elpaca 'org-extra-emphasis)
;; (elpaca 'emr) ;; iedit installed version lower than min require 0.97

;;; compat
;; compat installed version (30 2 9999) lower than min required 31 (2026-05-08)
(elpaca 'compat)

;; previous
(when nil
  ;;; Boot
  (my-elpaca-github "cask/shut-up")
  ;;; Editing
  (my-elpaca-github "R-emacs/r-mode")
  (elpaca 'json-mode) ;; use json-ts-mode.el
  (elpaca 'js2-mode)
  (elpaca 'js2-refactor)
  (elpaca 'ac-js2)
  (my-elpaca-github "bruceravel/gnuplot-mode" gnuplot)
  (elpaca 'cmake-mode) ;; taking long time
  (my-elpaca-github "takaxp/ivy-yasnippet")
  (my-elpaca-github "yasuyk/web-beautify")
  (my-elpaca-github "sbrisard/bratex")
  (elpaca 'logview)
  ;;; Display
  (my-elpaca-github "syohex/emacs-go-eldoc" go-eldoc)
  ;;; File management
  (my-elpaca-github "davep/uptimes.el" uptimes)
  ;;; Development
  (my-elpaca-github "gregsexton/origami.el" origami)
  (progn ;; auto-complete
    (elpaca 'skewer-mode)
    (elpaca 'auto-complete) ;; require 'skewer
    (elpaca 'auto-complete-clang))
  (elpaca (company-prescient :host github :repo "radian-software/prescient.el")
	  :main "company-prescient")
  (my-elpaca-github "xenodium/company-org-block")
  ;;; org
  (my-elpaca-github "takaxp/org-bookmark-heading")

  ;;; Frame/Window
  (elpaca 'popwin)
  (my-elpaca-github "seagle0128/doom-modeline"); :depends (eldoc-eval))

  (my-elpaca-github "k-talo/smooth-scroll.el")
  (my-elpaca-github "szermatt/mistty")
  (my-elpaca-github "jorgenschaefer/circe")
  (my-elpaca-github "skeeto/elfeed")
  (my-elpaca-github "remyhonig/elfeed-org")
  (my-elpaca-github "conao3/transient-dwim.el" transient-dwim)
  (my-elpaca-github "rougier/svg-lib")
  (my-elpaca-github "rougier/svg-tag-mode")
  (my-elpaca-github "DevelopmentCool2449/colorful-mode")
  (my-elpaca-github "zk-phi/gitmole")
  (my-elpaca-github "momomo5717/avy-migemo")
  (my-elpaca-github "NicolasPetton/pass")
  (my-elpaca-github "twlz0ne/elpl")
  (my-elpaca-github "mhayashi1120/Emacs-wgrep" wgrep)
  (my-elpaca-github "jacktasia/dumb-jump")
  (my-elpaca-github "radian-software/ctrlf")
  (my-elpaca-github "dedi/gxref") ;; emacs 25.1 or later
  (my-elpaca-github "oantolin/embark")
  (my-elpaca-github "oantolin/orderless")
  (my-elpaca-github "misohena/phscroll")
  (my-elpaca-github "pinard/org-grep")
  (my-elpaca-github "alphapapa/org-web-tools")
  (my-elpaca-github "alphapapa/org-ql")
  (my-elpaca-github "Fuco1/org-pretty-table")
  (my-elpaca-github "purcell/reformatter.el" reformatter)
  (progn
    (elpaca 'persist)
    (my-elpaca-github "ichernyshovvv/org-timeblock"))
  (progn
    (my-elpaca-github "tarsius/outline-minor-faces")
    (my-elpaca-github "tarsius/backline"))

  (elpaca 'org-bullets)
  (elpaca 'php-mode)
  (elpaca 'find-file-in-project)
  (elpaca 'pdf-tools)
  (elpaca 'python-mode)
  (elpaca 'password-store)
  (elpaca 'clang-format)
  (elpaca 'diffview)
  (elpaca 'yasnippet)

  (my-elpaca-github "emacs-jp/migemo")
  (when (< emacs-major-version 29)
    ;; integrated in Emacs 29 or later
    (my-elpaca-github "karlotness/tree-sitter.el" tree-sitter)
    (my-elpaca-github "ubolonton/emacs-tree-sitter")
    (my-elpaca-github "ubolonton/tree-sitter-langs"))
  (my-elpaca-github "emacsmirror/yatex"))


;;; run queues
(elpaca-process-queues)

(provide 'elpaca-config)
