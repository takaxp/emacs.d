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
(elpaca 'web-mode)
(my-elpaca-github "emacsmirror/ascii") ;; (elpaca 'ascii)
(my-elpaca-github "emacsmirror/po-mode") ;; avoid using savannah.gnu.org
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
(my-elpaca-github "emacsmirror/delight") ;; avoid using savannah.gnu.org
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
  (elpaca 'ivy-prescient)
  (elpaca 'smex)
  (my-elpaca-github "takaxp/ivy-rich")
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
(my-elpaca-github "emacsmirror/emms") ;; avoid using savannah.gnu.org

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File management
(elpaca 'crux)
(elpaca 'backup-each-save)
(elpaca 'dired-du)
(my-elpaca-github "Fuco1/dired-hacks")
(elpaca 'dired-recent)
(my-elpaca-github "takaxp/ivy-dired-history")
(when (eq system-type 'darwin)
  (elpaca 'osx-trash))
(my-elpaca-github "emacsmirror/undo-fu") ;; avoid using codeberg
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
(progn ;; corfu
  (elpaca 'kind-icon)
  (my-elpaca-github "xenodium/org-block-capf")
  (elpaca 'cape)
  (elpaca 'corfu-terminal)
  (elpaca 'corfu-prescient)
  (elpaca 'corfu))
(elpaca 'vterm)
;; (elpaca 'slime)

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
(elpaca 'async) ;; (my-elpaca-github "jwiegley/emacs-async" async)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility
(elpaca 'google-this)
(elpaca 'gt)
(when (eq system-type 'darwin)
  (elpaca 'osx-lib))
(elpaca 'gif-screencast)
(elpaca 'manage-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Under testing
(progn
  (elpaca 'gptel))

;;; For safety updating
(elpaca 'counsel) ;; (my-elpaca-github "abo-abo/swiper" counsel)

;;; magit
(progn
  (elpaca 'magit)
  (elpaca 'transient :inherit nil))

;;; org
(elpaca 'org-contrib)
(elpaca 'org)

;;; Having issues
;; (elpaca 'org-extra-emphasis)
;; (elpaca 'emr) ;; iedit installed version lower than min require 0.97

;;; compat
;; compat installed version (30 2 9999) lower than min required 31 (2026-05-08)
(elpaca 'compat)

;;; run timer to complete the sequence automatically
(defvar my-elpaca-kill-emacs-timer
  (run-at-time 0 my-elpaca-kill-emacs-count #'my-elpaca-kill-emacs))

;;; run queues
(elpaca-process-queues)

;;; additional/previous packages
;; (require 'elpaca-previous)

(provide 'elpaca-config)
