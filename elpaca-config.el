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
(elpaca 'corfu-prescient)

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

(defvar my-elpaca-kill-emacs-count 3)
(defvar my-elpaca-kill-emacs-timer
  (run-at-time 0 my-elpaca-kill-emacs-count #'my-elpaca-kill-emacs))
(defun my-elpaca-kill-emacs ()
  "partially taken from `elpaca-ui--progress-bar'."
  (cl-loop
   with total = 0 with finalized = 0
   for s in '(finished blocked failed other)
   for plen = (elpaca-alist-get s elpaca--status-counts 0)
   do
   (setq total (+ total plen))
   (when (memq s '(finished failed))
     (cl-incf finalized plen))
   (when (and (equal total finalized)
	      (eq (car (car elpaca--status-counts)) 'finished)
	      (eq plen 0))
     (cancel-timer my-elpaca-kill-emacs-timer)
     (dotimes (count my-elpaca-kill-emacs-count)
       (message "%s" (- my-elpaca-kill-emacs-count count))
       (sleep-for 1))
     (kill-emacs))))

;;; run queues
(elpaca-process-queues)

;;; additional/previous packages
;; (require 'elpaca-previous)

(provide 'elpaca-config)
