;; -*- lexical-binding: t; -*-
;;                                          https://takaxp.github.io/init.html
;; Disable nativecomp
(let ((enable t)) ;; {t, nil}
  (setq native-comp-jit-compilation enable
	native-comp-enable-subr-trampolines enable))

;; Disable package loading by package.el
(setq package-enable-at-startup nil)

(setq elpaca-queue-limit 24)
;; [250 packages]
;; 100 1:58
;; 64  1:13
;; 32  1:12, 1:05
;; 28  1:01
;; 24  0:59, 1:00
;; 20  0:59
;; 16  1:07, 1:05
;; 12  1:18
;; 8   1:57

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elpaca installer
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory
  (expand-file-name (format "elpaca/%s/" emacs-version) user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el"
						(:exclude "extensions"))
			      :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process
				 `("git" nil ,buffer t "clone"
                                   ,@(when-let*
					 ((depth (plist-get order :depth)))
                                       (list (format "--depth=%d" depth)
					     "--no-single-branch"))
                                   ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process
			   emacs nil buffer nil "-Q" "-L" "." "--batch"
                           "--eval"
			   "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
;; (add-hook 'after-init-hook #'elpaca-process-queues) ;; moved to bottom
(elpaca `(,@elpaca-order))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility
(defmacro my-elpaca-github (repo &optional sym)
  "Elpaca wrapper for GitHub OWNER/REPO."
  (let* ((name (file-name-nondirectory repo))
         (pkg-sym  (or sym (intern name))))
    `(elpaca (,pkg-sym :host github :repo ,repo))))

(defun my-elpaca-post-process ()
  (interactive)
  ;; (message "elpaca--waiting: %s" elpaca--waiting)
  ;; (message "elpaca--queues: %s" (length elpaca--queues))
  (when (fboundp 'my-elpaca-save-load-path)
    (message "--- saving load-path")
    (my-elpaca-save-load-path))
  ;; If you kill emacs here, do not use ":wait t" anywhere
  ;; (kill-emacs)
  )

(defun my--elpaca-delete-eln-file (package-name)
  "note: see `native-compile-prune-cache'"
  (when package-name
    (dolist (dir (butlast native-comp-eln-load-path))
      (let ((pkg (if (symbolp package-name)
                     (symbol-name package-name)
                   package-name)))
	(dolist (eln (directory-files
                      (concat dir comp-native-version-dir)
                      t (concat "^" pkg ".+\\.eln$")))
          (when (file-writable-p eln)
            (delete-file eln)
            (message "Deleting...%s" eln)))))))

;;;###autoload
(defun my-elpaca-nativecomp-package (&optional package-name non-force)
  "Remove .eln files and regenerated"
  (interactive)
  (unless (and package-name
	       (not (locate-library package-name)))
    (let* ((native-comp-always-compile (not non-force))
	   (package-name (if package-name
			     (if (symbolp package-name)
                                 (symbol-name package-name)
			       package-name)
			   ""))
	   (dir-or-pkg (format "%s%s" elpaca-builds-directory package-name)))
      (when native-comp-always-compile
	(my--elpaca-delete-eln-file package-name))
      (message "--- %s" dir-or-pkg)
      (native-compile-async dir-or-pkg 'recursively))) ;; not activated in batch-mode.
  (my-elpaca-reset-links)) ;; this may be overhead if called multiple times

;;;###autoload
(defun my-elpaca-reset-links ()
  (interactive)
  (when (shell-command-to-string
         (concat "export SYSTEMTYPE=\"darwin\""
                 " && ~/Dropbox/usr/emacs.d/bin/elpaca.sh -r"))
    (message "[elpaca] Link updated")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Boot
(my-elpaca-github "takaxp/postpone")
(elpaca (gcmh :host gitlab :repo "koral/gcmh"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core
(my-elpaca-github "Malabarba/aggressive-indent-mode" aggressive-indent)
(elpaca 'ws-butler) ;; fatal: Remote branch elpa/ws-butler not found in upstream origin

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coursor/Pointer
(my-elpaca-github "takaxp/ah")
(my-elpaca-github "takaxp/bsv")
(my-elpaca-github "joodland/bm")
(my-elpaca-github "emacsmirror/centered-cursor-mode")
(my-elpaca-github "zhangkaiyulw/smart-mark")
(my-elpaca-github "emacsmirror/syntax-subword")
(progn
  (my-elpaca-github "magnars/s.el" s)
  (my-elpaca-github "magnars/expand-region.el" expand-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing
(my-elpaca-github "ludwigpacifici/modern-cpp-font-lock")
(elpaca 'orgalist) ;; for ChangeLog mode, ELPA
(my-elpaca-github "yoshiki/yaml-mode")
(elpaca 'nhexl-mode) ;; fatal: Remote branch externals/nhexl-mode not found in upstream origin
(elpaca 'csv-mode) ;; fatal: Remote branch externals/csv-mode not found in upstream origin
(my-elpaca-github "dakrone/es-mode")
(elpaca 'markdown-mode)
(my-elpaca-github "emacsmirror/ascii")
(my-elpaca-github "doublep/logview")
(my-elpaca-github "fxbois/web-mode") ;; (elpaca 'web-mode)
(elpaca 'po-mode) ;; savannah.gnu.org
(my-elpaca-github "dominikh/go-mode.el" go-mode)
(elpaca (flyspell-correct-ivy :host github :repo "d12frosted/flyspell-correct"
			      :main "flyspell-correct-ivy.el"))
(my-elpaca-github "kchenphy/counsel-world-clock")
(elpaca (latex-math-preview :host gitlab
			    :repo "latex-math-preview/latex-math-preview"))
(when (eq system-type 'darwin)
  (my-elpaca-github "xuchunyang/osx-dictionary.el" osx-dictionary))
(progn ;; describe-number
  (my-elpaca-github "d5884/yabin")
  (my-elpaca-github "netromdk/describe-number"))
(elpaca 'smartparens)
(my-elpaca-github "ROCKTAKEY/grugru")
(my-elpaca-github "rubikitch/replace-from-region")
(progn ;; selected-related
  (my-elpaca-github "Kungsgeten/selected.el" selected)
  (my-elpaca-github "Wilfred/helpful")
  (my-elpaca-github "takaxp/counsel-selected")
  (my-elpaca-github "takaxp/help-fns-plus" help-fns+))
(my-elpaca-github "zk-phi/git-complete")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display
(my-elpaca-github "jdtsmith/mlscroll")
(my-elpaca-github "emacsmirror/delight") ;; or diminish
(progn ;; git-gutter
  (my-elpaca-github "syohex/emacs-git-gutter" git-gutter)
  (my-elpaca-github "nschum/fringe-helper.el" fringe-helper)
  (my-elpaca-github "emacsorphanage/git-gutter-fringe"))
(elpaca 'japanese-holidays)
(elpaca 'highlight-symbol)
(progn ;; Nerd-icons
  (my-elpaca-github "rainstormstudio/nerd-icons.el" nerd-icons)
  (my-elpaca-github "rainstormstudio/nerd-icons-dired")
  (my-elpaca-github "LuigiPiucco/nerd-icons-corfu")
  (my-elpaca-github "seagle0128/nerd-icons-ivy-rich")
  ;; icons-in-terminal
  (my-elpaca-github "seagle0128/icons-in-terminal.el" icons-in-terminal))
(my-elpaca-github "chuntaro/emacs-keypression" keypression)
(progn ;; ivy
  (my-elpaca-github "abo-abo/smex")
  (my-elpaca-github "Yevgnen/ivy-rich")
  (my-elpaca-github "syohex/emacs-counsel-gtags" counsel-gtags)
  (my-elpaca-github "ericdanan/counsel-projectile")
  (my-elpaca-github "akirak/ivy-omni-org")
  (my-elpaca-github "d12frosted/counsel-osx-app")
  (my-elpaca-github "ecraven/ivy-pass")
  (my-elpaca-github "franburstall/ivy-emms"))
(my-elpaca-github "takaxp/imenu-list")
(elpaca 'dimmer)
(my-elpaca-github "emacs-vs/rainbow-csv")
(my-elpaca-github "lewang/command-log-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Media
(elpaca 'emms) ;; savannah.gnu.org

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File management
(my-elpaca-github "bbatsov/crux")
(my-elpaca-github "conornash/backup-each-save")
(elpaca 'dired-du)
(my-elpaca-github "Fuco1/dired-hacks")
(my-elpaca-github "Vifon/dired-recent.el" dired-recent)
(my-elpaca-github "jixiuf/ivy-dired-history")
(when (eq system-type 'darwin)
  (my-elpaca-github "lunaryorn/osx-trash.el" osx-trash))
(elpaca 'undo-fu)
(my-elpaca-github "bbatsov/super-save")
(my-elpaca-github "takaxp/session")
(elpaca 'neotree)
(my-elpaca-github "takaxp/facecheck")
(my-elpaca-github "dacap/keyfreq")
(elpaca (disk-usage :host gitlab :repo "ambrevar/emacs-disk-usage"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Development
(elpaca 'gist)
(progn ;; flycheck
  (elpaca 'flycheck)
  (elpaca 'flycheck-pos-tip)
  (my-elpaca-github "ch1bo/flycheck-clang-tidy"))
(elpaca 'quickrun)
(elpaca 'ggtags)
(my-elpaca-github "AdamNiederer/0xc")
(my-elpaca-github "purcell/package-lint")
(elpaca 'projectile)
(progn ;; relint
  (my-elpaca-github "mattiase/xr")
  (my-elpaca-github "mattiase/relint"))
(elpaca 'editorconfig)
(progn
  (my-elpaca-github "AdamNiederer/elquery")
  (my-elpaca-github "AdamNiederer/cov"))
(my-elpaca-github "lassik/emacs-format-all-the-code" format-all)
(elpaca 'uuid)
(progn ;; corfu
  (my-elpaca-github "minad/cape")
  (my-elpaca-github "minad/corfu")
  (my-elpaca-github "jdtsmith/kind-icon")  ;; requires svg-lib
  (my-elpaca-github "xenodium/org-block-capf")
  ;; (elpaca 'corfu-terminal)
  )
(elpaca 'vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org mode
(progn ;; async.el
  (my-elpaca-github "astahlman/ob-async") ;; :depends (emacs-async)
  (elpaca 'org-download))
(progn ;; org-onit
  (my-elpaca-github "takaxp/org-onit")
  (my-elpaca-github "takaxp/org-plist"))
(my-elpaca-github "mallt/org-clock-today-mode" org-clock-today)
(elpaca 'orgbox)
(progn ;; ob
  (my-elpaca-github "zweifisch/ob-http")
  (my-elpaca-github "pope/ob-go"))
(elpaca (org-tree-slide
	 :host github :repo "takaxp/org-tree-slide" :branch "develop"))
(progn ;; ox-hugo
  (my-elpaca-github "hniksic/emacs-htmlize" htmlize)
  (elpaca 'ox-hugo))
(progn ;; ox
  (my-elpaca-github "0x60df/ox-qmd")
  (my-elpaca-github "larstvei/ox-gfm")
  (my-elpaca-github "yjwen/org-reveal" ox-reveal)
  (my-elpaca-github "jlumpe/ox-json"))
(when (eq system-type 'darwin)
  (elpaca (org-mac-link :host gitlab :repo "aimebertrand/org-mac-link")))
(my-elpaca-github "tarsius/orglink")
(my-elpaca-github "awth13/org-appear")
(my-elpaca-github "alphapapa/org-recent-headings")
(my-elpaca-github "facetframer/orgnav")
(my-elpaca-github "snosov1/toc-org")
(elpaca (org-screenshot :host github :repo "dfeich/org-screenshot"
			:main "org-attach-screenshot.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frame/Window
(my-elpaca-github "takaxp/moom")
(elpaca 'shackle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font/Face
(my-elpaca-github "tarsius/hl-todo")
(elpaca 'rainbow-mode)
(my-elpaca-github "sabof/edit-color-stamp")
(my-elpaca-github "k-talo/volatile-highlights.el" volatile-highlights)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; async

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility
(elpaca 'google-this)
(progn ;; gt.el
  (my-elpaca-github "lorniu/pdd.el" pdd)
  (my-elpaca-github "lorniu/gt.el" gt))
(when (eq system-type 'darwin)
  (my-elpaca-github "raghavgautam/osx-lib"))
(elpaca (gif-screencast :host gitlab :repo "ambrevar/emacs-gif-screencast"))
(elpaca 'manage-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Under test
(progn
  (elpaca 'gptel) ;; (my-elpaca-github "karthink/gptel")
  )


;; required at the end of this code to run all items for batch-mode
(when noninteractive
  (elpaca-wait))

;;; ivy/counsel/swiper
(elpaca (counsel :host github :repo "abo-abo/swiper" :main "counsel.el"))
(elpaca (ivy-prescient :host github :repo "radian-software/prescient.el")
	:main "ivy-prescient")
(elpaca (corfu-prescient :host github :repo "radian-software/prescient.el")
	:main "corfu-prescient")

;;; magit
(progn
  (my-elpaca-github "magit/transient")
  (elpaca 'magit))

;;; org
(elpaca 'org-contrib)
(elpaca 'org)

;;; async
(my-elpaca-github "jwiegley/emacs-async" async)

(elpaca-process-queues)
(provide 'elpaca-config)

;; Having issues
;; (elpaca 'org-extra-emphasis)
;; (elpaca 'emr) ;; iedit installed version lower than min require 0.97



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
