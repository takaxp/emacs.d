;; 1. Do not rely on the pre-defined recipes. Specify Git repository directly.
;; 2. Use emacsmirror not to install packages under ~/.emacs.d/elpa.
;; 3. Write private recipes in ~/.emacs.d/recipes if needed.
;; call my-elget-load-and-sync
(require 'elget-setup)

;;;###autoload
(defmacro my-elget-bundles ()
  "List of packages."

  ;; Fundamental (Under postpone.el)
  (el-get-bundle "takaxp/postpone")
  (el-get-bundle "takaxp/moom")
  (el-get-bundle "takaxp/ascii")
  (el-get-bundle "takaxp/help-fns-plus")
  (el-get-bundle "takaxp/dimmer.el") ;; to avoid downloding images
  (el-get-bundle "takaxp/session")
  (el-get-bundle "ws-butler")
  ;; (el-get-bundle "which-key")
  (el-get-bundle "aggressive-indent")
  (el-get-bundle "dacap/keyfreq")
  (el-get-bundle "conornash/backup-each-save")
  (el-get-bundle "bbatsov/super-save")
  (el-get-bundle "bbatsov/crux")
  (el-get-bundle "jwiegley/emacs-async")
  (el-get-bundle "Kungsgeten/selected.el" :name selected)
  (el-get-bundle "k-talo/smooth-scroll.el" :name smooth-scroll)
  (el-get-bundle "tarsius/hl-todo")
  (el-get-bundle "emacsmirror/centered-cursor-mode")
  (unless (eq system-type 'windows-nt)
    (el-get-bundle "zk-phi/git-complete"))
  (progn
    (el-get-bundle "oantolin/embark")
    (el-get-bundle "minad/corfu")
    (el-get-bundle "popon"
		   :type git
		   :url "https://codeberg.org/akib/emacs-popon")
    (el-get-bundle "corfu-terminal"
		   :type git
		   :url "https://codeberg.org/akib/emacs-corfu-terminal"
		   :depends (popon))
    (el-get-bundle "xenodium/org-block-capf")
    (el-get-bundle "jdtsmith/kind-icon")  ;; requires svg-lib
    (el-get-bundle "minad/cape")
    (el-get-bundle "oantolin/orderless"))

  ;; Org Mode
  (el-get-bundle "takaxp/org-onit")
  (el-get-bundle "takaxp/org-plist")
  (el-get-bundle "takaxp/org-bookmark-heading")
  (el-get-bundle "emacsmirror/org-contrib")
  ;;(el-get-bundle "org-tree-slide" :branch "develop")
  (el-get-bundle "org-tree-slide")
  (el-get-bundle "org-download")
  (el-get-bundle "org-bullets")
  (el-get-bundle "misohena/phscroll")
  (el-get-bundle "orgbox")
  (el-get-bundle "yjwen/org-reveal" :name ox-reveal)
  (el-get-bundle "hniksic/emacs-htmlize")
  (el-get-bundle "pinard/org-grep")
  (el-get-bundle "alphapapa/org-web-tools")
  (progn
    ;; (el-get-bundle "peg") ;; integrated at least 30.1
    (el-get-bundle "alphapapa/org-ql"))
  (el-get-bundle "dfeich/org-screenshot")
  (el-get-bundle "mallt/org-clock-today-mode")
  (progn ;; org-recent-headings
    (el-get-bundle "alphapapa/a.el" :name a)
    (el-get-bundle "alphapapa/frecency.el" :name frecency) ;; requires a.el
    (el-get-bundle "alphapapa/org-recent-headings") ;; requires frecency.el
    (el-get-bundle "facetframer/orgnav"))
  (el-get-bundle "IvanMalison/org-projectile")
  (el-get-bundle "emacsmirror/orgalist")
  (el-get-bundle "tarsius/orglink")
  (el-get-bundle "Fuco1/org-pretty-table")
  (el-get-bundle "xenodium/company-org-block")
  (el-get-bundle "awth13/org-appear")
  (el-get-bundle "org-mac-link"
		 :type git
		 :url "https://gitlab.com/aimebertrand/org-mac-link.git")
  (progn
    (el-get-bundle "persist")
    (el-get-bundle "ichernyshovvv/org-timeblock"))
  (progn
    (el-get-bundle "QiangF/org-extra-emphasis")
    (el-get-bundle "skeeto/skewer-mode"))
  (progn
    (el-get-bundle "tarsius/outline-minor-faces")
    (el-get-bundle "tarsius/backline"))

  ;; Org Mode - ox/ob
  (el-get-bundle "0x60df/ox-qmd")
  (el-get-bundle "larstvei/ox-gfm")
  (el-get-bundle "jlumpe/ox-json")
  (el-get-bundle "zweifisch/ob-http")
  (el-get-bundle "astahlman/ob-async")
  (el-get-bundle "pope/ob-go")
  (unless (eq system-type 'windows-nt)
    (el-get-bundle "kaushalmodi/tomelr")
    (el-get-bundle "kaushalmodi/ox-hugo"))

  ;; Major modes
  ;; download zip since python-mode git repository is extremely huge
  (el-get-bundle "python-mode"
		 :type http-zip
		 :url "https://gitlab.com/python-mode-devs/python-mode/-/archive/master/python-mode-master.zip")
  (el-get-bundle "emacsmirror/csv-mode")
  (el-get-bundle "emacsmirror/po-mode")
  (el-get-bundle "yaml-mode")
  (el-get-bundle "json-mode")
  (el-get-bundle "es-mode")
  (el-get-bundle "markdown-mode")
  (el-get-bundle "gnuplot-mode")
  (el-get-bundle "cmake-mode")
  (el-get-bundle "php-mode")
  (el-get-bundle "bruceravel/gnuplot-mode")
  (el-get-bundle "dominikh/go-mode.el" :name go-mode)
  (progn ;; pass
    (el-get-bundle "NicolasPetton/pass")
    (el-get-bundle "password-store"
		   :type http
		   :url "https://raw.githubusercontent.com/stuartsierra/password-store/master/contrib/emacs/password-store.el")
    (el-get-bundle "ecraven/ivy-pass")) ;; requires password-store.el
  (unless (eq system-type 'windows-nt)
    (el-get-bundle "emacsmirror/yatex"))

  ;; Development
  (el-get-bundle "gregsexton/origami.el" :name origami)
  (el-get-bundle "yasnippet")
  (el-get-bundle "ludwigpacifici/modern-cpp-font-lock")
  (el-get-bundle "AdamNiederer/0xc")
  (el-get-bundle "uuid")
  (progn
    (el-get-bundle "d5884/yabin")
    (el-get-bundle "netromdk/describe-number"))
  (el-get-bundle "flycheck")
  (el-get-bundle "flycheck-pos-tip")
  (el-get-bundle "ch1bo/flycheck-clang-tidy")
  (el-get-bundle "js2-mode")
  (el-get-bundle "js2-refactor")
  (el-get-bundle "web-mode")
  (el-get-bundle "yasuyk/web-beautify")
  (el-get-bundle "gist")
  (progn ;; helpful
    (el-get-bundle "Wilfred/loop.el")
    (el-get-bundle "Wilfred/elisp-refs")
    (el-get-bundle "Wilfred/helpful"))
  (el-get-bundle "purcell/package-lint")
  (progn
    (el-get-bundle "AdamNiederer/elquery")
    (el-get-bundle "AdamNiederer/cov"))
  (el-get-bundle "ggtags")
  (el-get-bundle "dedi/gxref") ;; emacs 25.1 or later
  (el-get-bundle "clang-format")
  (el-get-bundle "lassik/emacs-format-all-the-code")
  (el-get-bundle "emacsmirror/emr")
  (el-get-bundle "diffview")
  (el-get-bundle "projectile")
  (el-get-bundle "takaxp/facecheck")
  (el-get-bundle "twlz0ne/elpl")
  (el-get-bundle "mhayashi1120/Emacs-wgrep")
  (el-get-bundle "syohex/emacs-go-eldoc" :name go-eldoc)
  (el-get-bundle "jacktasia/dumb-jump")
  (el-get-bundle "karlotness/tree-sitter.el" :name tree-sitter)
  (el-get-bundle "ubolonton/emacs-tree-sitter")
  (el-get-bundle "ubolonton/tree-sitter-langs")

  ;; ivy modules
  (el-get-bundle "takaxp/counsel-selected")
  (el-get-bundle "jixiuf/ivy-dired-history")
  (el-get-bundle "ericdanan/counsel-projectile")
  (el-get-bundle "kchenphy/counsel-world-clock")
  (el-get-bundle "syohex/emacs-counsel-gtags")
  (el-get-bundle "radian-software/prescient.el" :name prescient)
  (el-get-bundle "radian-software/ctrlf")
  (el-get-bundle "momomo5717/avy-migemo")
  (el-get-bundle "mkcms/ivy-yasnippet" :depends (dash swiper yasnippet))
  ;; require ~/.emacs.d/recipes/ivy.rcp
  (el-get-bundle "akirak/ivy-omni-org")
  (el-get-bundle "abo-abo/smex")
  (el-get-bundle "franburstall/ivy-emms")
  (el-get-bundle "ROCKTAKEY/grugru")
  (el-get-bundle "Yevgnen/ivy-rich")

  ;; macOS support
  (when (eq system-type 'darwin)
    (el-get-bundle "raghavgautam/osx-lib")
    (el-get-bundle "lunaryorn/osx-trash.el" :name osx-trash)
    (el-get-bundle "xuchunyang/osx-dictionary.el" :name osx-dictionary))

  ;; Editing support
  (el-get-bundle "d12frosted/flyspell-correct")
  (el-get-bundle "magnars/expand-region.el" :name expand-region)
  (progn ;; skewer
    (el-get-bundle "skewer-mode")
    (el-get-bundle "auto-complete")) ;; require 'skewer
  (el-get-bundle "auto-complete-clang")
  (el-get-bundle "ac-js2")
  (el-get-bundle "emacs-jp/migemo")
  (el-get-bundle "abo-abo/hydra")
  (el-get-bundle "mattiase/xr")
  (el-get-bundle "mattiase/relint")
  (el-get-bundle "purcell/reformatter.el" :name reformatter)
  (el-get-bundle "emacsmirror/nhexl-mode")

  ;; Visualize
  (el-get-bundle "takaxp/bsv")
  (el-get-bundle "takaxp/imenu-list")
  (el-get-bundle "volatile-highlights")
  (el-get-bundle "highlight-symbol")
  (el-get-bundle "smartparens")
  (el-get-bundle "cask/shut-up")
  (el-get-bundle "emacsmirror/delight") ;; or diminish
  (el-get-bundle "manage-minor-mode")
  (el-get-bundle "syohex/emacs-git-gutter" :name git-gutter)
  (el-get-bundle "git-gutter-fringe")
  (el-get-bundle "doublep/logview")
  (el-get-bundle "rougier/svg-lib")
  (el-get-bundle "rougier/svg-tag-mode")
  (el-get-bundle "bm")
  (el-get-bundle "emacsmirror/rainbow-mode")
  (el-get-bundle "DevelopmentCool2449/colorful-mode")
  (el-get-bundle "seagle0128/doom-modeline" :depends (eldoc-eval))
  (el-get-bundle "disk-usage"
		 :type git
		 :url "https://gitlab.com/ambrevar/emacs-disk-usage.git")
  (el-get-bundle "ideasman42/emacs-undo-fu"
		 :type git
		 :url "https://codeberg.org/ideasman42/emacs-undo-fu.git")
  (el-get-bundle "zk-phi/gitmole")
  (el-get-bundle "chuntaro/emacs-keypression" :name keypression)
  (el-get-bundle "lewang/command-log-mode")
  (el-get-bundle "tarsius/outline-minor-faces")
  (el-get-bundle "tarsius/backline")
  (el-get-bundle "jdtsmith/mlscroll")
  (el-get-bundle "emacs-vs/rainbow-csv")
  ;; icons-in-terminal
  (el-get-bundle "seagle0128/icons-in-terminal.el" :name icons-in-terminal)
  (progn ;; Nerd-icons
    (el-get-bundle "rainstormstudio/nerd-icons.el" :name nerd-icons)
    (el-get-bundle "rainstormstudio/nerd-icons-dired")
    (el-get-bundle "seagle0128/nerd-icons-ivy-rich")
    (el-get-bundle "LuigiPiucco/nerd-icons-corfu"))

  ;; System related
  (el-get-bundle "Fuco1/dired-hacks")
  (el-get-bundle "calancha/dired-du")
  (el-get-bundle "Vifon/dired-recent.el" :name dired-recent)
  (el-get-bundle "neotree")
  (el-get-bundle "find-file-in-project")
  (el-get-bundle "gcmh"
		 :type git
		 :url "https://gitlab.com/koral/gcmh.git")

  ;; Frame and windows
  (el-get-bundle "popwin")
  (el-get-bundle "shackle")

  ;; Efficiency
  (el-get-bundle "rubikitch/replace-from-region")
  (el-get-bundle "quickrun")
  (el-get-bundle "latex-math-preview"
		 :type git
		 :url "https://gitlab.com/latex-math-preview/latex-math-preview.git")
  (el-get-bundle "sbrisard/bratex")
  (el-get-bundle "zhangkaiyulw/smart-mark")
  (el-get-bundle "emacsmirror/syntax-subword")
  (el-get-bundle "takaxp/ah")
  ;; (el-get-bundle "phikal/compat.el" :name compat)
  ;; (el-get-bundle "magit/transient")
  (el-get-bundle "conao3/transient-dwim.el"
		 :name transient-dwim
		 :depends (transient))

  ;; Applications
  (el-get-bundle "ag")
  (el-get-bundle "google-this")
  (el-get-bundle "japanese-holidays")
  (el-get-bundle "pdf-tools")
  (el-get-bundle "gif-screencast"
		 :type git
		 :url "https://gitlab.com/ambrevar/emacs-gif-screencast.git")
  (el-get-bundle "jorgenschaefer/circe")
  (when (memq system-type '(darwin windows-nt))
    (el-get-bundle "d12frosted/counsel-osx-app"))
  (el-get-bundle "skeeto/elfeed")
  (el-get-bundle "remyhonig/elfeed-org")
  (el-get-bundle "sabof/edit-color-stamp")
  (unless (eq system-type 'windows-nt)
    (el-get-bundle "vterm"))
  (el-get-bundle "karthink/gptel")

  ;; Log
  (el-get-bundle "davep/uptimes.el" :name "uptimes")

  ;; Terminal
  (el-get-bundle "szermatt/mistty")

  ;; Under consideration
  ;; (my-elget-bundles-trying)
  )

(defmacro my-elget-bundles-trying ()
  "List of packages under consideration."

  (el-get-bundle "consult")
  (el-get-bundle "vertico")

  (el-get-bundle "gonewest818/elisp-lint")
  (el-get-bundle "tumashu/ivy-posframe") ;; require swiper
  (el-get-bundle "yanghaoxie/which-key-posframe" :depends (posframe))
  (el-get-bundle "sebastiencs/company-box")
  ;; LSP
  (el-get-bundle "emacs-lsp/lsp-mode")
  (el-get-bundle "emacs-lsp/dap-mode")
  (el-get-bundle "emacs-lsp/lsp-ui")
  (el-get-bundle "emacs-lsp/lsp-ivy")
  (el-get-bundle "emacs-lsp/lsp-pyright")
  ;;
  (el-get-bundle "casouri/isolate")
  (el-get-bundle "dakra/statusbar.el" :name "statusbar")
  )

(defmacro my-elget-bundles-not-install ()
  "List of packages, not to be installed."

  ;; Fundamental (Under postpone.el)
  (el-get-bundle "syohex/emacs-utils")
  (el-get-bundle "emacsattic/mic-paren")

  ;; Org Mode
  (el-get-bundle "emacsmirror/org-edna")
  (el-get-bundle "alphapapa/org-dashboard")
  (el-get-bundle "unhammer/org-random-todo")
  (el-get-bundle "brabalan/org-review")
  (el-get-bundle "takaxp/emacs-easy-hugo") ;; using a private repo not to download images
  (el-get-bundle "kiwanami/emacs-calfw")
  (el-get-bundle "harrybournis/org-fancy-priorities")
  (el-get-bundle "poporg")
  (el-get-bundle "Fuco1/org-clock-budget")
  (unless (eq system-type 'windows-nt)
    (el-get-bundle "org-emms"
		   :type git
		   :url "https://git.sr.ht/~jagrg/org-emms")
    (el-get-bundle "org-trello"))
  ;; Org Mode - ox
  (el-get-bundle "ox-pandoc")
  (el-get-bundle "jkitchin/ox-ipynb")
  (el-get-bundle "marsmining/ox-twbs")
  ;; Org Mode - ob
  ;; Major mode
  (el-get-bundle "csharp-mode")
  (el-get-bundle "emacsmirror/ess")
  ;; Development
  (el-get-bundle "emacsmirror/rmsbolt")
  (el-get-bundle "bug-hunter")
  (el-get-bundle "editorconfig") ;; integrated to Emacs 30.1
  ;; Package management
  (el-get-bundle "use-package")
  (el-get-bundle "conao3/leaf.el" :name leaf)

  ;; ivy modules
  (el-get-bundle "asok/all-the-icons-ivy")
  (el-get-bundle "DarwinAwardWinner/amx")

  ;; macOS support
  ;; Editing support
  (el-get-bundle "company-mode/company-mode")
  (el-get-bundle "expez/company-quickhelp")
  (el-get-bundle "tiny")
  (el-get-bundle "magnars/multiple-cursors.el" :name multiple-cursors)
  (el-get-bundle "sabof/edit-color-stamp")
  ;; Visualize
  (el-get-bundle "domtronn/all-the-icons.el" :name all-the-icons)
  (el-get-bundle "jtbm37/all-the-icons-dired" :depends (all-the-icons))
  (progn ;; icons-in-terminal
    (el-get-bundle "emacsmirror/font-lock-plus" :name font-lock+)
    ;;  (el-get-bundle "sebastiencs/icons-in-terminal")
    (el-get-bundle "takaxp/icons-in-terminal.el" :name icons-in-terminal))
  (el-get-bundle "takaxp/icons-in-terminal-dired" :depends (icons-in-terminal))
  (el-get-bundle "takaxp/icons-in-terminal-ivy")
  (el-get-bundle "kawabata/rot47")
  (el-get-bundle "fancy-narrow")
  (el-get-bundle "back-button")
  (el-get-bundle "hide-lines")
  (el-get-bundle "kiennq/emacs-mini-modeline")
  (el-get-bundle "shrink-path"
                 :type git
                 :url "https://gitlab.com/bennya/shrink-path.el.git")
  ;; System related
  ;; Frame and windows
  (el-get-bundle "tabbar")
  (el-get-bundle "ajgrf/edwin")
  (el-get-bundle "emacsmirror/frame-tabs")
  ;; Efficiency
  (el-get-bundle "yuttie/initchart")
  (el-get-bundle "rolandwalker/ignoramus") ;; Ignore backups, build files, et al.
  (el-get-bundle "goto-chg")
  ;; Applications
  (el-get-bundle "skeeto/emacsql")
  (el-get-bundle "tarsius/keycast")
  (el-get-bundle "xuchunyang/gitter.el")
  (el-get-bundle "jamiguet/network-watch")
  (el-get-bundle "aaronbieber/sunshine.el" :name sunshine)

  ;; (el-get-bundle "takaxp/mu4e")
  )

;; =========================================================================

(defmacro my-elget-bundles-by-tag ()
  "Packages to install with specific Tag."

  ;; (el-get-bundle "emms") ;; with private recipe

  ;; Resolve Magit dependency
  (el-get-bundle "ghub")
  (el-get-bundle "llama")
  (el-get-bundle "emacs-compat/compat")
  (el-get-bundle "magit-popup")
  (el-get-bundle "magit/transient")
  (el-get-bundle "magit/libegit2"
                 :build `(("make" ,(format "EMACS=%s" el-get-emacs))))

  ;; with private recipe
  (dolist (recipe my-elget--private-recipes)
    (when noninteractive
      (if (my-elget-private-recipe-p recipe)
          (message "--- \"%s\" is being installed with private recipe" recipe)
        (error "No private recipe exists for \"%s\"" recipe)))
    (eval `(el-get-bundle ,recipe))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of package list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun my-elget-private-recipe-p (recipe)
  (let ((result nil))
    (dolist (dir my-elget--private-recipe-dirs)
      (when (file-exists-p (concat dir "/" recipe ".rcp"))
        (setq result t)))
    result))

;;;###autoload
(defun my-elget-update-org ()
  (interactive)
  (when (shell-command-to-string
         (concat "~/Dropbox/emacs.d/bin/el-get.sh -g tag"))
    (message "[el-get] Org mode updated")))

;;;###autoload
(defun my-elget-update-packages (packages &optional current total)
  (unless (eq (or current 0) 0)
    (message (make-string 80 ?-))
    (message "%s%4.1f[%%]" (make-string 72 ?\s)
             (* 100.0 (/ (float current) total)))
    (message (make-string 80 ?-)))
  (let ((index 0))
    (dolist (package packages)
      (setq index (1+ index))
      (eval `(el-get-update ,package))
      (message "[%3d/%3d] update: %s"
               (+ index (or current 0)) (or total 0) package))))

;;;###autoload
(defun my-elget-update-progress (&optional threads)
  "Update packages only for batch-mode."
  (unless noninteractive
    (user-error "This method is intended to use in batch-mode"))
  (my-elget-load-and-sync)
  (let* ((packages
          (reverse (el-get-list-package-names-with-status "installed")))
         (total (length packages))
         (unit (ceiling (/ (float total) (or threads my-elget-threads))))
         (index 0))
    (while packages
      (setq target-packages (nthcdr (- (length packages) unit) packages))
      (my-elget-update-packages (reverse target-packages) index total)
      (setq index (+ index (length target-packages)))
      (setq packages (butlast packages unit))))
  (my-elget-nativecomp-prune-current-cache))

;;;###autoload
(defun my-elget-remove-package (package)
  "Remove PACKAGE."
  (let ((pos (string-match "[^/]+$" package)))
    (funcall-interactively
     'el-get-remove (if pos (substring-no-properties package pos) package))))

;;;###autoload
(defun my-elget-list ()
  (interactive)
  (let ((index 0)
        (packages (el-get-list-package-names-with-status "installed")))
    (dolist (package packages)
      (setq index (1+ index))
      (message "(%3d) %s" index package))
    (message "[el-get] %s packages (installed)" (length packages))))

;;;###autoload
(defun my-elget-reset-links ()
  (interactive)
  (when (shell-command-to-string
         (concat "export SYSTEMTYPE=\"darwin\" &&"
                 " ~/Dropbox/emacs.d/bin/el-get.sh -r"))
    (message "[el-get] Link updated")))

(defun my--elget-delete-eln-file (package-name)
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
(defun my-elget-nativecomp-package (&optional package-name non-force)
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
	   (dir-or-pkg (format "~/.emacs.d/%s/el-get/%s"
			       emacs-version
			       package-name)))
      (when native-comp-always-compile
	(my--elget-delete-eln-file package-name))
      (message "--- %s" dir-or-pkg)
      (native-compile-async dir-or-pkg 'recursively)))) ;; not activated in batch-mode.

;;;###autoload
(defun my-elget-nativecomp-prune-current-cache ()
  "Remove all .eln files that are applicable to the current Emacs invocation.
see `native-compile-prune-cache'."
  (interactive)
  (unless (featurep 'native-compile)
    (user-error "This Emacs isn't built with native-compile support"))
  ;; The last item in native-comp-eln-load-path is assumed to be a system
  ;; directory, so don't try to delete anything there (bug#59658).
  (dolist (dir (butlast native-comp-eln-load-path))
    ;; If a directory is non absolute it is assumed to be relative to
    ;; `invocation-directory'.
    (setq dir (expand-file-name dir invocation-directory))
    (when (file-exists-p dir)
      (dolist (subdir (seq-filter
                       (lambda (f) (not (string-match (rx "/." (? ".") eos) f)))
                       (directory-files dir t)))
        (when (and (file-directory-p subdir)
                   (file-writable-p subdir)
                   (equal (file-name-nondirectory
                           (directory-file-name subdir))
                          comp-native-version-dir))
          (message "Deleting `%s'..." subdir)
          ;; We're being overly cautious here -- there shouldn't be
          ;; anything but .eln files in these directories.
          (dolist (eln (directory-files subdir t "\\.eln\\(\\.tmp\\)?\\'"))
            (when (file-writable-p eln)
              (delete-file eln)))
          (when (directory-empty-p subdir)
            (delete-directory subdir))))))
  (message "Cache cleared"))

;; for noninteractive
;;;###autoload
(defun my-elget-load-and-sync ()
  (my-elget-setup)
  (unless my-elget-initialize
    (eval '(my-elget-bundles-by-tag))
    (eval '(my-elget-bundles)))
  (el-get 'sync))

;; init
(unless noninteractive
  (my-elget-load-and-sync)
  (my-elget-reset-links))

(provide 'elget-config)
;;; elget-config.el ends here
