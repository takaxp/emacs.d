;; 1. Do not rely on the pre-defined recipes. Specify Git repository directly.
;; 2. Use emacsmirror not to install packages under ~/.emacs.d/elpa.
;; 3. Write private recipes in ~/.emacs.d/recipes if needed.

;;;###autoload
(defun my-elget-bundles ()
  "List of packages."
  (when noninteractive

    ;; Fundamental packages
    ;; (el-get-bundle "loop")
    ;; (el-get-bundle "s")
    ;; (el-get-bundle "dash")
    ;; (el-get-bundle "ht")

    (el-get-bundle "takaxp/postpone")
    (el-get-bundle "takaxp/ascii")
    (el-get-bundle "takaxp/help-fns-plus")
    (el-get-bundle "syohex/emacs-utils")
    (el-get-bundle "zk-phi/git-complete")
    ;; (el-get-bundle "org-mode"
    ;;                :type git
    ;;                :url "https://code.orgmode.org/bzg/org-mode.git")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; experimental ;;;
    (el-get-bundle "takaxp/htprofile")
    (el-get-bundle "alphapapa/yequake")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Package management ;;;
    (el-get-bundle "use-package")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; priority ;;;
    (el-get-bundle "emacsmirror/font-lock-plus" :name font-lock+)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org Mode ;;;
    (el-get-bundle "hniksic/emacs-htmlize")
    (el-get-bundle "ox-pandoc")
    (el-get-bundle "org-tree-slide")
    (el-get-bundle "org-download")
    (el-get-bundle "emacsorphanage/org-grep")
    (el-get-bundle "orgbox")
    (el-get-bundle "poporg")
    (el-get-bundle "alphapapa/org-web-tools")
    (el-get-bundle "emacsmirror/org-edna")
    (el-get-bundle "0x60df/ox-qmd")
    (el-get-bundle "larstvei/ox-gfm")
    (el-get-bundle "marsmining/ox-twbs")
    (el-get-bundle "kaushalmodi/ox-hugo")
    (el-get-bundle "zweifisch/ob-http")
    (el-get-bundle "astahlman/ob-async")
    (el-get-bundle "brabalan/org-review")
    (el-get-bundle "alphapapa/org-dashboard")
    (el-get-bundle "unhammer/org-random-todo")
    (el-get-bundle "dfeich/org-screenshot")
    (el-get-bundle "mallt/org-clock-today-mode")
    (el-get-bundle "alphapapa/a.el" :name a)
    (el-get-bundle "alphapapa/frecency.el" :name frecency) ;; requires a.el
    (el-get-bundle "alphapapa/org-recent-headings") ;; requires frecency.el
    (el-get-bundle "alphapapa/org-bookmark-heading")
    (el-get-bundle "facetframer/orgnav")
    (el-get-bundle "toc-org") ;; using a private recipe to exclude org
    (el-get-bundle "harrybournis/org-fancy-priorities")
    (el-get-bundle "kiwanami/emacs-calfw")
    (el-get-bundle "org-emms"
                   :type git
                   :url "https://gitlab.com/jagrg/org-emms.git")
    (el-get-bundle "takaxp/emacs-easy-hugo") ;; using a private repo not to download images
    (el-get-bundle "jkitchin/ox-ipynb")
    (el-get-bundle "org-bullets")
    (el-get-bundle "takaxp/org-reveal" :name ox-reveal :branch "org9.2")
    (el-get-bundle "IvanMalison/org-projectile")
    (el-get-bundle "org-trello")
    (el-get-bundle "emacsmirror/orgalist")
    (el-get-bundle "tarsius/orglink")
    (el-get-bundle "Fuco1/org-pretty-table")
    (el-get-bundle "jlumpe/ox-json")
    (el-get-bundle "takaxp/org-onit")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Major modes ;;;
    ;; download zip since python-mode git repository is extremely huge
    (el-get-bundle "python-mode"
                   :type http-zip
                   :url "https://gitlab.com/python-mode-devs/python-mode/-/archive/master/python-mode-master.zip")
    (el-get-bundle "csharp-mode")
    (el-get-bundle "yaml-mode")
    (el-get-bundle "json-mode")
    (el-get-bundle "emacsmirror/csv-mode")
    (el-get-bundle "es-mode")
    (el-get-bundle "markdown-mode")
    ;; (el-get-bundle "po-mode")
    (el-get-bundle "gnuplot-mode")
    (el-get-bundle "emacsmirror/ess")
    (el-get-bundle "emacsmirror/yatex")
    (el-get-bundle "cmake-mode")
    (el-get-bundle "php-mode")
    (el-get-bundle "bruceravel/gnuplot-mode")
    ;; (el-get-bundle "abo-abo/matlab-mode")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Development ;;;
    (el-get-bundle "gregsexton/origami.el" :name origami)
    (el-get-bundle "yasnippet")
    (el-get-bundle "editorconfig")
    (el-get-bundle "ludwigpacifici/modern-cpp-font-lock")
    (el-get-bundle "AdamNiederer/0xc")
    (el-get-bundle "uuid")
    (el-get-bundle "netromdk/describe-number")
    (el-get-bundle "flycheck")
    (el-get-bundle "flycheck-pos-tip")
    (el-get-bundle "ch1bo/flycheck-clang-tidy")
    (el-get-bundle "js2-mode")
    (el-get-bundle "js2-refactor")
    (el-get-bundle "web-mode")
    (el-get-bundle "yasuyk/web-beautify")
    (el-get-bundle "gist")
    (el-get-bundle "Wilfred/elisp-refs" :depends (loop))
    (el-get-bundle "Wilfred/helpful") ;; Requires elisp-refs
    (el-get-bundle "gonewest818/elisp-lint")
    (el-get-bundle "purcell/package-lint")
    (el-get-bundle "magit/libegit2"
                   :build `(("make" ,(format "EMACS=%s" el-get-emacs))))
    (el-get-bundle "magit/transient")
    (el-get-bundle "takaxp/magit" ;; require transient and libegit2
                   :branch "stable" ;; require Tags in the target commit
                   :build `(("make" ,(format "EMACSBIN=%s" el-get-emacs) "docs" "install")
                            ("touch" "lisp/magit-autoloads.el")))
    (el-get-bundle "AdamNiederer/cov")
    (el-get-bundle "ggtags")
    (el-get-bundle "dedi/gxref") ;; emacs 25.1 or later
    (el-get-bundle "bug-hunter")
    (el-get-bundle "clang-format")
    (el-get-bundle "lassik/emacs-format-all-the-code")
    (el-get-bundle "emacsmirror/emr")
    (el-get-bundle "emacsmirror/rmsbolt")
    (el-get-bundle "diffview")
    (el-get-bundle "projectile")
    (el-get-bundle "takaxp/facecheck")
    (el-get-bundle "twlz0ne/elpl")
    ;; (el-get-bundle "tern") ;; require npm
    ;; (el-get-bundle "tern-auto-complete")
    (el-get-bundle "mhayashi1120/Emacs-wgrep")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LSP ;;;
    (el-get-bundle "emacs-lsp/lsp-mode")
    (el-get-bundle "emacs-lsp/dap-mode")
    (el-get-bundle "emacs-lsp/lsp-ui")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Helm modules ;;;
    (when t
      (el-get-bundle "helm");; for M-x
      (el-get-bundle "helm-ag")
      (el-get-bundle "yasuyk/helm-bm")
      (el-get-bundle "emacs-helm/helm-descbinds")
      ;;  (el-get-bundle "yasuyk/helm-flycheck" :depends (helm flycheck dash))
      (el-get-bundle "helm-gtags")
      (el-get-bundle "helm-projectile")
      (el-get-bundle "helm-swoop")
      (el-get-bundle "takaxp/helm-selected")
      (el-get-bundle "helm-pass"
                     :type git
                     :url "https://gitlab.com/jabranham/helm-pass.git")
      (el-get-bundle "smihica/emmet-mode")
      (el-get-bundle "yasuyk/helm-emmet" :depends (emmet-mode))
      (el-get-bundle "emacs-helm/helm-emms")
      (el-get-bundle "jixiuf/helm-dired-history")

      ;; (el-get-bundle "helm-google")
      ;; (el-get-bundle "helm-ghq")
      ;; (el-get-bundle "helm-css-scss")
      ;; (el-get-bundle "helm-rtags")
      )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ivy modules ;;;
    (el-get-bundle "jixiuf/ivy-dired-history")
    (el-get-bundle "ericdanan/counsel-projectile")
    (el-get-bundle "syohex/emacs-counsel-gtags")
    (el-get-bundle "password-store"
                   :type http
                   :url "https://raw.githubusercontent.com/stuartsierra/password-store/master/contrib/emacs/password-store.el")
    (el-get-bundle "ecraven/ivy-pass") ;; requires password-store.el
    (el-get-bundle "abo-abo/swiper")
    ;; (el-get-bundle "takaxp/swiper" :branch "mytest")
    (el-get-bundle "Yevgnen/ivy-rich")
    (el-get-bundle "asok/all-the-icons-ivy")
    (el-get-bundle "takaxp/counsel-selected")
    (el-get-bundle "kchenphy/counsel-world-clock")
    (el-get-bundle "raxod502/prescient.el" :name prescient)
    (el-get-bundle "momomo5717/avy-migemo")
    (el-get-bundle "mkcms/ivy-yasnippet") ;; require ~/.emacs.d/recipes/ivy.rcp
    (el-get-bundle "akirak/ivy-omni-org")
    (el-get-bundle "abo-abo/smex")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OSX support ;;;
    (el-get-bundle "raghavgautam/osx-lib")
    (el-get-bundle "lunaryorn/osx-trash.el" :name osx-trash) ;; Archived in GitHub
    (el-get-bundle "xuchunyang/osx-dictionary.el" :name osx-dictionary)


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Editing support ;;;
    (el-get-bundle "aggressive-indent")
    (el-get-bundle "ws-butler")
    (el-get-bundle "d12frosted/flyspell-correct")
    (el-get-bundle "auto-complete")
    (el-get-bundle "auto-complete-clang")
    (el-get-bundle "company-mode/company-mode")
    (el-get-bundle "expez/company-quickhelp")
    (el-get-bundle "ac-js2")
    (el-get-bundle "migemo")
    (el-get-bundle "sabof/edit-color-stamp")
    (el-get-bundle "tiny")
    (el-get-bundle "rnkn/olivetti")
    (el-get-bundle "abo-abo/hydra")
    ;; (el-get-bundle "parinfer")
    (el-get-bundle "magnars/expand-region.el" :name expand-region)
    (el-get-bundle "magnars/multiple-cursors.el" :name multiple-cursors)
    (el-get-bundle "mattiase/xr")
    (el-get-bundle "mattiase/relint")
    (el-get-bundle "purcell/reformatter.el" :name reformatter)


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Visualize ;;;
    (el-get-bundle "volatile-highlights")
    (el-get-bundle "highlight-symbol")
    (el-get-bundle "fancy-narrow")
    (el-get-bundle "smartparens")
    (el-get-bundle "emacsattic/mic-paren")
    (el-get-bundle "cask/shut-up")
    (el-get-bundle "emacsmirror/delight") ;; or diminish
    (el-get-bundle "takaxp/dimmer.el" :name dimmer) ;; using a private repo not to download images
    (el-get-bundle "emacsmirror/centered-cursor-mode")
    (el-get-bundle "hide-lines")
    (el-get-bundle "undo-tree")
    (el-get-bundle "back-button")
    (el-get-bundle "takaxp/all-the-icons.el" :name all-the-icons)
    (el-get-bundle "jtbm37/all-the-icons-dired" :depends (all-the-icons))
    (el-get-bundle "k-talo/smooth-scroll.el" :name smooth-scroll)
    (el-get-bundle "manage-minor-mode")
    (el-get-bundle "syohex/emacs-git-gutter" :name git-gutter)
    (el-get-bundle "git-gutter-fringe")
    (el-get-bundle "casouri/isolate")
    (el-get-bundle "doublep/logview")
    (el-get-bundle "which-key")
    (el-get-bundle "yanghaoxie/which-key-posframe" :depends (posframe))
    (el-get-bundle "bm")
    (el-get-bundle "emacsmirror/rainbow-mode")
    ;; (el-get-bundle "TheBB/spaceline")
    (el-get-bundle "shrink-path"
                   :type git
                   :url "https://gitlab.com/bennya/shrink-path.el.git")
    (el-get-bundle "seagle0128/doom-modeline" :depends (eldoc-eval))
    (el-get-bundle "disk-usage"
                   :type git
                   :url "https://gitlab.com/ambrevar/emacs-disk-usage.git")
    (el-get-bundle "sebastiencs/company-box")
    ;; (el-get-bundle "casouri/eldoc-box")
    (el-get-bundle "tarsius/hl-todo")
    (el-get-bundle "kiennq/emacs-mini-modeline")
    ;; (el-get-bundle "emacsmirror/minibuffer-line")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; System related ;;;
    (el-get-bundle "Fuco1/dired-hacks")
    (el-get-bundle "calancha/dired-du")
    (el-get-bundle "Vifon/dired-recent.el" :name dired-recent)
    (el-get-bundle "neotree")
    (el-get-bundle "find-file-in-project")
    ;; (el-get-bundle "gited")


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Frame and windows ;;;
    (el-get-bundle "popwin")
    (el-get-bundle "shackle")
    (el-get-bundle "tabbar")

    (el-get-bundle "takaxp/moom")
    (el-get-bundle "emacsmirror/frame-tabs")
    (el-get-bundle "tumashu/ivy-posframe") ;; require swiper
    ;; (el-get-bundle "spaceline-all-the-icons")
    ;; (el-get-bundle "exwm") ;; failed to instal ... Bad Request
    (el-get-bundle "ajgrf/edwin")


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Robustness ;;;
    (el-get-bundle "session")
    (el-get-bundle "conornash/backup-each-save")


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Efficiency ;;;
    (el-get-bundle "Kungsgeten/selected.el" :name selected)
    (el-get-bundle "rubikitch/replace-from-region")
    (el-get-bundle "quickrun")
    (el-get-bundle "latex-math-preview"
                   :type git
                   :url "https://gitlab.com/latex-math-preview/latex-math-preview.git")
    (el-get-bundle "sbrisard/bratex")
    (el-get-bundle "zhangkaiyulw/smart-mark")
    (el-get-bundle "emacsmirror/syntax-subword")
    (el-get-bundle "goto-chg")
    (el-get-bundle "yuttie/initchart")
    (progn ;; frog-menu
      (el-get-bundle "clemera/frog-menu")
      (el-get-bundle "waymondo/frog-jump-buffer" :depends (avy)))
    ;; (el-get-bundle "esup")
    (el-get-bundle "rolandwalker/ignoramus") ;; Ignore backups, build files, et al.


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Applications ;;;
    (el-get-bundle "lugecy/lingr-el")
    (el-get-bundle "xuchunyang/gitter.el")
    (el-get-bundle "ag")
    (el-get-bundle "google-this")
    (el-get-bundle "japanese-holidays")
    (el-get-bundle "skeeto/emacsql")
    (el-get-bundle "aaronbieber/sunshine.el" :name sunshine)
    (el-get-bundle "NicolasPetton/pass")
    (el-get-bundle "pdf-tools")
    (el-get-bundle "gif-screencast"
                   :type git
                   :url "https://gitlab.com/ambrevar/emacs-gif-screencast.git")
    (el-get-bundle "emms"
                   :type git
                   :url "https://git.savannah.gnu.org/git/emms.git"
                   :load-path ("./lisp"))
    (el-get-bundle "tarsius/keycast")
    (el-get-bundle "jamiguet/network-watch")
    ;; (el-get-bundle "w3")
    ;; (el-get-bundle "japanlaw")
    ;; (el-get-bundle "google-maps")
    (el-get-bundle "d12frosted/counsel-osx-app")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Log ;;;
    (el-get-bundle "dacap/keyfreq")
    (el-get-bundle "davep/uptimes.el" :name "uptimes")
    ;; (el-get-bundle "sauron")

    (el-get 'sync)))

;;;###autoload
(defun my-elget-setup ()
  (interactive)
  ;; Install all packages to this directory
  (setq el-get-dir
        (expand-file-name "el-get" (locate-user-emacs-file emacs-version)))
  (add-to-list 'load-path (concat el-get-dir "/el-get"))
  (add-to-list 'load-path (concat el-get-dir "/postpone"))
  (unless (file-directory-p el-get-dir)
    (user-error (format "!! Before use this, be sure %s exists !!" el-get-dir)))

  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        ;; `el-get-silent-update' が使えるカスタマイズパッケージを使う．
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/takaxp/el-get/master/el-get-install.el")
      ;; オリジナルはこっち
      ;; "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el"
      (goto-char (point-max))
      (eval-print-last-sexp)))

  ;; use private recipes
  (add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")

  (setq el-get-git-shallow-clone t ;; "--depth 1"
        el-get-verbose nil ;; just for sure
        el-get-silent-update t ;; 出力されるメッセージの抑制
        gc-cons-threshold (* 512 1024 1024) ;; 512MB
        garbage-collection-messages t))

;;;###autoload
(defun ad:el-get-post-update-notification (package)
  "Notify the PACKAGE has been updated in customized form."
  (el-get-notify "el-get update" (format "%s" package)))

(advice-add 'el-get-post-update-notification
            :override #'ad:el-get-post-update-notification)

;;;###autoload
(defun my-reset-elget-links ()
  (interactive)
  (when (shell-command-to-string "export HOSTTYPE=\"intel-mac\" && ~/Dropbox/emacs.d/bin/update-elget.sh -r")
    (message "[el-get] Link updated.")))

(defun my-elget-update ()
  "Update function for batch-mode."
  (my-elget-setup)
  (my-elget-bundles))

;; init
(unless noninteractive
  (my-elget-setup))

(provide 'elget-config)
;;; elget-config.el ends here
