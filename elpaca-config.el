;; elpaca-config.el --- -*- lexical-binding: t; -*-

;; Disable nativecomp
(setq native-comp-jit-compilation nil
      native-comp-enable-subr-trampolines nil)

;; Disable package loading by package.el
(setq package-enable-at-startup nil)

(setq elpaca-queue-limit 24)
;; 100 1:58
;; 64 1:13
;; 32 1:12, 1:05
;; 28 1:01
;; 24 0:59, 1:00
;; 20 0:58.9
;; 16 1:07, 1:05
;; 12 1:18
;; 8 1:57


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTALL elpaca
(defconst elpaca--emacs-releases
  '(("27.1" . 20200804) ("27.2" . 20210319) ("28.1" . 20220403)
    ("28.2" . 20220912) ("29.1" . 20230730) ("29.2" . 20240118)
    ("29.3" . 20240324) ("29.4" . 20240622) ("30.1" . 20250223)
    ("30.2" . 20250814)))
(setq elpaca-core-date
      (let ((release (assoc emacs-version elpaca--emacs-releases
			    #'string-prefix-p)))
	;; Development version.
	(and release (> (length (version-to-list emacs-version)) 2)
	     (lwarn `(elpaca core stale ,(intern emacs-version)) :debug
		    "Emacs %s assigned %s elpaca-core-date."
		    emacs-version (car release)))
	(list (or (cdr release)
		  (and emacs-build-time
		       (string-to-number
			(format-time-string "%Y%m%d" emacs-build-time)))
		  (and (display-warning
			`(elpaca core ,(intern emacs-version))
			"Unable to determine elpaca-core-date")
		       -1)))))
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
  (message "elpaca--waiting: %s" elpaca--waiting)
  (message "elpaca--queues: %s" (length elpaca--queues))
  (when (fboundp 'my-elpaca-save-load-path)
    (message "--- saving load-path")
    (my-elpaca-save-load-path))
  ;; If you kill emacs here, do not use ":wait t" anywhere
  ;; (kill-emacs)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [NOTE] Both counsel and ivy are required to stably install related packages
;; (elpaca 'counsel) is failed ;; Unable to find main elisp file for counsel
;; (elpaca	(counsel :host github :repo "abo-abo/swiper" :main "counsel.el"))
;; (elpaca (ivy :host github :repo "abo-abo/swiper" :main "ivy.el"))
(elpaca (swiper :host github :repo "abo-abo/swiper")); :wait t))

(my-elpaca-github "takaxp/moom")
(my-elpaca-github "takaxp/ascii")
(my-elpaca-github "takaxp/session")
(my-elpaca-github "dacap/keyfreq")
(my-elpaca-github "conornash/backup-each-save")
(my-elpaca-github "bbatsov/super-save")
(my-elpaca-github "bbatsov/crux")
(my-elpaca-github "emacsmirror/centered-cursor-mode")
(my-elpaca-github "Kungsgeten/selected.el" selected)
(my-elpaca-github "hniksic/emacs-htmlize" htmlize)
(my-elpaca-github "takaxp/help-fns-plus" help-fns+)
(my-elpaca-github "k-talo/smooth-scroll.el")
(my-elpaca-github "tarsius/hl-todo")
(my-elpaca-github "takaxp/postpone")
(my-elpaca-github "takaxp/org-onit")
(my-elpaca-github "takaxp/org-plist")
(my-elpaca-github "takaxp/org-bookmark-heading")
(my-elpaca-github "IvanMalison/org-projectile")
(my-elpaca-github "tarsius/orglink")
(my-elpaca-github "awth13/org-appear")
(my-elpaca-github "ROCKTAKEY/grugru")
(my-elpaca-github "purcell/package-lint")

(progn ;; helpful
  (my-elpaca-github "Wilfred/loop.el")
  (my-elpaca-github "Wilfred/elisp-refs")
  (my-elpaca-github "Wilfred/helpful"))

;; Visualize
(my-elpaca-github "takaxp/bsv")
(my-elpaca-github "takaxp/imenu-list")
(my-elpaca-github "cask/shut-up")
(my-elpaca-github "emacsmirror/delight") ;; or diminish

;; ivy modules
(my-elpaca-github "radian-software/prescient.el" prescient)
(my-elpaca-github "radian-software/ctrlf")
(my-elpaca-github "abo-abo/smex")
(my-elpaca-github "magnars/expand-region.el" expand-region)
(my-elpaca-github "jdtsmith/mlscroll")
(my-elpaca-github "rubikitch/replace-from-region")
(my-elpaca-github "takaxp/ah")
(my-elpaca-github "takaxp/facecheck")

(elpaca 'ws-butler)
(elpaca 'aggressive-indent)
(elpaca 'dimmer)
(elpaca 'projectile)
(elpaca 'bm)
(elpaca 'volatile-highlights)
(elpaca 'highlight-symbol)
(elpaca 'smartparens)
(elpaca 'manage-minor-mode)
(elpaca 'google-this)
(elpaca 'japanese-holidays)
(elpaca 'uuid)
(progn ;; async.el
  (my-elpaca-github "jwiegley/emacs-async" async)
  (elpaca 'org-download))
(elpaca (gcmh :host gitlab :repo "koral/gcmh"))


;; icons-in-terminal
(my-elpaca-github "seagle0128/icons-in-terminal.el" icons-in-terminal)
(progn ;; Nerd-icons
  (my-elpaca-github "rainstormstudio/nerd-icons.el" nerd-icons)
  (my-elpaca-github "rainstormstudio/nerd-icons-dired")
  (my-elpaca-github "LuigiPiucco/nerd-icons-corfu"))

;; macOS support
(when (eq system-type 'darwin)
  (elpaca (org-mac-link :host gitlab :repo "aimebertrand/org-mac-link"))
  (my-elpaca-github "raghavgautam/osx-lib")
  (my-elpaca-github "lunaryorn/osx-trash.el" osx-trash)
  (my-elpaca-github "xuchunyang/osx-dictionary.el" osx-dictionary))

;; git-gutter
(my-elpaca-github "syohex/emacs-git-gutter" git-gutter)
(my-elpaca-github "nschum/fringe-helper.el" fringe-helper)
(my-elpaca-github "emacsorphanage/git-gutter-fringe")

(elpaca (org-tree-slide
	 :host github :repo "takaxp/org-tree-slide" :branch "develop"))


;; Org Mode - ox/ob
(my-elpaca-github "0x60df/ox-qmd")
(my-elpaca-github "larstvei/ox-gfm")
(my-elpaca-github "jlumpe/ox-json")
(my-elpaca-github "zweifisch/ob-http")
(my-elpaca-github "astahlman/ob-async") ;; :depends (emacs-async)
(my-elpaca-github "pope/ob-go")

(elpaca 'flycheck)
(elpaca 'flycheck-pos-tip)
(my-elpaca-github "ch1bo/flycheck-clang-tidy")

(my-elpaca-github "jixiuf/ivy-dired-history")
(my-elpaca-github "franburstall/ivy-emms")
(my-elpaca-github "Yevgnen/ivy-rich")
(my-elpaca-github "syohex/emacs-counsel-gtags" counsel-gtags)
(my-elpaca-github "takaxp/counsel-selected")
(my-elpaca-github "ericdanan/counsel-projectile")
(my-elpaca-github "kchenphy/counsel-world-clock")
(my-elpaca-github "akirak/ivy-omni-org")
(my-elpaca-github "seagle0128/nerd-icons-ivy-rich")
(my-elpaca-github "d12frosted/counsel-osx-app")


;;;;;;;;;;;;;;;;;;;;;

(my-elpaca-github "magit/ghub")
(my-elpaca-github "tarsius/llama")
(my-elpaca-github "emacs-compat/compat")
(my-elpaca-github "magit/magit-popup")
(my-elpaca-github "magit/transient")
(my-elpaca-github "zk-phi/git-complete")

(my-elpaca-github "oantolin/embark")
(my-elpaca-github "minad/corfu")
(my-elpaca-github "xenodium/org-block-capf")
(my-elpaca-github "jdtsmith/kind-icon")  ;; requires svg-lib
(my-elpaca-github "minad/cape")
(my-elpaca-github "oantolin/orderless")

(elpaca 'org-bullets)
(elpaca 'orgbox)

(my-elpaca-github "misohena/phscroll")
(my-elpaca-github "yjwen/org-reveal" ox-reveal)
(my-elpaca-github "pinard/org-grep")
(my-elpaca-github "alphapapa/org-web-tools")
(my-elpaca-github "alphapapa/org-ql")
(elpaca
 (org-screenshot :host github :repo "dfeich/org-screenshot" :main "org-attach-screenshot.el"))
(my-elpaca-github "mallt/org-clock-today-mode" org-clock-today)

(my-elpaca-github "Fuco1/org-pretty-table")
(my-elpaca-github "xenodium/company-org-block")
(progn
  (elpaca 'persist)
  (my-elpaca-github "ichernyshovvv/org-timeblock"))
(progn
  (my-elpaca-github "tarsius/outline-minor-faces")
  (my-elpaca-github "tarsius/backline"))

(elpaca 'yaml-mode)
(elpaca 'json-mode)
(elpaca 'es-mode)
(elpaca 'markdown-mode)
(elpaca 'php-mode)

(my-elpaca-github "bruceravel/gnuplot-mode" gnuplot)
(my-elpaca-github "dominikh/go-mode.el" go-mode)

(progn ;; pass
  (my-elpaca-github "ecraven/ivy-pass"))

;; Development
(my-elpaca-github "gregsexton/origami.el" origami)
(elpaca 'yasnippet)
(my-elpaca-github "AdamNiederer/0xc")
(my-elpaca-github "ludwigpacifici/modern-cpp-font-lock")
(progn
  (my-elpaca-github "d5884/yabin")
  (my-elpaca-github "netromdk/describe-number"))
(elpaca 'js2-mode)
(elpaca 'js2-refactor)
(elpaca 'web-mode)
(my-elpaca-github "yasuyk/web-beautify")
(elpaca 'gist)
(progn
  (my-elpaca-github "AdamNiederer/elquery")
  (my-elpaca-github "AdamNiederer/cov"))
(my-elpaca-github "dedi/gxref") ;; emacs 25.1 or later
(elpaca 'ggtags)
(elpaca 'clang-format)
(elpaca 'diffview)
(my-elpaca-github "twlz0ne/elpl")
(my-elpaca-github "mhayashi1120/Emacs-wgrep" wgrep)
(my-elpaca-github "syohex/emacs-go-eldoc" go-eldoc)
(my-elpaca-github "jacktasia/dumb-jump")
(when (< emacs-major-version 29)
  ;; integrated in Emacs 29 or later
  (my-elpaca-github "karlotness/tree-sitter.el" tree-sitter)
  (my-elpaca-github "ubolonton/emacs-tree-sitter")
  (my-elpaca-github "ubolonton/tree-sitter-langs"))




(my-elpaca-github "NicolasPetton/pass")
(unless (eq system-type 'windows-nt)
  (my-elpaca-github "emacsmirror/yatex"))

;; Editing support
(my-elpaca-github "d12frosted/flyspell-correct")

(progn ;; skewer
  (elpaca 'skewer-mode)
  (elpaca 'auto-complete)) ;; require 'skewer
(elpaca 'auto-complete-clang)
(elpaca 'ac-js2)
;; (my-elpaca-github "emacs-jp/migemo")
(my-elpaca-github "mattiase/xr")
(my-elpaca-github "mattiase/relint")
(my-elpaca-github "purcell/reformatter.el" reformatter)
(progn
  (my-elpaca-github "lorniu/pdd.el" pdd)
  (my-elpaca-github "lorniu/gt.el" gt))

(my-elpaca-github "doublep/logview")
(my-elpaca-github "rougier/svg-lib")
(my-elpaca-github "rougier/svg-tag-mode")
(my-elpaca-github "DevelopmentCool2449/colorful-mode")
(my-elpaca-github "seagle0128/doom-modeline"); :depends (eldoc-eval))
(elpaca (disk-usage
	 :host gitlab
	 :repo "ambrevar/emacs-disk-usage"))
(my-elpaca-github "zk-phi/gitmole")
(my-elpaca-github "chuntaro/emacs-keypression" keypression)
(my-elpaca-github "lewang/command-log-mode")
(my-elpaca-github "emacs-vs/rainbow-csv")
(my-elpaca-github "takaxp/ivy-yasnippet")
(my-elpaca-github "momomo5717/avy-migemo")
(my-elpaca-github "alphapapa/org-recent-headings")
(my-elpaca-github "facetframer/orgnav")

;; System related
(my-elpaca-github "Fuco1/dired-hacks")
(my-elpaca-github "Vifon/dired-recent.el" dired-recent)
(elpaca 'neotree)
(elpaca 'find-file-in-project)

;; Frame and windows
(elpaca 'popwin)
(elpaca 'shackle)

;; Efficiency
(elpaca 'quickrun)
(elpaca (latex-math-preview :host gitlab
			    :repo "latex-math-preview/latex-math-preview"))
(my-elpaca-github "sbrisard/bratex")
(my-elpaca-github "zhangkaiyulw/smart-mark")
(my-elpaca-github "emacsmirror/syntax-subword")
;; (my-elpaca-github "phikal/compat.el" compat)
;; (my-elpaca-github "magit/transient")
(my-elpaca-github "conao3/transient-dwim.el" transient-dwim)

;; Applications
(elpaca 'ag)
(elpaca 'pdf-tools)
(elpaca (gif-screencast
	 :host gitlab
	 :repo "ambrevar/emacs-gif-screencast"))
(my-elpaca-github "jorgenschaefer/circe")
(my-elpaca-github "skeeto/elfeed")
(my-elpaca-github "remyhonig/elfeed-org")
(my-elpaca-github "sabof/edit-color-stamp")
(unless (eq system-type 'windows-nt)
  (elpaca 'vterm))
(my-elpaca-github "karthink/gptel")

;; Log
(my-elpaca-github "davep/uptimes.el" uptimes)

;; Terminal
(my-elpaca-github "szermatt/mistty")


(elpaca 'undo-fu)
(elpaca 'corfu-terminal)
(elpaca 'python-mode)
(elpaca 'password-store)
(my-elpaca-github "lassik/emacs-format-all-the-code" format-all)



(elpaca 'po-mode)
(elpaca 'rainbow-mode)
(elpaca 'csv-mode)
(elpaca 'nhexl-mode)


(elpaca 'orgalist)
(elpaca 'dired-du)

;; Having issues
;; (elpaca 'org-extra-emphasis)
;; (elpaca 'emr) ;; iedit installed version lower than min require 0.97

;; (elpaca 'cmake-mode) ;; taking long time
(elpaca 'org)
(elpaca 'org-contrib)

;; required at the end of this code to run all items for batch-mode
(when noninteractive
  (elpaca-wait))

(elpaca-process-queues)
(provide 'elpaca-config)
