;;;; Basic configuration for Emacs
;;;;                                       Last Update: 2011-10-06@10:03
;;;;                                       Takaaki ISHIKAWA  <takaxp@ieee.org>
;;;; Cite: http://www.mygooglest.com/fni/dot-emacs.html

(message "* --[ Loading an init file, takaxp-init.el ] --")

;;; Enable narrowing
(put 'narrow-to-region 'disabled nil)

;;; Enable debugger
;(toggle-debug-on-error)

;;; Language and encoding
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq save-buffer-coding-system 'utf-8-unix)
;(set-buffer-process-coding-system 'utf-8 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)
;(set-locale-environment "ja_JP.UTF-8")
(set-locale-environment "en_US.UTF-8")
(when (eq window-system 'ns)
  (setq default-input-method "MacOSX")
  (mac-add-key-passed-to-system 'shift))

;;; [mode] ChangeLog
;(setq user-full-name "Your NAME")
;(setq user-mail-address "your@address.com")
(add-hook 'change-log-mode-hook
	  '(lambda() (setq tab-width 4) (setq left-margin 4)))

;;; [mode] Text
;; http://d.hatena.ne.jp/NeoCat/20080211
(add-hook 'text-mode-hook
	  '(lambda() 
	     (setq tab-width 4)	     
	     (setq tab-stop-list
		   '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
		       64 68 72 76 80))
	     (setq indent-line-function 'tab-to-tab-stop)))

;;; [mode] C/C++
(setq auto-mode-alist
      (append '(("\\.h\\'" . c++-mode)) auto-mode-alist))

;;; [mode] html
(setq auto-mode-alist
      (append '(("\\.html\\'" . html-helper-mode))
	      auto-mode-alist))

;;; [mode] YaTeX
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(setq YaTeX-kanji-code 4) ; 1=Shift JIS, 2=JIS, 3=EUC, 4=UTF-8
;; Disable auto line break
(add-hook 'yatex-mode-hook
	  '(lambda ()
	     (setq auto-fill-function nil)))

;;; [mode] PO
(when (autoload 'po-mode "po-mode" nil nil)
  (setq auto-mode-alist
	(cons '("\\.po[tx]?\\'\\|\\.po\\." . po-mode)
	      auto-mode-alist)))

;;; [mode] Info
(when (require 'info nil t)
  (add-to-list 'Info-additional-directory-list
	     "/Users/taka/devel/git/org-ja/work/"))

;;; [mode] org
;; see takaxp-org-mode.el

;;; Settings for emacs core system ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use unlimited undo buffer
(setq undo-outer-limit nil)

;; Set the GC size 10times larger
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;; Remember the last buffer and restore it when starting up
;; It will write files as .emacs.desktop and .emacs.desktop.locks
;; Cite: http://www.emacswiki.org/emacs/DeskTop
(desktop-save-mode 1)
(setq desktop-files-not-to-save "\\(^/tmp\\|^/var\\|^/ssh:\\)") 

;; history size to store
(setq history-length 1000)

;; Save command history
(savehist-mode 1)

;; .recentf auto save
; http://d.hatena.ne.jp/tomoya/20110217/1297928222
(when (require 'recentf nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 60 t 'recentf-save-list))
  (setq recentf-exclude '("^/tmp.*" "^/var/folders.*" "/TAGS$"))
  (recentf-mode 1))

;; Auto save
;; Cite: http://0xcc.net/misc/auto-save/
(when (require 'auto-save-buffers nil t)
  (run-with-idle-timer 0.5 t 'auto-save-buffers))

;; Backup with generation files by backup-dir.el
;; http://www.northbound-train.com/emacs.html
(make-variable-buffer-local 'backup-inhibited)
(when (and (require 'backup-dir nil t)
	   (file-directory-p "~/env/emacs_backup"))
  ;; backup path
  (setq bkup-backup-directory-info '((t "~/env/emacs_backup" ok-create)))
  ;; generation properties
  (setq delete-old-versions t
	kept-old-versions 0
	kept-new-versions 5
	version-control t))

;; C/Migemo
; NOTE: M-x migemo-toggle-isearch-enable
(setq completion-ignore-case t)
(when (and (executable-find "cmigemo")
	   (require 'migemo nil t))
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  ;; Use cache
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1024)
  ;; Encoding
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))

;; anything
(when (require 'anything-startup nil t)
  (require 'anything-c-moccur nil t)
;  (setq moccur-split-word t)
;  (setq anything-c-locate-options `("locate" "-w"))

  (when (require 'migemo nil t)
    (setq moccur-use-migemo t))
  
  (defun my-anything ()
    (interactive)
    (anything-other-buffer
     '(anything-c-source-recentf
       anything-c-source-file-name-history
       anything-c-source-buffers
       anything-c-source-emacs-commands
       anything-c-source-locate)
     " *my-anything*"))
  
  (defun my-anything-buffer ()
    (interactive)
    (anything-other-buffer
     '(anything-c-source-buffers)
     " *my-anthing-buffer*"))

  (when (eq window-system 'ns)
    (defun anything-spotlight ()
      "Spotlight search with anything.el"
      (interactive)
      (anything-other-buffer
       '(anything-c-source-mac-spotlight)
       " *anything-spotlight*")))

  (setq anything-candidate-number-limit 50) ; 50
  (setq anything-input-idle-delay 0.1)      ; 0.1
  (setq anything-idle-delay 0.5)            ; 0.5
  (setq anything-quick-update nil))         ; nil


;; Extensions for editing or viewing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Remember cursor position and make it possible to undo
;; Cite: M-x install-elisp http://www.emacswiki.org/cgi-bin/wiki/download/point-undo.el
(require 'point-undo nil t)

;;; Paste text only without any additional attributions (ex. Copy from Excel)
(setq yank-excluded-properties t)

;;; Rectangular editing（by sense-region.el）
;; C-space C-space: specify rectangular area visually,
;; C-w: kill-region, M-w: kill-ring-save, C-y: yank, M-%: query-replace,
;; C-M-%: query-replace-regexp, C-i: indent-for-tab-command),
;; M-x: comment-region
;; Note: Call this before org-mode, conflicting with remember.el (C-c r t)
(when (require 'sense-region nil t)
  (sense-region-on))
;; (cua-mode t) ;; cua-mode has also rectangular function

;;; Time stamp
(when (require 'time-stamp nil t)
  (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-active t)
  (setq time-stamp-start "Last Update: ")
  (setq time-stamp-format "%04y-%02m-%02d@%02H:%02M")
  (setq time-stamp-end "$")
  (setq time-stamp-line-limit 10)) ; def=8

;;; sdic (reqiure load-path setting)
(when (autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
  (autoload 'sdic-describe-word-at-point "sdic"
    "カーソルの位置の英単語の意味を調べる" t nil)
  (setq sdic-face-color "#3333FF")
  (setq sdic-default-coding-system 'utf-8)
  ;; Dictionary (English => Japanese)
  (setq sdic-eiwa-dictionary-list
	'((sdicf-client "~/Dropbox/Dic/EIJIRO5/EIJI-118.sdic")))
  ;; Dictionary (Japanese => English)
  (setq sdic-waei-dictionary-list
	'((sdicf-client "~/Dropbox/Dic/EIJIRO5/WAEI-118.sdic"))))

;;; Use aspell for spell checking instead of ispell.
;;; 'ns => sudo port install aspell aspell-dict-en
;;; 'x32 => installer.exe and aspell-en from http://aspell.net/win32/
(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  (when (eq window-system 'w32)
    (setq-default ispell-program-name "C:/Program Files/Aspell/bin/aspell.exe"))
  ;;(setq ispell-grep-command "grep")
  ;; for English and Japanese mixed
  (eval-after-load "ispell"
    '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]")))
  (setq ispell-dictionarry "english")
  (setq ispell-personal-dictionary "~/env/dot_files/.aspell.en.pws")
  ;; http://www.morishima.net/~naoto/fragments/archives/2005/12/20/flyspell/
  ;; This will also avoid an IM-OFF issue for flyspell-mode.
  (setq ispell-local-dictionary-alist
      '((nil "[a-zA-Z]" "[^a-zA-Z]" "'" t
	     ("-d" "en" "--encoding=utf-8") nil utf-8)))
;  (setq ispell-aspell-supports-utf8 t)
;  (setq ispell-encoding8-command t)
  )

;;; Flyspell
;;(dolist
;;    (hook
;;     '(text-mode-hook change-log-mode-hook c++-mode-hook
;;		      latex-mode-hook org-mode-hook))
;;  (add-hook hook (lambda () (flyspell-mode 1))))
;;(add-hook 'c++-mode-hook
;;	  (lambda () (flyspell-prog-mode)))


(provide 'takaxp-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Not Active

;;; Avoid warning (for sense-region)
;;; Warning: 'mapcar' called for effect; use 'mapc' or 'dolist' insted
;(setq byte-compile-warnings
;      '(free-vars unresolved callargs redefine obsolete noruntime
;		  cl-functions interactive-only make-local))

;;; idle-requie
;;(require 'idle-require)
;;(idle-require-mode 1)

;;; pdf-preview.el を読み込む
;;(require 'pdf-preview)

;;; EasyPG
;(require 'epa-setup)
;(epa-file-enable)

;; eblook
;(require 'eblook)
;(autoload 'edict-search-english "edic
;    "Search for a translation of an English word" t)
;(autoload 'edict-search-kanji "edict"
;     "Search for a translation of a Kanji sequence" t)
;(setq *edict-files* '("/Users/taka/Dropbox/Dic/LDOCE4"))
;(setq *edict-files* '("/Users/taka/Downloads/edict/edict"))

;; iBuffer で list-buffers をオーバーライド // C-x C-b で表示
;(defalias 'list-buffers 'ibuffer)


;;; lookup for dictionary (require EB Library, eblook, and lookup.el)
;; package download: http://sourceforge.net/projects/lookup
;; http://lookup.sourceforge.net/docs/ja/index.shtml#Top
;; http://www.bookshelf.jp/texi/lookup/lookup-guide.html#SEC_Top
;(load "lookup-autoloads") ; for 1.99
;(autoload 'lookup "lookup" nil t)
;(autoload 'lookup-region "lookup" nil t)
;(autoload 'lookup-word "lookup" nil t)
;(autoload 'lookup-select-dictionaries "lookup" nil t)
;; Search Agents
;; ndeb option requries "eblook" command
;(setq lookup-search-agents `((ndeb ,(concat homedir "/Dropbox/Dic/COBUILD5"))
;			     (ndeb ,(concat homedir "/Dropbox/Dic/LDOCE4"))))

;(setq lookup-use-bitmap nil)
;(setq ndeb-program-name "/usr/bin/eblook")
;(when (eq window-system 'ns)
;  (setq ndeb-program-name "/opt/local/bin/eblook")
;  (setq ndeb-program-arguments '("-q" "-e" "euc-jp"))
;  (setq ndeb-process-coding-system 'utf-8)) ; utf-8-hfs


;; .lookup/cache.el
;;(setq lookup-init-directory "~/env/dot_files/.lookup")
;;
;(setq lookup-search-modules
 ;     '(("default"
;	 ("ndeb:/Users/taka/Dropbox/Dic/COBUILD5/cobuild" :priority t)
;	 ("ndeb:/Users/taka/Dropbox/Dic/COBUILD5/wordbank" :priority t)
;	 ("ndeb:/Users/taka/Dropbox/Dic/LDOCE4/ldoce4" :priority t)
;	 ("ndeb:/Users/taka/Dropbox/Dic/LDOCE4/bank" :priority t)
;	 ("ndeb:/Users/taka/Dropbox/Dic/LDOCE4/colloc" :priority t)
;	 ("ndeb:/Users/taka/Dropbox/Dic/LDOCE4/activ" :priority t))))
;
;(setq lookup-agent-attributes
 ;     '(("ndeb:/Users/taka/Dropbox/Dic/COBUILD5"
;	 (dictionaries "cobuild" "wordbank"))
;	("ndeb:/Users/taka/Dropbox/Dic/LDOCE4"
;	 (dictionaries "ldoce4" "bank" "colloc" "activ"))))
;
;(setq lookup-dictionary-attributes
 ;     '(("ndeb:/Users/taka/Dropbox/Dic/COBUILD5/cobuild"
;	 (title . "COBUILD 5th Edition")
;	 (methods exact prefix))
;	("ndeb:/Users/taka/Dropbox/Dic/COBUILD5/wordbank"
;	 (title . "Wordbank")
;	 (methods))
;	("ndeb:/Users/taka/Dropbox/Dic/LDOCE4/ldoce4"
;	 (title . "Longman 4th Edition")
;	 (methods exact prefix))
;	("ndeb:/Users/taka/Dropbox/Dic/LDOCE4/bank"
;	 (title . "LDOCE4 Examples and Phrases")
;	 (methods exact prefix menu))
;	("ndeb:/Users/taka/Dropbox/Dic/LDOCE4/colloc"
;	 (title . "LDOCE4 Collocation")
;	 (methods exact prefix))
;	("ndeb:/Users/taka/Dropbox/Dic/LDOCE4/activ"
;	 (title . "Longman Activator")
;	 (methods exact prefix menu))))

;;(setq lookup-default-dictionary-options
;;      '((:stemmer .  stem-english)))
;(setq lookup-use-kakasi nil)

;; Flyspell does NOT have a good compatibility with IME of Macintosh
