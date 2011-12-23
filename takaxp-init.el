;;;; Basic configuration for Emacs
;;;;                                       Last Update: 2011-12-23@19:37
;;;;                                       Takaaki ISHIKAWA  <takaxp@ieee.org>
;;;; Cite: http://www.mygooglest.com/fni/dot-emacs.html (GREAT!)

(message "* --[ Loading an init file, takaxp-init.el ] --")

;;; Enable narrowing
(put 'narrow-to-region 'disabled nil)

;;; Language and encoding
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq save-buffer-coding-system 'utf-8-unix)
;(set-buffer-process-coding-system 'utf-8 'utf-8)
;(setq process-coding-system-alist
;      (cons '("grep" utf-8 . utf-8) process-coding-system-alist))
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)
;(set-locale-environment "ja_JP.UTF-8")
(set-locale-environment "en_US.UTF-8")
(when (and (eq window-system 'ns) (= emacs-major-version 23)) 
  (setq default-input-method "MacOSX")
  (mac-add-key-passed-to-system 'shift))

;;; Future works
;; (when (require 'mozc nil t)
;;   (setq default-input-method "japanese-mozc")
;;   (setq mozc-mode-string "M"))

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

;;; [mode] latex-math-preview
(require 'latex-math-preview nil t)

;;; [mode] PO (po-mode.el and po-mode+.el)
;; http://www.emacswiki.org/emacs/PoMode
;; http://www.emacswiki.org/emacs/po-mode+.el
(autoload 'po-mode "po-mode+" nil nil)
(setq auto-mode-alist
      (cons '("\\.po[tx]?\\'\\|\\.po\\." . po-mode)
	    auto-mode-alist))

;;; [mode] Info
(when (require 'info nil t)
  (add-to-list 'Info-additional-directory-list
	       (expand-file-name "~/devel/mygit/org-ja/work/")))

(defun org-info-ja (&optional node)
  "(Japanese) Read documentation for Org-mode in the info system.
With optional NODE, go directly to that node."
  (interactive)
  (info (format "(org-ja)%s" (or node ""))))


;;; [mode] org
;; see takaxp-org-mode.el

;;; Cycle-buffer
;; http://www.emacswiki.org/emacs/download/cycle-buffer.el
(autoload 'cycle-buffer "cycle-buffer" "Cycle forward." t)
(autoload 'cycle-buffer-backward "cycle-buffer" "Cycle backward." t)
;; Count a buffer shown in another window when using e2wm with two perspective
(setq cycle-buffer-allow-visible t)
(setq cycle-buffer-show-length 12)
(setq cycle-buffer-show-format '(" <(%s)>" . " %s"))


;;; Settings for emacs core system ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use unlimited undo buffer
(setq undo-outer-limit nil)

;; Set the GC size 10times larger
;(setq gc-cons-threshold (* 10 gc-cons-threshold))

;; history size to store
(setq history-length 1000)

;; Save command history
(savehist-mode 1)

;; (built-in) .recentf auto save
; http://d.hatena.ne.jp/tomoya/20110217/1297928222
(when (require 'recentf nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  ;; see takaxp-utility.el
  ;; (setq recentf-auto-save-timer
  ;;       (run-with-idle-timer 60 t 'recentf-save-list))
  (setq recentf-exclude '("^/tmp.*" "^/var/folders.*" "/TAGS$"))
  (recentf-mode 1))

;; (built-in) Remember the last buffer and restore it when starting up
;; It will write files as .emacs.desktop and .emacs.desktop.locks
;; Cite: http://www.emacswiki.org/emacs/DeskTop
(when (require 'desktop nil t)
  (desktop-save-mode 1)
  (setq desktop-files-not-to-save "\\(^/tmp\\|^/var\\|^/ssh:\\)"))

;;; Session.el
;;; http://emacs-session.sourceforge.net/
(when (require 'session nil t)
  (add-hook 'after-init-hook 'session-initialize)
  (add-to-list 'session-globals-exclude 'org-mark-ring)
  ;; Change save point of session.el
  (setq session-save-file (expand-file-name "~/Dropbox/.session"))
  ;; Combine with desktop.el
  (setq desktop-globals-to-save '(desktop-missing-file-warning)))

;; Auto save
;; Cite: http://0xcc.net/misc/auto-save/
(when (require 'auto-save-buffers nil t)
  (run-with-idle-timer 1.0 t 'auto-save-buffers))

;; Backup with generation files by backup-dir.el
;; Cite: http://www.emacswiki.org/emacs/BackupDirectory
;;       http://www.northbound-train.com/emacs-hosted/backup-dir.el
;;       http://www.northbound-train.com/emacs.html
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
;; NOTE: M-x migemo-toggle-isearch-enable
;; Cite: http://www.kaoriya.net/software/cmigemo
(when (and (executable-find "cmigemo")
	   (require 'migemo nil t))
  (setq completion-ignore-case t) ;; case-independent
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
  ;; http://svn.coderepos.org/share/lang/elisp/anything-c-moccur/trunk/anything-c-moccur.el
  ;; http://d.hatena.ne.jp/IMAKADO/20080724/1216882563
  (require 'anything-c-moccur nil t)
;  (setq moccur-split-word t)
;  (setq anything-c-locate-options `("locate" "-w"))

  ;; M-x install-elisp-from-emacswiki recentf-ext.el
  ;; http://www.emacswiki.org/cgi-bin/wiki/download/recentf-ext.el
  (require 'recentf-ext nil t)

  (when (require 'migemo nil t)
    (setq moccur-use-migemo t))

  ;; M-x anything-grep-by-name
  (setq anything-grep-alist
	'(("Org-files" ("egrep -Hin %s *.org" "~/Dropbox/org/"))
	  (".emacs.d" ("egrep -Hin %s *.el" "~/.emacs.d/"))
	  ("ChangeLog" ("egrep -Hin %s ChangeLog" "~/"))))
  ;;	("Spotlight" ("mdfind %s -onlyin ~/Dropbox/Documents/Library/" ""))))

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
;; Cite: http://www.emacswiki.org/cgi-bin/wiki/download/point-undo.el
(autoload 'point-undo "point-undo" nil t)

;;; Paste text only without any additional attributions (ex. Copy from Excel)
(setq yank-excluded-properties t)

;;; Rectangular editing
(setq cua-enable-cua-keys nil)
(cua-mode t)
;; not recommend
(setq cua-rectangle-mark-key (kbd "C-SPC"))

;;; (built-in) Time stamp
(when (require 'time-stamp nil t)
  (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-active t)
  (setq time-stamp-start "Last Update: ")
  (setq time-stamp-format "%04y-%02m-%02d@%02H:%02M")
  (setq time-stamp-end "$")
  (setq time-stamp-line-limit 10)) ; def=8

;;; sdic (reqiure load-path setting)
(autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
(autoload 'sdic-describe-word-at-point "sdic"
  "カーソルの位置の英単語の意味を調べる" t nil)
(setq sdic-face-color "#3333FF")
(setq sdic-default-coding-system 'utf-8)
;; Dictionary (English => Japanese)
(setq sdic-eiwa-dictionary-list
      '((sdicf-client "~/Dropbox/Dic/EIJIRO6/EIJI-128.sdic")))
;; Dictionary (Japanese => English)
(setq sdic-waei-dictionary-list
      '((sdicf-client "~/Dropbox/Dic/EIJIRO6/WAEI-128.sdic")))

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
	     ("-d" "en" "--encoding=utf-8") nil utf-8))))
;  (setq ispell-aspell-supports-utf8 t)
;  (setq ispell-encoding8-command t)

;;; Flyspell
;;(dolist
;;    (hook
;;     '(text-mode-hook change-log-mode-hook c++-mode-hook
;;		      latex-mode-hook org-mode-hook))
;;  (add-hook hook (lambda () (flyspell-mode 1))))
;;(add-hook 'c++-mode-hook
;;	  (lambda () (flyspell-prog-mode)))

;; Call zone as screen saver of Emacs (CAUTION: high-load)
;(run-with-idle-timer 600 t 'zone)

;; Count words (Toggle this mode: M-+)
;; http://taiyaki.org/elisp/word-count/src/word-count.el
(autoload 'word-count-mode "word-count" "Minor mode to count words." t nil)

(when (require 'gist nil t)
;;  (setq github-user "hoge")
  )

(when (require 'doxymacs nil t)
  (setq doxymacs-doxygen-style "JavaDoc")
  (define-key doxymacs-mode-map (kbd "C-c C-s") 'ff-find-other-file)
  (add-hook 'c-mode-common-hook 'doxymacs-mode)
  (add-hook 'font-lock-mode-hook
	    '(lambda () (interactive)
	       (when (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
		 (doxymacs-font-lock)))))

;; i-search with selected region
;; http://dev.ariel-networks.com/articles/emacs/part5/
(defadvice isearch-mode
  (around isearch-mode-default-string
	  (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))


;; The world clock 
;; see http://pastelwill.jp/wiki/doku.php?id=emacs
(require 'wclock nil t)

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
; Use expand-file-name!
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
