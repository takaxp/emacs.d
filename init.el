
;;;; Configurations for Emacs
;;;;                                       Takaaki ISHIKAWA  <takaxp@ieee.org>
;;;; Cite: http://www.mygooglest.com/fni/dot-emacs.html (GREAT!)

(eval-when-compile (require 'cl))

(defun autoload-if-found (functions file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (if (not (listp functions))
      (setq functions (list functions)))
  (and (locate-library file)
       (progn
         (dolist (function functions)
           (autoload function file docstring interactive type))
         t )))

(setq default-path "~/.emacs.d/")
(setq default-private-path "~/.emacs.d/")

(prefer-coding-system 'utf-8-unix)
(set-language-environment "Japanese")
(set-locale-environment "en_US.UTF-8") ; "ja_JP.UTF-8"
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)

(when (and (eq window-system 'ns)
           (or (= emacs-major-version 23) (= emacs-major-version 24)))
  (setq default-input-method "MacOSX")
  (mac-add-key-passed-to-system 'shift))

(when (eq window-system 'ns)
  (global-set-key (kbd "M-v") 'yank)
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super)
  (global-set-key [ns-drag-file] 'ns-find-file)) ; D&D for Emacs23
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Avoid adding a new line at the end of buffer
(setq next-line-add-newlines nil)

;; Limit the final word to a line break code (automatically correct)
(setq require-final-newline t)

(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

(global-auto-revert-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(global-set-key (kbd "C-M-t") 'beginning-of-buffer)
(global-set-key (kbd "C-M-b") 'end-of-buffer)
;; Backward page scrolling instead of M-v
(global-set-key (kbd "M-p") 'scroll-down)
;; Frontward page scrolling instead of C-v
(global-set-key (kbd "M-n") 'scroll-up)
;; Move cursor to a specific line
(global-set-key (kbd "C-c g") 'goto-line)

(global-set-key (kbd "C-M-p") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-M-n") '(lambda () (interactive) (other-window 1)))

;; Scroll window on a line-by-line basis
(setq scroll-conservatively 1000)
(setq scroll-step 1)
(setq scroll-margin 0) ; default=0

(when (autoload-if-found 'smooth-scrolling "smooth-scrolling" nil t)
  (setq smooth-scroll-margin 1))

;; Scroll window on a page-by-pabe basis with N line overlapping
(setq next-screen-context-lines 1)

(autoload-if-found '(scroll-one-up scroll-one-down) "smoothscroll" nil t)

(global-set-key (kbd "s-<up>") 'scroll-one-down)
(global-set-key (kbd "s-<down>") 'scroll-one-up)

(require 'point-undo nil t)

;; [point-undo.el] Move the cursor to the previous position
(global-set-key (kbd "<f7>") 'point-undo)
;; [point-undo.el] Redo of point-undo
(global-set-key (kbd "S-<f7>") 'point-redo)

(when (autoload-if-found
       '(cycle-buffer cycle-buffer-backward) "cycle-buffer" nil t)
  (eval-after-load "cycle-buffer"
    '(progn
       (setq cycle-buffer-allow-visible t)
       (setq cycle-buffer-show-length 12)
       (setq cycle-buffer-show-format '(" <(%s)>" . " %s")))))

(global-set-key (kbd "M-]") 'cycle-buffer)
(global-set-key (kbd "M-[") 'cycle-buffer-backward)

(cua-mode t)
(setq cua-enable-cua-keys nil)

(setq yank-excluded-properties t)

(add-hook 'before-save-hook 'time-stamp)
(eval-after-load "time-stamp"
  '(progn
     (setq time-stamp-start "Last Update: ")
     (setq time-stamp-format "%04y-%02m-%02d@%02H:%02M")
     (setq time-stamp-end "$")
     (setq time-stamp-line-limit 10))) ; def=8

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

(add-hook 'change-log-mode-hook
          '(lambda() (setq tab-width 4) (setq left-margin 4)))

(add-hook 'text-mode-hook
          '(lambda() 
             (setq tab-width 4)      
             (setq tab-stop-list
                   '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                       64 68 72 76 80))
             (setq indent-line-function 'tab-to-tab-stop)))

(setq auto-mode-alist
      (append '(("\\.h\\'" . c++-mode)) auto-mode-alist))

(setq auto-mode-alist
      (append '(("\\.html\\'" . html-helper-mode))
              auto-mode-alist))

(when (autoload-if-found 'info "info" nil t)
  (eval-after-load "info"
    '(progn
       (add-to-list 'Info-additional-directory-list
                    (expand-file-name "~/devel/mygit/org-ja/work/")))))

(defun org-info-ja (&optional node)
  "(Japanese) Read documentation for Org-mode in the info system.
   With optional NODE, go directly to that node."
  (interactive)
  (info (format "(org-ja)%s" (or node ""))))

;;; Use aspell for spell checking instead of ispell.
(when (and (autoload-if-found 'ispell-region "ispell" nil t)
           (executable-find "aspell"))
  (eval-after-load "ispell"
    '(progn
       (setq-default ispell-program-name "aspell")
       (when (eq window-system 'w32)
         (setq-default ispell-program-name
                       "C:/Program Files/Aspell/bin/aspell.exe"))
       ;;(setq ispell-grep-command "grep")
       ;; for English and Japanese mixed

       (add-to-list 'ispell-skip-region-alist '("[^\000-\377]"))
       (setq ispell-dictionarry "english")
       (setq ispell-personal-dictionary
                      (concat default-private-path ".aspell.en.pws"))

       ;; This will also avoid an IM-OFF issue for flyspell-mode.
       ;;  (setq ispell-aspell-supports-utf8 t)
       ;;  (setq ispell-encoding8-command t)
       (setq ispell-local-dictionary-alist
             '((nil "[a-zA-Z]" "[^a-zA-Z]" "'" t
                    ("-d" "en" "--encoding=utf-8") nil utf-8))))))

;; Spell checking within a specified region
(global-set-key (kbd "C-c 0") 'ispell-region)

(autoload 'latex-math-preview "latex-math-preview" nil t)

;(autoload 'po-mode "po-mode+" nil nil)
(autoload 'po-mode "po-mode" nil t)
(setq auto-mode-alist
      (cons '("\\.po[tx]?\\'\\|\\.po\\." . po-mode)
            auto-mode-alist))

(autoload 'word-count-mode "word-count" "Minor mode to count words." t)

(global-set-key (kbd "M-+") 'word-count-mode)

(when (autoload-if-found 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
  (setq auto-mode-alist
        (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
  ;; Disable auto line break
  (add-hook 'yatex-mode-hook
            '(lambda ()
               (setq auto-fill-function nil)))
  (eval-after-load "yatex"
    '(progn
       ;; 1=Shift JIS, 2=JIS, 3=EUC, 4=UTF-8
       (setq YaTeX-kanji-code 4))))

(autoload 'wclock "wclock" nil t)

(make-variable-buffer-local 'yas/trigger-key)
  (when (autoload-if-found
         '(yas/minor-mode yas/minor-mode-on yas/new-snippet) "yasnippet" nil t)
    (dolist (hook (list 'perl-mode-hook 'c-mode-common-hook))
      (add-hook hook 'yas/minor-mode-on))
    (add-hook 'emacs-lisp-mode-hook
              '(lambda () (unless (equal "*scratch*" (buffer-name))
                            (yas/minor-mode-on))))
    (add-hook 'org-mode-hook
              (lambda ()
                (yas/minor-mode-on)
                ;; org-cycle (<TAB>) との衝突を避ける
                (setq yas/trigger-key [tab])
                (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
                (define-key yas/keymap [tab] 'yas/next-field)))
    (eval-after-load "yasnippet"
      '(progn
         ;; org-cycle (<TAB>) との衝突を避ける
         (defun yas/org-very-safe-expand ()
           (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
         (setq yas/root-directory
               '("~/Dropbox/emacs.d/yas-dict"
                 "~/Dropbox/emacs.d/yasnippet/snippets"))
;         (yas/initialize)
         (mapc 'yas/load-directory yas/root-directory))))

(when (autoload-if-found
       '(sdic-describe-word sdic-describe-word-at-point)
       "sdic" nil t)
  (eval-after-load "sdic"
    '(progn
       (setq sdic-face-color "#3333FF")
       (setq sdic-default-coding-system 'utf-8)
       ;; Dictionary (English => Japanese)
       (setq sdic-eiwa-dictionary-list
             '((sdicf-client "~/Dropbox/Dic/EIJIRO6/EIJI-128.sdic")))
       ;; Dictionary (Japanese => English)
       (setq sdic-waei-dictionary-list
             '((sdicf-client "~/Dropbox/Dic/EIJIRO6/WAEI-128.sdic"))))))

;; カーソルの位置の英単語の意味を調べる
(global-set-key (kbd "C-M-w") 'sdic-describe-word-at-point)
;; ミニバッファに英単語を入れて英辞郎を使う
(global-set-key (kbd "C-c w") 'sdic-describe-word)

(when (autoload-if-found 'toggle-cacoo-minor-mode "cacoo" nil t)
  (global-set-key (kbd "M--") 'toggle-cacoo-minor-mode)
  (eval-after-load "cacoo"
    '(progn
       (require 'cacoo-plugins))))

(require 'mode-name-abbrev nil t)

(defvar my-narrow-display " N")
(setq mode-line-modes
      (mapcar (lambda (entry)
                (if (and (stringp entry)
                         (string= entry "%n"))
                    '(:eval (if (and (= 1 (point-min))
                                 (= (1+ (buffer-size)) (point-max))) ""
                          my-narrow-display)) entry))
              mode-line-modes))

(when (autoload-if-found 'echo-area-bell "echo-area-bell" nil t)
        (setq visible-bell t)
        (setq ring-bell-function 'echo-area-bell))

;; Show scroll bar or not
(set-scroll-bar-mode nil) ; 'right

;; Disable to show the tool bar.
(tool-bar-mode 0)

;; Disable to show the splash window at startup
(setq inhibit-startup-screen t)

;; Show line number in the mode line.
(line-number-mode t)

;; Show function name in the mode line.
(which-function-mode t)

(when (require 'mic-paren nil t)
      (paren-activate)
      (setq paren-sexp-mode nil)
      (set-face-foreground 'paren-face-match "#FFFFFF")
      ;; Deep blue: #6666CC, orange: #FFCC66
      (set-face-background 'paren-face-match "66CC66"))

(when (and (autoload-if-found 'migemo-init "migemo" nil t)
           (executable-find "cmigemo"))
  (add-hook 'isearch-mode-hook 'migemo-init)
  (eval-after-load "migemo"
    '(progn
       (setq completion-ignore-case t) ;; case-independent
       (setq migemo-command "cmigemo")
       (setq migemo-options '("-q" "--emacs" "-i" "\a"))
       (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
       (setq migemo-user-dictionary nil)
       (setq migemo-regex-dictionary nil)
       (setq migemo-use-pattern-alist t)
       (setq migemo-use-frequent-pattern-alist t)
       (setq migemo-pattern-alist-length 1024)
       (setq migemo-coding-system 'utf-8-unix))))

(when (autoload-if-found
       '(anything-other-buffer anything-complete
         anything-M-x anything-c-moccur-occur-by-moccur)
       "anything-startup" nil t)
  
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
    (defun my-anything-spotlight ()
      "Spotlight search with anything.el"
      (interactive)
      (anything-other-buffer
       '(anything-c-source-mac-spotlight)
       " *anything-spotlight*")))
  
  (eval-after-load "anything-startup"
    '(progn
       
       (require 'anything-c-moccur nil t)
       ;;  (setq moccur-split-word t)
       ;;  (setq anything-c-locate-options `("locate" "-w"))
       
       ;; M-x install-elisp-from-emacswiki recentf-ext.el
       ;; http://www.emacswiki.org/cgi-bin/wiki/download/recentf-ext.el
       ;;  (autoload-if-found 'recentf-ext "recentf-ext" nil t)
       (require 'recentf-ext nil t)
       
       (when (require 'migemo nil t)
         (setq moccur-use-migemo t))
       
       ;; M-x anything-grep-by-name
       (setq anything-grep-alist
             '(("Org-files" ("egrep -Hin %s *.org" "~/Dropbox/org/"))
               (".emacs.d" ("egrep -Hin %s *.el" "~/.emacs.d/"))
               ("ChangeLog" ("egrep -Hin %s ChangeLog" "~/"))))
       ;; ("Spotlight" ("mdfind %s -onlyin ~/Dropbox/Documents/Library/" ""))))
       
       (setq anything-candidate-number-limit 50) ; 50
       (setq anything-input-idle-delay 0.1)      ; 0.1
       (setq anything-idle-delay 0.5)            ; 0.5
       (setq anything-quick-update nil))))       ; nil

;; Show ibuffer powered by anything
(global-set-key (kbd "M-x") 'anything-M-x)
(global-set-key (kbd "C-c o") 'anything-c-moccur-occur-by-moccur)
(global-set-key (kbd "C-M-r") 'my-anything)
(global-set-key (kbd "C-M-s") 'my-anything-spotlight)
(global-set-key (kbd "C-x C-b") 'my-anything-buffer)

;;  (require 'bongo)
  (when (autoload-if-found 'bongo "bongo-mplayer" nil t)
    (defun init-bongo ()
      (interactive)
      (bongo)
      (find-file "~/Desktop/next/Tidy/hoge.bongo-playlist"))
    (eval-after-load "bongo-mplayer"
      '(progn
         ;; Volume control
;         (require volume.el nil t)
         (setq bongo-mplayer-extra-arguments '("-volume" "1"))
         ;; Avoid error when editing bongo buffers
         (setq yank-excluded-properties nil)
         ;; Use mplayer
         (setq bongo-enabled-backends '(mplayer)))))

(autoload-if-found 'org-mode "org-player" nil t)

(require 'google-maps nil t)
(require 'org-location-google-maps nil t)

(require 'google-weather nil t)
(when (require 'org-google-weather nil t)
 '(org-google-weather-use-google-icons t))

(setq undo-outer-limit nil)

;; Backup the buffer whenever the buffer is saved
(global-set-key (kbd "C-x C-s") '(lambda () (interactive) (save-buffer 16)))

(savehist-mode 1)

(setq history-length 1000)

(add-hook 'after-init-hook 'recentf-mode)
(eval-after-load "recentf"
  '(progn
     (setq recentf-max-saved-items 2000)
     (setq recentf-save-file (expand-file-name "~/Dropbox/.recentf"))
     (setq recentf-auto-cleanup 'never) ; default = 'mode
     (run-with-idle-timer 300 t 'recentf-save-list)
     (run-with-idle-timer 600 t 'recentf-cleanup)
     (setq recentf-exclude
           '("^/tmp\\.*" "^/private\\.*" "^/var/folders\\.*" "/TAGS$"))))

(when (require 'auto-save-buffers nil t)
  (run-with-idle-timer 1.0 t 'auto-save-buffers))

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

(when (autoload-if-found 'session-initialize "session" nil t)
  (add-hook 'after-init-hook 'session-initialize)
  (eval-after-load "session"
    '(progn
       (add-to-list 'session-globals-exclude 'org-mark-ring)
       ;; Change save point of session.el
       (setq session-save-file (expand-file-name "~/Dropbox/.session"))
       (setq session-initialize '(de-saveplace session keys menus places)
             session-globals-include '((kill-ring 100)
                                       (session-file-alist 100 t)
                                       (file-name-history 200)
                                       search-ring regexp-search-ring))
       (setq session-undo-check -1))))
       
;; FIXME
;;  (setq session-set-file-name-exclude-regexp
;;        "^/private/\\.\\*"))
;;          "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|^/private\\.*\\|^/var/folders\\.*"))

(global-set-key (kbd "C-;") 'comment-dwim) ;; M-; is the defualt
(global-set-key (kbd "C-c c") 'compile)

(when (autoload-if-found 'gist "gist" nil t)
;;  (setq github-user "hoge")
  )

(when (autoload-if-found 'doxymacs-mode "doxymacs" nil t)
  (add-hook 'c-mode-common-hook 'doxymacs-mode)
  (eval-after-load "doxymacs"
    '(progn
       (setq doxymacs-doxygen-style "JavaDoc")
       (add-hook 'font-lock-mode-hook
                 '(lambda () (interactive)
                    (when (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
                      (doxymacs-font-lock))))
       (define-key doxymacs-mode-map (kbd "C-c C-s") 'ff-find-other-file))))

(when (and (eq window-system 'ns) (= emacs-major-version 23))
  (autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
  (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
  (autoload 'matlab-shell "matlab" "Interactive Matlab mode." t))

(when (require 'auto-complete-config nil t)
          (ac-config-default)
          (defun ac-org-mode-setup ()
;;            (message " >> ac-org-mode-setup")
            (setq ac-sources '(
  ;;                             ac-source-abbrev ; Emacs の略語
        ;;;                         ac-source-css-property ; heavy
                               ac-source-dictionary ; 辞書
                               ac-source-features
                               ac-source-filename
                               ac-source-files-in-current-dir
                               ac-source-functions
  ;;                             ac-source-gtags
  ;;                             ac-source-imenu 
  ;;                             ac-source-semantic
  ;;                             ac-source-symbols 
  ;;                             ac-source-variables
  ;;                             ac-source-yasnippet
                               )))
          (add-hook 'org-mode-hook 'ac-org-mode-setup)
          (defun ac-default-setup ()
;;            (message " >> ac-default-setup")
            (setq ac-sources '(ac-source-abbrev
                               ac-source-dictionary
                               ac-source-words-in-same-mode-buffers)))
      ;      (setq ac-sources (append '(ac-source-abbrev
      ;                                 ac-source-dictionary
      ;                                 ac-source-words-in-same-mode-buffers)
      ;                               ac-sources)))
          (dolist (hook (list 'perl-mode-hook 'objc-mode-hook))
            (add-hook hook 'ac-default-setup))
          ;; *scratch* バッファでは無効化
          (add-hook 'lisp-mode-hook
                    '(lambda () (unless (equal "*scratch*" (buffer-name))
                                  (ac-default-setup))))
          ;; ac-modes にあるメジャーモードで有効にする
          ;; lisp, c, c++, java, perl, cperl, python, makefile, sh, fortran, f90
          (global-auto-complete-mode t)
          ;; 追加のメジャーモードを設定
          (add-to-list 'ac-modes 'objc-mode)
          (add-to-list 'ac-modes 'org-mode)
          ;; 辞書
          (add-to-list 'ac-dictionary-directories (concat default-path "ac-dict"))
          ;; n文字以上で補完表示する（"<s TAB" の場合 yasnippet が呼ばれる）
          (setq ac-auto-start 4)
          ;; n秒後にメニューを表示
          (setq ac-auto-show-menu 1.0)
          ;; ツールチップの表示
          (setq ac-use-quick-help t)
          (setq ac-quick-help-delay 2.0)
          (setq ac-quick-help-height 10)
          ;; C-n/C-p でメニューをたどる
          (setq ac-use-menu-map t)
          ;; TAB で補完（org-mode でも効くようにする）
          (define-key ac-completing-map [tab] 'ac-complete)
          ;; RET での補完を禁止
          (define-key ac-completing-map "\r" nil)
          ;; 補完メニューの表示精度を高める
          (setq popup-use-optimized-column-computation nil))
          ;;(setq ac-candidate-max 10)

(when (require 'auto-complete-clang nil t)
          ;; ac-cc-mode-setup のオーバーライド
          (defun ac-cc-mode-setup ()
;;            (message " >> Auto-complete-clang")
            ;;      (setq ac-clang-prefix-header "stdafx.pch")
;;            (setq ac-auto-start 0)
            (setq ac-clang-prefix-header "~/.emacs.d/stdafx.pch")
            (setq ac-clang-flags '("-w" "-ferror-limit" "1"
                                   "-fcxx-exceptions"))
            (setq ac-sources '(ac-source-clang
                               ac-source-yasnippet
                               ac-source-gtags))
            )
          (add-hook 'c-mode-common-hook 'ac-cc-mode-setup))

(when (and (eq window-system 'ns) (= emacs-major-version 23))
  (autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
  (autoload 'hideshowvis-minor-mode "hideshowvis"
    "Will indicate regions foldable with hideshow in the fringe." 'interactive)
  (add-hook 'emacs-lisp-mode-hook
            '(lambda () (unless (equal "*scratch*" (buffer-name))
                          (hideshowvis-enable))))
  (dolist (hook (list 'perl-mode-hook 'c-mode-common-hook))
    (add-hook hook 'hideshowvis-enable)))

(global-set-key (kbd "C-(") 'hs-hide-block)
(global-set-key (kbd "C-)") 'hs-show-block)

(when (autoload-if-found 'org-mode "org" "Org Mode" t)
    (eval-after-load "org"
      '(progn
         
;         (require 'org-install)
         (require 'org-extension nil t)
         (require 'org-habit)
         (require 'org-mobile)
         
         (setq auto-mode-alist
               (cons (cons "\\.org$" 'org-mode) auto-mode-alist))
         (push '("\\.txt\\'" . org-mode) auto-mode-alist)
         
         ;; Set checksum program path for windows
         (when (eq window-system 'w32)
           (setq org-mobile-checksum-binary "~/Dropbox/do/cksum.exe"))
         
         ;; org ファイルの集中管理
         (setq org-directory "~/Dropbox/org/")
         
         ;; Set default table export format
         (setq org-table-export-default-format "orgtbl-to-csv")
         
         ;; Toggle inline images display at startup
         (setq org-startup-with-inline-images t)
         
         ;; dvipng
         (setq org-export-with-LaTeX-fragments t)
         
         ;; orgバッファ内の全ての動的ブロックを保存直前に変更する
         (add-hook 'before-save-hook 'org-update-all-dblocks)
         
         ;; アーカイブファイルの名称を指定
         (setq org-archive-location "%s_archive::")
         
         ;; タイムスタンプによるログ収集設定
         (setq org-log-done t) ; t ではなく，'(done), '(state) を指定できる
         
         ;; ログをドロアーに入れる
         (setq org-log-into-drawer t)
         
         ;; タイマーの音
         ;; (lsetq org-clock-sound "");
  )))

(when (autoload-if-found 'org-mode "org" "Org Mode" t)
  (eval-after-load "org"
    '(progn
       ;; ~/Dropbox/Public は第三者に探索される可能性があるので要注意
       ;; default = ~/org.ics
       ;; C-c C-e i org-export-icalendar-this-file
       ;; C-c C-e I org-export-icalendar-all-agenda-files
       ;; C-c C-e c org-export-icalendar-all-combine-agenda-files
       ;;       (setq org-combined-agenda-icalendar-file "~/Dropbox/Public/orgAgenda.ics")
       
       ;; iCal の説明文
       (setq org-icalendar-combined-description "OrgModeのスケジュール出力")
       ;; カレンダーに適切なタイムゾーンを設定する（google 用には nil が必要）
       (setq org-icalendar-timezone "Asia/Tokyo")
  ;;; エクスポート後に，AppleScript で新しいカレンダーをリロードさせる
                                        ;(add-hook 'org-after-save-iCalendar-file-hook
                                        ;         (lambda ()
                                        ;           (shell-command
                                        ;            "osascript -e 'tell application \"iCal\" to reload calendars'")))
       (add-hook 'org-after-save-iCalendar-file-hook
                 (lambda ()
                   (let ((result
                          (shell-command
                           "scp -o ConnectTimeout=5 ~/Dropbox/org/org-ical.ics orz:~/public_html/ical")))
                     (if (eq result 0) (message "Uploading ... [DONE]")
                       (message "Uploading ... [MISS]")))))
       
       ;; DONE になった TODO はアジェンダから除外する
       (setq org-icalendar-include-todo t)
       ;; （通常は，<>--<> で区間付き予定をつくる．非改行入力で日付がNoteに入らない）
       (setq org-icalendar-use-scheduled '(event-if-todo))
  ;;; DL 付きで終日予定にする：締め切り日（スタンプで時間を指定しないこと）
       ;;         (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo))
       (setq org-icalendar-use-deadline '(event-if-todo))
       )))

(when (autoload-if-found 'org-mode "org" "Org Mode" t)
  (eval-after-load "org"
    '(progn
       (setq org-use-speed-commands t)
       (setq org-speed-commands-user
             (quote (("n" . show-next-org)
                     ("t" . show-today-org))))
       (defun show-next-org () (show-org-buffer "next.org"))
       (defun show-today-org () (show-org-buffer "today.org"))
)))

(when (autoload-if-found 'org-mode "org" "Org Mode" t) 
  (eval-after-load "org"
    '(progn
       (add-to-list 'org-structure-template-alist
                    '("C" "#+BEGIN_COMMENT\n?\n#+END_COMMENT" ""))
       (add-to-list 'org-structure-template-alist
                    '("S" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "<src lang=\"emacs-lisp\">\n\n</src>")))))

(when (autoload-if-found 'org-mode "org" "Org Mode" t)
  (eval-after-load "org"
    '(progn

       (add-to-list 'org-modules 'org-timer)
       (setq org-timer-default-timer 25)
;; (add-hook 'org-clock-in-hook
;;        '(lamda ()
;;                (if (not org-timer-current-timer)
;;                    (org-timer-set-timer '(16)))))

       (setq growl-pomodoro-default-task-name "doing the task")
       (setq growl-pomodoro-task-name 'growl-pomodoro-default-task-name)

       (defun set-growl-pomodoro-task-name ()
         (interactive "P")
         (setq growl-pomodoro-task-name
               (read-from-minibuffer "Task Name: " growl-pomodoro-default-task-name)))
       (add-hook 'org-timer-set-hook 'set-growl-pomodoro-task-name)
       
       (defun growl-pomodoro-timer ()
         (interactive)
         (shell-command-to-string
          (concat "growlnotify -s -a Emacs -t \"++ Pomodoro ++\" -m \""
                  "The end of " growl-pomodoro-task-name "!\""))
         (shell-command-to-string
          ;   (concat "say The end of " growl-pomodoro-task-name)
          (concat "say -v Kyoko " growl-pomodoro-task-name)
          ))
       (add-hook 'org-timer-done-hook 'growl-pomodoro-timer)
)))

(when (autoload-if-found 'org-mode "org" "Org Mode" t)
  (eval-after-load "org"
    '(progn

       ;; Font lock を使う
       (global-font-lock-mode 1)
       (add-hook 'org-mode-hook 'turn-on-font-lock)
       ;; ウィンドウの端で折り返す（想定と逆の振る舞い．どこかにバグがある）
       (setq org-startup-truncated nil)
       ;; サブツリー以下の * を略式表示する
       (setq org-hide-leading-stars t)
       ;; Color setting for TODO keywords
       ;; Color for priorities
       ;; (setq org-priority-faces
       ;;       '(("?A" :foreground "#E01B4C" :background "#FFFFFF" :weight bold)
       ;;      ("?B" :foreground "#1739BF" :background "#FFFFFF" :weight bold)
       ;;      ("?C" :foreground "#575757" :background "#FFFFFF" :weight bold)))
       ;; Color setting for Tags

       (setq org-todo-keyword-faces
             '(("CHECK"   :foreground "#CC00FF")
               ("SLEEP"   :foreground "#3366CC")
               ("WAIT"    :foreground "#CC00FF")
               ("DRAFT"   :foreground "#CC3333")
               ("REV1"    :foreground "#3366CC")
               ("REV2"    :foreground "#FFFFFF" :background "#3366CC")
               ("NOTICE"  :foreground "#FFFFFF" :background "#FF0000")
               ("FOCUS"   :foreground "#FF0000" :background "#FFCC66")
               ("TIME"    :foreground "#FF9900")))

       (setq org-tag-faces
             '(
;;; (:foreground "#0000FF" :bold t)     ; default. do NOT put this bottom
               ("Achievement" :foreground "#66CC66")
               ("Report"      :foreground "#66CC66")
               ("Background"  :foreground "#66CC99")
               ("Chore"       :foreground "#6699CC")
               ("Domestic"    :foreground "#6666CC")
               ("Ongoing"     :foreground "#CC6666") ; for non scheduled / reminder
               ("Repeat"      :foreground "#CC9999") ; for interval tasks
               ("Mag"         :foreground "#9966CC")
               ("buy"         :foreground "#9966CC")
               ("note"        :foreground "#6633CC")
               ("Implements"  :foreground "#CC9999" :weight bold)
               ("Coding"      :foreground "#CC9999")
               ("Editing"     :foreground "#CC9999" :weight bold)
               ("Duty"        :foreground "#CC9999" :weight bold)
               ("Survey"      :foreground "#CC9999" :weight bold)
               ("Home"        :foreground "#CC9999" :weight bold)
               ("Open"        :foreground "#CC9999" :weight bold)
               ("Test"        :foreground "#FF0000" :weight bold)
               ("DEBUG"       :foreground "#FFFFFF" :background "#9966CC")
               ("EVENT"       :foreground "#FFFFFF" :background "#9966CC")
               ("Thinking"    :foreground "#FFFFFF" :background "#96A9FF")
               ("Schedule"    :foreground "#FFFFFF" :background "#FF7D7D")
               ("OUTPUT"      :foreground "#FFFFFF" :background "#66CC99");;#5BDF8D
               ("Log"         :foreground "#008500")))
       )))

(when (autoload-if-found 'org-mode "org" "Org Mode" t)
  (eval-after-load "org"
    '(progn

       (setq org-todo-keywords
             '((sequence "TODO(t)" "FOCUS(f)" "WAIT(w)" "|" "DONE(d)")
               (sequence "DRAFT(D)" "REV1(r)" "REV2(R)" "|" "PUBLISH(p)")
               (sequence "NOTICE(N)" "SLEEP(s)" "|" "note(n)")
               (sequence "TIME(T)" "CHECK(C)" "|" "PASS(P)")
               ))

       ;; Global counting of TODO items
       (setq org-hierarchical-todo-statistics nil)
       ;; Global counting of checked TODO items
       (setq org-hierarchical-checkbox-statistics nil)

;;; block-update-time
       (defun org-dblock-write:block-update-time (params)
         (let ((fmt (or (plist-get params :format) "%Y-%m-%d")))
           (insert "" (format-time-string fmt (current-time)))))

;;; すべてのチェックボックスの cookies を更新する
       (defun do-org-update-statistics-cookies ()
         (interactive)
         (org-update-statistics-cookies 'all))

       )))

(when (autoload-if-found 'org-agenda "org" "Org Mode" t)
  (eval-after-load "org"
    '(progn

       ;; Set the view span as day in an agenda view, the default is week
       (setq org-agenda-span 'day)
       ;; アジェンダに警告を表示する期間
       (setq org-deadline-warning-days 7)
       ;; アジェンダビューでFOLLOWを設定
       ;; (setq org-agenda-start-with-follow-mode t)
       ;; Customized Time Grid
       (setq org-agenda-time-grid
             '((daily today require-timed)
               "----------------"
               (800 1000 1200 1400 1600 1800 2000 2200 2400 2600)))
       ;; アジェンダ作成対象（指定しないとagendaが生成されない）
       ;; ここを間違うと，MobileOrg, iCal export もうまくいかない
       (setq org-agenda-files
             '("~/Dropbox/org/org-ical.org" "~/Dropbox/org/next.org"
               "~/Dropbox/org/today.org"
               "~/Dropbox/org/work.org" "~/Dropbox/org/research.org")))))

(when (autoload-if-found 'org-capture "org-capture" "Org Mode" t)
  (eval-after-load "org"
    '(progn

       ;; 2010-06-13 の形式では，タグとして認識されない
       (defun get-current-date-tags () (format-time-string "%Y%m%d"))
       (setq org-default-notes-file (concat org-directory "next.org"))
       (defvar org-capture-words-notes-file (concat org-directory "words.org"))
       (defvar org-capture-notes-file (concat org-directory "note.org"))
       (defvar org-capture-research-file (concat org-directory "research.org"))
       (defvar org-capture-buffer-file (concat org-directory "buffer.org"))
       (defvar org-capture-today-file (concat org-directory "today.org"))
       (defvar org-capture-ical-file (concat org-directory "org-ical.org"))

       ;; see org.pdf:p73
       (setq org-capture-templates
             `(("t" "TODO 項目を INBOX に貼り付ける" entry
                (file+headline nil "INBOX") "** TODO %?\n\t")
               ("c" "同期カレンダーにエントリー" entry
                (file+headline ,org-capture-ical-file "Schedule")
                "** TODO %?\n\t") 
               ("d" "DRAFT 項目を INBOX に貼り付ける" entry
                (file+headline nil "INBOX") "** DRAFT %?\n\t")
               ("l" "本日のチェックリスト" entry
                (file+headline ,org-capture-today-file "Today")
                "** FOCUS 本日のチェックリスト %T\n（起床時間の記録）[[http://www.hayaoki-seikatsu.com/users/takaxp/][早起き日記]] \n（朝食）\n  - [ ] %?\n（昼食）\n（帰宅／夕食）\n----\n（研究速報）\n  - [ ] \n")
               ("i" "アイディアを書き込む" entry (file+headline nil "INBOX")
                "** %?\n  - \n\t%U")
               ("b" "Bug タグ付きの TODO 項目を貼り付ける" entry
                (file+headline nil "INBOX")
                "** TODO %? :bug:\n %i\n %a %t")
               ("w" ,(concat "英単語を " org-capture-words-notes-file
                             " に書き込む") entry
                             (file+headline ,org-capture-words-notes-file "WORDS")
                             "** %? :%(get-current-date-tags):\n「」\n  - ")
               ("g" ,(concat "英語ノートを " org-capture-words-notes-file
                             " に書き込む")
                entry (file+headline ,org-capture-words-notes-file "GRAMMER")
                "** %? :%(get-current-date-tags):\n\n%U")
               ("T" "時間付きエントリー" entry (file+headline nil "INBOX")
                "** %? %T--\n")
               ("n" "ノートとしてINBOXに貼り付ける" entry
                (file+headline nil "INBOX")
                "** %? :note:\n\t%U")
               ("D" "「ドラッカー365の金言」をノートする" entry
                (file+headline ,org-capture-notes-file "The Daily Drucker")
                "** 「%?」\nDrucker) \n  - \n  - \nACTION POINT:\n  - \nQUESTION:\n  - \n")
               ("r" ,(concat "研究ノートを " org-capture-research-file
                             " に書き込む")
                entry (file+headline ,org-capture-research-file "Survey")
                "** %? :note:\n# \n  - \n\t%U")
               ("`" ,(concat "ノートをバッファ " org-capture-buffer-file
                             " に書き込む")
                entry (file+headline ,org-capture-buffer-file "Buffer")
                "** %(get-random-string 16) %U\n\n%?\n\n----")))
       )))

(when (autoload-if-found 'org-mode "org" "Org Mode" t)
    (eval-after-load "org"
      '(progn
;; アラーム表示を有効にする
(appt-activate 1)
;; window を フレーム内に表示する
(setq appt-display-format 'window)
;; window を継続表示する時間[s]
(setq appt-display-duration 3)
;; ビープ音の有無
(setq appt-audible t)
;; 何分前から警告表示を開始するか[m]
(setq appt-message-warning-time 3)
;; モードラインにアラームを表示する
(setq appt-display-mode-line t)
;; org-agenda の内容をアラームに登録する
;; (org-agenda-to-appt t '((headline "TODO")))
;; 保存時にアラームを登録
;;(add-hook 'org-mode-hook
;;        (lambda() (add-hook 'before-save-hook
;;                            'org-agenda-to-appt t '((headline "TODO")))))
)))

(when (autoload-if-found 'org-refile "org" "Org Mode" t)
  (eval-after-load "org"
    '(progn
       (setq org-refile-targets
             (quote (("org-ical.org" :level . 1)
                     ("next.org" :level . 1)
                     ("sleep.org" :level . 1))))
       )))

(when (autoload-if-found 'org-refile "org" "Org Mode" t)
  (eval-after-load "org"
    '(progn
       (setq org-confirm-babel-evaluate nil)
       (setq org-src-fontify-natively t)
       (setq org-src-tab-acts-natively t))))

(when (autoload-if-found 'org-mode "org" "Org Mode" t)
    (eval-after-load "org"
      '(progn
;(setq org-mobile-files '("~/Dropbox/org/next.org" "1.org" "2.org"))
(setq org-mobile-files '("~/Dropbox/org/next.org"))
;(setq org-mobile-force-id-on-agenda-items nil)

;; Set a file to capture data from iOS devices
(setq org-mobile-inbox-for-pull (concat org-directory "captured.org"))

; Upload location stored org files (index.org will be created)
(setq org-mobile-directory "~/Dropbox/MobileOrg/")

;;; Menu to push or pull org files using MobileOrg
(defun org-mobile-sync ()
  (interactive)
  (let
      (org-mobile-sync-type
       (read-from-minibuffer "How do you sync the org files? (pull or push) "))
    (message "%s" org-mobile-sync-type)
    (cond
     ((string= "pull" org-mobile-sync-type)(org-mobile-pull))
     ((string= "push" org-mobile-sync-type)(org-mobile-push)))))

;; 起動時にモバイルで環境で編集したファイルを読み込む
(message "%s" "MobileOrg sync ... [pull]")
(org-mobile-pull) ;; need org-mode
)))

;; Org-tree-slide
  (when (autoload-if-found 'org-tree-slide-mode "org-tree-slide" nil t)
    ;; <f8>/<f9>/<f10>/<f11> are assigned to control org-tree-slide
    (global-set-key (kbd "<f8>") 'org-tree-slide-mode)
    (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
    (eval-after-load "org-tree-slide"
      '(progn
         (define-key org-tree-slide-mode-map (kbd "<f9>")
           'org-tree-slide-move-previous-tree)
         (define-key org-tree-slide-mode-map (kbd "<f10>")
           'org-tree-slide-move-next-tree)
         (define-key org-tree-slide-mode-map (kbd "<f11>")
           'org-tree-slide-content)
         ;; reset the default setting
         (define-key org-tree-slide-mode-map (kbd "<left>")  'backward-char)
         (define-key org-tree-slide-mode-map (kbd "<right>") 'forward-char)
         (org-tree-slide-narrowing-control-profile)
;         (org-tree-slide-presentation-profile)
         (setq org-tree-slide-skip-outline-level 4)
         (setq org-tree-slide-skip-done nil))))

(when (autoload-if-found 'org-mode "org" nil t)
  (eval-after-load "org"
    '(progn
       (require 'org-fstree nil t))))

(when (autoload-if-found 'cfw:open-org-calendar "calfw-org"
                           "Rich calendar for org-mode" t)
    (eval-after-load "calfw-org"
      '(progn
  
         ;; icalendar との連結
         (setq cfw:org-icalendars '("~/Dropbox/org/org-ical.org"))
  
         ;; org で使う表にフェイスを統一
         (setq cfw:fchar-junction ?+
               cfw:fchar-vertical-line ?|
               cfw:fchar-horizontal-line ?-
               cfw:fchar-left-junction ?|
               cfw:fchar-right-junction ?|
               cfw:fchar-top-junction ?+
               cfw:fchar-top-left-corner ?|
               cfw:fchar-top-right-corner ?| ))))
  
;         (add-hook 'window-configuration-change-hook 'cfw:resize-calendar)
         ;; (defun cfw:resize-calendar ()
         ;;   (interactive)
         ;;   (when (eq major-mode 'cfw:calendar-mode)
         ;;     (cfw:refresh-calendar-buffer nil)
         ;;     (message "Calendar resized.")))
  
         ;; (defun open-calfw-agenda-org ()
         ;;   (interactive)
         ;;   (cfw:open-org-calendar))
  
         ;; (setq org-agenda-custom-commands
         ;;       '(("w" todo "FOCUS")
         ;;         ("G" open-calfw-agenda-org "Graphical display in calfw"))))))

(when (require 'org-export-generic nil t)
  (org-set-generic-type
   "textile"
   '(:file-suffix ".textile"
                  :key-binding ?T
                  :title-format    "Title: %s\n\n"
                                        ;   :date-format     "Date: %s\n"
                  :date-export nil
                  :toc-export      nil
                  :author-export   nil
                  :tags-export     nil
                  :drawers-export  nil
                  :date-export     t
                  :timestamps-export  t
                  :priorities-export  nil
                  :todo-keywords-export t
                  :body-line-fixed-format "\t%s\n"
                                        ;:body-list-prefix "\n"
                  :body-list-format "* %s"
                  :body-list-suffix "\n"
                  :body-bullet-list-prefix ("* " "** " "*** " "**** " "***** ")
                  :body-number-list-format "# %s"
                  :body-number-list-suffix "\n"
                  :header-prefix ("" "" "### " "#### " "##### " "###### ")
                  :body-section-header-prefix ("h1. " "h2. " "h3. " "h4. " "h5. " "h6. ")
                  :body-section-header-format "%s"
                  :body-section-header-suffix ("\n\n")
                  :body-header-section-numbers nil
                  :body-header-section-number-format "%s) "
                  :body-line-format "%s\n"
                  :body-newline-paragraph "\n"
                  :bold-format "*%s*"
                  :italic-format "_%s_"
                  :underline-format "+%s+"
                  :strikethrough-format "-%s-"
                  :verbatim-format "`%s`"
                  :code-format "@%s@"
                  :body-line-wrap   75
                  :blockquote-start "\n<pre>\n"
                  :blockquote-end "\n</pre>\n"
))

  (org-set-generic-type
   "markdown" 
   '(:file-suffix     ".markdown"
                      :key-binding     ?M
                      :title-format    "Title: %s\n"
                      :date-format     "Date: %s\n"
                      :toc-export      nil
                      :author-export   t
                      :tags-export     nil
                      :drawers-export  nil
                      :date-export     t
                      :timestamps-export  t
                      :priorities-export  nil
                      :todo-keywords-export t
                      :body-line-fixed-format "\t%s\n"
                                        ;:body-list-prefix "\n"
                      :body-list-format "- %s"
                      :body-list-suffix "\n"
                      :header-prefix ("" "" "### " "#### " "##### " "###### ")
                      :body-section-header-prefix ("" "" "### " "#### " "##### " "###### ")
                      :body-section-header-format "%s\n"
                      :body-section-header-suffix (?= ?- "")
                      :body-header-section-numbers nil
                      :body-header-section-number-format "%s) "
                      :body-line-format "%s\n"
                      :body-newline-paragraph "\n"
                      :bold-format "**%s**"
                      :italic-format "_%s_"
                      :verbatim-format "`%s`"
                      :code-format "`%s`"
                      :body-line-wrap   75
                      )))

(setq org-export-odt-styles-file
      (concat (getenv "HOME") "/Dropbox/org/style.odt"))
(setq org-export-odt-preferred-output-format "pdf")
(setq org-export-odt-convert-processes
      '(("LibreOffice"
         "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")
        ("unoconv" "unoconv -f %f -o %d %i")))

(when (autoload-if-found 'org-mode "org" "Org Mode" t)
  (eval-after-load "org"
    '(progn
       (push '("[rR][eE][aA][dD][mM][eE]" . org-mode) auto-mode-alist))))

(when (autoload-if-found 'org-mode "org" "Org Mode" t)
  (eval-after-load "org"
    '(progn
       (setq alarm-table "~/Dropbox/org/today.org")
       (run-at-time "00:00" nil 'set-alarms-from-file alarm-table))))

(when (autoload-if-found 'org-mode "org" "Org Mode" t)
  (global-set-key (kbd "C-M-o") '(lambda () (interactive)
                                   (show-org-buffer "next.org")))
  (global-set-key (kbd "C-M-c") '(lambda () (interactive)
                                   (show-org-buffer "org-ical.org")))
  (global-set-key (kbd "C-M-9") '(lambda () (interactive)
                                   (show-org-buffer "buffer.org")))
  (global-set-key (kbd "C-M-0") '(lambda () (interactive)
                                   (show-org-buffer "today.org")))
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c r") 'org-capture)

  (require 'org-icalendar)
  (defun my-org-export-icalendar ()
    (interactive)
    (org-export-icalendar nil "~/Dropbox/org/org-ical.org"))
  
  (eval-after-load "org"
    '(progn
       
       ;; (org-transpose-element) が割り当てられているので取り返す．
       (org-defkey org-mode-map "\C-\M-t" 'beginning-of-buffer)
       
       ;;(define-key org-mode-map (kbd "C-c 1")
       ;;  'org-export-icalendar-combine-agenda-files)
       (define-key org-mode-map (kbd "C-c 1") 'my-org-export-icalendar)
       (define-key org-mode-map (kbd "C-c 2") 'do-org-update-statistics-cookies)
       (define-key org-mode-map (kbd "C-c m") 'org-mobile-sync)
       (define-key org-mode-map (kbd "<f5>") 'org-narrow-to-subtree)
       (define-key org-mode-map (kbd "S-<f5>") 'widen)
       )))

(set-face-foreground 'font-lock-regexp-grouping-backslash "#66CC99")
(set-face-foreground 'font-lock-regexp-grouping-construct "#9966CC")

;; Color of the current line
;; Cite: http://murakan.cocolog-nifty.com/blog/2009/01/emacs-tips-1d45.html
;; see also http://www.emacswiki.org/cgi-bin/emacs/highlight-current-line.el
(global-hl-line-mode t)
(set-face-background 'hl-line "#DEEDFF")

;; Cursor (see also takaxp-mac.el)
;(add-to-list 'default-frame-alist '(cursor-type . (hbar . 5)))
;(add-to-list 'default-frame-alist '(cursor-type . bar))

;(add-hook 'window-configuration-change-hook
(defun update-cursor-color ()
  (interactive)
  (if current-input-method (set-cursor-color "#91C3FF")
    (set-cursor-color "#AAAAAA")))
(update-cursor-color)
(run-with-idle-timer 10 t 'update-cursor-color)

(add-hook 'input-method-activate-hook
          (lambda () (set-cursor-color "#91C3FF")))
(add-hook 'input-method-inactivate-hook
          (lambda () (set-cursor-color "#AAAAAA")))

(when (and (eq window-system 'ns) (= emacs-major-version 23))
  ;; when IME is ON
  (mac-set-input-method-parameter
   "com.google.inputmethod.Japanese.base" 'title "G"))

(when (and (eq window-system 'ns) (= emacs-major-version 24))
  ;; when IME is ON
  (mac-set-input-method-parameter
   "com.google.inputmethod.Japanese.base" 'title "グ"))

;; Disable cursor blink
(blink-cursor-mode -1)

(defun my-ja-font-setter (spec)
    (set-fontset-font nil 'japanese-jisx0208 spec)
    (set-fontset-font nil 'katakana-jisx0201 spec)
    (set-fontset-font nil 'japanese-jisx0212 spec)
    (set-fontset-font nil '(#x0080 . #x024F) spec)
    (set-fontset-font nil '(#x0370 . #x03FF) spec)
    (set-fontset-font nil 'mule-unicode-0100-24ff spec))
  
  (defun my-ascii-font-setter (spec)
    (set-fontset-font nil 'ascii spec))      
  
  (cond
   ;; CocoaEmacs
   ((eq window-system 'ns)
    (when (or (= emacs-major-version 23) (= emacs-major-version 24))
      (let
          ;; 1) Monaco, Hiragino/Migu 2M : font-size=12, -apple-hiragino=1.2
          ;; 2) Inconsolata, Migu 2M     : font-size=14, 
          ;; 3) Inconsolata, Hiragino    : font-size=14, -apple-hiragino=1.0

;; Fonts

          ((font-size 12)
;          ((font-size 28) ; for mirroring presentation (1440x900)
;           (ascii-font "Inconsolata")
           (ascii-font "Monaco")
           (ja-font "Migu 2M"))
        ;; (ja-font "Hiragino Maru Gothic Pro")) 
        (my-ascii-font-setter (font-spec :family ascii-font :size font-size))
        (my-ja-font-setter (font-spec :family ja-font :size font-size)))
      
      ;; Fix ratio provided by set-face-attribute for fonts display
      (setq face-font-rescale-alist
            '(("^-apple-hiragino.*" . 1.0) ; 1.2
              (".*Migu.*" . 1.2)
              (".*Inconsolata.*" 1.0)
              (".*osaka-bold.*" . 1.0)     ; 1.2
              (".*osaka-medium.*" . 1.0)   ; 1.0
              (".*courier-bold-.*-mac-roman" . 1.0) ; 0.9
              ;; (".*monaco cy-bold-.*-mac-cyrillic" . 1.0)
              ;; (".*monaco-bold-.*-mac-roman" . 1.0) ; 0.9
              ("-cdac$" . 1.0)))           ; 1.3
      
      ;; Space between lines
      (set-default 'line-spacing 1)
      ;; Anti aliasing with Quartz 2D
      (setq mac-allow-anti-aliasing t)))
   
   ((eq window-system 'w32) ; windows7
    (let
        ((font-size 14)
         (font-height 100)
         (ascii-font "Inconsolata")
         ;; (ja-font "Meiryo UI"))
         (ja-font "メイリオ"))
      (my-ja-font-setter
       (font-spec :family ja-font :size font-size :height font-height))
      (my-ascii-font-setter (font-spec :family ascii-font :size font-size)))
    (setq face-font-rescale-alist '((".*Inconsolata.*" . 1.0))) ; 0.9
    (set-default 'line-spacing 1))
   
   (window-system ; for SuSE Linux 12.1
    (let
        ((font-size 14)
         (font-height 100)
         (ascii-font "Inconsolata")
         ;; (ja-font "MigMix 1M")
         (ja-font "Migu 1M"))
      (my-ja-font-setter
       (font-spec :family ja-font :size font-size :height font-height))
      (my-ascii-font-setter (font-spec :family ascii-font :size font-size)))
    (setq face-font-rescale-alist '((".*MigMix.*" . 2.0)
                                    (".*Inconsolata.*" . 1.0))) ; 0.9
    (set-default 'line-spacing 1)))

(autoload-if-found 'rainbow-mode "rainbow-mode" nil t)

;; To avoid an error setting up the frame width (only for Emacs23)
;(set-frame-width (selected-frame) 81)
;(set-frame-width (selected-frame) 80)

;; Default window position to show a Emacs frame
;; Dynabook UX: top=0, left=0, width=80, height=32
(cond
 ((eq window-system 'ns) ; for Macintosh
  (setq initial-frame-alist
        (append
         '((top . 22)  ; Y-pos from (0,0) the height of menu bar is 22pix.
           (left . 0)  ; X-pos from (0,0) ; 420 is the center for MBP
           ;; 26 is the setting for Butler's Docklet
           ;; 837 is the setting for right side for MBP
           (width . 80) ; Width  : character count
           (height . 35); Height : character count
           (alpha . (100 75))
           (vertical-scroll-bars . nil)
           ) initial-frame-alist)))

 ((eq window-system 'x) ; for Linux
  (setq initial-frame-alist
        (append
         '((vertical-scroll-bars . nil)
           (top . 0)
           (left . 0)
           (width . 80)
           (height . 38)
           ) initial-frame-alist)))

 (t                     ; for Windows
  (setq initial-frame-alist
        (append
         '((vertical-scroll-bars . nil)         
           (top . 0)
           (left . 0)
           (width . 80)
           (height . 26)
           ) initial-frame-alist))))

;; Apply the initial setting to default
(setq default-frame-alist initial-frame-alist)

(when (autoload-if-found 'e2wm:dp-two "e2wm" nil t)
  (eval-after-load "e2wm"
    '(progn
       (setq e2wm:c-two-recipe
             '(- (:lower-size 10)
                 (| left right)
                 sub))
       (setq e2wm:c-two-winfo
             '((:name left )
               (:name right )
               (:name sub :default-hide t)))
       ;; left, prev
       (setq e2wm:c-two-right-default 'left)

       ;; To avoid rebooting issue when using desktop.el and recentf.el
       (add-hook 'kill-emacs-hook 'e2wm:stop-management))))

(when (autoload-if-found 
         '(change-frame-width-single
           change-frame-width-double
           frame-ctr-open-height-ring
           move-frame-with-user-specify move-frame-left move-frame-to-center
           move-frame-right move-frame-to-edge-top move-frame-to-edge-bottom)
         "frame-ctr" nil t)
    (eval-after-load "frame-ctr"
      '(progn
         (frame-ctr-make-height-ring '(56 20 40))))) ; for Emacs24
;         (frame-ctr-make-height-ring '(60 68 20 40))))) ; for Emacs23

;; Move the frame to somewhere (default: 0,0)
(global-set-key (kbd "M-0") 'move-frame-with-user-specify)
;; Move the frame to left side of the current position (require 'frame-cmds)
(global-set-key (kbd "M-1") '(lambda () (interactive) (move-frame-left 200)))
;; Move the frame to the center of the window display (require 'frame-ctr)
(global-set-key (kbd "M-2") 'move-frame-to-center)
;; Move the frame to right side of the current position (require 'frame-cmds)
(global-set-key (kbd "M-3") '(lambda () (interactive) (move-frame-right 200)))
;; Set the frame width single size
;;  C-u C-x - => e2wm OFF, single size width and double height, move center
(global-set-key (kbd "C-x -") 'change-frame-width-single)
;; Set the frame width double size
;;  C-u C-x = => e2wm ON, double size width and height, move to the center
(global-set-key (kbd "C-x =") 'change-frame-width-double)
;; Move the current frame to the top of the window display
(global-set-key (kbd "<f1>") 'move-frame-to-edge-top)
;; Move the current frame to the bottom of the window display
(global-set-key (kbd "S-<f1>") 'move-frame-to-edge-bottom)
;; Cycle heights
(global-set-key (kbd "<f2>") 'frame-ctr-open-height-ring)

(when (require 'popwin nil t)
  (setq special-display-function 'popwin:special-display-popup-window)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:special-display-config
        (append
         '(("*Completions*" :height 10 :position bottom :noselect t)
           ("CAPTURE-next.org" :height 10 :position bottom :noselect t)
           ("CAPTURE-org-ical.org" :height 10 :position bottom :noselect t)
           ("*Org-todo*"    :height 10 :position bottom)
           ("*Calendar*"    :height 10 :position bottom)
           ("*wclock*"      :height 10 :position bottom)
           ("*Org Agenda*"  :height 10 :position bottom)
           ("*Agenda Commands*"  :height 10 :position bottom)
           ("*Org Select*"  :height 10 :position bottom)
           ("*Occur*"       :height 10 :position bottom)
           ("*sdic*"        :height 10 :position top)
           ("*anything*"    :height 10 :position bottom)
           ("*anything M-x*" :height 10 :position bottom)
           ("*anything complete*"    :height 10 :position bottom)
           ("*my-anything*" :height 10 :position bottom)
           ("*my-anything-buffer*"    :height 10 :position bottom)
           ;;            ("*cfw-calendar*" :height 40 :position top)
           ("*eshell*"      :height 10 :position bottom))
         popwin:special-display-config)))

(when (autoload-if-found 'pomodoro:start "pomodoro" nil t)
  (eval-after-load "pomodoro"
    '(progn
       
       ;; 作業時間終了後に開くファイルを指定しない
       (setq pomodoro:file nil)

       ;; ●だけで表現する（残り時間表示なし）
       (setq pomodoro:mode-line-time-display nil)
       
       ;; 長い休憩に入るまでにポモドーロする回数
       (setq pomodoro:iteration-for-long-rest 8)

       ;; 作業時間関連
       (setq pomodoro:work-time 25      ; 作業時間
             pomodoro:rest-time 5       ; 休憩時間
             pomodoro:long-rest-time 60 ; 長い休憩時間
             pomodoro:max-iteration 16) ; ポモドーロする回数

       ;; タイマーの表示をノーマルフェイスにする
       (set-face-bold-p 'pomodoro:timer-face nil)

       ;; 作業中（赤），休憩中（青），長い休憩中（緑）にする
       (set-face-foreground 'pomodoro:work-face "#F53838")
       (set-face-foreground 'pomodoro:rest-face "#3869FA")
       (set-face-foreground 'pomodoro:long-rest-face "#00B800")

       ;; Mac ユーザ向け．Kyokoさんに指示してもらう
       (add-hook 'pomodoro:finish-work-hook
                 (lambda ()
                   (shell-command-to-string
                    (concat "say -v Kyoko "
                            (number-to-string (floor pomodoro:rest-time))
                            "分間，休憩しろ"))))
       
       (add-hook 'pomodoro:finish-rest-hook
                 (lambda ()
                   (shell-command-to-string
                    (concat "say -v Kyoko "
                            (number-to-string (floor pomodoro:work-time))
                            "分間，作業しろ"))))
       
       (add-hook 'pomodoro:long-rest-hook
                 (lambda ()
                   (shell-command-to-string
                    (concat "say -v Kyoko これから"
                            (number-to-string (floor pomodoro:long-rest-time))
                            "分間の休憩です")))))))

(global-set-key (kbd "<f12>") 'takaxp:open-file-ring)
(global-set-key (kbd "M-4") 'my-window-resizer)
(autoload-if-found
 '(eval-org-buffer
   kyoko-mad-mode-toggle org2dokuwiki-cp-kill-ring
   open-current-directory set-alarms-from-file takaxp:open-file-ring
   show-org-buffer get-random-string init-auto-install
   add-itemize-head insert-formatted-current-date
   insert-formatted-current-time insert-formatted-signature
   export-timeline-business export-timeline-private reload-ical-export
   browse-url-chrome count-words-buffer do-test-applescript
   my-window-resizer)
 "utility" nil t)

(provide 'init)
