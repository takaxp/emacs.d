;;;; Configurations for Emacs
;;;;                                       Takaaki ISHIKAWA  <takaxp@ieee.org>
;;;; Cite: http://www.mygooglest.com/fni/dot-emacs.html (GREAT!)

(eval-when-compile (require 'cl-lib))

(defun eval-after-autoload-if-found
    (functions file &optional docstring interactive type after-body)
  "Set up autoload and eval-after-load for FUNCTIONS iff. FILE has found."
  (let ((enabled t)
        (package nil))
    (when (boundp 'disabled-packages)
      (dolist (package disabled-packages)
        (when (and (stringp (car package)) (equal file (car package)))
          (when (cdr package)
            (setq enabled nil)))))
    (when enabled ;; if disabled then return nil
      (when (locate-library file)
        (mapc (lambda (func)
                (autoload func file docstring interactive type))
              (if (listp functions)
                  functions
                (list functions)))
        (when after-body
          (eval-after-load file `(progn ,@after-body)))
        t))))

(defun library-p (libraries)
  "Return t when every specified library can be located. "
  (let ((result t))
    (mapc (lambda (library)
            (unless (locate-library library)
              (message "--- [NOT FOUND] %s" library)
              (setq result nil)))
          (if (listp libraries)
              libraries
            (list libraries)))
    result))

(add-hook 'after-init-hook
          (lambda ()
            (message "--- Emacs booting time: %.0f [msec]"
                     (* 1000
                        (float-time (time-subtract
                                     after-init-time
                                     before-init-time))))))

(setq confirm-kill-emacs 'y-or-n-p)

(prefer-coding-system 'utf-8-unix)
(set-language-environment "Japanese")
(set-locale-environment "en_US.UTF-8") ; "ja_JP.UTF-8"
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)

(when (fboundp 'mac-add-key-passed-to-system)
  (setq default-input-method "MacOSX")
  (mac-add-key-passed-to-system 'shift))

(when (eval-after-autoload-if-found
       '(my:ag) "ag" nil t nil
       '((setq ag-highlight-search t)
         (setq ag-reuse-buffers t) ;; nil=別ウィンドウが開く
         (setq ag-reuse-window t)  ;; nil=結果を選択時に別ウィンドウに結果:を出す
         ;; q でウィンドウを抜ける
         (define-key ag-mode-map (kbd "q") 'delete-window)

         (defun my:ag ()
           (interactive)
           (call-interactively 'ag)
           (switch-to-buffer-other-frame "*ag search*"))))

  (global-set-key (kbd "C-M-f") 'my:ag))

(when (eq window-system 'ns)
  (global-set-key (kbd "M-v") 'yank)
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super)
  (global-set-key [ns-drag-file] 'ns-find-file) ; D&D for Emacs23
  (setq ns-pop-up-frames nil)) ; D&D for Emacs23
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Avoid adding a new line at the end of buffer
(setq next-line-add-newlines nil)

;; Limit the final word to a line break code (automatically correct)
(setq require-final-newline t)

(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

(global-auto-revert-mode 1)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq mouse-drag-copy-region t)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)
;; (add-hook 'org-mode-hook
;;           '(lambda()
;;              (setq indent-line-function 'insert-tab)))

(when (eval-after-autoload-if-found
       '(aggressive-indent-mode) "aggressive-indent")

  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'perl-mode-hook 'aggressive-indent-mode)
  (add-hook 'c-mode-common-hook 'aggressive-indent-mode)
  (add-hook 'python-mode-hook 'aggressive-indent-mode)
  (add-hook 'nxml-mode-hook 'aggressive-indent-mode)
  (add-hook 'web-mode-hook 'aggressive-indent-mode))

(setq vc-follow-symlinks t)

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

;; Scroll window on a page-by-pabe basis with N line overlapping
(setq next-screen-context-lines 1)

(eval-after-autoload-if-found
 '(cycle-buffer cycle-buffer-backward) "cycle-buffer" nil t nil
    '((setq cycle-buffer-allow-visible t)
      (setq cycle-buffer-show-length 12)
      (setq cycle-buffer-show-format '(" <(%s)>" . " %s"))))

(global-set-key (kbd "M-]") 'cycle-buffer)
(global-set-key (kbd "M-[") 'cycle-buffer-backward)

(when (eval-after-autoload-if-found
       '(bm-toggle my:bm-next bm-buffer-save bm-buffer-restore
                   bm-buffer-save-all bm-repository-save
                   bm-repository-load) "bm" nil t nil
                   '((setq-default bm-buffer-persistence t) ;; t
                     (setq bm-repository-file "~/Dropbox/emacs.d/.bookmark")
                     ;; autoload との組み合わせでは無意味
                     ;;（after-init-hook を利用せよ）
                     ;; (setq bm-restore-repository-on-load t)
                     (setq bm-persistent-face 'bm-face)
                     (defun my:bm-toggle ()
                       "bm-toggle with updating history"
                       (interactive)
                       (bm-toggle)
                       (bm-save))

                     (defun my:bm-next ()
                       "bm-next with org-mode"
                       (interactive)
                       (bm-next)
                       (when (and (equal major-mode 'org-mode)
                                  (not (org-before-first-heading-p)))
                         (widen)
                         (org-overview)
                         (org-reveal)
                         (org-cycle-hide-drawers 'all)
                         (org-show-entry)
                         (show-children)
                         (org-show-siblings)))))

  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hook 'bm-buffer-restore)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook 'bm-save)

  (global-set-key (kbd "<f10>") 'my:bm-toggle)
  (global-set-key (kbd "<C-f10>") 'my:bm-next))

(when (eval-after-autoload-if-found
       '(centered-cursor-mode) "centered-cursor-mode")

  (add-hook 'isearch-mode-hook
            '(lambda () (interactive) (centered-cursor-mode 1)))
  (add-hook 'isearch-mode-end-hook
            '(lambda () (interactive) (centered-cursor-mode -1))))

(setq yank-excluded-properties t)

;; org-tre-slide が有効ならタイムスタンプを更新しない
;; （Undo範囲が限定されてしまうため）

;; #+UPDATE 用
(when (eval-after-autoload-if-found
       '(update-stamp) "update-stamp" nil t nil
       '((setq update-stamp-start "UPDATE:[ \t]*")
         (setq update-stamp-format "%02H:%02M:%02S")
         (setq update-stamp-end "$")
         (setq update-stamp-line-limit 10))) ; def=8

  (add-hook 'before-save-hook
            '(lambda ()
               (if (boundp 'org-tree-slide-mode)
                   (unless org-tree-slide-mode) (update-stamp))
               (update-stamp))))

;; #+DATE 用
(when (eval-after-autoload-if-found
       '(time-stamp) "time-stamp" nil t nil
       '((setq time-stamp-start "DATE:[ \t]*")
         (setq time-stamp-format "%04y-%02m-%02d")
         (setq time-stamp-end "$")
         (setq time-stamp-line-limit 10) ; def=8
         ))

  (add-hook 'before-save-hook
            '(lambda ()
               (if (boundp 'org-tree-slide-mode)
                   (unless org-tree-slide-mode) (time-stamp))
               (time-stamp))))

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
          '(lambda()
             (setq tab-width 4)
             (setq left-margin 4)))

(push '("\\.h$" . c++-mode) auto-mode-alist)

(when (eval-after-autoload-if-found
       '(csharp-mode) "csharp-mode" "Major mode for editing C# mode.")

  (push '("\\.cs$" . csharp-mode) auto-mode-alist))

(eval-after-autoload-if-found
 '(info org-info-ja) "info" nil t nil
 '((add-to-list 'Info-additional-directory-list
                (expand-file-name "~/devel/mygit/org-ja/work/"))
   (defun org-info-ja (&optional node)
     "(Japanese) Read documentation for Org-mode in the info system.
    With optional NODE, go directly to that node."
     (interactive)
     (info (format "(org-ja)%s" (or node ""))))))

(when (eval-after-autoload-if-found
       '(R-mode R) "ess-site" "Emacs Speaks Statistics mode")
  (push '("\\.[rR]$" . R-mode) auto-mode-alist))

(add-hook 'nxml-mode-hook
          '(lambda ()
             (define-key nxml-mode-map "\r" 'newline-and-indent)
             (setq auto-fill-mode -1)
             (setq nxml-slash-auto-complete-flag t)
             (setq tab-width 1)
             (setq nxml-child-indent 1)
             (setq indent-tabs-mode t)
             (setq nxml-attribute-indent 0)))

(when (eval-after-autoload-if-found
       '(yaml-mode) "yaml-mode")
  (push '("\\.yml$" . yaml-mode) auto-mode-alist))

(when (eval-after-autoload-if-found
       '(json-mode) "json-mode")
  (push '("\\.json$" . json-mode) auto-mode-alist))

(when (eval-after-autoload-if-found '(js2-mode) "js2-mode")
  (push '("\\.js$" . js2-mode) auto-mode-alist))

(when (eval-after-autoload-if-found '(ac-js2-mode) "ac-js2")
  (add-hook 'js2-mode-hook 'ac-js2-mode))

(if (executable-find "tern")
    (when (eval-after-autoload-if-found
           '(tern-mode) "tern" nil t nil
           '((tern-mode 1)
             (when (require 'tern-auto-complete nil t)
               (tern-ac-setup))))
      (add-hook 'js2-mode-hook 'tern-mode))

  (message "--- tern is NOT installed in this system."))

(when (eval-after-autoload-if-found
       '(csv-mode) "csv-mode")
  (push '("\\.csv$" . csv-mode) auto-mode-alist))

(eval-after-autoload-if-found '(ascii-on ascii-off) "ascii")

;;; Use aspell for spell checking instead of ispell.
(when (executable-find "aspell")
  (eval-after-autoload-if-found
   '(ispell-region) "ispell" nil t nil
   '((setq-default ispell-program-name "aspell")
     (when (eq window-system 'w32)
       (setq-default ispell-program-name
                     "C:/Program Files/Aspell/bin/aspell.exe"))
     ;;(setq ispell-grep-command "grep")
     ;; for English and Japanese mixed

     (add-to-list 'ispell-skip-region-alist '("[^\000-\377]"))
     (setq ispell-dictionarry "english")
     (setq ispell-personal-dictionary "~/.emacs.d/.aspell.en.pws")

     ;; This will also avoid an IM-OFF issue for flyspell-mode.
     ;;  (setq ispell-aspell-supports-utf8 t)
     ;;  (setq ispell-encoding8-command t)
     (setq ispell-local-dictionary-alist
           '((nil "[a-zA-Z]" "[^a-zA-Z]" "'" t
                  ("-d" "en" "--encoding=utf-8") nil utf-8))))))

;; Spell checking within a specified region
(global-set-key (kbd "C-c f 5") 'ispell-region)

(when (eval-after-autoload-if-found       
       '(latex-math-preview-expression
         latex-math-preview-insert-symbol
         latex-math-preview-save-image-file
         latex-math-preview-beamer-frame)
       "latex-math-preview" nil t nil
       '((define-key latex-math-preview-expression-mode-map (kbd "<f7>")
           'latex-math-preview-delete-buffer)))

  (global-set-key (kbd "<f7>") 'latex-math-preview-expression))

;;(autoload 'po-mode "po-mode+" nil nil)
;;(autoload 'po-mode "po-mode" nil t)
(when (eval-after-autoload-if-found
       '(po-mode) "po-mode")
  (push '("\\.po[tx]?\\'\\|\\.po\\$" . po-mode) auto-mode-alist))

(global-set-key (kbd "M-=") 'count-words)

(when (eval-after-autoload-if-found
       '(yatex-mode) "yatex" "Yet Another LaTeX mode" t nil
       '((setq YaTeX-kanji-code 4))) ;; 1=Shift JIS, 2=JIS, 3=EUC, 4=UTF-8
  (push '("\\.tex$" . yatex-mode) auto-mode-alist)
  ;; Disable auto line break
  (add-hook 'yatex-mode-hook
            '(lambda () (setq auto-fill-function nil))))

(eval-after-autoload-if-found 'wclock "wclock")

(when (eval-after-autoload-if-found
       '(yas-global-mode yas-minor-mode)
       "yasnippet" nil t nil
       '((setq yas-verbosity 2)
         (setq yas-snippet-dirs
               (list "~/Dropbox/emacs.d/yas-dict"
                     'yas-installed-snippets-dir)) ;; for Cask
         (custom-set-variables '(yas-trigger-key [tab]))
         (yas-global-mode 1)
         ))

  (dolist (hook
           (list 'perl-mode-hook
                 'c-mode-common-hook 'js2-mode-hook 'org-mode-hook
                 'python-mode-hook))
    (add-hook hook 'yas-minor-mode)))

(when (eval-after-autoload-if-found
       '(osx-dictionary-search-pointer osx-dictionary-search-input)
       "osx-dictionary" nil t nil
       '((setq osx-dictionary-dictionary-choice "英辞郎 第七版")))
  (global-set-key (kbd "C-M-w") 'osx-dictionary-search-pointer))

(when (eval-after-autoload-if-found
       '(web-mode) "web-mode" "web-mode" t nil
       '((defun my:web-indent-fold ()
           (interactive)
           (web-mode-fold-or-unfold)
           (web-mode-buffer-indent)
           (indent-for-tab-command))

         ;; indent
         (setq web-mode-markup-indent-offset 1)

         ;; 色の設定
         (custom-set-faces
          ;; custom-set-faces was added by Custom.
          ;; If you edit it by hand, you could mess it up, so be careful.
          ;; Your init file should contain only one such instance.
          ;; If there is more than one, they won't work right.
          '(default ((t (:background "#FFFFFF" :foreground "#202020"))))
          '(web-mode-comment-face ((t (:foreground "#D9333F"))))
          '(web-mode-css-at-rule-face ((t (:foreground "#FF7F00"))))
          '(web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))
          '(web-mode-css-rule-face ((t (:foreground "#A0D8EF"))))
          '(web-mode-doctype-face ((t (:foreground "#82AE46"))))
          '(web-mode-html-attr-name-face ((t (:foreground "#C97586"))))
          '(web-mode-html-attr-value-face ((t (:foreground "#82AE46"))))
          '(web-mode-html-tag-face ((t (:foreground "##4682ae" :weight bold))))
          '(web-mode-server-comment-face ((t (:foreground "#D9333F")))))
         (define-key web-mode-map (kbd "<tab>") 'my:web-indent-fold)))


  ;; web-mode で開くファイルの拡張子を指定
  (setq auto-mode-alist
        (append '(("\\.phtml\\'" . web-mode)
                  ("\\.tpl\\.php\\'" . web-mode)
                  ("\\.jsp\\'" . web-mode)
                  ("\\.as[cp]x\\'" . web-mode)
                  ("\\.erb\\'" . web-mode)
                  ("\\.mustache\\'" . web-mode)
                  ("\\.djhtml\\'" . web-mode)
                  ("\\.html?\\'" . web-mode))
                auto-mode-alist)))

(when (eval-after-autoload-if-found
       '(emmet-mode) "emmet-mode" nil t nil
       '((setq emmet-indentation 2)
         (setq emmet-move-cursor-between-quotes t)))
  (push '("\\.xml\\'" . nxml-mode) auto-mode-alist)
  (push '("\\.rdf\\'" . nxml-mode) auto-mode-alist)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'nxml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

(eval-after-autoload-if-found
 '(describe-number describe-number-at-point) "describe-number")

(when (require 'diminish nil t)
  (with-eval-after-load "isearch" (diminish 'isearch-mode))
  (with-eval-after-load "smooth-scroll" (diminish 'smooth-scroll-mode))
  (with-eval-after-load "whitespace" (diminish 'global-whitespace-mode))
  (with-eval-after-load "centered-cursor-mode"
    (diminish 'centered-cursor-mode))
  (with-eval-after-load "volatile-highlights"
    (diminish 'volatile-highlights-mode))
  (with-eval-after-load "aggressive-indent"
    (diminish 'aggressive-indent-mode " Ai"))
  (with-eval-after-load "emmet-mode" (diminish 'emmet-mode " e"))
  (with-eval-after-load "abbrev" (diminish 'abbrev-mode " a"))
  (with-eval-after-load "yasnippet" (diminish 'yas-minor-mode " y"))
  (with-eval-after-load "doxymacs" (diminish 'doxymacs-mode " d"))
  (with-eval-after-load "rainbow-mode" (diminish 'rainbow-mode))
  (with-eval-after-load "guide-key" (diminish 'guide-key-mode))
  (with-eval-after-load "org-autolist" (diminish 'org-autolist-mode))
;;;  (with-eval-after-load "helm" (diminish 'helm-mode " H"))
  )

(add-hook 'c-mode-hook '(lambda () (setq mode-name "C")))
(add-hook 'js2-mode-hook '(lambda () (setq mode-name "JS")))
(add-hook 'c++-mode-hook '(lambda () (setq mode-name "C++")))
(add-hook 'csharp-mode-hook '(lambda () (setq mode-name "C#")))
(add-hook 'prog-mode-hook '(lambda () (setq mode-name "S")))
(add-hook 'emacs-lisp-mode-hook '(lambda () (setq mode-name "el")))
(add-hook 'python-mode-hook '(lambda () (setq mode-name "py")))
(add-hook 'perl-mode-hook '(lambda () (setq mode-name "pl")))
(add-hook 'web-mode-hook '(lambda () (setq mode-name "W")))
(add-hook 'lisp-interaction-mode-hook '(lambda () (setq mode-name "Lisp")))

(defvar my:narrow-display " N")
(setq mode-line-modes
      (mapcar
       (lambda (entry)
         (if (and (stringp entry)
                  (string= entry "%n"))
             '(:eval (if (and (= 1 (point-min))
                              (= (1+ (buffer-size)) (point-max))) ""
                       my:narrow-display)) entry))
       mode-line-modes))

(with-eval-after-load "vc-hooks"
  (setcdr (assq 'vc-mode mode-line-format)
          '((:eval (replace-regexp-in-string "^ Git" " " vc-mode)))))

(set-face-attribute 'mode-line nil :overline "#203e6f" :box nil)
(set-face-foreground 'mode-line "#203e6f")
(set-face-background 'mode-line "#b2cefb")
(set-face-attribute 'mode-line-inactive nil :overline "#94bbf9" :box nil)
(set-face-foreground 'mode-line-inactive  "#94bbf9")
(set-face-background 'mode-line-inactive "#d8e6fd")

(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq initial-buffer-choice t)

(when (require 'buffer nil t)
  (setq initial-scratch-message nil)
  (setq initial-major-mode 'buffer-mode)
  (set-face-attribute 'header-line nil :overline nil :underline "#203e6f")
  (set-face-foreground 'header-line "#203e6f")
  (set-face-background 'header-line "#d8e6fd")

  (defvar my:header-clock 
    (setq header-line-format
          (concat
           "  Buffers                                                       "
           (format-time-string "%Y-%m-%d (%a.)")))))

(global-set-key (kbd "C-M-s")
                '(lambda () (interactive) (switch-to-buffer "*scratch*")))

;; Show scroll bar or not
(set-scroll-bar-mode nil) ; 'right

;; Disable to show the tool bar.
(tool-bar-mode 0)

;; Disable to show the splash window at startup
(setq inhibit-startup-screen t)

;; Show line number in the mode line.
(line-number-mode t)

;; Show clock in in the mode line
(setq display-time-format "%H:%M")
(setq display-time-default-load-average nil)
(display-time-mode t)

(when (eval-after-autoload-if-found
       '(paren-activate) "mic-paren" nil t nil
       '((setq paren-sexp-mode nil)
         (set-face-foreground 'paren-face-match "#FFFFFF")
         ;; Deep blue: #6666CC, orange: #FFCC66
         (set-face-background 'paren-face-match "66CC66")))

  (paren-activate))

;; スペース
(defface my:face-b-1
  '((t (:background "gray" :bold t :underline "red")))
  nil :group 'font-lock-highlighting-faces)
;; タブだけの行
(defface my:face-b-2
  '((t (:background "orange" :bold t :underline "red")))
  nil :group 'font-lock-highlighting-faces)
;; 半角スペース
(defface my:face-b-3 '((t (:background "orange")))
  nil :group 'font-lock-highlighting-faces)

(defvar my:face-b-1 'my:face-b-1)
(defvar my:face-b-2 'my:face-b-2)
(defvar my:face-b-3 'my:face-b-3)
(defadvice font-lock-mode (before my:font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   ;; "[\t]+$" 行末のタブ
   '(("　" 0 my:face-b-1 append)
     ;;       ("[ ]+$" 0 my:face-b-3 append)
     ("[\t]+$" 0 my:face-b-2 append))))
(ad-enable-advice 'font-lock-mode 'before 'my:font-lock-mode)
(ad-activate 'font-lock-mode)

(when
    (eval-after-autoload-if-found
     '(migemo-init) "migemo" nil t nil
     '((setq completion-ignore-case t) ;; case-independent
       (setq migemo-command "cmigemo")
       (setq migemo-options '("-q" "--emacs" "-i" "\a"))
       (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
       (setq migemo-user-dictionary nil)
       (setq migemo-regex-dictionary nil)
       (setq migemo-use-pattern-alist t)
       (setq migemo-use-frequent-pattern-alist t)
       (setq migemo-pattern-alist-length 1024)
       (setq migemo-coding-system 'utf-8-unix)))

  (when (executable-find "cmigemo")
    (add-hook 'isearch-mode-hook 'migemo-init)))

(eval-after-autoload-if-found
 '(helm-M-x helm-locate helm-recentf helm-buffers-list
            helm-occur helm-swoop helm-flycheck)
 "helm-config" nil t nil
 '((helm-mode 1)
   (when (require 'diminish nil t)
     (diminish 'helm-mode " H"))
   (when (require 'helm-swoop nil t)
     (set-face-attribute
      'helm-swoop-target-line-face nil :background "#FFEDDC")
     (set-face-attribute
      'helm-swoop-target-word-face nil :background "#FF5443"))
   (when (require 'helm-google nil t)
     (setq helm-google-tld "co.jp"))
   ;;        (require 'recentf-ext nil t)
   ;; helm-find-files を呼ばせない
   (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
   ;; helm-mode-ag を呼ばせない
   (add-to-list 'helm-completing-read-handlers-alist '(ag . nil))
   ;; helm-mode-org-set-tags を呼ばせない
   (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . nil))
   (setq helm-display-source-at-screen-top nil)
            ;;;         (setq helm-display-header-line nil)
   ;; helm-autoresize-mode を有効にしつつ 30% に固定
   (helm-autoresize-mode 1)
   (setq helm-autoresize-max-height 30)
   (setq helm-autoresize-min-height 30)         
   (set-face-attribute 'helm-source-header nil
                       :height 1.0 :family "Verdana" :weight 'normal
                       :foreground "#666666" :background "#DADADA")
   (when (eq window-system 'ns)
     (setq helm-locate-command "mdfind -name %s %s")
     )

   ;; この修正が必要
   (when (require 'helm-migemo nil t)
     (defun helm-compile-source--candidates-in-buffer (source)
       (helm-aif (assoc 'candidates-in-buffer source)
           (append source
                   `((candidates
                      . ,(or (cdr it)
                             (lambda ()
                               ;; Do not use `source' because other plugins
                               ;; (such as helm-migemo) may change it
                               (helm-candidates-in-buffer
                                (helm-get-current-source)))))
                     (volatile) (match identity)))
         source)))
   ))

(when (eval-after-autoload-if-found
       '(helm-M-x helm-locate helm-recentf helm-buffers-list
                  helm-occur helm-swoop helm-flycheck)
       "helm-config")

  (global-set-key (kbd "C-c f f") 'helm-locate)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "M-s") 'helm-swoop)
  (global-set-key (kbd "C-M-r") 'helm-recentf)
  (global-set-key (kbd "C-c o") 'helm-occur)
  (global-set-key (kbd "M-x") 'helm-M-x))

(when (eval-after-autoload-if-found
       '(git-gutter-mode) "git-gutter-fringe" "Org Mode" t nil
       '((setq git-gutter:lighter "")
         ;; "!"
         (fringe-helper-define 'git-gutter-fr:modified nil
           "...XX..."
           "...XX..."
           "...XX..."
           "...XX..."
           "...XX..."
           "........"
           "...XX..."
           "...XX...")
         ;; "+"
         (fringe-helper-define 'git-gutter-fr:added nil
           "........"
           "...XX..."
           "...XX..."
           ".XXXXXX."
           ".XXXXXX."
           "...XX..."
           "...XX..."
           "........")
         ;; "-"
         (fringe-helper-define 'git-gutter-fr:deleted nil
           "........"
           "........"
           "........"
           ".XXXXXX."
           ".XXXXXX."
           "........"
           "........"
           "........")
         (setq git-gutter-fr:side 'left-fringe)
         (set-face-foreground 'git-gutter-fr:added    "#FF2600")
         (set-face-foreground 'git-gutter-fr:modified "orange")
         (set-face-foreground 'git-gutter-fr:deleted  "medium sea green")))

  (add-hook 'emacs-lisp-mode-hook 'git-gutter-mode)
  (add-hook 'lisp-mode-hook 'git-gutter-mode)
  (add-hook 'perl-mode-hook 'git-gutter-mode)
  (add-hook 'python-mode-hook 'git-gutter-mode)
  (add-hook 'c-mode-common-hook 'git-gutter-mode)
  (add-hook 'nxml-mode-hook 'git-gutter-mode)
  (add-hook 'web-mode-hook 'git-gutter-mode))

(with-eval-after-load "calendar"
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)

  (when (require 'japanese-holidays nil t)
    (setq calendar-holidays
          (append japanese-holidays
                  holiday-local-holidays holiday-other-holidays))
    (setq mark-holidays-in-calendar t)
    (setq japanese-holiday-weekend-marker
          '(holiday nil nil nil nil nil japanese-holiday-saturday))
    (setq japanese-holiday-weekend '(0 6))
    (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
    (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)))

(when (eval-after-autoload-if-found
       '(guide-key-mode) "guide-key" nil t nil
       '((setq guide-key/guide-key-sequence '("C-c f"))
         (setq guide-key/popup-window-position 'bottom)
         (setq guide-key/idle-delay 0.5)
         (setq guide-key/highlight-command-regexp
               '(("my:" . "red")
                 ("takaxp:" . "blue")))
         (guide-key-mode 1)))

  (with-eval-after-load "helm" (require 'guide-key nil t))
  (with-eval-after-load "org" (require 'guide-key nil t)))

(eval-after-autoload-if-found
 '(google-maps) "google-maps" nil t nil
 '((require 'org-location-google-maps nil t)))

(setq undo-outer-limit nil)

(when (eval-after-autoload-if-found
       '(my:undo-tree-visualize) "undo-tree" nil t nil
       '((global-undo-tree-mode)
         (setq undo-tree-mode-lighter nil) ;; モードライン領域を節約
         (defun my:undo-tree-visualizer-quit ()
           (interactive)
           (undo-tree-visualizer-quit)
           (delete-window)
           (set-frame-width (selected-frame) 80))
         (defun my:undo-tree-visualize ()
           (interactive)
           (set-frame-width (selected-frame) 163)
           (undo-tree-visualize))

         (define-key undo-tree-visualizer-mode-map (kbd "q")
           'my:undo-tree-visualizer-quit)
         ;; undo-tree-map にも必要
         (define-key undo-tree-map (kbd "C-x u")
           'my:undo-tree-visualize)))

  (global-set-key (kbd "C-x u") 'my:undo-tree-visualize))

;; *.~
(setq make-backup-files nil)
;; .#*
(setq auto-save-default nil)
;; auto-save-list
(setq auto-save-list-file-prefix nil)

(when (eval-after-autoload-if-found
       '(savehist-mode) "savehist" nil t nil
       '((setq savehist-file "~/Dropbox/emacs.d/.history")))
  (savehist-mode 1))

(setq history-length 2000)

(when (eval-after-autoload-if-found
       '(rencetf-mode recentf-save-list-without-msg) "recentf" nil t nil
       '((defun recentf-save-list-without-msg ()
           (interactive)
           (let ((message-log-max nil))
             (if (require 'shut-up nil t)
                 (shut-up (recentf-save-list))
               (recentf-save-list)))
           (message ""))
         (defun recentf-cleanup-without-msg ()
           (interactive)
           (let ((message-log-max nil))
             (if (require 'shut-up nil t)
                 (shut-up (recentf-cleanup))
               (recentf-cleanup)))
           (message ""))
         (add-hook 'focus-out-hook 'recentf-save-list-without-msg)
         (add-hook 'focus-out-hook 'recentf-cleanup-without-msg)

         (setq recentf-max-saved-items 2000)
         (setq recentf-save-file
               (expand-file-name "~/.emacs.d/recentf"))
         (setq recentf-auto-cleanup 'never)
         (setq recentf-exclude
               '(".recentf" "^/tmp\\.*"
                 "^/private\\.*" "^/var/folders\\.*" "/TAGS$"))))

  (add-hook 'after-init-hook 'recentf-mode))

(when (and (require 'auto-save-buffers nil t)
           (require 'shut-up nil t))
  (run-with-idle-timer
   1.6 t
   #'(lambda ()
       (cond ((equal major-mode 'undo-tree-visualizer-mode) nil)
             ((equal major-mode 'diff-mode) nil)
             ((string-match "Org Src" (buffer-name)) nil)
             (t (shut-up (auto-save-buffers)))))))

(when (require 'backup-each-save nil t)
  (setq backup-each-save-mirror-location "~/.emacs.d/backup")
  (setq backup-each-save-time-format "%y-%m-%d_%M:%S")
  (setq backup-each-save-size-limit 1048576)
  (add-hook 'after-save-hook
            '(lambda () (unless (equal (buffer-name) "recentf")
                          (backup-each-save)))))

(when (eval-after-autoload-if-found
       '(recursive-delete-backup-files delete-backup-files) "utility")

  ;; backup-each-save が作るファイルのうち条件にあうものを終了時に削除
  (add-hook 'kill-emacs-hook
            #'(lambda () (recursive-delete-backup-files 7))))

(when (eval-after-autoload-if-found
       'session-initialize "session" nil t nil
       '((add-to-list 'session-globals-exclude 'org-mark-ring)
         ;; Change save point of session.el
         (setq session-save-file
               (expand-file-name "~/Dropbox/emacs.d/.session"))
         (setq session-initialize '(de-saveplace session keys menus places)
               session-globals-include '((kill-ring 100)
                                         (session-file-alist 100 t)
                                         (file-name-history 200)
                                         search-ring regexp-search-ring))
         (setq session-undo-check -1)))

  (add-hook 'after-init-hook 'session-initialize))
;; FIXME
;;  (setq session-set-file-name-exclude-regexp
;;        "^/private/\\.\\*"))
;;          "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|^/private\\.*\\|^/var/folders\\.*"))

(global-set-key (kbd "C-;") 'comment-dwim) ;; M-; is the defualt
(global-set-key (kbd "C-c c") 'compile)

(eval-after-autoload-if-found '(gist) "gist")

(when (eval-after-autoload-if-found
       'doxymacs-mode "doxymacs" nil t nil
       '((setq doxymacs-doxygen-style "JavaDoc")
         (add-hook 'font-lock-mode-hook
                   '(lambda () (interactive)
                      (when (or (eq major-mode 'c-mode)
                                (eq major-mode 'c++-mode))
                        (doxymacs-font-lock))))
         (define-key doxymacs-mode-map (kbd "C-c C-s") 'ff-find-other-file)))

  (add-hook 'c-mode-common-hook 'doxymacs-mode))

(when (and (eq window-system 'ns) (> emacs-major-version 23))
  (when (eval-after-autoload-if-found
         '(matlab-mode matlab-shell) "matlab")
    (push '("\\.m$" . matlab-mode) auto-mode-alist)))

(when (eval-after-autoload-if-found
       '(flycheck-mode) "flycheck" nil t nil
       '(;;http://qiita.com/senda-akiha/items/cddb02cfdbc0c8c7bc2b
         (require 'helm-flycheck nil t)
         (when (require 'flycheck-pos-tip nil t)
           '(custom-set-variables
             '(flycheck-display-errors-function
               #'flycheck-pos-tip-error-messages)))))

  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-common-hook 'flycheck-mode)
  (add-hook 'perl-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode))

(when (eval-after-autoload-if-found
       '(ac-default-setup ac-org-mode-setup)
       "auto-complete" nil t nil
       '((require 'auto-complete-config nil t)
         (ac-config-default)
         ;; 追加のメジャーモードを設定
         (add-to-list 'ac-modes 'objc-mode)
         (add-to-list 'ac-modes 'org-mode)
         ;; ac-modes にあるメジャーモードで有効にする
         ;;lisp, c, c++, java, perl, cperl, python, makefile, sh, fortran, f90
         (global-auto-complete-mode t)
         ;; 辞書
         (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
         ;; history
         (setq ac-comphist-file "~/Dropbox/config/ac-comphist.dat")
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
         (setq popup-use-optimized-column-computation nil)
         ;;(setq ac-candidate-max 10)

         (defun ac-org-mode-setup ()
           ;;            (message " >> ac-org-mode-setup")
           (setq ac-sources '(
                                   ;;; ac-source-abbrev ; Emacs の略語
                                   ;;; ac-source-css-property ; heavy
                              ac-source-dictionary ; 辞書
                              ac-source-features
                              ac-source-filename
                              ac-source-files-in-current-dir
                              ac-source-functions
                                   ;;; ac-source-gtags
                                   ;;; ac-source-imenu 
                                   ;;; ac-source-semantic
                                   ;;; ac-source-symbols 
                                   ;;; ac-source-variables
                                   ;;; ac-source-yasnippet
                              )))

         (defun ac-default-setup ()
           ;;            (message " >> ac-default-setup")
           (setq ac-sources '(ac-source-abbrev
                              ac-source-dictionary
                              ac-source-words-in-same-mode-buffers)))
         ))

  (dolist (hook (list 'python-mode-hook 'perl-mode-hook 'objc-mode-hook))
    (add-hook hook 'ac-default-setup))
  ;; *scratch* バッファでは無効化
  (add-hook 'lisp-mode-hook
            '(lambda () (unless (equal "*scratch*" (buffer-name))
                          (ac-default-setup))))
  (add-hook 'org-mode-hook 'ac-org-mode-setup))

(when (eval-after-autoload-if-found
       '(auto-complete ac-cc-mode-setup) "auto-complete" nil t nil
       '((require 'auto-complete-clang nil t)
         ;; ac-cc-mode-setup のオーバーライド
         (defun ac-cc-mode-setup ()
           (setq ac-clang-prefix-header "~/.emacs.d/stdafx.pch")
           (setq ac-clang-flags '("-w" "-ferror-limit" "1"
                                  "-fcxx-exceptions"))
           (setq ac-sources '(ac-source-clang
                              ac-source-yasnippet
                              ac-source-gtags)))))

  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup))

(when (eval-after-autoload-if-found
       '(quickrun) "quickrun")

  (add-hook 'c-mode-common-hook
            '(lambda () (define-key c++-mode-map (kbd "<f5>") 'quickrun)))
  (add-hook 'python-mode-hook
            '(lambda () (define-key python-mode-map (kbd "<f5>") 'quickrun)))
  (add-hook 'perl-mode-hook
            '(lambda () (define-key perl-mode-map (kbd "<f5>") 'quickrun))))

(when (eval-after-autoload-if-found
       'org-mode "org" "Org Mode" t nil
       '((require 'org-extension nil t)
         (require 'org-habit)
         (require 'org-mobile)

         ;;     (when (require 'pomodoro nil t)
         ;;       (pomodoro:start nil))

         ;; C-c & が yasnippet にオーバーライドされているのを張り替える
         (define-key org-mode-map (kbd "C-c 4") 'org-mark-ring-goto)

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
         ;; (add-hook 'before-save-hook 'org-update-all-dblocks)

         ;; アーカイブファイルの名称を指定
         (setq org-archive-location "%s_archive::")

         ;; タイムスタンプによるログ収集設定
         (setq org-log-done t) ; t ではなく，'(done), '(state) を指定できる

         ;; ログをドロアーに入れる
         (setq org-log-into-drawer t)

         ;; アンダースコアをエクスポートしない（_{}で明示的に表現できる）
         (setq org-export-with-sub-superscripts nil)

         ;; #+OPTIONS: \n:t と同じ
         (setq org-export-preserve-breaks t)

         ;; タイマーの音
         ;; (lsetq org-clock-sound "");

         ;; helm を立ち上げる
         (require 'helm-config nil t)

         ;; - を優先．親のブリッツ表示を継承させない
         (setq org-list-demote-modify-bullet
               '(("+" . "-")
                 ("*" . "-")
                 ("1." . "-")
                 ("1)" . "-")
                 ("A)" . "-")
                 ("B)" . "-")
                 ("a)" . "-")
                 ("b)" . "-")
                 ("A." . "-")
                 ("B." . "-")
                 ("a." . "-")
                 ("b." . "-")))
         ))

  (push '("\\.txt$" . org-mode) auto-mode-alist))

(with-eval-after-load "org"
  ;; ~/Dropbox/Public は第三者に探索される可能性があるので要注意
  ;; default = ~/org.ics
  ;; C-c C-e i org-export-icalendar-this-file
  ;; C-c C-e I org-export-icalendar-all-agenda-files
  ;; C-c C-e c org-export-icalendar-all-combine-agenda-files
  ;; (setq org-combined-agenda-icalendar-file "~/Dropbox/Public/orgAgenda.ics")

  (setq org-icalendar-combined-agenda-file "~/Desktop/org-ical.ics")

  ;; iCal の説明文
  (setq org-icalendar-combined-description "OrgModeのスケジュール出力")
  ;; カレンダーに適切なタイムゾーンを設定する（google 用には nil が必要）
  (setq org-icalendar-timezone "Asia/Tokyo")

  ;; DONE になった TODO はアジェンダから除外する
  (setq org-icalendar-include-todo t)
  ;; （通常は，<>--<> で区間付き予定をつくる．非改行入力で日付がNoteに入らない）
  (setq org-icalendar-use-scheduled '(event-if-todo))
         ;;; DL 付きで終日予定にする：締め切り日（スタンプで時間を指定しないこと）
  ;; (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo))
  (setq org-icalendar-use-deadline '(event-if-todo))

  (when (require 'ox-icalendar nil t)
    (defun my:ox-icalendar ()
      (interactive)
      (let ((temp-agenda-files org-agenda-files))
        (setq org-agenda-files '("~/Dropbox/org/org-ical.org"))
        ;; org-icalendar-export-to-ics を使うとクリップボードが荒れる
        (org-icalendar-combine-agenda-files)
        (setq org-agenda-files temp-agenda-files)
        ;; Dropbox/Public のフォルダに公開する
        ;;           (shell-command
        ;;            (concat "cp " org-icalendar-combined-agenda-file " "
        ;;                    org-icalendar-dropbox-agenda-file))
        (if (eq 0 (shell-command
                   (concat "scp -o ConnectTimeout=5 "
                           org-icalendar-combined-agenda-file " "
                           org-ical-file-in-orz-server)))
            (message "Uploading... [DONE]")
          (message "Uploading... [MISS]"))
        (my:ox-icalendar-cleanup)
        ))

    (defun my:ox-icalendar-cleanup ()
      (interactive)
      (when (file-exists-p
             (expand-file-name org-icalendar-combined-agenda-file))
        (shell-command-to-string
         (concat "rm -rf " org-icalendar-combined-agenda-file))))
    ;;     (add-hook 'focus-out-hook 'my:ox-icalendar-cleanup)
    ))

(with-eval-after-load "org"
  (setq org-use-speed-commands t)
  (add-to-list 'org-speed-commands-user '("d" org-todo "DONE"))
  (add-to-list 'org-speed-commands-user '("P" my:proportional-font-toggle))
  (add-to-list 'org-speed-commands-user 
               '("$" call-interactively 'org-archive-subtree)))

(with-eval-after-load "org"
  (add-to-list 'org-modules 'org-timer)
  (setq org-timer-default-timer "25")
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
  (add-hook 'org-timer-done-hook 'growl-pomodoro-timer))

(with-eval-after-load "org"
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
  ;;  '(("?A" :foreground "#E01B4C" :background "#FFFFFF" :weight bold)
  ;;    ("?B" :foreground "#1739BF" :background "#FFFFFF" :weight bold)
  ;;    ("?C" :foreground "#575757" :background "#FFFFFF" :weight bold)))
  ;; Color setting for Tags

  ;; #CC3333     
  (setq org-todo-keyword-faces
        '(("FOCUS"   :foreground "#FF0000" :background "#FFCC66")
          ("BUG"     :foreground "#FF0000" :background "#FFCC66")
          ("OTW"     :foreground "#EE3300" :background "#FFEE99")
          ("CHECK"   :foreground "#FF9900" :background "#FFF0F0" :underline t)
          ("ICAL"    :foreground "#33CC66")
          ("WAIT"    :foreground "#CCCCCC" :background "#666666")
          ("EDIT"    :foreground "#FF33CC")
          ("READ"    :foreground "#9933CC")
          ("MAIL"    :foreground "#CC3300")
          ("PLAN"    :foreground "#FF6600")
          ("REV1"    :foreground "#3366FF")
          ("REV2"    :foreground "#3366FF" :background "#99CCFF")
          ("REV3"    :foreground "#FFFFFF" :background "#3366FF")
          ("SLEEP"    :foreground "#9999CC")))

  ;; (:foreground "#0000FF" :bold t)     ; default. do NOT put this bottom    
  (setq org-tag-faces
        '(("Achievement" :foreground "#66CC66")
          ("Report"      :foreground "#66CC66")
          ("Background"  :foreground "#66CC99")
          ("Chore"       :foreground "#6699CC")
          ("Domestic"    :foreground "#6666CC")
          ("Doing"       :foreground "#FF0000")   
          ("Ongoing"     :foreground "#CC6666") ; for non scheduled/reminder
          ("Repeat"      :foreground "#CC9999") ; for interval tasks
          ("Mag"         :foreground "#9966CC")
          ("buy"         :foreground "#9966CC")
          ("pay"         :foreground "#CC6699")
          ("secret"      :foreground "#FF0000")
          ("note"        :foreground "#6633CC")
          ("print"       :foreground "#6633CC")
          ("Study"       :foreground "#6666CC")
          ("Implements"  :foreground "#CC9999" :weight bold)
          ("Coding"      :foreground "#CC9999")
          ("Editing"     :foreground "#CC9999" :weight bold)
          ("work"        :foreground "#CC9999" :weight bold)
          ("Survey"      :foreground "#CC9999" :weight bold)
          ("Home"        :foreground "#CC9999" :weight bold)
          ("Open"        :foreground "#CC9999" :weight bold)
          ("Blog"        :foreground "#FF33CC" :background "#9966CC")
          ("Test"        :foreground "#FF0000" :weight bold)
          ("drill"       :foreground "#66BB66" :underline t)
          ("DEBUG"       :foreground "#FFFFFF" :background "#9966CC")
          ("EVENT"       :foreground "#FFFFFF" :background "#9966CC")
          ("Thinking"    :foreground "#FFFFFF" :background "#96A9FF")
          ("Schedule"    :foreground "#FFFFFF" :background "#FF7D7D")
          ("INPUT"       :foreground "#FFFFFF" :background "#CC6666")
          ("OUTPUT"      :foreground "#FFFFFF" :background "#66CC99")
          ("CYCLE"       :foreground "#FFFFFF" :background "#6699CC")
          ("weekend"     :foreground "#FFFFFF" :background "#CC6666")
          ("Log"         :foreground "#008500"))))
;;#5BDF8D

(with-eval-after-load "org"
  (setq org-todo-keywords
        '((sequence "TODO(t)" "FOCUS(f)" "ICAL(c)" "|" "DONE(d)")
          (sequence "BUG(b)" "READ(r)" "EDIT(e)" "MAIL(m)" "PLAN(p)" "|")
          (sequence "CHECK(C)" "OTW(o)" "WAIT(w)" "SLEEP(s)" "|")
          (sequence "REV1(1)" "REV2(2)" "REV3(3)" "|")))

  ;; Global counting of TODO items
  (setq org-hierarchical-todo-statistics nil)
  ;; Global counting of checked TODO items
  (setq org-hierarchical-checkbox-statistics nil)

  ;; block-update-time
  (defun org-dblock-write:block-update-time (params)
    (let ((fmt (or (plist-get params :format) "%Y-%m-%d")))
      (insert "" (format-time-string fmt (current-time)))))

  ;; すべてのチェックボックスの cookies を更新する
  (defun do-org-update-statistics-cookies ()
    (interactive)
    (org-update-statistics-cookies 'all)))

(with-eval-after-load "org"
  (setq org-image-actual-width '(256))
  (add-to-list 'image-file-name-extensions "jp2")
  ;; (add-to-list 'image-file-name-extensions "j2c")
  (add-to-list 'image-file-name-extensions "bmp")
  (add-to-list 'image-file-name-extensions "psd"))

(with-eval-after-load "org-agenda"
  ;; sorting strategy
  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up timestamp-up priority-down category-keep)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep)))
  ;; Set the view span as day in an agenda view, the default is week
  (setq org-agenda-span 'day)
  ;; アジェンダに警告を表示する期間
  (setq org-deadline-warning-days 2)
  ;; アジェンダビューでFOLLOWを設定
  ;; (setq org-agenda-start-with-follow-mode t)
  ;; Customized Time Grid
  (setq org-agenda-time-grid
        '((daily today require-timed)
          "----------------"
          (800 1000 1200 1400 1600 1800 2000 2200 2400 2600)))
  (setq org-agenda-current-time-string "< d('- ' ) now!")
  (setq org-agenda-timegrid-use-ampm t)

  ;; アジェンダ作成対象（指定しないとagendaが生成されない）
  ;; ここを間違うと，MobileOrg, iCal export もうまくいかない
  (setq org-agenda-files
        '("~/Dropbox/org/org-ical.org" "~/Dropbox/org/next.org"
          "~/Dropbox/org/today.org" "~/Dropbox/org/buffer.org"
          "~/Dropbox/org/stock.org"
          "~/Dropbox/org/work.org" "~/Dropbox/org/research.org"))

  (add-hook 'org-finalize-agenda-hook
            '(lambda () (org-agenda-to-appt t '((headline "TODO")))))

  ;; 移動直後にagendaバッファを閉じる（ツリーの内容はSPACEで確認可）
  (org-defkey org-agenda-mode-map [(tab)]
              '(lambda () (interactive)
                 (org-agenda-goto)
                 (with-current-buffer "*Org Agenda*"
                   (org-agenda-quit))))

  ;; 特定タグを持つツリーリストを一発移動（org-tags-view, org-tree-slide）
  (defvar my:doing-tag "Doing")
  (defun my:sparse-doing-tree ()
    (interactive)
    (org-tags-view nil my:doing-tag))
  ;; Doingタグをトグルする
  (defun my:toggle-doing-tag ()
    (interactive)
    (when (eq major-mode 'org-mode)
      (save-excursion
        (save-restriction
          (unless (org-at-heading-p)
            (outline-previous-heading))
          (if (string-match
               (concat ":" my:doing-tag ":") (org-get-tags-string))
              (org-toggle-tag my:doing-tag 'off)
            (org-toggle-tag my:doing-tag 'on))
          (org-reveal)))))

  (custom-set-faces
   ;; '(org-agenda-clocking ((t (:background "#300020"))))
   '(org-agenda-structure ((t (:underline t :foreground "#6873ff"))))
   '(org-agenda-date-today ((t (:weight bold :foreground "#4a6aff"))))
   '(org-agenda-date ((t (:weight bold :foreground "#6ac214"))))
   '(org-agenda-date-weekend ((t (:weight bold :foreground "#ff8d1e"))))
   '(org-time-grid ((t (:foreground "#0a4796"))))
   '(org-warning ((t (:foreground "#ff431a"))))
   '(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   )

  (define-key org-mode-map (kbd "<f11>") 'my:toggle-doing-tag)
  (define-key org-mode-map (kbd "C-<f11>") 'my:sparse-doing-tree))

(with-eval-after-load "org"
  ;; アラーム表示を有効にする
  (appt-activate 1)
  ;; window を フレーム内に表示する
  (setq appt-display-format 'window)
  ;; window を継続表示する時間[s]
  (setq appt-display-duration 5)
  ;; ビープ音の有無
  (setq appt-audible nil)
  ;; 何分前から警告表示を開始するか[m]
  (setq appt-message-warning-time 30)
  ;; 警告表示開始から何分ごとにリマインドするか[m]
  (setq appt-display-interval 5)
  ;; モードラインにアラームを表示する
  (setq appt-display-mode-line t)
  ;; org-agenda の内容をアラームに登録する（重い）
  ;; (add-hook 'org-mode-hook
  ;;           '(lambda ()
  ;;              (org-agenda-to-appt t '((headline "TODO")))))
  ;; 定期的に更新する
  (defvar org-agenda-to-appt-activate t)
  (defun my:org-agenda-to-appt ()
    (interactive)
    (org-agenda-to-appt t '((headline "TODO"))))
  (add-hook 'focus-out-hook
            #'(lambda ()
                (when org-agenda-to-appt-activate
                  (my:org-agenda-to-appt)
                  (setq org-agenda-to-appt-activate nil))))
  (run-with-idle-timer
   10 t #'(lambda ()
            (setq org-agenda-to-appt-activate t)))

  (define-key org-mode-map (kbd "C-c f 3") 'my:org-agenda-to-appt))

(when
    (eval-after-autoload-if-found
     '(org-capture) "org-capture" nil t nil
     '(;; 2010-06-13 の形式では，タグとして認識されない
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
               ("d" "Doingタグ付きのタスクをInboxに投げる" entry
                (file+headline nil "INBOX")
                "** TODO %? :Doing:\n  - \n")
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
                entry (file+headline ,org-capture-buffer-file "Buffers")
                "** %(get-random-string 16) %U\n\n%?\n\n----"))))))

(with-eval-after-load "org"
  (setq org-refile-targets
        (quote (("org-ical.org" :level . 1)
                ("research.org" :level . 1)
                ("work.org" :level . 1)
                ("next.org" :level . 1)
                ("sleep.org" :level . 1)))))

(with-eval-after-load "org"
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  ;; org-src-window-setup (current-window, other-window, other-frame)
  (setq org-src-window-setup 'current-window)
  (require 'ob-http nil t)
  ;; Add ":results output" after program name
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (C . t)
     (perl . t)
     (sh . t)
     (latex . t)
     (R . t)
     (python . t)))
  ;; (require 'ob-C nil t)
  ;; (require 'ob-perl nil t)
  ;; (require 'ob-sh nil t)
  ;; (require 'ob-python nil t)

  ;; 実装済みの言語に好きな名前を紐付ける
  (add-to-list 'org-src-lang-modes '("cs" . csharp))
  (add-to-list 'org-src-lang-modes '("zsh" . sh)))

(with-eval-after-load "org"
  (add-to-list 'org-structure-template-alist
               '("C" "#+BEGIN_COMMENT\n?\n#+END_COMMENT" ""))
  (add-to-list 'org-structure-template-alist
               '("S" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "<src lang=\"emacs-lisp\">\n\n</src>")))

;; Org-tree-slide
(when (eval-after-autoload-if-found
       'org-tree-slide-mode "org-tree-slide" nil t nil
       '(;; <f8>/<f9>/<f10>/<f11> are assigned to control org-tree-slide
         (define-key org-tree-slide-mode-map (kbd "<f9>")
           'org-tree-slide-move-previous-tree)
         (define-key org-tree-slide-mode-map (kbd "<f10>")
           'org-tree-slide-move-next-tree)
         (org-tree-slide-narrowing-control-profile)
         (setq org-tree-slide-modeline-display 'outside)
         (setq org-tree-slide-skip-outline-level 5)
         (setq org-tree-slide-skip-done nil)))

  (global-set-key (kbd "<f8>") 'org-tree-slide-mode)
  (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle))

(with-eval-after-load "org-tree-slide"
  (defun my:org-clock-in ()
    (setq vc-display-status nil) ;; モードライン節約
    (org-clock-in))

  (defun my:org-clock-out ()
    (setq vc-display-status t) ;; モードライン節約解除
    (require 'org-clock nil t)
    (when (org-clocking-p) (org-clock-out)))

  (add-hook 'org-tree-slide-before-move-previous-hook
            'my:org-clock-out)
  (add-hook 'org-tree-slide-before-move-next-hook
            'my:org-clock-out)
  (add-hook 'org-tree-slide-stop-hook 'my:org-clock-out)

  (add-hook 'org-tree-slide-before-narrow-hook
            '(lambda ()
               (when
                   (and (or (equal (buffer-name) "work.org")
                            (equal (buffer-name) "effort.org"))
                        (and (or (eq (org-outline-level) 2)
                                 (eq (org-outline-level) 3))
                             (looking-at (concat "^\\*+ "
                                                 org-not-done-regexp))))
                 (my:org-clock-in)))))

(when (eval-after-autoload-if-found
       '(org-tree-slide) "org-tree-slide" nil t nil
       '((defcustom use-proportional-font nil
           "The status of FONT property"
           :type 'boolean
           :group 'org-mode)

         (set-face-attribute 'variable-pitch nil
                             :family "Verdana"
                             :height 125)

         (defun my:proportional-font-toggle ()
           (interactive)
           (setq use-proportional-font (not use-proportional-font))
           (if use-proportional-font
               (org-entry-put nil "FONT" "PROPORTIONAL")
             (org-delete-property "FONT")))

         (add-hook 'org-tree-slide-before-narrow-hook
                   '(lambda ()
                      (if (equal "PROPORTIONAL"
                                 (org-entry-get-with-inheritance "FONT"))
                          (buffer-face-set 'variable-pitch)
                        (buffer-face-mode 0))))
         (add-hook 'org-tree-slide-stop-hook
                   '(lambda ()
                      (buffer-face-mode 0))))))

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c f p") 'my:proportional-font-toggle))

(eval-after-autoload-if-found
 'org-mode "org" nil t nil
 '((require 'org-fstree nil t)))

(when (eval-after-autoload-if-found
       '(my:cfw-open-org-calendar cfw:open-org-calendar)
       "calfw-org" "Rich calendar for org-mode" t nil
       '(
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
               cfw:fchar-top-right-corner ?| )

         (defun my:org-mark-ring-goto-calfw ()
           (interactive)
           (org-mark-ring-goto))

         (defun my:cfw-open-org-calendar ()
           (interactive)
           (change-frame-width-double)
           (cfw:open-org-calendar))

         (defun my:cfw-burry-buffer ()
           (interactive)
           (bury-buffer)
           (change-frame-width-single))

         (defun cfw:org-goto-date ()
           "Move the cursor to the specified date."
           (interactive)
           (cfw:navi-goto-date
            (cfw:emacs-to-calendar (org-read-date nil 'to-time))))

         (define-key cfw:calendar-mode-map (kbd "j") 'cfw:org-goto-date)
         (define-key cfw:org-schedule-map (kbd "q") 'my:cfw-burry-buffer)))

  (global-set-key (kbd "C-c f c") 'my:cfw-open-org-calendar))

;;         (add-hook 'window-configuration-change-hook 'cfw:resize-calendar)
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

(eval-after-autoload-if-found
 '(ox-odt) "ox-odt" nil t nil
 '((setq org-odt-styles-file
         (concat (getenv "HOME") "/Dropbox/emacs.d/config/style.odt"))
   ;; (setq org-odt-content-template-file
   ;;       (concat (getenv "HOME") "/Dropbox/emacs.d/config/style.ott"))
   (setq org-odt-preferred-output-format "pdf") ;; docx
   ;; ox-odt.el の 自作パッチの変数（DOCSTRINGが記述されていない）
   (setq org-odt-apply-custom-punctuation t)
   (setq org-odt-convert-processes
         '(("LibreOffice"
            "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")
           ("unoconv" "unoconv -f %f -o %d %i")))))

(eval-after-autoload-if-found
 '(org-crypt org-encrypt-entry org-decrypt-entry) "org-crypt" "Org Mode" t nil
 '((setq org-crypt-key "<insert your key>")
   ;; org-encrypt-entries の影響を受けるタグを指定
   (setq org-tags-exclude-from-inheritance (quote ("secret")))
   ;; 自動保存の確認を無効に
   (setq org-crypt-disable-auto-save 'nil)))

(with-eval-after-load "org"
  (add-to-list 'org-modules 'org-mac-iCal)
  (add-to-list 'org-modules 'org-mac-link) ;; includes org-mac-message
  (define-key org-mode-map (kbd "C-c c") 'org-mac-grab-link))

(with-eval-after-load "org"
  (defvar terminal-notifier-command
    (executable-find
     "terminal-notifier.app/Contents/MacOS/terminal-notifier")
    "Path to terminal-notifier.
      Download from https://github.com/julienXX/terminal-notifier/releases")

  (defvar terminal-notifier-sound nil)
  (with-eval-after-load "org-clock"
    (when (not org-clock-sound)
      ;; Select from Preferences: { Funk | Glass | ... | Purr | Pop ... }
      (setq terminal-notifier-sound "Pop"))) 

  (defun terminal-notifier-notify (title message &optional sound)
    "Show a message with `terminal-notifier-command`."
    (if (and sound (not (string= sound "")))
        (start-process "terminal-notifier" "*terminal-notifier*"
                       terminal-notifier-command
                       "-title" title "-message" message
                       "-activate" "org.gnu.Emacs" "-sound" sound) ;; FIXME
      (start-process "terminal-notifier" "*terminal-notifier*"
                     terminal-notifier-command
                     "-title" title "-message" message
                     "-activate" "org.gnu.Emacs")))

  (defvar use-terminal-notifier t)
  (defun terminal-notifier-toggle ()
    (interactive)
    (setq use-terminal-notifier (not use-terminal-notifier))
    (my:org-agenda-to-appt)
    (message "terminal-notifier: %s" use-terminal-notifier))
  (define-key org-mode-map (kbd "C-c f n") 'terminal-notifier-toggle)

  ;; eval (org-notify "hoge") to test this setting
  (setq org-show-notification-handler
        #'(lambda (message)
            (when use-terminal-notifier
              (terminal-notifier-notify
               "Message from org-mode" message terminal-notifier-sound))))

  (defun advice:appt-disp-window (min-to-app new-time appt-msg)
    "Extension to support org-notify."
    (let ((time-msg (concat "in " min-to-app " min.")))
      (when (string= min-to-app "0")
        (setq time-msg "<= Do Now!!"))
      (org-notify (concat "\\\"" appt-msg "\" " time-msg))))
  (advice-add 'appt-disp-window :before #'advice:appt-disp-window))

(when (eval-after-autoload-if-found
       '(org-grep) "org-grep" nil t nil
       '((setq org-grep-extensions nil)
         (add-to-list 'org-grep-directories "~/.emacs.d")
         (add-to-list 'org-grep-directories "~/.emacs.d/.cask/package")))

  (global-set-key (kbd "C-M-g") 'org-grep))

(when (eval-after-autoload-if-found
       'org-mode "org")
  (push '("[rR][eE][aA][dD][mM][eE]" . org-mode) auto-mode-alist))

(with-eval-after-load "utility"
  (set-alarms-from-file "~/Dropbox/org/today.org")
  (defun my:update-alarms-from-file ()
    (when (string= "today.org" (buffer-name))
      (set-alarms-from-file "~/Dropbox/org/today.org"))))

(when (library-p "utility")
  (add-hook 'after-save-hook 'my:update-alarms-from-file))

(when (eval-after-autoload-if-found
       'org-mode "org" "Org Mode" t nil
       '(;; (org-transpose-element) が割り当てられているので取り返す．
         (org-defkey org-mode-map "\C-\M-t" 'beginning-of-buffer)

         ;;(define-key org-mode-map (kbd "C-c 1")
         ;;  'org-export-icalendar-combine-agenda-files)
         (define-key org-mode-map (kbd "C-c f 1") 'my:ox-icalendar)
         (defun my:do-org-update-staistics-cookies ()
           (interactive)
           (message "Update statistics ...")
           (do-org-update-statistics-cookies))
         (define-key org-mode-map (kbd "C-c f 2")
           'my:do-org-update-staistics-cookies)
         (define-key org-mode-map (kbd "C-c m") 'org-mobile-sync)
         (define-key org-mode-map (kbd "<f5>") 'org-narrow-to-subtree)
         (define-key org-mode-map (kbd "S-<f5>") 'widen)))

  (global-set-key (kbd "S-<f12>")
                  '(lambda () (interactive) (show-org-buffer "work.org")))
  (global-set-key (kbd "C-M-o")
                  '(lambda () (interactive) (show-org-buffer "next.org")))
  (global-set-key (kbd "C-M-c") 
                  '(lambda () (interactive) (show-org-buffer "org-ical.org")))
  (global-set-key (kbd "C-M-9") 
                  '(lambda () (interactive) (show-org-buffer "buffer.org")))
  (global-set-key (kbd "C-M-0") 
                  '(lambda () (interactive) (show-org-buffer "today.org")))
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c r") 'org-capture))

(when (eval-after-autoload-if-found
       '(org-autolist-mode) "org-autolist" nil t nil nil)
  (add-hook 'org-mode-hook #'(lambda () (org-autolist-mode))))

(set-face-foreground 'font-lock-regexp-grouping-backslash "#66CC99")
(set-face-foreground 'font-lock-regexp-grouping-construct "#9966CC")

(require 'generic-x nil t)

;; Color of the current line
;; Cite: http://murakan.cocolog-nifty.com/blog/2009/01/emacs-tips-1d45.html
;; see also http://www.emacswiki.org/cgi-bin/emacs/highlight-current-line.el
(global-hl-line-mode t)
(set-face-background 'hl-line "#DEEDFF")

;; Cursor (see also takaxp-mac.el)
;;(add-to-list 'default-frame-alist '(cursor-type . (hbar . 5)))
;;(add-to-list 'default-frame-alist '(cursor-type . bar))
;;(add-hook 'window-configuration-change-hook

(defvar my:cursor-color-ime-on "#91C3FF")
(defvar my:cursor-color-ime-off "#AAAAAA")
(set-cursor-color my:cursor-color-ime-on)

(when (and (eq window-system 'ns) (>= emacs-major-version 24))
  ;; when IME is ON
  (when (fboundp 'mac-set-input-method-parameter)
    (mac-set-input-method-parameter
     "com.google.inputmethod.Japanese.base" 'title "グ"))

  (defun my:update-cursor-color ()
    (interactive)
    (if current-input-method (set-cursor-color my:cursor-color-ime-off)
      (set-cursor-color my:cursor-color-ime-on)))
  ;; (my:update-cursor-color)
  (run-with-idle-timer 10 t 'my:update-cursor-color))

(add-hook 'input-method-activate-hook
          (lambda () (set-cursor-color my:cursor-color-ime-on)))
(add-hook 'input-method-inactivate-hook
          (lambda () (set-cursor-color my:cursor-color-ime-off)))

;; Disable cursor blink
(blink-cursor-mode -1)

(defvar my:font-size 12)
(defun my:ja-font-setter (spec)
  (set-fontset-font nil 'japanese-jisx0208 spec)
  (set-fontset-font nil 'katakana-jisx0201 spec)
  (set-fontset-font nil 'japanese-jisx0212 spec)
  (set-fontset-font nil '(#x0080 . #x024F) spec)
  (set-fontset-font nil '(#x0370 . #x03FF) spec)
  (set-fontset-font nil 'mule-unicode-0100-24ff spec))
(defun my:ascii-font-setter (spec)
  (set-fontset-font nil 'ascii spec))      

(cond
 ;; CocoaEmacs
 ((eq window-system 'ns)
  (when (>= emacs-major-version 23)
    (let
        ;; 1) Monaco, Hiragino/Migu 2M : font-size=12, -apple-hiragino=1.2
        ;; 2) Inconsolata, Migu 2M     : font-size=14, 
        ;; 3) Inconsolata, Hiragino    : font-size=14, -apple-hiragino=1.0

        ;; Fonts

        ((font-size my:font-size)
         ;;          ((font-size 28) ; for mirroring presentation (1440x900)
         ;;           (ascii-font "Inconsolata")
         (ascii-font "Monaco")
         (ja-font "Migu 2M"))
      ;; (ja-font "Hiragino Maru Gothic Pro")) 
      (my:ascii-font-setter (font-spec :family ascii-font :size font-size))
      (my:ja-font-setter (font-spec :family ja-font :size font-size)))

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

    ;; Anti aliasing with Quartz 2D
    (setq mac-allow-anti-aliasing t)))

 ((eq window-system 'w32) ; windows7
  (let
      ((font-size 14)
       (font-height 100)
       (ascii-font "Inconsolata")
       ;; (ja-font "Meiryo UI"))
       (ja-font "メイリオ"))
    (my:ja-font-setter
     (font-spec :family ja-font :size font-size :height font-height))
    (my:ascii-font-setter (font-spec :family ascii-font :size font-size)))
  (setq face-font-rescale-alist '((".*Inconsolata.*" . 1.0))) ; 0.9
  )   
 (window-system ; for SuSE Linux 12.1
  (let
      ((font-size 14)
       (font-height 100)
       (ascii-font "Inconsolata")
       ;; (ja-font "MigMix 1M")
       (ja-font "Migu 1M"))
    (my:ja-font-setter
     (font-spec :family ja-font :size font-size :height font-height))
    (my:ascii-font-setter (font-spec :family ascii-font :size font-size)))
  (setq face-font-rescale-alist '((".*MigMix.*" . 2.0)
                                  (".*Inconsolata.*" . 1.0))) ; 0.9
  ))

(set-default 'line-spacing 0.2)

(eval-after-autoload-if-found
 'diff-mode "diff-mode" nil t nil   
 '((set-face-attribute 'diff-added-face nil
                       :background nil :foreground "green"
                       :weight 'normal)
   (set-face-attribute 'diff-removed-face nil
                       :background nil :foreground "firebrick1"
                       :weight 'normal)

   (set-face-attribute 'diff-file-header-face nil
                       :background nil :weight 'extra-bold)

   (set-face-attribute 'diff-hunk-header-face nil
                       :foreground "chocolate4"
                       :background "white" :weight 'extra-bold
                       :inherit nil)))

(defun my:night-time-p (begin end)
  (let* ((ch (string-to-number (format-time-string "%H" (current-time))))
         (cm (string-to-number (format-time-string "%M" (current-time))))
         (ct (+ cm (* 60 ch))))
    (if (> begin end)
        (or (<= begin ct) (<= ct end))
      (and (<= begin ct) (<= ct end)))))

(defvar night-time-in 21)
(defvar night-time-out 5)

(when (my:night-time-p (* night-time-in 60) (* night-time-out 60))
  ;;(when (require 'gotham-theme nil t)
  ;;(load-theme 'gotham t)
  (when (require 'night-theme nil t)
    (load-theme 'night t)
    (setq my:cursor-color-ime-on "#8599ff")))

(when (eval-after-autoload-if-found
       '(rainbow-mode) "rainbow-mode")
  (add-hook 'emmet-mode-hook 'rainbow-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'org-mode-hook 'rainbow-mode))

(when (eval-after-autoload-if-found
       '(volatile-highlights-mode) "volatile-highlights" nil t nil
       '((set-face-attribute
          'vhl/default-face nil :foreground "#FF3333" :background "#FFCDCD")
         (volatile-highlights-mode t)

         ;; ふわっとエフェクトの追加（ペースト時の色 => カーソル色 => 本来色）
         (defun my:vhl-change-color ()
           (let
               ((next 0.2)
                (reset 0.5)
                (colors '("#F8D3D7" "#F2DAE1" "#EBE0EB" "#E5E7F5" "#DEEDFF")))
             (dolist (color colors)
               (run-at-time next nil
                            'set-face-attribute
                            'vhl/default-face
                            nil :foreground "#FF3333" :background color)
               (setq next (+ 0.05 next)))
             (run-at-time reset nil 'vhl/clear-all))
           (set-face-attribute 'vhl/default-face
                               nil :foreground "#FF3333"
                               :background "#FFCDCD"))

         (defun my:yank (&optional ARG)
           (interactive)
           (yank ARG)
           (my:vhl-change-color))
         (global-set-key (kbd "M-v") 'my:yank)
         (global-set-key (kbd "C-y") 'my:yank)

         (with-eval-after-load "org"
           (define-key org-mode-map (kbd "C-y")
             '(lambda () (interactive)
                (org-yank)
                (my:vhl-change-color))))))

  (add-hook 'org-mode-hook 'volatile-highlights-mode)
  (add-hook 'emacs-lisp-mode-hook 'volatile-highlights-mode)
  (add-hook 'emmet-mode-hook 'rainbow-mode))

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
           (alpha . (100 90))
           (cursor-height . 16)
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

(eval-after-autoload-if-found 
 'e2wm:dp-two "e2wm" nil t nil
 '((setq e2wm:c-two-recipe
         '(- (:lower-size 10)
             (| left right)
             sub))
   (setq e2wm:c-two-winfo
         '((:name left )
           (:name right )
           (:name sub :default-hide t)))

   ;; 高さを画面の最大に矯正
   (when (require 'frame-ctr nil t)
     (setq frame-height-tall (max-frame-height))
     (setq frame-height-small frame-height-tall))

   ;; left, prev
   (setq e2wm:c-two-right-default 'left)

   ;; To avoid rebooting issue when using desktop.el and recentf.el
   (add-hook 'kill-emacs-hook 'e2wm:stop-management)))

(defvar frame-ctr-autoloads
  '(move-frame-with-user-specify
    move-frame-to-center move-frame-to-edge-top move-frame-right
    move-frame-left move-frame-to-edge-bottom frame-ctr-open-height-ring
    fit-frame-to-fullscreen set-font-size-input max-frame-height
    reset-font-size increase-font-size decrease-font-size set-font-size))

(when (eval-after-autoload-if-found
       frame-ctr-autoloads "frame-ctr" nil t nil 
       '((make-frame-height-ring)
         (cond
          ((eq (display-pixel-width) 1920) (setq fullscreen-fontsize 38))
          ((eq (display-pixel-width) 1440) (setq fullscreen-fontsize 28))
          ((eq (display-pixel-width) 1366) (setq fullscreen-fontsize 19))
          ((eq (display-pixel-width) 800) (setq fullscreen-fontsize 14))
          (t (setq fullscreen-fontsize 12)))

         ;; リングの状態を最新に更新（max-frame-height が変わるため）
         (add-hook 'frame-ctr-after-fullscreen-hook
                   'make-frame-height-ring)))

  ;; Move the frame to somewhere (default: 0,0)
  (global-set-key (kbd "M-0") 'move-frame-with-user-specify)
  ;; Move the frame to left side of the current position (require 'frame-cmds)
  (global-set-key (kbd "M-1") '(lambda () (interactive) (move-frame-left 40)))
  ;; Move the frame to the center of the window display (require 'frame-ctr)
  (global-set-key (kbd "M-2") 'move-frame-to-center)
  ;; Move the frame to right side of the current position(require 'frame-cmds)
  (global-set-key (kbd "M-3") '(lambda () (interactive)(move-frame-right 40)))
  ;; Move the current frame to the top of the window display
  (global-set-key (kbd "<f1>") 'move-frame-to-edge-top)
  ;; Move the current frame to the bottom of the window display
  (global-set-key (kbd "S-<f1>") 'move-frame-to-edge-bottom)
  ;; Cycle heights
  (global-set-key (kbd "<f2>") 'frame-ctr-open-height-ring)

  (global-set-key (kbd "C-x C-9") 'fit-frame-to-fullscreen)
  (global-set-key (kbd "C-x C-0") 'reset-font-size)
  (global-set-key (kbd "C-=") 'increase-font-size)
  (global-set-key (kbd "C--") 'decrease-font-size))

;; setting for e2wm
(when (eval-after-autoload-if-found
       '(change-frame-width-single
         change-frame-width-double change-frame-double-window
         change-frame-single-window) "frame-ctr-e2wm")

  ;; Set the frame width single size
  ;;  C-u C-x - => e2wm OFF, single size width and double height, move center
  (global-set-key (kbd "C-x -") 'change-frame-single-window)
  ;; Set the frame width double size
  ;;  C-u C-x = => e2wm ON, double size width and height, move to the center
  (global-set-key (kbd "C-x =") 'change-frame-double-window)
  (global-set-key (kbd "C-c f s") 'change-frame-width-single)
  (global-set-key (kbd "C-c f d") 'change-frame-width-double))

(when (eval-after-autoload-if-found
       '(popwin-mode) "popwin" nil t nil
       '(;; for emacs 24.1
         ;; (setq special-display-function 'popwin:special-display-popup-window)
         ;; (setq display-buffer-function 'popwin:display-buffer)
         ;; for emacs 24.3
         ;; (setq special-display-alist 'popwin:special-display-popup-window)
         ;; (setq display-buffer-alist 'popwin:display-buffer)
         ;; (push '("*sdic*" :position top) popwin:special-display-config)

         (setq popwin:special-display-config
               (append
                '(("CAPTURE-next.org" :height 10 :position bottom :noselect t)
                  ("CAPTURE-org-ical.org" :height 10 :position bottom :noselect t)
                  ("*Help*"          :height 20 :position bottom)
                  ("dict-app-result" :height 20 :position bottom)
                  ("*osx-dictionary*" :height 20 :position bottom)
                  ("*wclock*"        :height 10 :position bottom)
                  ("*Org Agenda*"    :height 10 :position bottom)
                  ("*Org Select*"  :height 10 :position bottom)
                  ("*Occur*"       :height 10 :position bottom)
;;;            (undo-tree-visualizer-buffer-name :height 10 :position top)
;;;            (undo-tree-diff-buffer-name :height 20 :position bottom)
                  ("*eshell*"      :height 10 :position bottom))
                popwin:special-display-config))))
  (popwin-mode 1))

(defvar utility-autoloads
  '(takaxp:date
    takaxp:window-resizer takaxp:open-file-ring my:update-alarms-from-file
    my:desktop-notify my:daylight-theme my:night-theme 
    eval-org-buffer kyoko-mad-mode-toggle
    org2dokuwiki-cp-kill-ring open-current-directory set-alarms-from-file
    reload-ical-export show-org-buffer get-random-string init-auto-install
    add-itemize-head insert-formatted-current-date
    insert-formatted-current-time insert-formatted-signature
    export-timeline-business export-timeline-private 
    browse-url-chrome count-words-buffer do-test-applescript
    delete-backup-files recursive-delete-backup-files describe-timer))

(when (eval-after-autoload-if-found utility-autoloads "utility")
  (global-set-key (kbd "<f12>") 'takaxp:open-file-ring)
  (global-set-key (kbd "C-c t") 'takaxp:date)
  (global-set-key (kbd "C-c f 4") 'takaxp:window-resizer))

(provide 'init)
