;;;; Configurations for Emacs
;;;;                                       Takaaki ISHIKAWA  <takaxp@ieee.org>
;;;; Cite: http://www.mygooglest.com/fni/dot-emacs.html (GREAT!)

(defun my:load-init-time ()
  "Loading time of user init files including time for `after-init-hook'."
  (let ((time1 (float-time
                (time-subtract after-init-time before-load-init-time)))
        (time2 (float-time
                (time-subtract (current-time) before-load-init-time))))
    (message (concat "Loading init files: %.0f [msec], "
                     "of which %.f [msec] for `after-init-hook'.")
             (* 1000 time1) (* 1000 (- time2 time1)))))
(add-hook 'after-init-hook 'my:load-init-time t)

(defun my:emacs-init-time ()
  (message "Emacs booting time: %.0f [msec] from `emacs-init-time'."
           (* 1000
              (float-time (time-subtract
                           after-init-time
                           before-init-time)))))
(add-hook 'after-init-hook 'my:emacs-init-time)

(setq gc-cons-threshold 134217728)

(eval-when-compile
  (require 'cl-lib)
  (require 'org)
  (require 'org-clock)
  (require 'org-mac-link))

(defun eval-after-autoload-if-found
    (functions file &optional docstring interactive type after-body)
  "Set up autoload and eval-after-load for FUNCTIONS iff. FILE has found."
  (let ((enabled t)
        (package nil))
    (when (boundp 'loading-packages)
      (dolist (package loading-packages)
        (when (and (stringp (car package)) (equal file (car package)))
          (unless (cdr package)
            (setq enabled nil)
            (message "--- A setting for `%s' was NOT loaded explicitly"
                     (car package))))))
    (when enabled ;; if disabled then return nil.
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
              (message "--- NOT FOUND: %s" library)
              (setq result nil)))
          (if (listp libraries)
              libraries
            (list libraries)))
    result))

(defun passed-clock-p (target)
  (let
      ((hour nil)
       (min nil)
       (current-hour nil)
       (current-min nil))
    (when (string-match "\\([0-2]?[0-9]\\):\\([0-5][0-9]\\)" target)
      (setq hour (substring target (match-beginning 1) (match-end 1)))
      (setq min (substring target (match-beginning 2) (match-end 2)))
      (setq current-hour (format-time-string "%H" (current-time)))
      (setq current-min (format-time-string "%M" (current-time)))
      (< (+ (* (string-to-number hour) 60)
            (string-to-number min))
         (+ (* (string-to-number current-hour) 60)
            (string-to-number current-min))))))

(defvar window-focus-p t)
(defun window-focus-p () 
  (if window-focus-p t nil))
(add-hook 'focus-in-hook '(lambda () (setq window-focus-p t)))
(add-hook 'focus-out-hook '(lambda () (setq window-focus-p nil)))

(setq byte-compile-warnings '(not obsolete))
(setq ad-redefinition-action 'accept)

(setq debug-on-error nil)

(setq confirm-kill-emacs 'y-or-n-p)

(global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)

(setq echo-keystrokes 0.5)

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
         ;; nil: 別ウィンドウが開く
         (setq ag-reuse-buffers t)
         ;; nil: 結果を選択時に別ウィンドウに結果を出す
         (setq ag-reuse-window nil)
         ;; q でウィンドウを抜ける
         ;; (define-key ag-mode-map (kbd "q") 'delete-window)

         ;; 自動的に出力バッファに移動
         (defun my:ag ()
           (interactive)
           (call-interactively 'ag)
           (switch-to-buffer-other-frame "*ag search*"))))

  (global-set-key (kbd "C-M-f") 'my:ag))

(when (or (eq window-system 'ns) (eq window-system 'mac))
  (global-set-key (kbd "M-v") 'yank)
  (when (boundp 'ns-command-modifier)
    (setq ns-command-modifier 'meta))
  (when (boundp 'ns-alternate-modifier)
    (setq ns-alternate-modifier 'super))
  (when (boundp 'ns-pop-up-frames)
    (setq ns-pop-up-frames nil))
  (global-set-key [ns-drag-file] 'ns-find-file) ; D&D for Emacs23
  ) ; D&D for Emacs23
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

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

;; (add-hook 'emacs-lisp-mode-hook
;;           '(lambda ()
;;              (setq indent-tabs-mode t)
;;              (setq tab-width 8)
;;              (setq indent-line-function 'lisp-indent-line)))

(when (eval-after-autoload-if-found
       '(aggressive-indent-mode) "aggressive-indent" nil t)

  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'perl-mode-hook 'aggressive-indent-mode)
  (add-hook 'c-mode-common-hook 'aggressive-indent-mode)
  ;; (add-hook 'python-mode-hook 'aggressive-indent-mode)
  ;; (add-hook 'nxml-mode-hook 'aggressive-indent-mode)
  ;; (add-hook 'web-mode-hook 'aggressive-indent-mode)
  )

(setq vc-follow-symlinks t)

(when (eq window-system 'ns)
  (when (boundp 'mac-ime-cursor-type) ;; private patch
    (setq mac-ime-cursor-type '(bar . 2)))
  (defun ns-ime-toggle ()
    (interactive)
    (when (fboundp 'mac-get-current-input-source)
      ;;      (mac-toggle-input-source
      (mac-toggle-input-method
       (if (string-match "\\.base$" (mac-get-current-input-source))
           nil t))))
  (global-set-key (kbd "S-SPC") 'ns-ime-toggle)
  (define-key isearch-mode-map (kbd "S-SPC") 'ns-ime-toggle)
  (defun ns-org-heading-auto-ascii ()
    (when (and window-focus-p
               (eq major-mode 'org-mode)
               (or (looking-at org-heading-regexp)
                   (equal (buffer-name) org-agenda-buffer-name)))
      (mac-toggle-input-method nil)))
  (when (fboundp 'mac-toggle-input-method)
    (run-with-idle-timer 1 t 'ns-org-heading-auto-ascii)))

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

(when (eval-after-autoload-if-found
       '(cycle-buffer cycle-buffer-backward) "cycle-buffer" nil t nil
       '((setq cycle-buffer-allow-visible t)
         (setq cycle-buffer-show-length 12)
         (setq cycle-buffer-show-format '(" <(%s)>" . " %s"))))

  (global-set-key (kbd "M-]") 'cycle-buffer)
  (global-set-key (kbd "M-[") 'cycle-buffer-backward))

(when (eval-after-autoload-if-found
       '(my:bm-toggle
         my:bm-next bm-buffer-save bm-buffer-restore bm-buffer-save-all
         bm-repository-save bm-repository-load bm-load-and-restore)
       "bm" nil t nil
       '((setq-default bm-buffer-persistence t) ;; t
         (setq bm-cycle-all-buffers t)
         ;; (setq bm-toggle-buffer-persistence t)
         (setq bm-repository-file "~/Dropbox/emacs.d/.bookmark")
         ;; autoload との組み合わせでは無意味
         ;;（after-init-hook を利用せよ）
         ;; (setq bm-restore-repository-on-load t)
         (setq bm-buffer-persistence t)
         (setq bm-persistent-face 'bm-face)
         (setq bm-repository-file
               (expand-file-name "~/Dropbox/emacs.d/.bm-repository"))
         (bm-repository-load)

         (defun my:bm-toggle ()
           "bm-toggle with updating history"
           (interactive)
           (let ((bm (concat
                      (buffer-name) "::"
                      (if (and (equal major-mode 'org-mode)
                               (not (org-before-first-heading-p)))
                          (nth 4 (org-heading-components))
                        (format "%s" (line-number-at-pos))))))
             (if (bm-bookmark-at (point))
                 (bookmark-delete bm)
               (bookmark-set bm)))
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

  ;; ファイルオープン時にブックマークを復帰
  (add-hook 'find-file-hook 'bm-buffer-restore)
  (global-set-key (kbd "<f9>") 'my:bm-toggle)
  (global-set-key (kbd "<C-f9>") 'my:bm-next))

(when (eval-after-autoload-if-found
       '(centered-cursor-mode) "centered-cursor-mode" nil t)

  (add-hook 'isearch-mode-hook
            '(lambda () (centered-cursor-mode 1)))
  (add-hook 'isearch-mode-end-hook
            '(lambda () (centered-cursor-mode -1))))

(setq yank-excluded-properties t)

;; org-tre-slide が有効ならタイムスタンプを更新しない
;; （Undo範囲が限定されてしまうため）

;; #+UPDATE 用
(when (eval-after-autoload-if-found
       '(update-stamp) "update-stamp" nil t nil
       '((setq update-stamp-start "#+UPDATE:[ \t]*")
         (setq update-stamp-format "%:y-%02m-%02dT%02H:%02M:%02S%Z")
         (setq update-stamp-end "$")
         (setq update-stamp-line-limit 10))) ; def=8

  (add-hook 'before-save-hook
            '(lambda ()
               (if (boundp 'org-tree-slide-mode)
                   (unless org-tree-slide-mode
                     (update-stamp))
                 (update-stamp)))))
;; #+DATE 用
(when (eval-after-autoload-if-found
       '(time-stamp) "time-stamp" nil t nil
       '((setq time-stamp-start "#+DATE:[ \t]*")
         (setq time-stamp-format "%:y-%02m-%02dT%02H:%02M:%02S%Z")
         (setq time-stamp-end "$")
         (setq time-stamp-line-limit 10) ; def=8
         ))

  (add-hook 'before-save-hook
            '(lambda ()
               (if (boundp 'org-tree-slide-mode)
                   (unless org-tree-slide-mode
                     (time-stamp))
                 (time-stamp)))))

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
             (orgstruct-mode)
             (setq tab-width 4)
             (setq left-margin 4)))

(push '("\\.h$" . c++-mode) auto-mode-alist)
(when (eval-after-autoload-if-found
       'modern-c++-font-lock-mode "modern-cpp-font-lock" nil t)
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(when (eval-after-autoload-if-found
       '(csharp-mode) "csharp-mode" "Major mode for editing C# mode." nil t)

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
       '(R-mode R) "ess-site" "Emacs Speaks Statistics mode" nil t)
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
       '(yaml-mode) "yaml-mode" nil t)
  (push '("\\.yml$" . yaml-mode) auto-mode-alist))

(when (eval-after-autoload-if-found
       '(json-mode) "json-mode" nil t)
  (push '("\\.json$" . json-mode) auto-mode-alist))

(when (eval-after-autoload-if-found
       '(js2-mode) "js2-mode" nil t nil
       '((require 'js2-refactor nil t)))

  (push '("\\.js$" . js2-mode) auto-mode-alist)
  (when (eval-after-autoload-if-found
         '(ac-js2-mode ac-js2-setup-auto-complete-mode) "ac-js2" nil t)
    (add-hook 'js2-mode-hook 'ac-js2-mode)))

  (if (executable-find "tern")
      (when (eval-after-autoload-if-found
             '(tern-mode) "tern" nil t nil
             '((tern-mode 1)
               ;; tern-command shall be overwritten by actual path
               (setq tern-command `("node" ,(executable-find "tern")))
               (when (require 'tern-auto-complete nil t)
                 (tern-ac-setup))))
        (add-hook 'js2-mode-hook 'tern-mode))

    (message "--- tern is NOT installed in this system."))

(when (eval-after-autoload-if-found
       '(csv-mode) "csv-mode" nil t)
  (push '("\\.csv$" . csv-mode) auto-mode-alist))

(eval-after-autoload-if-found '(ascii-on ascii-off) "ascii" nil t)

(when (eval-after-autoload-if-found 'es-mode "es-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.es$" . es-mode)))

(when (eval-after-autoload-if-found 'gnuplot-mode "gnuplot-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.plt$" . gnuplot-mode)))

(when (eval-after-autoload-if-found 'markdown-mode "markdown-mode" nil t)
  (push '("\\.markdown$" . markdown-mode) auto-mode-alist)
  (push '("\\.md$" . markdown-mode) auto-mode-alist))

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

     (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
     (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))
     (setq ispell-dictionarry "english")
     (setq ispell-personal-dictionary "~/Dropbox/emacs.d/.aspell.en.pws")

     ;; This will also avoid an IM-OFF issue for flyspell-mode.
     ;;  (setq ispell-aspell-supports-utf8 t)
     ;;  (setq ispell-encoding8-command t)
     (setq ispell-local-dictionary-alist
           '((nil "[a-zA-Z]" "[^a-zA-Z]" "'" t
                  ("-d" "en" "--encoding=utf-8") nil utf-8))))))

;; Spell checking within a specified region
(global-set-key (kbd "C-c f 7") 'ispell-region)
(global-set-key (kbd "<f7>") 'ispell-complete-word) ;; 補完候補の表示

(when (eval-after-autoload-if-found
       '(flyspell-prog-mode flyspell-mode) "flyspell" nil t nil
       '(;; C-; をオーバーライド
         (define-key flyspell-mode-map (kbd "C-;") 'comment-dwim)
         (setq flyspell-duplicate-distance 0)
         (setq flyspell-mode-line-string nil)
         ;; (setq flyspell-large-region 200)
         (set-face-attribute 'flyspell-duplicate nil
                             :foreground "#EA5506" :bold t
                             :background nil :underline t)
         (set-face-attribute 'flyspell-incorrect nil
                             :foreground "white" :bold t
                             :background "#BA2636" :underline t)
         ;; Auto complete との衝突を回避
         (with-eval-after-load "auto-complete"
           (ac-flyspell-workaround))))

  (dolist (hook
           '(text-mode-hook
             change-log-mode-hook latex-mode-hook yatex-mode-hook
             git-commit-mode-hook org-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))

  ;; コメント行のみを対象にする
  (dolist (hook
           '(c-mode-common-hook emacs-lisp-mode-hook))
    (add-hook hook #'flyspell-prog-mode)))

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
       '(po-mode) "po-mode" nil t)
  (push '("\\.po[tx]?\\'\\|\\.po\\$" . po-mode) auto-mode-alist))

(global-set-key (kbd "M-=") 'count-words)

(when (eval-after-autoload-if-found
       '(yatex-mode) "yatex" "Yet Another LaTeX mode" t nil
       '((setq YaTeX-kanji-code 4))) ;; 1=Shift JIS, 2=JIS, 3=EUC, 4=UTF-8
  (push '("\\.tex$" . yatex-mode) auto-mode-alist)
  ;; Disable auto line break
  (add-hook 'yatex-mode-hook
            '(lambda () (setq auto-fill-function nil))))

(eval-after-autoload-if-found
 '(display-time-world) "time" nil t nil
 '((define-key display-time-world-mode-map "q" 'delete-window)))

(when (eval-after-autoload-if-found
       '(yas-minor-mode yas-global-mode)
       "yasnippet" nil t nil
       '((setq yas-verbosity 2)
         (setq yas-snippet-dirs
               (list "~/Dropbox/emacs.d/yas-dict"
                     'yas-installed-snippets-dir)) ;; for Cask

         (defun my:yas-expand-src-edit (&optional field)
           "Override `yas-expand'. Kick `org-edit-special' directly in src-block."
           (interactive)
           (cond ((and (equal major-mode 'org-mode)
                       (org-in-src-block-p t))
                  (org-edit-special))
                 (t
                  (yas-expand field))))

         (defun my:yas-expand (&optional field)
           "Disable `yas-expand' in src-block."
           (interactive)
           (cond ((and (equal major-mode 'org-mode)
                       (org-at-heading-p))
                  (org-cycle))
                 ((and (equal major-mode 'org-mode)
                       (org-in-src-block-p t)
                       (not (and (fboundp 'org-src-edit-buffer-p)
                                 (org-src-edit-buffer-p))))
                  (org-cycle))
                 (t (yas-expand field))))

         ;; 本家できちんと対応されたので，不要になった．
         ;; (define-key yas-minor-mode-map (kbd "<tab>") 'my:yas-expand)

         (yas-global-mode 1)))

  (dolist (hook
           (list 
            'perl-mode-hook 'c-mode-common-hook 'js2-mode-hook 'org-mode-hook
            'python-mode-hook 'emacs-lisp-mode-hook))
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
 '(describe-number describe-number-at-point) "describe-number" nil t)

(if (executable-find "js-beautify")
    (eval-after-autoload-if-found
     'js2-mode "js2-mode" nil t nil
     '((when (require 'web-beautify nil t)
         (define-key js2-mode-map (kbd "C-c b") 'web-beautify-js)
         (define-key js2-mode-map (kbd "C-c b") 'web-beautify-css))))
  (message "--- js-beautify is NOT installed.")
  (message "--- Note: npm -g install js-beautify"))

(when (require 'diminish nil t)
  (with-eval-after-load "isearch" (diminish 'isearch-mode))
  (with-eval-after-load "autorevert" (diminish 'auto-revert-mode))
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
  (with-eval-after-load "highlight-symbol" (diminish 'highlight-symbol-mode))
  (with-eval-after-load "which-key" (diminish 'which-key-mode))
  ;; (with-eval-after-load "org-autolist" (diminish 'org-autolist-mode))
  ;;;  (with-eval-after-load "helm" (diminish 'helm-mode " H"))
  )

(add-hook 'c-mode-hook #'(lambda () (setq mode-name "C")))
(add-hook 'js2-mode-hook #'(lambda () (setq mode-name "JS")))
(add-hook 'c++-mode-hook #'(lambda () (setq mode-name "C++")))
(add-hook 'csharp-mode-hook #'(lambda () (setq mode-name "C#")))
(add-hook 'prog-mode-hook #'(lambda () (setq mode-name "S")))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (setq mode-name "el")))
(add-hook 'python-mode-hook #'(lambda () (setq mode-name "py")))
(add-hook 'perl-mode-hook #'(lambda () (setq mode-name "pl")))
(add-hook 'web-mode-hook #'(lambda () (setq mode-name "W")))
(add-hook 'lisp-interaction-mode-hook #'(lambda () (setq mode-name "Lisp")))

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
  ;;  :underline "#203e6f"
  (set-face-foreground 'header-line "#333333") ;; "#203e6f"
  (set-face-background 'header-line "#ffb08c")
  (set-face-attribute 'header-line nil
                      :inherit nil
                      :overline nil
                      :underline nil)
  (setq header-line-format
        (concat
         "  Buffers                                                    "
         (format-time-string "W%U: %Y-%m-%d %a.")))
  )

(global-set-key (kbd "C-M-s")
                #'(lambda () (interactive)
                    (switch-to-buffer "*scratch*")))

;; Show scroll bar or not
(when window-system
  (set-scroll-bar-mode nil)) ; 'right

;; Disable to show the tool bar.
(when window-system
  (tool-bar-mode -1))

;; Disable to show the splash window at startup
(setq inhibit-startup-screen t)

;; Show line number in the mode line.
(line-number-mode t)

;; Show clock in in the mode line
(setq display-time-format "%H%M.%S") ;; %y%m%d.
(setq display-time-interval 1)
(setq display-time-default-load-average nil)
(display-time-mode 1)

(when (eval-after-autoload-if-found
       '(paren-activate) "mic-paren" nil t nil
       '((setq paren-sexp-mode nil)
         (set-face-foreground 'paren-face-match "#FFFFFF")
         ;; Deep blue: #6666CC, orange: #FFCC66
         (set-face-background 'paren-face-match "#66CC66")))

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

;;show EOF
;; (defun set-buffer-end-mark()
;;   (let ((overlay (make-overlay (point-max) (point-max))))
;;     (overlay-put overlay 'before-string #("[EOF]" 0 5 (face highlight)))
;;     (overlay-put overlay 'insert-behind-hooks
;;                  '((lambda (overlay after beg end &optional len)
;;                      (when after
;;                        (move-overlay overlay (point-max) (point-max))))))))
;; (add-hook 'find-file-hooks 'set-buffer-end-mark)

(setq-default indicate-buffer-boundaries
              '((top . nil) (bottom . left) (down . left)))

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

  (if (executable-find "cmigemo")
      (add-hook 'isearch-mode-hook 'migemo-init)
    (message "--- cmigemo is NOT installed.")))

(when (eval-after-autoload-if-found
       '(helm-M-x helm-locate helm-recentf helm-buffers-list
                  helm-occur helm-swoop helm-flycheck helm-bookmarks)
       "helm-config" nil t nil
       '((helm-mode 1)
         (when (require 'diminish nil t)
           (diminish 'helm-mode " H"))
         (when (require 'helm-swoop nil t)
           ;; カーソルの単語が org の見出し（*の集まり）なら検索対象にしない．
           (setq helm-swoop-pre-input-function
                 #'(lambda()
                     (unless (thing-at-point-looking-at "^\\*+")
                       (thing-at-point 'symbol))))
           ;; 配色設定
           (set-face-attribute
            'helm-swoop-target-line-face nil :background "#FFEDDC")
           (set-face-attribute
            'helm-swoop-target-word-face nil :background "#FF5443"))

         (when (require 'helm-files nil t)
           (define-key helm-find-files-map
             (kbd "<tab>") 'helm-execute-persistent-action)
           (define-key helm-read-file-map
             (kbd "<tab>") 'helm-execute-persistent-action))

         (when (require 'helm-google nil t)
           (setq helm-google-tld "co.jp"))

         ;; (require 'recentf-ext nil t)
         ;; helm-find-files を呼ばせない
         ;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
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
         (when (or (eq window-system 'ns) (eq window-system 'mac))
           (setq helm-locate-command "mdfind -name %s %s")
           )

         (require 'helm-css-scss nil t)
         (require 'helm-emmet nil t)
         (require 'helm-bm nil t)

         ;; この修正が必要
         ;; (when (require 'helm-migemo nil t)
         ;;   (defun helm-compile-source--candidates-in-buffer (source)
         ;;     (helm-aif (assoc 'candidates-in-buffer source)
         ;;         (append source
         ;;                 `((candidates
         ;;                    . ,(or (cdr it)
         ;;                           (lambda ()
         ;;                             ;; Do not use `source' because other plugins
         ;;                             ;; (such as helm-migemo) may change it
         ;;                             (helm-candidates-in-buffer
         ;;                              (helm-get-current-source)))))
         ;;                   (volatile) (match identity)))
         ;;       source)))
         ))

  (global-set-key (kbd "C-c f b") 'helm-bookmarks))

(when (eval-after-autoload-if-found
       '(helm-M-x helm-locate helm-recentf helm-buffers-list
                  helm-occur helm-swoop helm-flycheck)
       "helm-config" nil t)

  (global-set-key (kbd "C-c f l") 'helm-locate)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "M-s M-s") 'helm-swoop)
  (global-set-key (kbd "C-M-r") 'helm-recentf)
  (global-set-key (kbd "C-c o") 'helm-occur)
  (global-set-key (kbd "M-x") 'helm-M-x))

(when (eval-after-autoload-if-found
       'helm-descbinds "helm-descbinds" nil t)

  (global-set-key (kbd "C-h d") 'helm-descbinds))

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

(with-eval-after-load "calendar"
  (setq calendar-week-start-day 1)
  (copy-face 'default 'calendar-iso-week-header-face)
  (set-face-attribute 'calendar-iso-week-header-face nil
                      :height 1.0 :foreground "#1010FF"
                      :background (face-background 'default))
  (setq calendar-intermonth-header
        (propertize " w"
                    'font-lock-face 'calendar-iso-week-header-face))

  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil
                      :height 1.0 :foreground "orange"
                      :background (face-background 'default))

  (setq calendar-intermonth-text
        '(propertize
          (format "%02d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian
                     (list month
                           (- day (1- calendar-week-start-day)) year)))))
          'font-lock-face 'calendar-iso-week-face)))

   (global-set-key (kbd "C-c f c c") 'calendar)

(when (eval-after-autoload-if-found
       '(which-key-mode) "which-key" nil t nil
       '((setq which-key-idle-delay 1.0)
         (which-key-mode 1)))
  (add-hook 'org-mode-hook #'which-key-mode)
  (add-hook 'helm-after-initialize-hook #'which-key-mode))

(when (eval-after-autoload-if-found
       '(my:activate-stock-ticker stock-ticker-global-mode)
       "stock-ticker" nil t nil
       '((defun stock-ticker--parse (data)
           "Parse financial DATA into list of display strings."
           (let ((qs (assoc-default
                      'quote (assoc-default
                              'results (assoc-default 'query data)))))
             (mapcar
              (lambda (q)
                (let ((percent (assoc-default 'PercentChange q))
                      (name (assoc-default 'Name q))
                      (symbol (assoc-default 'Symbol q))
                      (change (assoc-default 'Change q)))
                  (format " %s:%s"
                          (substring
                           (if (or
                                (string-match "=" symbol)
                                (string-match "\\^" symbol))
                               name symbol) 0 3)
                          (if percent percent ""))))
              qs)))
         (setq stock-ticker-display-interval 5)
         (setq stock-ticker-symbols '("^N225" "DOW"))
         (setq stock-ticker-update-interval 300)

         (defun my:activate-stock-ticker (&optional duration)
           "Activate stock-ticker within the given duration."
           (stock-ticker-global-mode 1)
           (unless (numberp duration)
             (setq duration 90))
           (run-with-timer duration nil 'stock-ticker-global-mode -1))))

  (add-hook 'focus-in-hook 'my:activate-stock-ticker))

(when (eval-after-autoload-if-found 
       '(highlight-symbol-mode highlight-symbol-nav-mode)
       "highlight-symbol" nil t nil
       '((setq highlight-symbol-idle-delay 0.5)))
  (add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
  (add-hook 'c-mode-common-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode))

(eval-after-autoload-if-found
 '(google-maps) "google-maps" nil t nil
 '((require 'org-location-google-maps nil t)))

;; dired-du
(eval-after-autoload-if-found '(dired-du-mode) "dired-du" nil t)

;; dired-narrow
(eval-after-autoload-if-found '(dired-narrow-mode) "dired-narrow" nil t)

;; all-the-icons-dired
(eval-after-autoload-if-found
 '(all-the-icons-dired-mode) "all-the-icons-dired" nil t)

;; gited
(eval-after-autoload-if-found '(gited-list-branches) "gited" nil t)

(with-eval-after-load "dired"
  (add-hook 'dired-mode-hook #'dired-narrow-mode)
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
  (define-key dired-mode-map "\C-x\C-g" 'gited-list-branches))

(setq undo-outer-limit nil)

(when (eval-after-autoload-if-found
       '(my:undo-tree-visualize) "undo-tree" nil t nil
       '((global-undo-tree-mode)
         (defvar undo-tree-active nil)
         (setq undo-tree-mode-lighter nil) ;; モードライン領域を節約
         (defun my:undo-tree-visualizer-quit ()
           (interactive)
           (undo-tree-visualizer-quit)
           (delete-window)
           (when undo-tree-active
             (set-frame-width (selected-frame)
                              (- (frame-width) 63))
             (setq undo-tree-active nil))
           (when (< (frame-width) target-frame-width)
             (set-frame-width (selected-frame) target-frame-width)))
         (defun my:undo-tree-visualize ()
           (interactive)
           (when (and (not undo-tree-active) (not (eq buffer-undo-list t)))
             (set-frame-width (selected-frame)
                              (+ (frame-width) 63))
             (setq undo-tree-active t))
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

(with-eval-after-load "helm-config"
  (when (require 'auto-save-buffers nil t)
    (run-with-idle-timer
     1.6 t
     #'(lambda ()
         (cond ((equal major-mode 'undo-tree-visualizer-mode) nil)
               ((equal major-mode 'diff-mode) nil)
               ((string-match "Org Src" (buffer-name)) nil)
               (t (if (require 'shut-up nil t)
                      (shut-up (auto-save-buffers))
                    (auto-save-buffers))))))))

(when (eval-after-autoload-if-found
       '(backup-each-save) "backup-each-save" nil t nil
       '((setq backup-each-save-mirror-location "~/.emacs.d/backup")
         (setq backup-each-save-time-format "%y-%m-%d_%M:%S")
         (setq backup-each-save-size-limit 1048576)))

  ;; なぜか (backup-each-save) の直接呼び出しだとだめ
  (with-eval-after-load "helm-config"
    (require 'backup-each-save nil t))

  ;; %y-%m-%d_%M:%S で終わるファイルを本来のメジャーモードで開く
  (add-to-list 'auto-mode-alist '("-[0-9-]\\{8\\}_[0-9:]\\{5\\}$" nil t))

  (add-hook 'after-save-hook
            '(lambda () (unless (equal (buffer-name) "recentf")
                          (backup-each-save)))))

(when (eval-after-autoload-if-found
       '(recursive-delete-backup-files delete-backup-files) "utility" nil t)

  ;; backup-each-save が作るファイルのうち条件にあうものを終了時に削除
  (add-hook 'kill-emacs-hook
            #'(lambda () (recursive-delete-backup-files 7))))

(defun my:backup (file &optional dropbox)
  "Backup a file to `Dropbox/backup' directory. If `dropbox' option is provided then the value is uased as a root directory."
  (interactive "P")
  (let ((system (system-name))
        (rootdir (or dropbox "~/Dropbox")))
    (if (and system 
             (and (stringp rootdir) (stringp file))
             (file-directory-p (or rootdir (expand-file-name rootdir)))
             (file-readable-p (or file (expand-file-name file))))
        (shell-command-to-string
         (concat "cp -f " file " " rootdir "/backup/" system "/"))
      (message (format "--- backup failure: %s" file)))))

(defun my:backup-recentf ()
  (my:backup "~/.emacs.d/recentf"))

(run-with-idle-timer 600 t 'my:backup-recentf)

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

(when (eval-after-autoload-if-found
       '(neotree neotree-toggle) "neotree" nil t nil
       '((require 'frame-ctr nil t)
         (setq neo-show-hidden-files nil)
         (setq neo-persist-show t)
         (setq neo-theme 'arrow)
         (setq neo-smart-open t)
         (defun advice:neotree-show ()
           "Extension to support change frame width when opening neotree."
           (unless (neo-global--window-exists-p)
             (set-frame-width (selected-frame) (+ (frame-width) 26))
             (sit-for 0.01)
             (set-frame-width (selected-frame) (+ (frame-width) 2))))
         (advice-add 'neotree-show :before #'advice:neotree-show)
         (defun advice:neotree-hide ()
           "Extension to support change frame width when hiding neotree."
           (when (neo-global--window-exists-p)
             (set-frame-width (selected-frame) (- (frame-width) 2))
             (sit-for 0.01)
             (set-frame-width (selected-frame) (- (frame-width) 26)))
           (when (< (frame-width) target-frame-width)
             (set-frame-width (selected-frame) target-frame-width)))
         (advice-add 'neotree-hide :before #'advice:neotree-hide)
         (when neo-persist-show
           (add-hook 'popwin:before-popup-hook
                     #'(lambda () (setq neo-persist-show nil)))
           (add-hook 'popwin:after-popup-hook
                     #'(lambda () (setq neo-persist-show t))))
         ))
  (global-set-key (kbd "C-c n") 'neotree-toggle))

(with-eval-after-load "dired"
  (setq dired-use-ls-dired nil)
  (when (require 'osx-trash nil t)
    (setq delete-by-moving-to-trash t)
    (osx-trash-setup)))

(when (eval-after-autoload-if-found
       '(helpful-function) "helpful" nil t nil
       '((require 'helm-config nil t)))
  (global-set-key (kbd "C-h f") 'helpful-function))

(global-set-key (kbd "C-;") 'comment-dwim) ;; M-; is the defualt
(global-set-key (kbd "C-c c") 'compile)

(eval-after-autoload-if-found 'gist "gist" nil t)

(when (eval-after-autoload-if-found
       'doxymacs-mode "doxymacs" nil t nil
       '((setq doxymacs-doxygen-style "JavaDoc")
         (add-hook 'font-lock-mode-hook
                   '(lambda ()
                      (when (or (eq major-mode 'c-mode)
                                (eq major-mode 'c++-mode))
                        (doxymacs-font-lock))))
         (define-key doxymacs-mode-map (kbd "C-c C-s") 'ff-find-other-file)))

  (add-hook 'c-mode-common-hook 'doxymacs-mode))

(when (and (or (eq window-system 'ns)  (eq window-system 'mac))
           (> emacs-major-version 23))
  (when (eval-after-autoload-if-found
         '(matlab-mode matlab-shell) "matlab" nil t)
    (push '("\\.m$" . matlab-mode) auto-mode-alist)))

(when (eval-after-autoload-if-found
         '(flycheck-mode) "flycheck" nil t nil
         '(;;http://qiita.com/senda-akiha/items/cddb02cfdbc0c8c7bc2b
           (require 'helm-flycheck nil t)
           (setq flycheck-gcc-language-standard "c++11")
           (setq flycheck-clang-language-standard "c++11")
           (when (require 'flycheck-pos-tip nil t)
             '(custom-set-variables
               '(flycheck-display-errors-function
                 #'flycheck-pos-tip-error-messages)))
           ))

;; (flycheck-add-next-checker 'javascript-jshint
;; 'javascript-gjslint)

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
         (setq ac-auto-show-menu 2.0)
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
                              ac-source-abbrev ; Emacs の略語
                                   ;;; ac-source-css-property ; heavy
                              ac-source-dictionary ; 辞書
                              ac-source-features
                              ac-source-filename
                              ac-source-files-in-current-dir
                              ac-source-functions
                                   ;;; ac-source-gtags
                                   ;;; ac-source-imenu 
                                   ;;; ac-source-semantic
                              ac-source-symbols 
                              ac-source-variables
                                   ;;; ac-source-yasnippet
                              )))

         (defun ac-default-setup ()
           ;;            (message " >> ac-default-setup")
           ;; ac-source-words-in-same-mode-buffers
           (setq ac-sources '(ac-source-filename
                              ac-source-abbrev
                              ac-source-dictionary
                              )))
         ))

  (dolist (hook
           (list 'org-mode-hook 'python-mode-hook
                 'perl-mode-hook 'objc-mode-hook))
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
       '(origami-mode origami-toggle-node) "origami" nil t nil
       '((define-key origami-mode-map (kbd "C-t") #'origami-toggle-node)
         (define-key origami-mode-map (kbd "C-u C-t")
           #'origami-toggle-all-nodes)))
  (add-hook 'emacs-lisp-mode-hook #'origami-mode)
  (add-hook 'c-mode-common-hook #'origami-mode)
  (add-hook 'yatex-mode-hook #'origami-mode))

(when (eval-after-autoload-if-found '(quickrun) "quickrun" nil t)

  (add-hook 'c-mode-common-hook
            '(lambda () (define-key c++-mode-map (kbd "<f5>") 'quickrun)))
  (add-hook 'python-mode-hook
            '(lambda () (define-key python-mode-map (kbd "<f5>") 'quickrun)))
  (add-hook 'perl-mode-hook
            '(lambda () (define-key perl-mode-map (kbd "<f5>") 'quickrun)))
  (add-hook 'gnuplot-mode-hook
            '(lambda () (define-key gnuplot-mode-map (kbd "<f5>") 'quickrun)))
  )

(if (executable-find "gtags")
    (when (eval-after-autoload-if-found
           '(ggtags-mode) "ggtags" nil t nil
           '((setq ggtags-completing-read-function t)
             (define-key ggtags-mode-map (kbd "M-]") nil)
             ))
      (dolist (hook (list
                     'perl-mode-hook 'emacs-lisp-mode-hook 'js2-mode-hook
                     'python-mode-hook  'c-mode-common-hook))
        (add-hook hook (lambda () (ggtags-mode 1)))))
  (message "--- gtags is NOT installed in this system."))

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

         ;; org-clock の計測時間をモードラインではなくタイトルに表示する
         (setq org-clock-clocked-in-display 'frame-title)

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
        '(("FOCUS"    :foreground "#FF0000" :background "#FFCC66")
          ("BUG"      :foreground "#FF0000" :background "#FFCC66")
          ("CHECK"    :foreground "#FF9900" :background "#FFF0F0" :underline t)
          ("ICAL"     :foreground "#33CC66")
          ("APPROVED" :foreground "#66CC66")
          ("QUESTION" :foreground "#FF0000")
          ("WAIT"     :foreground "#CCCCCC" :background "#666666")
          ("EDIT"     :foreground "#FF33CC")
          ("READ"     :foreground "#9933CC")
          ("MAIL"     :foreground "#CC3300" :background "#FFEE99")
          ("PLAN"     :foreground "#FF6600")
          ("PLAN2"    :foreground "#FFFFFF" :background "#FF6600")
          ("REV1"     :foreground "#3366FF")
          ("REV2"     :foreground "#3366FF" :background "#99CCFF")
          ("REV3"     :foreground "#FFFFFF" :background "#3366FF")
          ("SLEEP"    :foreground "#9999CC")))

  ;; (:foreground "#0000FF" :bold t)     ; default. do NOT put this bottom    
  (setq org-tag-faces
        '(("Achievement" :foreground "#66CC66")
          ("Report"      :foreground "#66CC66")
          ("Background"  :foreground "#66CC99")
          ("Chore"       :foreground "#6699CC")
          ("Domestic"    :foreground "#6666CC")
          ("Doing"       :foreground "#FF0000")   
          ("Review"      :foreground "#6633CC")
          ("Revisit"     :foreground "#6633CC")
          ("Redmine"     :foreground "#CC6666")
          ("Ongoing"     :foreground "#CC6666") ; for non scheduled/reminder
          ("Repeat"      :foreground "#CC9999") ; for interval tasks
          ("Mag"         :foreground "#9966CC")
          ("buy"         :foreground "#9966CC")
          ("pay"         :foreground "#CC6699")
          ("secret"      :foreground "#FF0000")
          ("emacs"       :foreground "#6633CC")
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
          ("Blog"        :foreground "#9966CC")
          ("Test"        :foreground "#FF0000" :weight bold)
          ("Attach"      :foreground "#FF0000" :underline t :weight bold)
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
        '((sequence "TODO(t)" "PLAN(p)" "PLAN2(P)" "|" "DONE(d)")
          (sequence "READ(r)" "EDIT(e)" "|" "DONE(d)")
          (sequence "CHECK(C)" "FOCUS(f)" "ICAL(c)"  "|" "DONE(d)")
          (sequence "WAIT(w)" "SLEEP(s)" "QUESTION(q)" "|" "DONE(d)")
          (sequence "REV1(1)" "REV2(2)" "REV3(3)" "|" "APPROVED(a)")))

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
  (setq org-image-actual-width 256)
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
  (setq org-deadline-warning-days 0)
  ;; 時間幅が明示的に指定されない場合のデフォルト値（分指定）
  (setq org-agenda-default-appointment-duration 60)
  ;; アジェンダビューでFOLLOWを設定
  ;; (setq org-agenda-start-with-follow-mode t)
  ;; Customized Time Grid
  (setq org-agenda-time-grid
        '((daily today require-timed)
          "----------------"
          (0800 1000 1200 1400 1600 1800 2000 2200 2400)))
  (setq org-agenda-current-time-string "< d('- ' ) now!")
  (setq org-agenda-timegrid-use-ampm t)

  ;; アジェンダ作成対象（指定しないとagendaが生成されない）
  ;; ここを間違うと，MobileOrg, iCal export もうまくいかない
  (setq org-agenda-files
        '("~/Dropbox/org/org-ical.org" "~/Dropbox/org/next.org"
          "~/Dropbox/org/trigger.org" "~/Dropbox/org/wg1.org" 
          "~/Dropbox/org/work.org" "~/Dropbox/org/academic.org"))

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
          (org-back-to-heading t)
          ;; before 9
          ;; (unless (org-at-heading-p)
          ;;   (outline-previous-heading))
          (org-toggle-tag my:doing-tag
                          (if (string-match
                               (concat ":" my:doing-tag ":")
                               (org-get-tags-string))
                              'off 'on))
          (org-set-tags nil t)))
      (org-reveal)))

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
  (define-key org-mode-map (kbd "C-<f11>") 'my:sparse-doing-tree)

  ;; 所定の時刻に強制的にAgendaを表示
  (defvar my:org-agenda-auto-popup-list
    '("01:00" "11:00" "14:00" "17:00" "20:00" "23:00"))
  (defun my:popup-agenda ()
    (interactive)
    (let ((status use-dialog-box))
      (setq use-dialog-box nil)
      (when (y-or-n-p-with-timeout "Popup agenda now?" 10 nil)
        (org-agenda-list))
      (message "")
      (setq use-dialog-box status)))
  (defun my:popup-agenda-set-timers ()
    (interactive)
    (cancel-function-timers 'my:popup-agenda)
    (dolist (triger my:org-agenda-auto-popup-list)
      (unless (passed-clock-p triger)
        (run-at-time triger nil 'my:popup-agenda))))
  (my:popup-agenda-set-timers)
  (run-at-time "24:00" nil 'my:popup-agenda-set-timers) ;; for next day

  ;; ついでに calendar.app も定期的に強制起動する
  (defun my:popup-calendar ()
    (interactive)
    (if (window-focus-p)
        (shell-command-to-string "open -a calendar.app")
      (message "--- input focus is OUT now.")))
  (defun my:popup-calendar-set-timers ()
    (interactive)
    (cancel-function-timers 'my:popup-calendar)
    (dolist (triger my:org-agenda-auto-popup-list)
      (unless (passed-clock-p triger)
        (run-at-time triger nil 'my:popup-calendar))))
  (when (or (eq window-system 'ns) (eq window-system 'mac))
    (my:popup-calendar-set-timers)
    (run-at-time "24:00" nil 'my:popup-calendar-set-timers))) ;; for next day

(with-eval-after-load "org"
  (when (library-p "orgbox")
    (require 'orgbox nil t)))

(with-eval-after-load "org-agenda"
  (when (require 'org-review nil t)
    (add-to-list 'org-agenda-custom-commands
                 '("r" "Review projects" tags-todo "-CANCELLED/"
                   ((org-agenda-overriding-header "Reviews Scheduled")
                    (org-agenda-skip-function 'org-review-agenda-skip)
                    (org-agenda-cmp-user-defined 'org-review-compare)
                    (org-agenda-sorting-strategy '(user-defined-down)))))
    (org-defkey org-agenda-mode-map "\C-c\C-r"
                'org-review-insert-last-review)))

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
  ;; org-agenda の内容をアラームに登録する
  ;; 定期的に更新する
  (defun my:org-agenda-to-appt ()
    (interactive)
    (org-agenda-to-appt t '((headline "TODO"))))
  (run-with-idle-timer 500 t 'my:org-agenda-to-appt)

  (define-key org-mode-map (kbd "C-c f 3") 'my:org-agenda-to-appt))

(when
    (eval-after-autoload-if-found
     '(org-capture) "org-capture" nil t nil
     '(;; 2010-06-13 の形式では，タグとして認識されない
       (defun get-current-date-tags () (format-time-string "%Y%m%d"))
       (setq org-default-notes-file (concat org-directory "next.org"))
       (defvar org-capture-words-notes-file (concat org-directory "words.org"))
       (defvar org-capture-notes-file (concat org-directory "note.org"))
       (defvar org-capture-academic-file (concat org-directory "academic.org"))
       (defvar org-capture-buffer-file (concat org-directory "buffer.org"))
       (defvar org-capture-today-file (concat org-directory "trigger.org"))
       (defvar org-capture-ical-file (concat org-directory "org-ical.org"))
       (defvar org-capture-article-file (concat org-directory "article.org"))

       ;; see org.pdf:p73
       (setq org-capture-templates
             `(("t" "TODO 項目を INBOX に貼り付ける" entry
                (file+headline ,org-default-notes-file "INBOX") "** TODO %?\n\t")
               ("a" "記事リストにエントリー" entry
                (file+headline ,org-capture-article-file "INBOX")
                "** READ %?\n\t") 
               ("c" "同期カレンダーにエントリー" entry
                (file+headline ,org-capture-ical-file "Scheduled")
                "** TODO %?\n\t") 
               ("d" "Doingタグ付きのタスクをInboxに投げる" entry
                (file+headline ,org-default-notes-file "INBOX")
                "** TODO %? :Doing:\n  - \n")
               ("l" "本日のチェックリスト" entry
                (file+headline ,org-capture-today-file "Today")
                "** FOCUS 本日のチェックリスト %T\n（起床時間の記録）[[http://www.hayaoki-seikatsu.com/users/takaxp/][早起き日記]] \n（朝食）\n  - [ ] %?\n（昼食）\n（帰宅／夕食）\n----\n（研究速報）\n  - [ ] \n")
               ("i" "アイディアを書き込む" entry (file+headline ,org-default-notes-file "INBOX")
                "** %?\n  - \n\t%U")
               ("b" "Bug タグ付きの TODO 項目を貼り付ける" entry
                (file+headline ,org-default-notes-file "INBOX")
                "** TODO %? :bug:\n %i\n %a %t")
               ("w" ,(concat "英単語を " org-capture-words-notes-file
                             " に書き込む") entry
                             (file+headline ,org-capture-words-notes-file "WORDS")
                             "** %? :%(get-current-date-tags):\n「」\n  - ")
               ("g" ,(concat "英語ノートを " org-capture-words-notes-file
                             " に書き込む")
                entry (file+headline ,org-capture-words-notes-file "GRAMMER")
                "** %? :%(get-current-date-tags):\n\n%U")
               ("T" "時間付きエントリー" entry (file+headline ,org-default-notes-file "INBOX")
                "** %? %T--\n")
               ("n" "ノートとしてINBOXに貼り付ける" entry
                (file+headline ,org-default-notes-file "INBOX")
                "** %? :note:\n\t%U")
               ("D" "「ドラッカー365の金言」をノートする" entry
                (file+headline ,org-capture-notes-file "The Daily Drucker")
                "** 「%?」\nDrucker) \n  - \n  - \nACTION POINT:\n  - \nQUESTION:\n  - \n")
               ("r" ,(concat "研究ノートを " org-capture-academic-file
                             " に書き込む")
                entry (file+headline ,org-capture-academic-file "Survey")
                "** %? :note:\n# \n  - \n\t%U")
               ("`" ,(concat "ノートをバッファ " org-capture-buffer-file
                             " に書き込む")
                entry (file+headline ,org-capture-buffer-file "Buffers")
                "** %(get-random-string 16) %U\n\n%?\n\n----"))))))

(with-eval-after-load "org"
  ;; 履歴が生成されるのを抑制．
  ;; [2/3]のような完了数が見出しにある時に転送先候補が重複するため．

  (defun advice:org-refile (&optional arg default-buffer rfloc msg)
    "Extension to support keeping org-refile-history empty."
    (let ((l (org-outline-level))
          (b (buffer-name)))
      (apply arg default-buffer rfloc msg)
      (save-excursion
        (save-restriction
          (if (> l (org-outline-level))
              (outline-backward-same-level 1)
            (outline-up-heading 1))
          (org-update-statistics-cookies nil) ;; Update in source       
          (org-refile-goto-last-stored)
          (org-update-parent-todo-statistics) ;; Update in destination
          (unless (equal b (buffer-name))
            (switch-to-buffer b))))
      )
    (setq org-refile-history nil)
    (org-refile-cache-clear))
  (advice-add 'org-refile :around #'advice:org-refile)

  (setq org-refile-targets
        (quote (("org-ical.org" :level . 1)
                ("academic.org" :level . 1)
                ("maybe.org" :level . 1)
                ("article.org" :level . 1)
                ("work.org" :level . 1)
                ("english.org" :level . 1)
                ("wg1.org" :level . 1)
                ("univ.org" :level . 1)
                ("trigger.org" :level . 1)
                ("next.org" :level . 1)
                ("sleep.org" :level . 1))))
  )

(with-eval-after-load "org"
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  ;; org-src-window-setup (current-window, other-window, other-frame)
  (setq org-src-window-setup 'current-window)
  (require 'ob-http nil t)
  (require 'ob-gnuplot nil t)
  (require 'ox-qmd nil t) ;; Quita-style
  (require 'ox-gfm nil t) ;; Github-style
  ;; Add ":results output" after program name
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (C . t)
     (gnuplot . t)
     (perl . t)
     (shell . t)
     (latex . t)
     (sqlite . t)
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
       '(org-tree-slide my:proportional-font-toggle) "org-tree-slide" nil t nil
       '((defcustom use-proportional-font nil
           "The status of FONT property"
           :type 'boolean
           :group 'org-mode)

         (set-face-attribute 'variable-pitch nil
                             :family "Verdana"
                             ;; :family "Comic Sans MS"
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
 '(org-tree-slide-mode) "org-tree-slide" nil t nil
 '(;; FIXME 複数のバッファで並行動作させるとおかしくなる．hide-lines の問題？
   (when (and nil (require 'hide-lines nil t))
     (defvar my:org-src-block-faces nil)
     (defun my:show-headers ()
       (setq org-src-block-faces 'my:org-src-block-faces)
       (hide-lines-show-all))
     (defun my:hide-headers ()
       (setq my:org-src-block-faces 'org-src-block-faces)
       (setq org-src-block-faces
             '(("emacs-lisp" (:background "cornsilk"))))
       (hide-lines-matching "#\\+BEGIN_SRC")
       (hide-lines-matching "#\\+END_SRC"))
     (add-hook 'org-tree-slide-play-hook 'my:hide-headers)
     (add-hook 'org-tree-slide-stop-hook 'my:show-headers)

     (defun advice:org-edit-src-code (&optional code edit-buffer-name)
       (interactive)
       (my:show-headers))
     (advice-add 'org-edit-src-code :before #'advice:org-edit-src-code)
     ;; Block 外で呼ばれると，my:show-headers が呼ばれてしまう
     (defun advice:org-edit-src-exit ()
       (interactive)
       (my:hide-headers))
     (advice-add 'org-edit-src-exit :after #'advice:org-edit-src-exit))))

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

  (global-set-key (kbd "C-c f c w") 'my:cfw-open-org-calendar))

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
 '((add-to-list 'org-odt-data-dir 
                (concat (getenv "HOME") "/Dropbox/emacs.d/config/"))
   (setq org-odt-styles-file
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

(when (eval-after-autoload-if-found
       '(org-twbs-export-to-html org-publish-all) "ox-twbs" nil t nil
       '(
         ))
  (with-eval-after-load "org"
    (require 'ox-twbs nil t)))

(with-eval-after-load "org"
  (when (require 'org-crypt nil t)
    (setq org-crypt-key "<insert your key>")
    ;; org-encrypt-entries の影響を受けるタグを指定
    (setq org-tags-exclude-from-inheritance (quote ("secret")))
    ;; 自動保存の確認を無効に
    (setq org-crypt-disable-auto-save 'nil)
    (define-key org-mode-map (kbd "C-c f l") 'org-encrypt-entry)
    (define-key org-mode-map (kbd "C-c f u") 'org-decrypt-entry)))

(with-eval-after-load "org"
  (add-to-list 'org-modules 'org-mac-iCal)
  (add-to-list 'org-modules 'org-mac-link) ;; includes org-mac-message
  (define-key org-mode-map (kbd "C-c c") 'org-mac-grab-link))

(with-eval-after-load "org-mac-link"

  (defcustom org-mac-grab-Papers-app-p t
    "Add menu option [P]apers to grab links from the Papers.app."
    :tag "Grab Papers.app links"
    :group 'org-mac-link
    :type 'boolean)

  (defun org-mac-papers-insert-frontmost-paper-link ()
    (interactive)
    (let ((result (org-mac-papers-get-frontmost-paper-link)))
      (if result
          (insert result)
        (message "Please open Papers.app and select a paper."))))

  (defun org-mac-papers-get-frontmost-paper-link ()
    (interactive)
    (message "Applescript: Getting Papers link...")
    (let ((result (org-as-mac-papers-get-paper-link)))
      (if (or (eq result nil) (string= result ""))
          nil        
        (org-mac-paste-applescript-links result))))

  (defun org-as-mac-papers-get-paper-link ()
    (do-applescript
     (concat
      "if application \"Papers\" is running then\n"
      "	tell application \"Papers\" to activate\n"
      "	delay 0.3\n"
      "	set the clipboard to \"\"\n"
      "	tell application \"System Events\" to tell process \"Papers\"\n"
      "		keystroke \"l\" using {command down, shift down}\n"
      "	end tell\n"
      "	delay 0.2\n"
      "	set aLink to the clipboard\n"
      "	tell application \"System Events\" to tell process \"Papers\"\n"
      ;; "		keystroke \"c\" using {command down, alt down\}\n"
      "		keystroke \"m\" using {command down, option down\}\n"
      "	end tell\n"
      "	delay 0.2\n"
      "	set aName to the clipboard\n"
      "	tell application \"Emacs\" to activate\n"
      "	return (get aLink) & \"::split::\" & (get aName) as string\n"
      "else\n"
      "	return\n"
      "end if\n"))

    (add-to-list 'org-mac-link-descriptors
                 `("P" "apers" org-mac-papers-insert-frontmost-paper-link
                   ,org-mac-grab-Papers-app-p) t)))

(with-eval-after-load "org"
  (when (eq system-type 'darwin)
    ;; Open `papers3://' link by C-c C-o.
    ;; (org-add-link-type will be obsoleted from Org 9.
    (org-link-set-parameters
     "papers3"
     :follow (lambda (path)
               (let ((cmd (concat "open papers3:" path)))
                 (shell-command-to-string cmd)
                 (message "%s" cmd))))))

(eval-after-autoload-if-found
 '(org-attach du-org-attachments) "org-attach" nil t nil
 '(;; org-insert-link で添付ファイルへのリンクをペーストできるようにする
   (setq org-attach-store-link-p t)
   ;; 自動付与されるタグの名前を変更
   (setq org-attach-auto-tag "Attach")
   ;; git-annex を使用しない
   (setq org-attach-git-annex-cutoff nil)

   (defvar org-attach-directory-absolute
     (concat (getenv "HOME")
             "/Dropbox/org/"
             (if (boundp 'org-attach-directory)
                 "data/")))

   (defun chomp (str)
     "Chomp leading and tailing whitespace from STR."
     (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                          str)
       (setq str (replace-match "" t t str)))
     str)

   (defun du-org-attachments ()
     "Show directory size for org-attachments."
     (interactive)
     (message "--- %s"
              (chomp (shell-command-to-string
                      (concat "/usr/bin/du -sh "
                              org-attach-directory-absolute)))))
   ))

(when (eval-after-autoload-if-found
       '(org-attach-screenshot my:org-attach-screenshot)
       "org-attach-screenshot" nil t nil
       '((when (executable-find "screencapture")
           (setq org-attach-screenshot-command-line "screencapture %f"))
         (defun my:org-attach-screenshot ()
           (interactive)
           (org-attach-screenshot t ""))))
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c f <f12>") 'my:org-attach-screenshot)))

(with-eval-after-load "helm-config"
  (defvar terminal-notifier-command
    (executable-find
     "terminal-notifier.app/Contents/MacOS/terminal-notifier")
    "Path to terminal-notifier.
   Download from https://github.com/julienXX/terminal-notifier/releases")

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

  (with-eval-after-load "org"
    (defvar terminal-notifier-sound nil)
    (with-eval-after-load "org-clock"
      (when (not org-clock-sound)
        ;; Select from Preferences: { Funk | Glass | ... | Purr | Pop ... }
        (setq terminal-notifier-sound "Pop"))) 

    (defvar use-terminal-notifier t)
    (defun terminal-notifier-toggle ()
      (interactive)
      (setq use-terminal-notifier (not use-terminal-notifier))
      (my:org-agenda-to-appt)
      (message "terminal-notifier: %s" use-terminal-notifier))

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
    (advice-add 'appt-disp-window :before #'advice:appt-disp-window)))

(when (eval-after-autoload-if-found
       '(org-grep) "org-grep" nil t nil
       '(
         (setq org-grep-extensions '(".org" ".org_archive"))
         (add-to-list 'org-grep-directories "~/.emacs.d")
         (add-to-list 'org-grep-directories "~/.emacs.d/.cask/package")

         ;; "q"押下の挙動を調整
         (defun org-grep-quit ()
           (interactive)
           (delete-window))

         ;; for MacOSX
         (when (or (eq window-system 'ns) (eq window-system 'mac))
           (defun org-grep-from-org-shell-command (regexp)
             (if org-grep-directories
                 (concat "find -E "
                         (if org-grep-directories
                             (mapconcat #'identity org-grep-directories " ")
                           org-directory)
                         (and org-grep-extensions
                              (concat " -regex '.*("
                                      (mapconcat #'regexp-quote org-grep-extensions "|")
                                      ")$'"))
                         " -print0 | xargs -0 grep " org-grep-grep-options
                         " -n -- " (shell-quote-argument regexp))
               ":")))
         ))

  (global-set-key (kbd "C-M-g") 'org-grep))

(with-eval-after-load "org"
  (require 'ox-reveal nil t))

(when (eval-after-autoload-if-found
       'org-mode "org" nil t)
  (push '("[rR][eE][aA][dD][mM][eE]" . org-mode) auto-mode-alist))

(with-eval-after-load "utility"
  (set-alarms-from-file "~/Dropbox/org/trigger.org"))

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
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c r") 'org-capture))

(eval-after-autoload-if-found 'org-dashboard-display "org-dashboard" nil t)

(with-eval-after-load "org"
  (when (and (require 'org-clock-today nil t)
             (fboundp 'org-clock-today-mode))
    (add-hook 'org-mode-hook #'(lambda () (org-clock-today-mode 1)))))

(with-eval-after-load "org"
  (when (require 'org-recent-headings nil t)
    (define-key org-mode-map (kbd "C-c f r") 'org-recent-headings-helm)
    (setq org-recent-headings-save-file "~/.emacs.d/org-recent-headings.dat")
    (if (require 'shut-up nil t)
        (shut-up (org-recent-headings-mode 1))
      (org-recent-headings-mode 1))))

(when (eval-after-autoload-if-found '(orgnav-search-root) "orgnav" nil t)
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c f n")
      #'(lambda () (interactive)
          (orgnav-search-root 3 'orgnav--goto-action)))))

;; To avoid an error setting up the frame width (only for Emacs23)
                                        ;(set-frame-width (selected-frame) 81)
                                        ;(set-frame-width (selected-frame) 80)
;; Default window position to show a Emacs frame
;; Dynabook UX: top=0, left=0, width=80, height=32
(cond
 ((or (eq window-system 'ns) (eq window-system 'mac)) ; for Macintosh
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
(set-face-foreground 'vertical-border (face-background 'default))
(set-face-background 'vertical-border (face-background 'default))
(set-face-background 'fringe (face-background 'default))

;; カーソルの色
(defvar my:cursor-color-ime-on "#FF9300")
(defvar my:cursor-color-ime-off "#91C3FF") ;; #FF9300, #999999, #749CCC
(defvar my:cursor-type-ime-on '(bar . 2)) ;; box
(defvar my:cursor-type-ime-off '(bar . 2))
(setq blink-cursor-blinks 0)
(setq blink-cursor-interval 0.3)
(setq blink-cursor-delay 16)
(blink-cursor-mode 1)

(cond
 ((eq window-system 'ns)
  (when (fboundp 'mac-set-input-method-parameter)
    (mac-set-input-method-parameter
     "com.google.inputmethod.Japanese.base" 'title "あ"))

  (when (fboundp 'mac-get-current-input-source)
    (if (string= (mac-get-current-input-source)
                 "com.google.inputmethod.Japanese.base")
        (progn
          (add-to-list 'default-frame-alist 
                       `(cursor-type . ,my:cursor-type-ime-on))
          (set-cursor-color my:cursor-color-ime-on))
      (progn
        (add-to-list 'default-frame-alist
                     `(cursor-type . ,my:cursor-type-ime-off))
        (set-cursor-color my:cursor-color-ime-off))))

  ;; http://tezfm.blogspot.jp/2009/11/cocoa-emacs.html
  ;; バッファ切替時に input method を切り替える
  (when (and (fboundp 'mac-handle-input-method-change)
             (require 'cl nil t))
    (add-hook
     'post-command-hook
     (lexical-let ((previous-buffer nil))
       #'(lambda ()
           (unless (eq (current-buffer) previous-buffer)
             ;; (message "Change IM %S -> %S" previous-buffer (current-buffer))
             (if (bufferp previous-buffer) (mac-handle-input-method-change))
             (setq previous-buffer (current-buffer))))))))

 ((eq window-system 'mac) ;; EMP: Emacs Mac Port
  (when (fboundp 'mac-input-source)
    (defun my:mac-keyboard-input-source ()
      (if (string-match "\\.Roman$" (mac-input-source))
          (progn
            (setq cursor-type my:cursor-type-ime-off)
            (add-to-list 'default-frame-alist
                         `(cursor-type . ,my:cursor-type-ime-off))
            (set-cursor-color my:cursor-color-ime-off))
        (progn
          (setq cursor-type my:cursor-type-ime-on)
          (add-to-list 'default-frame-alist
                       `(cursor-type . ,my:cursor-type-ime-on))
          (set-cursor-color my:cursor-color-ime-on))))

    (when (fboundp 'mac-auto-ascii-mode)
      ;; (mac-auto-ascii-mode 1)
      ;; IME ON/OFF でカーソルの種別や色を替える
      (add-hook 'mac-selected-keyboard-input-source-change-hook
                'my:mac-keyboard-input-source)
      ;; IME ON の英語入力＋決定後でもカーソルの種別や色を替える
      ;; (add-hook 'mac-enabled-keyboard-input-sources-change-hook
      ;;           'my:mac-keyboard-input-source)

      (declare-function my:mac-keyboard-input-source "init" nil)
      (my:mac-keyboard-input-source))))
 (t nil))

(eval-after-autoload-if-found 
 '(my:e2wm:dp-two e2wm:dp-two e2wm:start-management) "e2wm" nil t nil
 '((defun my:e2wm:dp-two ()
     (interactive)
     (e2wm:dp-two)
     (setq e2wm:c-two-recipe
           '(- (:lower-size 10)
               (| left right)
               sub))
     (setq e2wm:c-two-winfo
           '((:name left )
             (:name right )
             (:name sub :default-hide t)))

     ;; left, prev
     (setq e2wm:c-two-right-default 'left))

   ;; 高さを画面の最大に矯正
   (when (require 'frame-ctr nil t)
     (setq frame-height-tall (max-frame-height))
     (setq frame-height-small frame-height-tall))

   ;; 幅を画面の最大に矯正
   (add-hook 'e2wm:pre-start-hook
             #'(lambda ()
                 (set-frame-width
                  (selected-frame)
                  (/ (- (display-pixel-width) 30) (frame-char-width)))))

   ;; 幅をデフォルト値に戻す
   (add-hook 'e2wm:post-stop-hook
             #'(lambda () (set-frame-width (selected-frame) target-frame-width)))

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
                   'make-frame-height-ring)
         (add-hook 'frame-ctr-after-fullscreen-hook
                   'move-frame-to-edge-top)))

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
  ;; Full screen with same frame-width
  (global-set-key (kbd "C-x C-9") 'fit-frame-to-fullscreen)
  (global-set-key (kbd "C-x C-0") 'reset-font-size)
  (global-set-key (kbd "C-_") 'text-scale-decrease)
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C-=")
                  '(lambda () (interactive)
                     (increase-font-size 1)
                     (move-frame-to-edge-top)
                     (frame-ctr-print-status)))
  (global-set-key (kbd "C--")
                  '(lambda () (interactive) 
                     (decrease-font-size 2)
                     (move-frame-to-edge-top)
                     (frame-ctr-print-status))))

;; setting for e2wm
(when (eval-after-autoload-if-found
       '(change-frame-width-single
         change-frame-width-double change-frame-double-window
         change-frame-single-window) "frame-ctr-e2wm" nil t)

  ;; Set the frame width single size
  ;;  C-u C-x - => e2wm OFF, single size width and double height, move center
  (global-set-key (kbd "C-x -") 'change-frame-single-window)
  ;; Set the frame width double size
  ;;  C-u C-x = => e2wm ON, double size width and height, move to the center
  (global-set-key (kbd "C-x =") 'change-frame-double-window)
  (global-set-key (kbd "C-c f s") 'change-frame-width-single)
  (global-set-key (kbd "C-c f d") 'change-frame-width-double))

;; for emacs 24.1
;; (setq special-display-function 'popwin:special-display-popup-window)
;; (setq display-buffer-function 'popwin:display-buffer)
;; for emacs 24.3
;; (setq special-display-alist 'popwin:special-display-popup-window)
;; (setq display-buffer-alist 'popwin:display-buffer)
;; (push '("*sdic*" :position top) popwin:special-display-config)

(with-eval-after-load "helm-config"
  (when (require 'popwin nil t)
    (popwin-mode 1)
    ;; Performed
    (push '("*Help*" :height 20 :position bottom :dedicated t)
          popwin:special-display-config)
    (push '("*osx-dictionary*" :height 20 :position top)
          popwin:special-display-config)

    ;; Checking...

    (push '("CAPTURE-next.org" :height 10 :position bottom :noselect t)
          popwin:special-display-config)
    (push '("CAPTURE-org-ical.org":height 10 :position bottom :noselect t)
          popwin:special-display-config)
    (push '("dict-app-result" :height 20 :position bottom)
          popwin:special-display-config)
    (push '("*timer*" :height 20 :position bottom :dedicated t)
          popwin:special-display-config)
    (push '("Calendar" :position top :dedicated t)
          popwin:special-display-config)
    (push '("*wclock*" :height 10 :position bottom :dedicated t)
          popwin:special-display-config)
    (push '("*Org Dashboard*" :position bottom)
          popwin:special-display-config)
    (push '("*Org Select*" :height 10 :position bottom)
          popwin:special-display-config)
    (push '("*Org Grep*" :height 20 :position bottom :dedicated t)
          popwin:special-display-config)
    (push '("*Occur*" :height 10 :position bottom)
          popwin:special-display-config)
    (push '("*Shell Command Output*" :height 10 :position bottom :dedicated t)
          popwin:special-display-config)
    (push '("*eshell*" :height 10 :position bottom)
          popwin:special-display-config)
;;;            (undo-tree-visualizer-buffer-name :height 10 :position top)
;;;            (undo-tree-diff-buffer-name :height 20 :position bottom)

    ;; Not performed
    (push '("*Org Agenda*" :height 10 :position top)
          popwin:special-display-config)

    ))

(set-face-foreground 'font-lock-regexp-grouping-backslash "#66CC99")
(set-face-foreground 'font-lock-regexp-grouping-construct "#9966CC")

(with-eval-after-load "helm-config"
  (require 'generic-x nil t))

;; Color of the current line
;; Cite: http://murakan.cocolog-nifty.com/blog/2009/01/emacs-tips-1d45.html
;; see also http://www.emacswiki.org/cgi-bin/emacs/highlight-current-line.el
(when window-system
  (global-hl-line-mode t)
  ;; (set-face-background 'hl-line "#DEEDFF")
  (custom-set-faces
   '(hl-line
     ((((background dark)) :background "#484c5c")
      (t (:background "#DEEDFF"))))))

(when (and (eq window-system 'ns) (>= emacs-major-version 24))

  (add-hook 'input-method-activate-hook
            (lambda ()
              (setq cursor-type my:cursor-type-ime-on)
              (set-cursor-color my:cursor-color-ime-on)
              ))

  (add-hook 'input-method-inactivate-hook
            (lambda ()
              (setq cursor-type my:cursor-type-ime-off)
              (set-cursor-color my:cursor-color-ime-off))))

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
 ((or (eq window-system 'ns) (eq window-system 'mac))
  (when (>= emacs-major-version 23)
    ;; 1) Monaco, Hiragino/Migu 2M : font-size=12, -apple-hiragino=1.2
    ;; 2) Inconsolata, Migu 2M     : font-size=14, 
    ;; 3) Inconsolata, Hiragino    : font-size=14, -apple-hiragino=1.0
    (let
        ;; Fonts
        ((font-size my:font-size)
         ;; ((font-size 28) ; for mirroring presentation (1440x900)
         ;; (ascii-font "Inconsolata")
         (ascii-font "Monaco")
         (ja-font "Migu 2M")) ;; "Hiragino Maru Gothic Pro"
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
    ))

 ((eq window-system 'ns)
  ;; Anti aliasing with Quartz 2D
  (when (boundp 'mac-allow-anti-aliasing)
    (setq mac-allow-anti-aliasing t)))

 ((eq window-system 'w32) ; windows7
  (let ((font-size 14)
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
  ;; (when (require 'gotham-theme nil t)
  ;; (load-theme 'gotham t))
  ;; (when (require 'material-theme nil t)
  ;;   (load-theme 'material t)
  (when (require 'night-theme nil t)
    (load-theme 'night t)
    ;; (setq my:cursor-color-ime-on "#8599ff")
    ))

(when (eval-after-autoload-if-found
       '(rainbow-mode) "rainbow-mode" nil t)
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
           (when window-system
             (my:vhl-change-color)))
         (global-set-key (kbd "M-v") 'my:yank)
         (global-set-key (kbd "C-y") 'my:yank)

         (with-eval-after-load "org"
           (defun my:org-yank ()
             (interactive)
             (org-yank)
             (when window-system
               (my:vhl-change-color)))
           (define-key org-mode-map (kbd "C-y") 'my:org-yank))))


  (add-hook 'org-mode-hook 'volatile-highlights-mode)
  (add-hook 'emacs-lisp-mode-hook 'volatile-highlights-mode)
  (add-hook 'emmet-mode-hook 'volatile-highlights-mode))

(with-eval-after-load "org"
  (global-set-key (kbd "C-c f t") 'open-current-directory))

(when (eval-after-autoload-if-found
       '(google-this google-this-word) "google-this" nil t)
  (defun my:google-this ()
    (interactive)
    (google-this (current-word) t))
  (global-set-key (kbd "C-c f g") 'my:google-this))

(when (eval-after-autoload-if-found
       '(lingr-login my:lingr-login) "lingr" nil t nil
       '((setq lingr-icon-mode t)
         (setq lingr-icon-fix-size 24)
         (setq lingr-image-convert-program "/usr/local/bin/convert")
         (defun my:lingr-login ()
           (when (string= "Sat" (format-time-string "%a"))
             (lingr-login)))
         ))

  (unless (passed-clock-p "22:00")
    (run-at-time "22:00" nil 'my:lingr-login)))

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
    my:browse-url-chrome count-words-buffer do-test-applescript
    delete-backup-files recursive-delete-backup-files describe-timer))

(when (eval-after-autoload-if-found utility-autoloads "utility" nil t)
  (global-set-key (kbd "<f12>") 'takaxp:open-file-ring)
  (global-set-key (kbd "C-c t") 'takaxp:date)
  (global-set-key (kbd "C-c f 4") 'takaxp:window-resizer))

(if (not
     (and (executable-find "pass")
          (library-p '("helm-pass" "helm-config"))))
    (message "--- pass is NOT installed.")
  (when (eval-after-autoload-if-found 
         'helm-pass "helm-pass" nil t nil
         '((require 'helm-config nil t)))
    (global-set-key (kbd "C-c f p") 'helm-pass)))

(eval-after-autoload-if-found
 '(multi-term) "multi-term" nil t nil
 '((setenv "HOSTTYPE" "intel-mac")
   (my:night-theme)))

(eval-after-autoload-if-found
 '(osx-lib-say osx-lib-say-region) "osx-lib" nil t nil
 '((setq osx-lib-say-ratio 100)
   (setq osx-lib-say-voice "Samantha"))) ;; Alex

(provide 'init)
