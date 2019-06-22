;; -*- lexical-binding: t -*-

(defun ad:emacs-init-time ()
  "Return a string giving the duration of the Emacs initialization."
  (interactive)
  (let ((str
         (format "%.3f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time)))))
    (if (called-interactively-p 'interactive)
        (message "%s" str)
      str)))

(advice-add 'emacs-init-time :override #'ad:emacs-init-time)

(defun future-time-p (time)
  "Return non-nil if provided TIME formed of \"10:00\" is the future time."
  (not (time-less-p
        (apply 'encode-time
               (let ((t1 (decode-time))
                     (t2 (parse-time-string time)))
                 (setf (nth 0 t1) 0)
                 (setf (nth 1 t1) (nth 1 t2))
                 (setf (nth 2 t1) (nth 2 t2))
                 t1))
        (current-time))))
;; (when (future-time-p "10:00") (run-at-time...))

(autoload-if-found
 '(fancy-narrow-to-region
   fancy-widen
   org-fancy-narrow-to-block
   org-fancy-narrow-to-element
   org-fancy-narrow-to-subtree)
 "fancy-narrow" nil t)

(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

(setq mouse-drag-copy-region t)

(setq compilation-scroll-output t)

(setq confirm-kill-emacs 'y-or-n-p)

(when (autoload-if-found '(cask-mode) "cask-mode" nil t)
  (push '("/Cask\\'" . cask-mode) auto-mode-alist))

(when (autoload-if-found
       '(paradox-list-packages
         my-list-packages my-setup-cask
         my-reset-load-path
         advice:paradox-quit-and-close)
       "paradox" nil t)

  (with-eval-after-load "paradox"
    (defvar my-default-load-path nil)
    (defun my-list-packages ()
      "Call paradox-list-packages if available instead of list-packages."
      (interactive)
      (setq my-default-load-path load-path)
      (my-setup-cask)
      (if (fboundp 'paradox-list-packages)
          (paradox-list-packages nil)
        (list-packages nil)))

    (defun my-reset-load-path ()
      "Revert `load-path' to `my-default-load-path'."
      (shell-command-to-string "~/Dropbox/emacs.d/bin/update-cask.sh link")
      (setq load-path my-default-load-path)
      (message "--- Reverted to the original `load-path'."))

    ;; (declare-function advice:paradox-quit-and-close "init" (kill))

    (when (and (fboundp 'cask-load-path)
               (fboundp 'cask-initialize))
      (defun my-setup-cask ()
        "Override `load-path' to use cask."
        (when (or (require 'cask "/usr/local/opt/cask/cask.el" t)
                  (require 'cask "~/.cask/cask.el" t))
          (setq load-path (cask-load-path (cask-initialize))))))

    (defun advice:paradox-quit-and-close (_kill)
      (my-reset-load-path))
    (advice-add 'paradox-quit-and-close :after
                #'advice:paradox-quit-and-close)

    (custom-set-variables
     '(paradox-github-token t))

    (unless noninteractive
      (when (fboundp 'paradox-enable)
        (paradox-enable)))))

(setq vc-follow-symlinks t)

(unless noninteractive
  (global-auto-revert-mode 1))

(when (fboundp 'pixel-scroll-mode)
  (pixel-scroll-mode 1)) ;; 26.1

(when (autoload-if-found
       '(aggressive-indent-mode)
       "aggressive-indent" nil t)

  (dolist (hook
           '(;; python-mode-hook
             ;; nxml-mode-hook
             ;; web-mode-hook
             emacs-lisp-mode-hook
             lisp-mode-hook perl-mode-hook c-mode-common-hook))

    (add-hook hook #'aggressive-indent-mode)))

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(when (autoload-if-found
       '(ws-butler-mode ws-butler-global-mode)
       "ws-butler" nil t)

  (with-eval-after-load "ws-butler"
    (custom-set-variables
     '(ws-butler-global-exempt-modes
       (append '(org-mode empty-booting-mode)
               ws-butler-global-exempt-modes))))

  (unless noninteractive
    (ws-butler-global-mode)))

(if (executable-find "ag")
    (when (autoload-if-found
           '(my-ag ag)
           "ag" nil t)

      (autoload-if-found '(helm-ag) "helm-ag" nil t)
      (global-set-key (kbd "C-M-f") 'my-ag)

      (with-eval-after-load "ag"
        (custom-set-variables
         '(ag-highlight-search t)
         '(ag-reuse-buffers t)		;; nil: 別ウィンドウが開く
         '(ag-reuse-window nil))	;; nil: 結果を選択時に別ウィンドウに結果を出す

        ;; q でウィンドウを抜ける
        ;; (define-key ag-mode-map (kbd "q") 'delete-window)
        (defun my-ag ()
          "Switch to search result."
          (interactive)
          (call-interactively 'ag)
          (switch-to-buffer-other-frame "*ag search*"))))

  (unless noninteractive
    (message "--- ag is NOT installed in this system.")))

(global-set-key (kbd "C-M-t") 'beginning-of-buffer)
(global-set-key (kbd "C-M-b") 'end-of-buffer)
;; Backward page scrolling instead of M-v
(global-set-key (kbd "M-p") 'scroll-down)
;; Frontward page scrolling instead of C-v
(global-set-key (kbd "M-n") 'scroll-up)
;; Move cursor to a specific line
(global-set-key (kbd "C-c g") 'goto-line)

(global-set-key (kbd "C-M-p") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-M-n") (lambda () (interactive) (other-window 1)))

(defun ad:mark-sexp (f &optional arg allow-extend)
  "Set mark ARG sexps from point.
When the cursor is at the end of line or before a whitespace, set ARG -1."
  (interactive "P\np")
  (funcall f (if (and (not (bolp))
                      (not (eq (preceding-char) ?\ ))
                      (or (eolp)
                          (eq (following-char) ?\ )
                          (memq (preceding-char) '(?\) ?\> ?\] ?\}))))
                 -1 arg)
           allow-extend))
(advice-add 'mark-sexp :around #'ad:mark-sexp)

(when (require 'expand-region nil t)
  (defun ad:er:mark-sexp (f &optional arg allow-extend)
    "If the cursor is on a symbol, expand the region along the symbol."
    (interactive "P\np")
    (if (and (not (use-region-p))
             (symbol-at-point))
        (er/mark-symbol)
      (funcall f arg allow-extend)))
  (advice-add 'mark-sexp :around #'ad:er:mark-sexp))

;; Scroll window on a line-by-line basis
(setq scroll-conservatively 1000)
(setq scroll-step 1)
(setq scroll-preserve-screen-position t) ;; スクロール時にスクリーン内で固定
;;  (setq scroll-margin 0) ; default=0

;; Scroll window on a page-by-page basis with N line overlapping
(setq next-screen-context-lines 1)

(defcustom move-cursor-hook nil
  "Hook runs when you move the cursor."
  :type 'hook
  :group 'convenience)

(defun ad:cur:next-line (&optional _arg _try-vscroll)
  (run-hooks 'move-cursor-hook))
(defun ad:cur:previous-line (&optional _arg _try-vscroll)
  (run-hooks 'move-cursor-hook))
(defun ad:cur:forward-char (&optional _N)
  (run-hooks 'move-cursor-hook))
(defun ad:cur:backward-char (&optional _N)
  (run-hooks 'move-cursor-hook))
(defun ad:cur:syntax-subword-forward (&optional _N)
  (run-hooks 'move-cursor-hook))
(defun ad:cur:syntax-subword-backward (&optional _N)
  (run-hooks 'move-cursor-hook))
(defun ad:cur:move-beginning-of-line (_ARG)
  (run-hooks 'move-cursor-hook))
(defun ad:cur:move-end-of-line (_ARG)
  (run-hooks 'move-cursor-hook))

(advice-add 'next-line :before #'ad:cur:next-line)
(advice-add 'previous-line :before #'ad:cur:previous-line)
(advice-add 'forward-char :before #'ad:cur:forward-char)
(advice-add 'backward-char :before #'ad:cur:backward-char)
(advice-add 'syntax-subword-forward :before #'ad:cur:syntax-subword-forward)
(advice-add 'syntax-subword-backward :before #'ad:cur:syntax-subword-backward)
(advice-add 'move-beginning-of-line :before #'ad:cur:move-beginning-of-line)
(advice-add 'move-end-of-line :before #'ad:cur:move-end-of-line)

(when (autoload-if-found
       '(smooth-scroll-mode)
       "smooth-scroll" nil t)

  (with-eval-after-load "smooth-scroll"
    (custom-set-variables
     '(smooth-scroll/vscroll-step-size 6)
     '(smooth-scroll/hscroll-step-size 6)))
  (smooth-scroll-mode t))

(global-set-key (kbd "M-]") 'bs-cycle-next)
(when (display-graphic-p)
  (global-set-key (kbd "M-[") 'bs-cycle-previous))

(with-eval-after-load "bs"
  (custom-set-variables
   '(bs-cycle-configuration-name "files-and-scratch")))

(when (autoload-if-found
       '(my-toggle-bm
         my-bm-next bm-buffer-save bm-buffer-restore bm-buffer-save-all
         bm-repository-save bm-repository-load)
       "bm" nil t)

  ;; ファイルオープン時にブックマークを復帰
  (global-set-key (kbd "<f10>") 'my-toggle-bm)
  (global-set-key (kbd "<C-f10>") 'my-bm-next)
  (global-set-key (kbd "<S-f10>") 'bm-show-all)
  (add-hook 'find-file-hook #'bm-buffer-restore)

  (with-eval-after-load "bm"
    ;; (require 'helm-bm nil t)
    ;; (setq bm-annotation-width 30)
    (setq-default bm-buffer-persistence t)
    (setq bm-restore-repository-on-load t)
    (setq bm-cycle-all-buffers t)
    ;; (setq bm-toggle-buffer-persistence t)
    (setq bm-buffer-persistence t)
    (setq bm-persistent-face 'bm-face)
    (setq bm-repository-file
          (expand-file-name "~/Dropbox/emacs.d/.bm-repository"))

    (unless noninteractive
      (bm-repository-load)
      (add-hook 'kill-buffer-hook 'bm-buffer-save)
      (add-hook 'after-save-hook 'bm-buffer-save)
      (add-hook 'after-revert-hook 'bm-buffer-restore)
      (add-hook 'kill-emacs-hook #'my-bm-save-all))

    (defun ad:bm-show-mode ()
      "Enable truncate mode when showing bm list."
      (toggle-truncate-lines 1))
    (advice-add 'bm-show-mode :after #'ad:bm-show-mode)

    (defun my-bm-save-all ()
      (bm-buffer-save-all)
      (bm-repository-save))

    (defun my-toggle-bm ()
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
      (bm-buffer-save-all)
      (bm-repository-save))

    (defun my-bm-next ()
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

(when (autoload-if-found
       '(centered-cursor-mode)
       "centered-cursor-mode" nil t)

  (add-hook 'isearch-mode-hook
            (lambda () (centered-cursor-mode 1)))
  (add-hook 'isearch-mode-end-hook
            (lambda () (centered-cursor-mode -1))))

(when (require 'smart-mark nil t)
  (progn ;; C-M-SPC SPC SPC ... C-g の場合に正しくカーソルと元に戻す．
    (defun ad:smart-mark-set-restore-before-mark (&rest _arg)
      (unless (memq this-command
                    '(er/expand-region er/mark-symbol er/contract-region))
        (setq smart-mark-point-before-mark (point))))
    (advice-add 'smart-mark-set-restore-before-mark :override
                #'ad:smart-mark-set-restore-before-mark)
    (when (require 'expand-region-core nil t)
      ;; (defun ad:er:keyboard-quit ()
      ;;   (when (memq last-command '(er/expand-region er/contract-region))
      ;;     (when smart-mark-point-before-mark
      ;;       (goto-char smart-mark-point-before-mark))))
      ;; (advice-add 'keyboard-quit :after #'ad:er:keyboard-quit)
      (defadvice keyboard-quit (before collapse-region activate)
        (when (memq last-command '(er/expand-region er/contract-region))
          (er/contract-region 0)
          (when smart-mark-point-before-mark
            (goto-char smart-mark-point-before-mark))))))

  (unless noninteractive
    (smart-mark-mode 1)))

(when (require 'syntax-subword nil t)
  (unless noninteractive
    (global-syntax-subword-mode 1)))

(when (autoload-if-found
       '(goto-last-change goto-last-change-reverse)
       "goto-chg" nil t)

  (global-set-key (kbd "C-,") 'goto-last-change)
  (global-set-key (kbd "C-.") 'goto-last-change-reverse)

  (with-eval-after-load "flyspell"
    (define-key flyspell-mode-map (kbd "C-,") 'goto-last-change)
    (define-key flyspell-mode-map (kbd "C-.") 'goto-last-change-reverse)))

(setq yank-excluded-properties t)

(when (autoload-if-found
       '(time-stamp my-time-stamp)
       "time-stamp" nil t)

  (add-hook 'before-save-hook #'my-time-stamp)

  (with-eval-after-load "time-stamp"
    (setq time-stamp-start "#\\+date:[ \t]*")
    (setq time-stamp-end "$")
    (setq time-stamp-line-limit 10) ;; def=8
    (setq time-stamp-default-format "%04y-%02m-%02d")

    (defun my-time-stamp ()
      (setq time-stamp-format
            (if (eq major-mode 'org-mode)
                "[%04y-%02m-%02d %3a %02H:%02M]"
              time-stamp-default-format))
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

(defun my-orgalist-activate ()
  (when (require 'orgalist nil t)
    (orgalist-mode 1))) ;; originally orgstruct-mode

(add-hook 'change-log-mode-hook
          #'(lambda()
              (my-orgalist-activate)
              (setq tab-width 4)
              (setq left-margin 4)))

(when (autoload-if-found
       '(info org-info-ja)
       "info" nil t)

  (with-eval-after-load "info"
    (add-to-list 'Info-additional-directory-list
                 (expand-file-name "~/devel/mygit/org-ja/work/"))

    (defun org-info-ja (&optional node)
      "(Japanese) Read documentation for Org-mode in the info system.
    With optional NODE, go directly to that node."
      (interactive)
      (info (format "(org-ja)%s" (or node ""))))))

(add-hook 'nxml-mode-hook
          (lambda ()
            (define-key nxml-mode-map "\r" 'newline-and-indent)
            (auto-fill-mode -1)
            (setq indent-tabs-mode t)
            (setq nxml-slash-auto-complete-flag t)
            (setq tab-width 1)
            (setq nxml-child-indent 1)
            (setq nxml-attribute-indent 0)))

(autoload-if-found '(ascii-on ascii-off) "ascii" nil t)

(if (executable-find "cmake")
    (when (autoload-if-found
           '(cmake-mode)
           "cmake-mode" nil t)

      (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
      (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode)))
  (message "--- cmake is NOT installed."))

(when (autoload-if-found
       '(ispell-region ispell-complete-word)
       "ispell" nil t)

  ;; Spell checking within a specified region
  (global-set-key (kbd "C-c f 7") 'ispell-region)
  ;; 補完候補の表示（flyspell が使える時はそちらを優先して <f7> にする．
  (global-set-key (kbd "<f7>") 'ispell-word)
  ;; (if (autoload-if-found '(helm-ispell) "helm-ispell" nil t)
  ;;     #'helm-ispell #'ispell-word)))

  (with-eval-after-load "ispell"
    (setq ispell-encoding8-command t)
    ;; for English and Japanese mixed
    (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
    ;; http://endlessparentheses.com/ispell-and-org-mode.html
    (add-to-list 'ispell-skip-region-alist '("^#\\+begin_src" . "^#\\+end_src"))
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))

    (cond
     ((executable-find "hunspell")
      (setenv "LC_ALL" "en_US")
      ;; (message "--- hunspell loaded.")
      (setenv "DICPATH" "/Applications/LibreOffice.app/Contents/Resources/extensions/dict-en")
      (setq ispell-local-dictionary-alist
            '(("ja_JP" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
               ("-d" "en_US") nil utf-8)
              ("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
               ("-d" "en_US") nil utf-8)))
      (setq ispell-local-dictionary "en_US")
      (setq ispell-dictionary ispell-local-dictionary)
      (if shutup-p
          ;; 必要．しかも ispell-program-name 指定の前で．
          ;; ただし，ispell-local-dictionary-alist の後で．
          (shut-up (ispell-change-dictionary "en_US" t))
        (ispell-change-dictionary "en_US" t))
      (setq-default ispell-program-name (executable-find "hunspell"))
      ;; Not regal way, but it's OK (usually ispell-local-dictionary-alist)

      (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)
      (setq ispell-personal-dictionary "~/Dropbox/emacs.d/.hunspell.en.dic"))

     ((executable-find "aspell")
      ;; (message "--- aspell loaded.")
      (setq-default ispell-program-name "aspell")
      (when (eq window-system 'w32)
        (setq-default ispell-program-name
                      "C:/Program Files/Aspell/bin/aspell.exe"))
      (setq ispell-dictionary "english")
      ;; This will also avoid an IM-OFF issue for flyspell-mode.
      ;; (setq ispell-aspell-supports-utf8 t) ;; Obsolete
      (setq ispell-local-dictionary-alist
            '((nil "[a-zA-Z]" "[^a-zA-Z]" "'" t
                   ("-d" "en" "--encoding=utf-8") nil utf-8)))
      (setq ispell-personal-dictionary "~/Dropbox/emacs.d/.aspell.en.pws")
      )

     (t
      nil))))

(when (autoload-if-found
       '(flyspell-prog-mode flyspell-mode)
       "flyspell" nil t)

  (defvar major-mode-with-flyspell
    '(text-mode change-log-mode latex-mode yatex-mode
                git-commit-mode org-mode))
  (defvar major-mode-with-flyspell-prog
    '(c-mode-common emacs-lisp-mode perl-mode python-mode))
  (defvar my-flyspell-target-modes
    (append major-mode-with-flyspell
            major-mode-with-flyspell-prog))

  ;; バッファ内の全てをチェック対象にするモードの hook に flyspell 起動を登録
  (dolist (hook major-mode-with-flyspell)
    (add-hook (intern (format "%s-hook" hook))
              (lambda () (flyspell-mode 1))))

  ;; コメント行のみをチェック対象にする
  (dolist (hook major-mode-with-flyspell-prog)
    (add-hook (intern (format "%s-hook" hook))
              #'flyspell-prog-mode))

  (with-eval-after-load "flyspell"
    ;; C-; をオーバーライド
    (define-key flyspell-mode-map (kbd "C-;") 'comment-dwim)
    (setq flyspell-duplicate-distance 0)
    ;; (setq flyspell-mode-line-string " F")
    (setq flyspell-mode-line-string "")
    ;; (setq flyspell-large-region 200)
    (set-face-attribute 'flyspell-duplicate nil
                        :foreground "#EA5506" :bold t
                        :background nil :underline t)
    (set-face-attribute 'flyspell-incorrect nil
                        :foreground "#BA2636" :bold nil
                        :background nil :underline t)

    ;; ispell-complete-word のキーバインドを上書き
    ;; (when (and (require 'helm nil t)
    ;;            (require 'flyspell-correct-helm nil t))
    ;;   (global-set-key (kbd "<f7>") 'flyspell-correct-word-generic))

    ;; Auto complete との衝突を回避
    (with-eval-after-load "auto-complete"
      (ac-flyspell-workaround))

    (defun my-flyspell-ignore-nonascii (beg end _info)
      "incorrect判定をASCIIに限定"
      (string-match "[^!-~]" (buffer-substring beg end)))
    (add-hook 'flyspell-incorrect-hook #'my-flyspell-ignore-nonascii)

    (defun my-flyspell-on ()
      (cond
       ((memq major-mode major-mode-with-flyspell)
        (turn-on-flyspell))
       ((memq major-mode major-mode-with-flyspell-prog)
        (flyspell-prog-mode))
       (t
        nil)))

    (defun my-flyspell-off ()
      (when (memq major-mode my-flyspell-target-modes)
        (turn-off-flyspell)))

    ;; [FIXME] nextstep+inline-patch版で flyspell すると，日本語nyuuのようになる場合があるので，それを回避（IME が ONになったら一時的に flyspell を止める）
    (add-hook 'my-ime-off-hook #'my-flyspell-on)
    (add-hook 'my-ime-on-hook #'my-flyspell-off)))

(global-set-key (kbd "M-=") 'count-words)

(with-eval-after-load "time"
  (define-key display-time-world-mode-map "q" 'delete-window))

(with-eval-after-load "view"
  (define-key view-mode-map (kbd "n") 'next-line)
  (define-key view-mode-map (kbd "p") 'previous-line)
  (define-key view-mode-map (kbd "f") 'forward-char)
  (define-key view-mode-map (kbd "b") 'backward-char)
  (when (require 'origami nil t)
    (define-key view-mode-map (kbd "<tab>") 'origami-toggle-node)
    (define-key view-mode-map (kbd "S-<tab>") 'origami-toggle-all-nodes))

  (defun ad:view--enable () (my-mode-line-on))
  (defun ad:view--disable () (my-mode-line-off))
  (advice-add 'view--enable :before #'ad:view--enable)
  (advice-add 'view--disable :before #'ad:view--disable))

(global-set-key (kbd "C-c h m") #'hydra-multiple-cursors/body)
(autoload-if-found '(mc/num-cursors mc/edit-lines) "multiple-cursors" nil t)
(when (require 'hydra nil t)
  ;; see https://github.com/abo-abo/hydra/wiki/multiple-cursors
  (defhydra hydra-multiple-cursors (:hint nil)
    "
==================================================================
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
    ("l" mc/edit-lines) ;;  :exit t
    ("a" mc/mark-all-like-this) ;;  :exit t
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("s" mc/mark-all-in-region-regexp) ;;  :exit t
    ("0" mc/insert-numbers) ;;  :exit t
    ("A" mc/insert-letters) ;;  :exit t
    ("<mouse-1>" mc/add-cursor-on-click)
    ;; Help with click recognition in this hydra
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore)
    ("q" nil)))

(when (autoload-if-found
       '(latex-math-preview-expression
         latex-math-preview-insert-symbol
         latex-math-preview-save-image-file
         latex-math-preview-beamer-frame)
       "latex-math-preview" nil t nil)

  (global-set-key (kbd "<f6>") 'latex-math-preview-expression)

  (with-eval-after-load "latex-math-preview"
    (setq latex-math-preview-command-path-alist
          '((latex . "latex")
            (dvipng . "dvipng")
            (dvips . "dvips")))
    (define-key latex-math-preview-expression-mode-map (kbd "<f6>")
      'latex-math-preview-delete-buffer)))

(with-eval-after-load "yatex"
  (put 'YaTeX-insert-braces 'begend-guide 2)

  (defun ad:YaTeX-insert-begin-end (env region-mode)
    "Insert \\begin{mode-name} and \\end{mode-name}.
This works also for other defined begin/end tokens to define the structure."
    (setq YaTeX-current-completion-type 'begin)
    (let*((ccol (current-column)) beg beg2 exchange
	        (_arg region-mode)		;for old compatibility
	        (indent-column (+ ccol YaTeX-environment-indent))(_i 1) _func)
      (if (and region-mode (> (point) (mark)))
	        (progn (exchange-point-and-mark)
	               (setq exchange t
		                   ccol (current-column)
		                   indent-column (+ ccol YaTeX-environment-indent))))
      ;;VER2 (insert "\\begin{" env "}" (YaTeX-addin env))
      (setq beg (point))
      (YaTeX-insert-struc 'begin env)
      (setq beg2 (point))
      (insert "\n")
      (indent-to indent-column)
      (save-excursion
        ;;indent optional argument of \begin{env}, if any
        (while (> (point-beginning-of-line) beg)
	        (skip-chars-forward "\\s " (point-end-of-line))
	        (indent-to indent-column)
	        (forward-line -1)))
      (require 'yatexenv)
      (if region-mode
	        ;;if region-mode, indent all text in the region
	        (save-excursion
	          (if (fboundp (intern-soft (concat "YaTeX-enclose-" env)))
	              (funcall (intern-soft (concat "YaTeX-enclose-" env))
		                     (point) (mark))
	            (while (< (progn (forward-line 1) (point)) (mark))
	              (if (eolp) nil
		              (skip-chars-forward " \t\n")
		              (indent-to indent-column))))))
      (if region-mode (exchange-point-and-mark))
      (indent-to ccol)
      ;;VER2 (insert "\\end{" env "}\n")
      (YaTeX-insert-struc 'end env)
      (YaTeX-reindent ccol)
      (if region-mode
	        (progn
	          (insert "\n")
	          (or exchange (exchange-point-and-mark)))
        (goto-char beg2)
        (YaTeX-intelligent-newline nil)
        (YaTeX-indent-line))
      (YaTeX-package-auto-usepackage env 'env)
      (if YaTeX-current-position-register
	        (point-to-register YaTeX-current-position-register))))

  (advice-add 'YaTeX-insert-begin-end
              :override #'ad:YaTeX-insert-begin-end))

(when (autoload-if-found
       '(yas-minor-mode yas-global-mode)
       "yasnippet" nil t)

  (dolist (hook
           (list
            'perl-mode-hook 'c-mode-common-hook 'js2-mode-hook 'org-mode-hook
            'python-mode-hook 'emacs-lisp-mode-hook))
    (add-hook hook #'yas-minor-mode))

  (with-eval-after-load "yasnippet"
    (setq yas-verbosity 2)
    (setq yas-snippet-dirs
          (list "~/Dropbox/emacs.d/yas-dict"
                'yas-installed-snippets-dir)) ;; for Cask
    (unless noninteractive
      (yas-global-mode 1))))

(when (autoload-if-found
       '(osx-dictionary-search-pointer osx-dictionary-search-input)
       "osx-dictionary" nil t)

  (global-set-key (kbd "C-M-w") #'osx-dictionary-search-pointer)
  (global-set-key (kbd "C-c f w") #'osx-dictionary-search-input)

  (with-eval-after-load "osx-dictionary"
    (custom-set-variables
     '(osx-dictionary-dictionary-choice "英辞郎 第七版"))))

(autoload-if-found
 '(describe-number describe-number-at-point)
 "describe-number" nil t)

(if (executable-find "js-beautify")
    (when (autoload-if-found
           '(js2-mode)
           "js2-mode" nil t)

      (with-eval-after-load "js2-mode"
        (when (require 'web-beautify nil t)
          (define-key js2-mode-map (kbd "C-c b") 'web-beautify-js)
          (define-key js2-mode-map (kbd "C-c b") 'web-beautify-css))))

  (message "--- js-beautify is NOT installed.")
  (message "--- Note: npm -g install js-beautify"))

(when (autoload-if-found
       '(smartparens-global-mode turn-on-show-smartparens-mode)
       "smartparens" nil t)

  (defun my-smartparens-mode ()
    (smartparens-global-mode)
    (remove-hook 'yatex-mode-hook #'my-smartparens-mode)
    (remove-hook 'org-mode-hook #'my-smartparens-mode))
  (add-hook 'yatex-mode-hook #'my-smartparens-mode)
  (add-hook 'org-mode-hook #'my-smartparens-mode)

  (with-eval-after-load "smartparens"
    (setq-default sp-highlight-pair-overlay nil)
    (setq-default sp-highlight-wrap-overlay nil)
    (setq-default sp-highlight-wrap-tag-overlay nil)
    (sp-local-pair 'org-mode "$" "$")
    (sp-local-pair 'org-mode "~" "~")
    (sp-local-pair 'org-mode "+" "+")
    (sp-local-pair 'org-mode "=" "=")
    (sp-local-pair 'org-mode "_" "_")
    (sp-local-pair 'yatex-mode "$" "$")
    (sp-pair "`" nil :actions :rem)
    (sp-pair "'" nil :actions :rem)
    (sp-pair "[" nil :actions :rem)))

(autoload-if-found
 '(query-replace-from-region query-replace-regexp-from-region)
 "replace-from-region" nil t)

(when (autoload-if-found
       '(selected-global-mode)
       "selected" nil t)

  (defun my-activate-selected ()
    (selected-global-mode 1)
    (selected--on) ;; must call expclitly here
    (remove-hook 'activate-mark-hook #'my-activate-selected))
  (add-hook 'activate-mark-hook #'my-activate-selected)

  (with-eval-after-load "selected"
    (define-key selected-keymap (kbd ";") #'comment-dwim)
    (define-key selected-keymap (kbd "e") #'my-eval-region)
    ;; (define-key selected-keymap (kbd "E") #'my-eval-region-as-function)
    (define-key selected-keymap (kbd "=") #'count-words-region)
    (define-key selected-keymap (kbd "f") #'describe-function)
    (define-key selected-keymap (kbd "v") #'describe-variable)
    (when (require 'helpful nil t)
      (define-key selected-keymap (kbd "@") #'helpful-at-point)
      (define-key selected-keymap (kbd "m") #'helpful-macro)
      (define-key selected-keymap (kbd "o") #'helpful-symbol)
      (define-key selected-keymap (kbd "f") #'helpful-function)
      (define-key selected-keymap (kbd "v") #'helpful-variable))
    (define-key selected-keymap (kbd "w") #'osx-dictionary-search-pointer)
    (define-key selected-keymap (kbd "5") #'query-replace-from-region)
    (define-key selected-keymap (kbd "g") #'my-google-this)
    (define-key selected-keymap (kbd "s") #'osx-lib-say-region)
    (define-key selected-keymap (kbd "q") #'selected-off)
    (define-key selected-keymap (kbd "x") #'my-hex-to-decimal)
    (define-key selected-keymap (kbd "X") #'my-decimal-to-hex)

    (defun my-eval-region ()
      (interactive)
      (when (use-region-p)
        (eval-region (region-beginning) (region-end) t)))
    (setq selected-org-mode-map (make-sparse-keymap))

    (define-key selected-org-mode-map (kbd "t") #'org-table-convert-region)
    (define-key selected-keymap (kbd "-") #'my-org-list-insert-items)
    (define-key selected-keymap (kbd "_")
      #'my-org-list-insert-checkbox-into-items)

    (when (require 'expand-region nil t)
      (define-key selected-keymap (kbd "SPC") #'er/expand-region))

    ;; (when (require 'helm-selected nil t)
    ;;   (define-key selected-keymap (kbd "h") 'helm-selected))

    (when (require 'counsel-selected nil t)
      (define-key selected-keymap (kbd "h") 'counsel-selected))

    (when (require 'help-fns+ nil t)
      (defun my-describe-selected-keymap ()
        (interactive)
        (describe-keymap 'selected-keymap))
      (define-key selected-keymap (kbd "H") #'my-describe-selected-keymap))))

(autoload-if-found
 '(isolate-quick-add
   isolate-long-add isolate-quick-delete
   isolate-quick-chnge isolate-long-change)
 "isolate" nil t)

(when (autoload-if-found
       '(git-complete)
       "git-complete" nil t)

  (global-set-key (kbd "C-c f <tab>") 'git-complete))

(when (autoload-if-found
       '(bratex-config)
       "bratex" nil t)

  (add-hook 'yatex-mode-hook #'bratex-config))

(setq echo-keystrokes 0.5)

;; Show line number in the mode line.
(unless noninteractive
  (line-number-mode 1))

(when (autoload-if-found
       '(my-toggle-display-line-numbers-mode)
       "display-line-numbers" nil t)

  (with-eval-after-load "hl-line"
    (custom-set-faces
     `(line-number-current-line
       ((t (:bold t :background ,(face-attribute 'hl-line :background)))))))

  (with-eval-after-load "display-line-numbers"
    (custom-set-faces
     '(line-number-current-line
       ((t (:bold t)))))

    (custom-set-variables
     '(display-line-numbers-width-start t))

    (defun my-toggle-display-line-numbers-mode ()
      "Toggle variable `global-display-line-numbers-mode'."
      (interactive)
      (if (fboundp 'global-display-line-numbers-mode) ;; 26.1 or later
          (let ((flag (if global-display-line-numbers-mode -1 1)))
            (global-display-line-numbers-mode flag))
        (user-error "The display-line-numbers is NOT supported")))))

;; Show clock in in the mode line
(setq display-time-format "%H%M.%S") ;; %y%m%d.
(setq display-time-interval 1)
(setq display-time-default-load-average nil)
(unless noninteractive
  (display-time-mode 1))

(when (require 'mic-paren nil t)
  (setq paren-sexp-mode nil)
  (set-face-foreground 'paren-face-match "#FFFFFF")
  ;; Deep blue: #6666CC, orange: #FFCC66
  (set-face-background 'paren-face-match "#66CC66")
  (unless noninteractive
    (paren-activate)))

;; スペース
(defface my-face-b-1
  '((t (:background "gray" :bold t :underline "red")))
  nil :group 'font-lock-highlighting-faces)
;; タブだけの行
(defface my-face-b-2
  '((t (:background "orange" :bold t :underline "red")))
  nil :group 'font-lock-highlighting-faces)
;; 半角スペース
(defface my-face-b-3 '((t (:background "orange")))
  nil :group 'font-lock-highlighting-faces)

(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-b-3 'my-face-b-3)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   ;; "[\t]+$" 行末のタブ
   '(("　" 0 my-face-b-1 append)
     ("[ ]+$" 0 my-face-b-3 append)
     ("[\t]+$" 0 my-face-b-2 append))))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;;show EOF
;; (defun set-buffer-end-mark()
;;   (let ((overlay (make-overlay (point-max) (point-max))))
;;     (overlay-put overlay 'before-string #("[EOF]" 0 5 (face highlight)))
;;     (overlay-put overlay 'insert-behind-hooks
;;                  '((lambda (overlay after beg end &optional len)
;;                      (when after
;;                        (move-overlay overlay (point-max) (point-max))))))))
;; (add-hook 'find-file-hooks #'set-buffer-end-mark)

;; 改行文字の文字列表現
(set 'eol-mnemonic-dos "(CRLF)")
(set 'eol-mnemonic-unix "(LF)")
(set 'eol-mnemonic-mac "(CR)")
(set 'eol-mnemonic-undecided "(?)")

;; 文字エンコーディングの文字列表現
(defun my-coding-system-name-mnemonic (coding-system)
  (let* ((base (coding-system-base coding-system))
         (name (symbol-name base)))
    (cond ((string-prefix-p "utf-8" name) "U8")
          ((string-prefix-p "utf-16" name) "U16")
          ((string-prefix-p "utf-7" name) "U7")
          ((string-prefix-p "japanese-shift-jis" name) "SJIS")
          ((string-match "cp\\([0-9]+\\)" name) (match-string 1 name))
          ((string-match "japanese-iso-8bit" name) "EUC")
          (t "???")
          )))

(defun my-coding-system-bom-mnemonic (coding-system)
  (let ((name (symbol-name coding-system)))
    (cond ((string-match "be-with-signature" name) "[BE]")
          ((string-match "le-with-signature" name) "[LE]")
          ((string-match "-with-signature" name) "[BOM]")
          (t ""))))

(defun my-buffer-coding-system-mnemonic ()
  "Return a mnemonic for `buffer-file-coding-system'."
  (let* ((code buffer-file-coding-system)
         (name (my-coding-system-name-mnemonic code))
         (bom (my-coding-system-bom-mnemonic code)))
    (format "%s%s" name bom)))

;; `mode-line-mule-info' の文字エンコーディングの文字列表現を差し替える
(setq-default mode-line-mule-info
              (cl-substitute '(:eval (my-buffer-coding-system-mnemonic))
                             "%z" mode-line-mule-info :test 'equal))

(when (require 'delight nil t)
  (delight
   '(;; Major modes
;;     (c-mode "C" :major)
;;     (c++mode "C++" :major)
     (js2-mode "JS" :major)
     (csharp-mode "C#" :major)
     (prog-mode "Pr" :major)
     (emacs-lisp-mode "El" :major)
     (python-mode "Py" :major)
     (perl-mode "Pl" :major)
     (web-mode "W" :major)
     (change-log-mode "ChangeLog" :major)
     (lisp-interaction-mode "Lisp" :major)

     ;; Shorten for minor modes
     (ggtags-mode " G" "ggtags")
     ;; (orgstruct-mode " OrgS" "org")
     (orgalist-mode " ol" "orgalist")

     ;; Stop to display for minor modes
     (org-fancy-priorities-mode nil "org-fancy-priorities")
     (smooth-scroll-mode nil "smooth-scroll")
     (eldoc-mode nil "eldoc")
     (ivy-mode nil "ivy")
     (counsel-mode nil "counsel")
     (centered-cursor-mode nil "centered-cursor-mode")
     (volatile-highlights-mode nil "volatile-highlights")
     (aggressive-indent-mode nil "aggressive-indent")
     (all-the-icons-dired-mode nil "all-the-icons-dired")
     (yas-minor-mode nil "yasnippet")
     (auto-complete-mode nil "auto-complete")
     (company-mode nil "company")
     (ws-butler-mode nil "ws-butler")
     (isearch-mode nil "isearch")
     (auto-revert-mode nil "autorevert")
     (global-whitespace-mode nil "whitespace")
     (emmet-mode nil "emmet-mode")
     (helm-mode nil "helm-mode")
     (abbrev-mode nil "abbrev")
     (doxymacs-mode nil "doxymacs")
     (editorconfig-mode nil "editorconfig")
     (rainbow-mode nil "rainbow-mode")
     (highlight-symbol-mode nil "highlight-symbol")
     (which-key-mode nil "which-key")
     (fancy-narrow-mode nil "fancy-narrow")
     (smartparens-mode nil "smartparens")
     (projectile-mode nil "projectile")
     (selected-minor-mode nil "selected"))))

(if (executable-find "cmigemo")
    (when (autoload-if-found
           '(migemo-init)
           "migemo" nil t)

      (add-hook 'isearch-mode-hook #'migemo-init)

      (with-eval-after-load "migemo"
        (custom-set-variables
         '(completion-ignore-case t) ;; case-independent
         '(migemo-command "cmigemo")
         '(migemo-options '("-q" "--emacs" "-i" "\a"))
         '(migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
         '(migemo-user-dictionary nil)
         '(migemo-regex-dictionary nil)
         '(migemo-use-pattern-alist t)
         '(migemo-use-frequent-pattern-alist t)
         '(migemo-pattern-alist-length 1024)
         '(migemo-coding-system 'utf-8-unix))))
  (message "--- cmigemo is NOT installed."))

(when (autoload-if-found
       '(helm-google)
       "helm-google" nil t)

  (with-eval-after-load "helm-google"
    (custom-set-variables
     '(helm-google-tld "co.jp"))))

(when (autoload-if-found
       '(git-gutter-mode)
       "git-gutter" nil t)

  (dolist (hook
           '(emacs-lisp-mode-hook
             lisp-mode-hook perl-mode-hook python-mode-hook
             c-mode-common-hook nxml-mode-hook web-mode-hook))
    (add-hook hook #'git-gutter-mode))

  (with-eval-after-load "git-gutter"
    (custom-set-variables
     '(git-gutter:lighter ""))

    (when (require 'git-gutter-fringe nil t)
      (custom-set-variables
       '(git-gutter-fr:side 'left-fringe))

      ;; (require 'fringe-helper nil t) ;; byte-compile 時に明示的に指定が必要．
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
      (set-face-foreground 'git-gutter-fr:added    "#FF2600")
      (set-face-foreground 'git-gutter-fr:modified "orange")
      (set-face-foreground 'git-gutter-fr:deleted  "medium sea green"))))

(with-eval-after-load "calendar"

  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)

  (when (require 'japanese-holidays nil t)
    (setq calendar-holidays
          (append japanese-holidays
                  holiday-local-holidays holiday-other-holidays))
    (setq mark-holidays-in-calendar t)
    (setq japanese-holiday-weekend-marker
          '(holiday nil nil nil nil nil japanese-holiday-saturday))
    (setq japanese-holiday-weekend '(0 6))

    (add-hook 'calendar-today-visible-hook #'japanese-holiday-mark-weekend)
    (add-hook 'calendar-today-invisible-hook #'japanese-holiday-mark-weekend)))

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

(when (autoload-if-found
       '(which-key-mode)
       "which-key" nil t)

  (with-eval-after-load "which-key"
    (custom-set-variables
     '(which-key-idle-delay 1.0)))

  (unless noninteractive
    (which-key-mode 1)))

(when (autoload-if-found
       '(highlight-symbol-mode highlight-symbol-nav-mode)
       "highlight-symbol" nil t)

  (dolist (hook '(emacs-lisp-mode-hook c-mode-common-hook prog-mode-hook))
    (add-hook hook #'highlight-symbol-mode))

  (with-eval-after-load "highlight-symbol"
    (custom-set-variables
     '(highlight-symbol-idle-delay 0.5))))

(when (autoload-if-found
       '(all-the-icons-dired-mode ad:all-the-icons-dired--display)
       "all-the-icons-dired" nil t)

  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)

  (with-eval-after-load "all-the-icons"
    (setq all-the-icons-scale-factor 1.0)
    (add-to-list 'all-the-icons-dir-icon-alist
                 '("google[ _-]drive" all-the-icons-alltheicon "google-drive"
                   :height 1.0 :v-adjust -0.1)))

  (with-eval-after-load "all-the-icons-dired"
    (unless noninteractive
      (unless (require 'font-lock+ nil t)
        (user-error "font-lock+ is NOT installed for all-the-icons."))

      (defun ad:all-the-icons-dired--display ()
        "Display the icons of files in a dired buffer."
        (when (and (not all-the-icons-dired-displayed) dired-subdir-alist)
          (setq-local all-the-icons-dired-displayed t)
          (let ((inhibit-read-only t)
                (remote-p (and (fboundp 'tramp-tramp-file-p)
                               (tramp-tramp-file-p default-directory))))
            (save-excursion
              (goto-char (point-min))
              (setq tab-width 1)
              (while (not (eobp))
                (when (dired-move-to-filename nil)
                  (let ((file (dired-get-filename 'verbatim t)))
                    (unless (member file '("." ".."))
                      (let ((filename (dired-get-filename nil t)))
                        (if (file-directory-p filename)
                            (let* ((matcher (all-the-icons-match-to-alist file all-the-icons-dir-icon-alist))
                                   (icon (cond
                                          (remote-p
                                           (all-the-icons-octicon "file-directory" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                          ((file-symlink-p filename)
                                           (all-the-icons-octicon "file-symlink-directory" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                          ((all-the-icons-dir-is-submodule filename)
                                           (all-the-icons-octicon "file-submodule" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                          ((file-exists-p (format "%s/.git" filename))
                                           (all-the-icons-octicon "repo" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                          (t (apply (car matcher) (list (cadr matcher) :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust))))))
                              (insert (concat icon "\t")))
                          (insert (concat (all-the-icons-icon-for-file file :v-adjust all-the-icons-dired-v-adjust) "\t"))))))) ;; :height 0.9
                (forward-line 1))))))
      (advice-add 'all-the-icons-dired--display :override #'ad:all-the-icons-dired--display))))

(when (autoload-if-found
       '(turn-on-eldoc-mode)
       "eldoc" nil t)

  (dolist (hook '(emacs-lisp-mode-hook org-mode-hook c-mode-common-hook))
    (add-hook hook #'turn-on-eldoc-mode))

  (with-eval-after-load "eldoc"
    (custom-set-variables
     '(eldoc-idle-delay 1.0))))

(autoload-if-found '(keycast-mode) "keycast" nil t)

(when (autoload-if-found
       '(dimmer-mode dimmer-process-all dimmer-off dimmer-on
                     my-toggle-dimmer dimmer-permanent-off
                     ad:dimmer-org-agenda--quit)
       "dimmer" nil t)

  (defvar my-dimmer-mode nil)

  (with-eval-after-load "dimmer"
    (custom-set-variables
     '(dimmer-exclusion-regexp
       "^\\*[Hh]elm\\|^ \\*Minibuf\\|^ \\*Neo\\|^ \\*Echo\\|^\\*Calendar\\|*Org\\|^ \\*LV*")
     '(dimmer-fraction 0.6))

    (defun my-toggle-dimmer ()
      (interactive)
      (if (setq my-dimmer-mode (not my-dimmer-mode))
          (dimmer-on) (dimmer-off)))

    (defun dimmer-permanent-off ()
      (setq my-dimmer-mode nil)
      (dimmer-off))

    (defun dimmer-off ()
      (dimmer-process-all)
      (dimmer-mode -1))

    (defun dimmer-on ()
      (when my-dimmer-mode
        (dimmer-mode 1)
        (dimmer-process-all)))

    (add-hook 'focus-out-hook #'dimmer-off)
    (add-hook 'focus-in-hook #'dimmer-on)

    ;; for org-agenda
    (add-hook 'org-agenda-mode-hook #'dimmer-permanent-off)
    (defun ad:dimmer-org-agenda--quit (&optional _bury)
      (when (fboundp 'dimmer-on)
        (setq my-dimmer-mode t)
        (dimmer-on)))
    (advice-add 'org-agenda--quit :after #'ad:dimmer-org-agenda--quit)

    ;; for helm and helm-swoop
    (add-hook 'minibuffer-setup-hook #'dimmer-off)
    (add-hook 'minibuffer-exit-hook #'dimmer-on))

  (unless noninteractive
    (setq my-dimmer-mode (dimmer-mode 1))))

(when (autoload-if-found
       '(help/hydra/timestamp/body)
       "hydra" nil t)

  (global-set-key (kbd "C-c h t") #'help/hydra/timestamp/body)

  (with-eval-after-load "hydra"
    (require 'org nil t)
    (custom-set-faces
     '(hydra-face-blue
       ((((background light))
         :foreground "orange red" :bold t)
        (((background dark))
         :foreground "orange" :bold t))))

    (defhydra help/hydra/timestamp (:color blue :hint none)
      "
   === Timestamp ===                                                     _q_uit
0.  ?i? (_i_so 8601)    ?n? (_n_ow)    ?w? (_w_eek)    ?a? (week-d_a_y)
_1_.  ?t? (ISO 8601 including _t_imezone)
_2_.  ?r?    (Org Mode: _r_ight now)
_3_.  ?s?          (Org Mode: by _s_elect)
"
      ("q" nil)
      ("i" help/insert-datestamp (format-time-string "%F"))
      ("n" help/insert-currenttime (format-time-string "%H:%M"))
      ("w" help/insert-week (format-time-string "%W"))
      ("a" help/insert-month-and-day (format-time-string "%m%d"))
      ("t" help/insert-timestamp (help/get-timestamp))
      ("r" help/org-time-stamp-with-seconds-now
       (format-time-string "<%F %a %H:%M>"))
      ("s" org-time-stamp (format-time-string "<%F %a>"))
      ("0" help/show-my-date)
      ("1" help/insert-timestamp)
      ("2" help/org-time-stamp-with-seconds-now)
      ("3" org-time-stamp))
    (defun help/show-my-date ()
      "Produces and show date and time in preferred format."
      (interactive)
      (message (format-time-string "%Y-%m-%d (%a.) W:%W @%H:%M"))
      (hydra-keyboard-quit))
    (defun help/insert-currenttime ()
      "Produces and inserts the current time."
      (interactive)
      (insert (format-time-string "%H:%M")))
    (defun help/insert-week ()
      "Produces and inserts the week number."
      (interactive)
      (insert (format-time-string "%W")))
    (defun help/insert-month-and-day ()
      "Inserts a month and day pair in 4-degits."
      (interactive)
      (insert (format-time-string "%m%d")))
    (defun help/insert-datestamp ()
      "Produces and inserts a partial ISO 8601 format timestamp."
      (interactive)
      (insert (format-time-string "%F")))
    (defun help/insert-timestamp ()
      "Inserts a full ISO 8601 format timestamp."
      (interactive)
      (insert (help/get-timestamp)))
    (defun help/org-time-stamp-with-seconds-now ()
      (interactive)
      (let ((current-prefix-arg '(16)))
        (call-interactively 'org-time-stamp)))
    (defun help/get-timestamp ()
      "Produces a full ISO 8601 format timestamp."
      (interactive)
      (let* ((timestamp-without-timezone (format-time-string "%Y-%m-%dT%T"))
             (timezone-name-in-numeric-form (format-time-string "%z"))
             (timezone-utf-offset
              (concat (substring timezone-name-in-numeric-form 0 3)
                      ":"
                      (substring timezone-name-in-numeric-form 3 5)))
             (timestamp (concat timestamp-without-timezone
                                timezone-utf-offset)))
        timestamp))))

(when (autoload-if-found
       '(emms-play-file
         emms-play-playlist emms-play-directory my-play-bgm
         emms-next emms-previous emms-stop emms-pause)
       "emms" nil t)

  (global-set-key (kbd "C-c e b") 'my-play-bgm)
  (let ((base "C-c e "))
    (global-set-key (kbd (concat base "n")) 'emms-next)
    (global-set-key (kbd (concat base "p")) 'emms-previous)
    (global-set-key (kbd (concat base "s")) 'emms-stop)
    (global-set-key (kbd (concat base "SPC")) 'emms-pause))

  (with-eval-after-load "emms-mode-line"
    (defun ad:emms-mode-line-playlist-current ()
      "Display filename in mode-line, not full-path."
      (format emms-mode-line-format
              (file-name-nondirectory
               (emms-track-description
				        (emms-playlist-current-selected-track)))))
    (advice-add 'emms-mode-line-playlist-current :override
                #'ad:emms-mode-line-playlist-current))

  (with-eval-after-load "emms-mode-line-icon"
    (setq emms-mode-line-format "%s")
    (defun ad:emms-mode-line-icon-function ()
      "Replace the default musical note icon with a Unicode character."
      (concat " "
              emms-mode-line-icon-before-format
              "♪"
              (emms-mode-line-playlist-current)))
    (advice-add 'emms-mode-line-icon-function :override
                #'ad:emms-mode-line-icon-function))

  (with-eval-after-load "emms"
    (when (require 'emms-setup nil t)
      (emms-standard)
      (emms-default-players))

    (unless noninteractive
      (require 'org-emms nil t)) ;; emms のリンクに対応させる
    (require 'helm-emms nil t)

    (defun my-play-bgm ()
      (interactive)
      (let ((file "~/Dropbox/12-audio/ffximusic104.m4a"))
        (when (file-exists-p file)
          (emms-play-file file)
          (emms-toggle-repeat-track))))

    ;; setup an additional player
    (if (executable-find "mpv")
        (when (require 'emms-player-mpv nil t)
          (add-to-list 'emms-player-list 'emms-player-mpv)

          ;; (defvar emms-player-mpv-ontop nil)
          ;; (defun emms-player-mpv-toggle-ontop ()
          ;;   "Toggle float on top."
          ;;   (interactive)
          ;;   (if emms-player-mpv-ontop
          ;;       (emms-player-mpv-disable-ontop)
          ;;     (emms-player-mpv-enable-ontop)))

          ;; (defun emms-player-mpv-enable-ontop ()
          ;;   "Enable float on top."
          ;;   (let ((cmd (emms-player-mpv--format-command "set ontop yes")))
          ;;     (call-process-shell-command cmd nil nil nil))
          ;;   (setq emms-player-mpv-ontop t))

          ;; (defun emms-player-mpv-disable-ontop ()
          ;;   "Disable float on top."
          ;;   (let ((cmd (emms-player-mpv--format-command "set ontop no")))
          ;;     (call-process-shell-command cmd nil nil nil))
          ;;   (setq emms-player-mpv-ontop nil))

          ;; (global-set-key (kbd "C-c e t") 'emms-player-mpv-toggle-ontop)
          )

      (message "--- mpv is NOT installed."))))

(when (autoload-if-found
       '(google-maps)
       "google-maps" nil t)

  (with-eval-after-load "google-maps"
    (require 'org-location-google-maps nil t)))

(when (autoload-if-found
       '(sunshine-forecast sunshine-quick-forecast)
       "sunshine" nil t)

  (with-eval-after-load "sunshine"
    ;; (setq sunshine-location "Tokyo, Japan")
    ;; (setq sunshine-appid "................................")
    (custom-set-variables
     '(sunshine-show-icons t)
     '(sunshine-units 'metric))))

(when (autoload-if-found
       '(rencetf-mode
         my-recentf-save-list-silence
         my-recentf-cleanup-silence
         recentf-open-files)
       "recentf" nil t)

  (with-eval-after-load "recentf"
    (custom-set-variables
     '(recentf-max-saved-items 2000)
     '(recentf-save-file (expand-file-name "~/.emacs.d/recentf"))
     '(recentf-auto-cleanup 'never)
     '(recentf-exclude
       '(".recentf" "bookmarks" "^/tmp\\.*"
         "^/private\\.*" "^/var/folders\\.*" "/TAGS$")))

    (defun my-recentf-save-list-silence ()
      (interactive)
      (let ((message-log-max nil))
        (if (fboundp 'shut-up)
            (shut-up (recentf-save-list))
          (recentf-save-list)))
      (message ""))

    (defun my-recentf-cleanup-silence ()
      (interactive)
      (when (file-exists-p "/Volumes/orzHDn")
        (let ((message-log-max nil))
          (if shutup-p
              (shut-up (recentf-cleanup))
            (recentf-cleanup)))
        (message "")))
    (add-hook 'focus-out-hook #'my-recentf-save-list-silence)
    (add-hook 'focus-out-hook #'my-recentf-cleanup-silence))

  (unless noninteractive
    (let ((message-log-max nil))
      (recentf-mode 1))))

;; (add-hook 'after-init-hook #'recentf-mode))

(with-eval-after-load "recentf"
  (defun my-backup-recentf ()
    (when (require 'utility nil t)
      (my-backup recentf-save-file))) ;; "~/.emacs.d/recentf"
  (run-with-idle-timer 180 t 'my-backup-recentf))

(when (autoload-if-found
       '(backup-each-save my-auto-backup)
       "backup-each-save" nil t)

  (add-hook 'after-save-hook #'my-auto-backup)

  ;; %y-%m-%d_%M:%S で終わるファイルを本来のメジャーモードで開く
  (add-to-list 'auto-mode-alist '("-[0-9-]\\{8\\}_[0-9:]\\{5\\}$" nil t))

  (with-eval-after-load "backup-each-save"
    (defun my-auto-backup ()
      (unless (equal (buffer-name) "recentf")
        (backup-each-save)))
    (setq backup-each-save-mirror-location "~/.emacs.d/backup")
    (setq backup-each-save-time-format "%y-%m-%d_%M:%S")
    (setq backup-each-save-size-limit 1048576)))

(with-eval-after-load "dired"
  (setq completion-ignored-extensions
        (append completion-ignored-extensions
                '("./" "../" ".xlsx" ".docx" ".pptx" ".DS_Store")))

  ;; Use build-in `wdired-mode'.
  ;; (define-key dired-mode-map (kbd "R") 'wdired-change-to-wdired-mode)

  ;; http://elpa.gnu.org/packages/gited.html
  (when (require 'gited nil t)
    (define-key dired-mode-map (kbd "C-x C-g") 'gited-list-branches))

  ;; https://github.com/Fuco1/dired-hacks
  (when (require 'dired-narrow nil t)
    (define-key dired-mode-map (kbd "/") 'dired-narrow))
  (require 'dired-du nil t)
  ;; (when (require 'helm-config nil t)
  ;;   (require 'helm-dired-history nil t))
  (when (require 'ivy-dired-history nil t)
    ;; ivy-dired-history-variable は，session.el で明示的に管理中．
    ;; check session-globals-include
    (define-key dired-mode-map "," 'dired))

  (when (require 'dired-x nil t)
    (dired-extra-startup))
  (defun my-reveal-in-finder ()
    "Reveal the current buffer in Finder."
    (interactive)
    (shell-command-to-string "open ."))
  ;; dired-x を読み込んだあとじゃないとだめ
  (define-key dired-mode-map (kbd "F") 'my-reveal-in-finder)

  ;; https://github.com/xuchunyang/emacs.d
  ;; type "!" or "X" in dired
  (when (eq system-type 'darwin)
    (setq dired-guess-shell-alist-user
          (list
           (list (rx (and "."
                          (or
                           ;; Videos
                           "mp4" "avi" "mkv" "rmvb"
                           ;; Torrent
                           "torrent"
                           ;; PDF
                           "pdf"
                           ;; Image
                           "gif" "png" "jpg" "jpeg")
                          string-end)) "open")))))

(when (autoload-if-found
       '(dired-recent-open dired-recent-mode)
       "dired-recent" nil t)

  (global-set-key (kbd "C-x C-d") 'dired-recent-open)

  (with-eval-after-load "dired-recent"
    (require 'helm-config nil t)
    (dired-recent-mode 1)))

(with-eval-after-load "dired"
  (setq dired-use-ls-dired nil)
  (when (require 'osx-trash nil t)
    (setq delete-by-moving-to-trash t)
    (osx-trash-setup)))

(when (autoload-if-found
       '(my-undo-tree-visualize)
       "undo-tree" nil t)

  (global-set-key (kbd "C-x u") 'my-undo-tree-visualize)

  (with-eval-after-load "undo-tree"
    (global-undo-tree-mode)
    (setq undo-tree-mode-lighter nil) ;; モードライン領域を節約

    (defvar my-undo-tree-active nil)
    (defvar my-undo-tree-width 90)

    (defun my-undo-tree-visualize ()
      (interactive)
      (if (require 'moom nil t)
          (moom-change-frame-width-double)
        (when (and (not my-undo-tree-active)
                   (not (eq buffer-undo-list t)))
          (set-frame-width nil (+ (frame-width) my-undo-tree-width))
          (setq my-undo-tree-active t)))
      (undo-tree-visualize))

    (define-key undo-tree-map (kbd "C-x u") 'my-undo-tree-visualize)

    (defun my-undo-tree-visualizer-quit ()
      (interactive)
      (undo-tree-visualizer-quit)
      (if (require 'moom nil t)
          (moom-change-frame-width-single)
        (delete-window)
        (when my-undo-tree-active
          (set-frame-width nil (- (frame-width) my-undo-tree-width))
          (setq my-undo-tree-active nil)))
      (when (< (frame-width) 80)
        (set-frame-width nil 80)))

    (define-key undo-tree-visualizer-mode-map (kbd "q")
      'my-undo-tree-visualizer-quit)))

(when (require 'auto-save-buffers nil t)

  (defun my-ox-hugo-auto-saving-p ()
    (when (eq major-mode 'org-mode)
      (or (and (boundp 'org-capture-mode) ;; when activating org-capture
               org-capture-mode)
          (and (fboundp 'org-entry-get)
               (equal "" (org-entry-get (point) "EXPORT_FILE_NAME"))))))

  (defun my-auto-save-buffers ()
    (cond ((eq major-mode 'undo-tree-visualizer-mode) nil)
          ((eq major-mode 'diff-mode) nil)
          ((string-match "Org Src" (buffer-name)) nil)
          ((my-ox-hugo-auto-saving-p) nil)
          (t
           (auto-save-buffers))))

  (run-with-idle-timer 1.6 t #'my-auto-save-buffers))

(when (autoload-if-found
       '(neotree neotree-toggle)
       "neotree" nil t)

  (global-set-key (kbd "C-c n") #'neotree-toggle)

  (with-eval-after-load "neotree"
    (custom-set-variables
     '(neo-show-hidden-files t)
     '(neo-theme 'arrow)
     '(neo-smart-open t)
     '(neo-window-width 25)
     '(neo-show-hidden-files nil)
     '(neo-window-position 'left))
    ;; (setq neo-vc-integration '(face char)) ;; It's heavy at 2017-08-31

    ;; アイコン表示
    (when (require 'all-the-icons-dired nil t)
      (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

    (defvar my-neo-activated nil) ;; fail save
    (defvar my-neo-adjusted-window-width (+ 3 neo-window-width))
    (defun ad:neotree-show ()
      "Extension to support change frame width when opening neotree."
      (unless (neo-global--window-exists-p)
        (when (and (require 'moom nil t)
                   (not my-neo-activated))
          (setq moom-frame-width-single
                (+ moom-frame-width-single my-neo-adjusted-window-width))
          (setq moom-frame-width-double
                (+ moom-frame-width-double my-neo-adjusted-window-width)))
        (set-frame-width nil (+ (frame-width) my-neo-adjusted-window-width))
        (setq my-neo-activated t)))
    (advice-add 'neotree-show :before #'ad:neotree-show)

    (defun ad:neotree-hide ()
      "Extension to support change frame width when closing neotree."
      (when (neo-global--window-exists-p)
        (when (and (require 'moom nil t)
                   my-neo-activated)
          (setq moom-frame-width-single
                (- moom-frame-width-single my-neo-adjusted-window-width))
          (setq moom-frame-width-double
                (- moom-frame-width-double my-neo-adjusted-window-width)))
        (set-frame-width nil (- (frame-width) my-neo-adjusted-window-width))
        (when (> 80 (frame-width)) ;; fail safe
          (set-frame-width nil 80))
        (setq my-neo-activated nil)))
    (advice-add 'neotree-hide :before #'ad:neotree-hide)))

(when (autoload-if-found
       '(helpful-key helpful-function helpful-variable helpful-at-point
                     helpful-symbol)
       "helpful" nil t)

  (global-set-key (kbd "C-h k") 'helpful-key)
  (global-set-key (kbd "C-h @") 'helpful-at-point)
  (global-set-key (kbd "C-h o") 'helpful-symbol)
  (global-set-key (kbd "C-h f") 'helpful-function)
  (global-set-key (kbd "C-h v") 'helpful-variable)

  (with-eval-after-load "helpful"
    (define-key helpful-mode-map (kbd "@") #'helpful-at-point)))

(when (autoload-if-found
       '(facecheck-at-point facecheck-mode)
       "facecheck" nil t)
  (with-eval-after-load "facecheck"
    (facecheck-mode 1)))

(when (autoload-if-found
       '(keyfreq-mode keyfreq-autosave-mode ad:keyfreq-show)
       "keyfreq" nil t) ;; will require 'cl and 'gv (10-20[ms])

  (with-eval-after-load "keyfreq"
    (defun ad:keyfreq-show ()
      "Extension to make the buffer view-only."
      (interactive)
      (if shutup-p
          (shut-up (view-buffer keyfreq-buffer))
        (view-buffer keyfreq-buffer)))
    (advice-add 'keyfreq-show :after #'ad:keyfreq-show)
    ;; (define-key keyfreq-mode-map (kbd "q")
    ;;   (lambda () (interactive)
    ;;       (when (string= (buffer-name) keyfreq-buffer)
    ;;         (kill-buffer-and-window))))
    (setq keyfreq-file
          (expand-file-name "~/Dropbox/emacs.d/.keyfreq"))
    (keyfreq-autosave-mode 1))

  (unless noninteractive
    (keyfreq-mode 1)))

(when (autoload-if-found
       '(disk-usage)
       "disk-usage" nil t)

  (with-eval-after-load "disk-usage"
    (when (eq system-type 'darwin)
      (custom-set-variables
       '(disk-usage-du-command "du")))))

(require 'uptimes nil t)

(global-set-key (kbd "C-;") 'comment-dwim) ;; M-; is the defualt
(global-set-key (kbd "C-c c") 'compile)

(autoload-if-found '(gist-mode) "gist" nil t)

(when (autoload-if-found
       '(doxymacs-mode)
       "doxymacs" nil t)

  (add-hook 'c-mode-common-hook #'doxymacs-mode)

  (with-eval-after-load "doxymacs"
    (setq doxymacs-doxygen-style "JavaDoc")
    (add-hook 'font-lock-mode-hook
              (lambda ()
                  (when (memq major-mode '(c-mode c++-mode))
                    (doxymacs-font-lock))))
    (define-key doxymacs-mode-map (kbd "C-c C-s") 'ff-find-other-file)))

(when (and (memq window-system '(mac ns))
           (> emacs-major-version 23))
  (when (autoload-if-found
         '(matlab-mode matlab-shell)
         "matlab" nil t)
    (push '("\\.m$" . matlab-mode) auto-mode-alist)))

(when (autoload-if-found
       '(flycheck-mode)
       "flycheck" nil t)

  (dolist (hook
           '(js2-mode-hook c-mode-common-hook perl-mode-hook python-mode-hook))
    (add-hook hook #'flycheck-mode))

  (with-eval-after-load "flycheck"
    (require 'helm-flycheck nil t)
    (setq flycheck-gcc-language-standard "c++14")
    (setq flycheck-clang-language-standard "c++14")
    ;; TODO: really needed?
    ;; (when (require 'flycheck-clang-tidy nil t)
    ;;   (add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup))
    ;; http://qiita.com/senda-akiha/items/cddb02cfdbc0c8c7bc2b
    ;; (when (require 'flycheck-pos-tip nil t)
    ;;   '(custom-set-variables
    ;;     '(flycheck-display-errors-function
    ;;       #'flycheck-pos-tip-error-messages)))
    ))

;; (flycheck-add-next-checker 'javascript-jshint
;; 'javascript-gjslint)

(when (autoload-if-found
       '(origami-mode origami-toggle-node)
       "origami" nil t)

  (dolist (hook '(emacs-lisp-mode-hook c-mode-common-hook yatex-mode-hook))
    (add-hook hook #'origami-mode))

  (with-eval-after-load "origami"
    (define-key origami-mode-map (kbd "C-t") #'origami-toggle-node)
    (define-key origami-mode-map (kbd "C-u C-t")
      #'origami-toggle-all-nodes)))

(when (autoload-if-found
       '(auto-complete ac-cc-mode-setup)
       "auto-complete" nil t)

  (add-hook 'c-mode-common-hook #'ac-cc-mode-setup)

  (with-eval-after-load "auto-complete"
    (when (require 'auto-complete-clang nil t)
      (setq ac-clang-executable (executable-find "clang"))
      ;; ac-cc-mode-setup のオーバーライド
      ;; "-w" "-ferror-limit" "1"
      (defun ac-cc-mode-setup ()
        (setq ac-clang-auto-save t)
        (setq ac-clang-prefix-header "~/.emacs.d/stdafx.pch")
        (setq ac-clang-flags '("-x" "c++-header" "-fcxx-exceptions"
                               "-std=c++14" "-stdlib=libc++"
                               "-I/opt/local/include"
                               "-I/opt/local/include/netpbm"
                               "-I/Users/taka/devel/icp/lib"))
        (setq ac-sources '(ac-source-clang
                           ac-source-yasnippet
                           ac-source-gtags))))))

(when (autoload-if-found
       '(quickrun)
       "quickrun" nil t)

  (add-hook 'c-mode-common-hook
            (lambda () (define-key c++-mode-map (kbd "<f5>") 'quickrun)))
  (add-hook 'python-mode-hook
            (lambda () (define-key python-mode-map (kbd "<f5>") 'quickrun)))
  (add-hook 'perl-mode-hook
            (lambda () (define-key perl-mode-map (kbd "<f5>") 'quickrun)))
  (add-hook 'gnuplot-mode-hook
            (lambda () (define-key gnuplot-mode-map (kbd "<f5>") 'quickrun))))

(if (not (executable-find "gtags"))
    (message "--- global is NOT installed in this system.")

  (when (autoload-if-found
         '(ggtags-mode)
         "ggtags" nil t)

    (with-eval-after-load "ggtags"
      ;; (setq ggtags-completing-read-function t) ;; nil for helm
      (define-key ggtags-mode-map (kbd "M-]") nil))

    (dolist (hook (list 'c-mode-common-hook 'python-mode-hook))
      (add-hook hook (lambda () (ggtags-mode 1)))))

  (when (autoload-if-found
         '(helm-gtags-mode)
         "helm-gtags" nil t)

    (add-hook 'c-mode-common-hook #'helm-gtags-mode)
    (add-hook 'python-mode-hook #'helm-gtags-mode)

    (with-eval-after-load "helm-gtags"
      (custom-set-variables
       '(helm-gtags-mode-name "")))))

(when (autoload-if-found
       '(0xc-convert 0xc-convert-point my-decimal-to-hex my-hex-to-decimal)
       "0xc" nil t)

  (global-set-key (kbd "C-c f h") '0xc-convert)

  (with-eval-after-load "0xc"
    (defun my-decimal-to-hex ()
      (interactive)
      (0xc-convert 16 (word-at-point)))
    (defun my-hex-to-decimal ()
      (interactive)
      (0xc-convert 10 (word-at-point)))))

(with-eval-after-load "hexl"
  (custom-set-variables
   '(hexl-bits 8)))

(when (autoload-if-found
       '(uuid-string my-uuid-string)
       "uuid" nil t)

  (with-eval-after-load "uuid"
    (defun my-uuid-string ()
      (interactive)
      (insert (uuid-string)))))

(autoload-if-found '(package-lint-current-buffer) "package-lint" nil t)

(when (autoload-if-found
       '(projectile-mode)
       "projectile" nil t)

  (with-eval-after-load "neotree"
    ;; M-x helm-projectile-switch-project (C-c p p)
    (setq projectile-switch-project-action 'neotree-projectile-action)

    (defun ad:neotree-dir (path)
      "Extension to change the frame width automatically."
      (interactive "DDirectory: ")
      (unless (neo-global--window-exists-p)
        (neotree-show))
      (neo-global--open-dir path)
      (neo-global--select-window))
    ;; (advice-add 'neotree-dir :override #'ad:neotree-dir) ;; FIXME
    )

  (with-eval-after-load "projectile"
    (defun ad:projectile-visit-project-tags-table ()
      "Extensions to skip calling `visit-tags-table'."
      nil)
    (advice-add 'projectile-visit-project-tags-table :override
                #'ad:projectile-visit-project-tags-table)

    (setq projectile-mode-line-lighter "")
    (setq projectile-dynamic-mode-line nil)
    (setq projectile-tags-command "gtags")
    (setq projectile-tags-backend 'ggtags)
    (setq projectile-tags-file-name "GTAGS")

    (setq projectile-use-git-grep t)
    ;; (setq projectile-mode-line
    ;;       '(:eval (format " P:%s" (projectile-project-name))))
    (setq projectile-mode-line "")

    (setq icon-title-format
          (setq frame-title-format
                '((:eval
                   (let ((project-name (projectile-project-name)))
                     (unless (string= "-" project-name)
                       (format "(%s) - " project-name))))
                  "%b"))))

  (unless noninteractive
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (projectile-mode 1)))

(when (autoload-if-found
       '(magit-status)
       "magit" nil t)

  (global-set-key (kbd "C-c m") 'magit-status)

  (with-eval-after-load "magit"
    (require 'helm-config nil t) ;; プロジェクト一覧に helm を適用する
    (when (boundp 'magit-repository-directories)
      (setq magit-repository-directories
            '(("~/devel/git" . 1)
              ("~/devel/mygit" . 1))))))

(autoload-if-found '(relint-current-buffer) "relint" nil t)

(if (executable-find "editorconfig")
    (when (require 'editorconfig nil t)
      (unless noninteractive
        ;; (add-to-list 'editorconfig-exclude-modes 'org-mode)
        ;; (when (require 'editorconfig-charset-extras nil t)
        ;;   (add-hook 'editorconfig-custom-hooks
        ;;             #'editorconfig-charset-extras))
        (editorconfig-mode 1)))
  (message "--- editorconfig is NOT installed."))

(autoload-if-found '(cov-mode) "cov" nil t)

(require 'format-all nil t)

(autoload-if-found '(rmsbolt-mode) "rmsbolt" nil t)

(defun my-company-activate ()
  (remove-hook 'emacs-lisp-mode-hook #'my-company-activate)
  (remove-hook 'org-mode-hook #'my-company-activate)
  (require 'company nil t))
(add-hook 'emacs-lisp-mode-hook #'my-company-activate)
(add-hook 'org-mode-hook #'my-company-activate)

(with-eval-after-load "company"
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  ;; To complete file path, move `company-files' to the fist item of the list
  (delq 'company-files company-backends)
  (add-to-list 'company-backends 'company-files)
  ;; 補完候補に番号を表示
  (setq company-show-numbers t)
  (global-company-mode)
  (when (require 'company-quickhelp nil t)
    (company-quickhelp-mode)))

;; 1. TODO/DOING/DONE に trello 側のカードを変えておく．
;; 2. M-x org-trello-install-key-and-token
;; ~/.emacs.d/.trello/<account>.el が作られる
;; 3. M-x org-trello-install-board-metadata
;; Trello 側の情報を基にして current-buffer にプロパティブロックが挿入される
;; 4. C-u M-x org-trello-sync-buffer で pull
;; 5. M-x org-trello-sync-buffer で push
(when (autoload-if-found
       '(my-push-trello my-pull-trello my-activate-org-trello)
       "org-trello" nil t)

  (defvar org-trello-current-prefix-keybinding nil) ;; To avoid an error
  (add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))
  (defun my-activate-org-trello ()
    (let ((filename (buffer-file-name (current-buffer))))
      (when (and filename
                 (string= "trello" (file-name-extension filename))
                 (require 'org-trello nil t))
        (org-trello-mode))))
  (add-hook 'org-mode-hook #'my-activate-org-trello)

  (with-eval-after-load "org-trello"
    (defun my-push-trello-card () (interactive) (org-trello-sync-card))
    (defun my-pull-trello-card () (interactive) (org-trello-sync-card t))
    (defun my-push-trello () (interactive) (org-trello-sync-buffer))
    (defun my-pull-trello () (interactive) (org-trello-sync-buffer t))))

(with-eval-after-load "moom"
  (when (require 'olivetti nil t)
    (setq olivetti-lighter nil)
    (setq-default olivetti-body-width 80)
    (defun ad:turn-on-olivetti-mode ()
      "Disable `visual-line-mode'."
      (unless moom--maximized
        (olivetti-mode -1))
      (visual-line-mode -1))
    (advice-add 'turn-on-olivetti-mode :after #'ad:turn-on-olivetti-mode)
    (add-hook 'find-file-hook #'turn-on-olivetti-mode)
    (defun ad:olivetti:moom-toggle-frame-maximized ()
      (if moom--maximized
          (turn-on-olivetti-mode)
        (olivetti-mode -1)))
    (advice-add 'moom-toggle-frame-maximized :after
                #'ad:olivetti:moom-toggle-frame-maximized)))

(global-set-key (kbd "<f5>") 'my-toggle-mode-line)
(with-eval-after-load "moom"
  (defun ad:moom-toggle-frame-maximized ()
    (when (eq major-mode 'org-mode)
      (org-redisplay-inline-images))
    (when (and mode-line-format
               (not my-toggle-modeline-global))
      (my-mode-line-off)))
  (advice-add 'moom-toggle-frame-maximized
              :after #'ad:moom-toggle-frame-maximized))

;; (make-variable-buffer-local 'my-mode-line-format)
(defvar-local my-mode-line-format nil)
(set-default 'my-mode-line-format mode-line-format)
(defvar my-toggle-modeline-global t)
(unless (display-graphic-p)
  (setq my-toggle-modeline-global t)) ;; Enforce modeline in Terminal
(defun my-toggle-modeline-global ()
  (interactive)
  (setq my-toggle-modeline-global (not my-toggle-modeline-global))
  (if my-toggle-modeline-global
      (my-mode-line-on)
    (my-mode-line-off)))

(defun my-mode-line-off ()
  "Turn off mode line."
  (when (fboundp 'dimmer-on)
    (dimmer-on))
  (when (fboundp 'pomodoro:visualize-stop)
    (pomodoro:visualize-stop))
  (when mode-line-format
    (setq my-mode-line-format mode-line-format))
  (setq mode-line-format nil))

(defun my-mode-line-on ()
  "Turn on mode line."
  (when (fboundp 'dimmer-off)
    (dimmer-off))
  (when (fboundp 'pomodoro:visualize-start)
    (pomodoro:visualize-start))
  (unless my-mode-line-format
    (error "Invalid value: %s" my-mode-line-format))
  (setq mode-line-format my-mode-line-format)
  (redraw-frame))

(defun my-toggle-mode-line ()
  "Toggle mode line."
  (interactive)
  (if mode-line-format
      (my-mode-line-off)
    (my-mode-line-on))
  (message "%s" (if mode-line-format "( ╹ ◡╹)ｂ ON !" "( ╹ ^╹)ｐ OFF!")))

(add-hook 'find-file-hook
          (lambda () (unless my-toggle-modeline-global
                       (if shutup-p
                           (shut-up (my-mode-line-off))
                         (my-mode-line-off))))
          t) ;; 他の設定（olivetti.elなど）とぶつかるので最後に追加

;; init
(my-mode-line-off)

(unless noninteractive
  (postpone-message "winner-mode")
  (when (require 'winner nil t)
    (define-key winner-mode-map (kbd "C-x g") 'winner-undo)
    (define-key winner-mode-map (kbd "C-(") 'winner-undo)
    (define-key winner-mode-map (kbd "C-)") 'winner-redo)
    (winner-mode 1)))

(when (require 'shackle nil t)
  (setq shackle-default-ratio 0.33)
  (setq shackle-rules
        '(("*osx-dictionary*" :align above :popup t)
          ("*wclock*" :align above :popup t :select t)
          ("*Checkdoc Status*" :align above :popup t :noselect t)
          ;; ("*Help*" :align t :select 'above :popup t :size 0.3)
          ;; ("^\*Helm.+" :regexp t :align above :size 0.2)
          ))
  (unless noninteractive
    (shackle-mode 1)))

(with-eval-after-load "checkdoc"
  (defun my-delete-checkdoc-window ()
    (interactive)
    (let ((checkdoc-window (get-buffer-window "*Checkdoc Status*")))
      (when checkdoc-window
        (delete-window checkdoc-window)))
    (checkdoc-minor-mode -1))

  (defun ad:checkdoc ()
    (interactive)
    (define-key checkdoc-minor-mode-map (kbd "q")
      'my-delete-checkdoc-window)
    (define-key checkdoc-minor-mode-map (kbd "C-g")
      'my-delete-checkdoc-window)
    (checkdoc-minor-mode 1))

  (advice-add 'checkdoc :before #'ad:checkdoc))

(with-eval-after-load "moom"
  (when (and (not noninteractive)
             (eq my-toggle-modeline-global 'doom)
             (require 'doom-modeline nil t))
    (custom-set-variables
     '(doom-modeline-buffer-file-name-style 'truncate-except-project)
     '(doom-modeline-bar-width 1)
     '(doom-modeline-height (let ((font (face-font 'mode-line)))
                              (if (and font (fboundp 'font-info))
                                  (floor (* 0.8 ;; 1.0
                                            (* 2 (aref (font-info font) 2))))
                                10)))
     '(doom-modeline-minor-modes t))
    (declare-function ad:doom-modeline-buffer-file-state-icon "init" nil)
    (defun ad:doom-modeline-buffer-file-state-icon
        (icon &optional text face height voffset)
      "Displays an ICON with FACE, HEIGHT and VOFFSET.
TEXT is the alternative if it is not applicable.
Uses `all-the-icons-material' to fetch the icon."
      (if doom-modeline-icon
          (when icon
            (doom-modeline-icon-material
             icon
             :face face
             :height (or height 0.85) ;; 1.1
             :v-adjust (or voffset -0.225))) ;; -0.225
        (when text
          (propertize text 'face face))))
    (advice-add 'doom-modeline-buffer-file-state-icon :override
                #'ad:doom-modeline-buffer-file-state-icon)
    (size-indication-mode 1)
    (doom-modeline-mode 1)))

(set-face-foreground 'font-lock-regexp-grouping-backslash "#66CC99")
(set-face-foreground 'font-lock-regexp-grouping-construct "#9966CC")

(unless noninteractive
  (require 'generic-x nil t))

(unless noninteractive
  (global-hl-line-mode 1))

(defvar my-hl-active-period 120
  "Disable `hl-line' after this period")

(defun my-hl-line-disable ()
  "Disable `hl-line'."
  (global-hl-line-mode -1))

(defun my-hl-line-enable ()
  "Enable `hl-line'."
  (unless global-hl-line-mode
    (global-hl-line-mode 1)))

(defun my-ime-off-hline ()
  (custom-set-faces
   '(hl-line
     ((((background dark)) :background "#484c5c")
      (t (:background "#DEEDFF"))))))

(defun my-ime-on-hline ()
  (custom-set-faces
   '(hl-line
     ((((background dark)) :background "#594d5d")
      (t (:background "#fff0de"))))))

(add-hook 'move-cursor-hook #'my-hl-line-enable)
(run-with-idle-timer my-hl-active-period t #'my-hl-line-disable)
(add-hook 'focus-in-hook #'my-hl-line-enable)
(add-hook 'focus-out-hook #'my-hl-line-disable)
(add-hook 'my-ime-on-hook #'my-ime-on-hline)
(add-hook 'my-ime-off-hook #'my-ime-off-hline)
(add-hook 'my-ime-on-hook #'my-hl-line-enable)
(add-hook 'my-ime-off-hook #'my-hl-line-enable)

(setq blink-cursor-blinks 0)
(setq blink-cursor-interval 0.2)
(setq blink-cursor-delay 30)
(unless noninteractive
  (postpone-message "blink-cursor-mode")
  (blink-cursor-mode 1))

(with-eval-after-load "diff-mode"
  (set-face-attribute 'diff-added nil
                      :background nil :foreground "green"
                      :weight 'normal)

  (set-face-attribute 'diff-removed nil
                      :background nil :foreground "firebrick1"
                      :weight 'normal)

  (set-face-attribute 'diff-file-header nil
                      :background nil :weight 'extra-bold)

  (set-face-attribute 'diff-hunk-header nil
                      :foreground "chocolate4"
                      :background "white" :weight 'extra-bold
                      :inherit nil))

(when (require 'hl-todo nil t)
  (when window-system
    (add-to-list 'hl-todo-keyword-faces '("" . "orange"))
    (add-to-list 'hl-todo-keyword-faces '("" . "red"))
    (add-to-list 'hl-todo-keyword-faces '("" . "Seagreen3")))

  (defun my-hl-todo-reload ()
    (global-hl-todo-mode -1)
    (global-hl-todo-mode))

  (defun my-hl-todo-light-theme ()
    (setq hl-todo-keyword-faces
          '(("HOLD" . "#d0bf8f")
            ("TODO" . "#FF0000")
            ("NEXT" . "#dca3a3")
            ("THEM" . "#dc8cc3")
            ("PROG" . "#7cb8bb")
            ("OKAY" . "#7cb8bb")
            ("DONT" . "#5f7f5f")
            ("FAIL" . "#8c5353")
            ("DONE" . "SeaGreen")
            ("NOTE"   . "#d0bf8f")
            ("KLUDGE" . "#d0bf8f")
            ("HACK"   . "#d0bf8f")
            ("TEMP"   . "#d0bf8f")
            ("FIXME"  . "##3030FF")
            ("XXX+"   . "#cc9393")
            ("\\?\\?\\?+" . "#cc9393")))
    (my-hl-todo-reload))
  (add-hook 'my-light-theme-hook #'my-hl-todo-light-theme)

  (defun my-hl-todo-dark-theme ()
    (setq hl-todo-keyword-faces
          '(("HOLD" . "#d0bf8f")
            ("TODO" . "#cc9393")
            ("NEXT" . "#dca3a3")
            ("THEM" . "#dc8cc3")
            ("PROG" . "#7cb8bb")
            ("OKAY" . "#7cb8bb")
            ("DONT" . "#5f7f5f")
            ("FAIL" . "#8c5353")
            ("DONE" . "#afd8af")
            ("NOTE"   . "#d0bf8f")
            ("KLUDGE" . "#d0bf8f")
            ("HACK"   . "#d0bf8f")
            ("TEMP"   . "#d0bf8f")
            ("FIXME"  . "DodgerBlue1")
            ("XXX+"   . "#cc9393")
            ("\\?\\?\\?+" . "#cc9393")))
    (my-hl-todo-reload))
  (add-hook 'my-dark-theme-hook #'my-hl-todo-dark-theme)

  (global-hl-todo-mode))

(when (autoload-if-found
       '(rainbow-mode)
       "rainbow-mode" nil t)

  (dolist (hook '(emmet-mode-hook emacs-lisp-mode-hook org-mode-hook))
    (add-hook hook #'rainbow-mode)))

(when (and (executable-find "qt_color_picker")
           (autoload-if-found
            '(edit-color-stamp)
            "edit-color-stamp" nil t))

  (global-set-key (kbd "C-c f c p") 'edit-color-stamp))

(when (autoload-if-found
       '(volatile-highlights-mode my-vhl-change-color)
       "volatile-highlights" nil t)

  (dolist (hook '(org-mode-hook emacs-lisp-mode-hook emmet-mode-hook))
    (add-hook hook #'volatile-highlights-mode))

  (with-eval-after-load "volatile-highlights"
    (set-face-attribute
     'vhl/default-face nil :foreground "#FF3333" :background "#FFCDCD")
    (volatile-highlights-mode t)

    ;; ふわっとエフェクトの追加（ペースト時の色 => カーソル色 => 本来色）
    (defun my-vhl-change-color ()
      (interactive)
      (let ((next 0.2)
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

    (defun my-yank (&optional ARG)
      (interactive)
      (yank ARG)
      (when window-system
        (my-vhl-change-color)))
    (global-set-key (kbd "M-v") 'my-yank)
    (global-set-key (kbd "C-y") 'my-yank)

    (with-eval-after-load "org"
      (define-key org-mode-map (kbd "C-y") 'my-org-yank)
      (defun my-org-yank ()
        (interactive)
        (org-yank)
        (when window-system
          (my-vhl-change-color))))))

(when (autoload-if-found
       '(pomodoro:start)
       "pomodoro" nil t)

  ;; (with-eval-after-load "postpone"
  ;;   (when (and (not noninteractive)
  ;;              (not (boundp 'pomodoro:timer)))
  ;;     ;; 重複起動を回避
  ;;     (pomodoro:start nil)))

  (with-eval-after-load "pomodoro"
    ;; 作業時間終了後に開くファイルを指定しない
    (setq pomodoro:file nil)
    ;; ●だけで表現する（残り時間表示なし）
    (setq pomodoro:mode-line-time-display nil)
    ;; ●の置き換え
    (setq pomodoro:mode-line-work-sign ">>")
    (setq pomodoro:mode-line-rest-sign "<<")
    (setq pomodoro:mode-line-long-rest-sign "<>")
    ;; 長い休憩に入るまでにポモドーロする回数
    (setq pomodoro:iteration-for-long-rest 4)
    ;; 作業時間関連
    (setq pomodoro:work-time 25     ; 作業時間
          pomodoro:rest-time 5      ; 休憩時間
          pomodoro:long-rest-time 30 ; 長い休憩時間
          pomodoro:max-iteration 16) ; ポモドーロする回数
    ;; タイマーの表示をノーマルフェイスにする
    (set-face-bold-p 'pomodoro:timer-face nil)
    ;; 作業中（赤），休憩中（青），長い休憩中（緑）にする
    (custom-set-faces
     '(pomodoro:work-face
       ((((background dark)) :foreground "#DB4C46" :bold t)
        (t (:foreground "#A130C4" :bold t)))) ;; #8248c4 , #956dc4, #9d64c4
     '(pomodoro:rest-face
       ((((background dark)) :foreground "#3869FA" :bold t)
        (t (:foreground "#203e6f" :bold t))))
     '(pomodoro:long-rest-face
       ((((background dark)) :foreground "#008890" :bold t)
        (t (:foreground "#1c9b08" :bold t))))) ;; 00B800

    (defun my-pomodoro-status ()
      "Show the current `pomodoro' status in minibuffer when focus-in."
      (interactive)
      (when pomodoro:timer
        (let ((message-log-max nil))
          (message
           (format "[%d/%s] %s to go "
                   pomodoro:work-count
                   pomodoro:max-iteration
                   (pomodoro:time-to-string pomodoro:remainder-seconds))))))
    (add-hook 'focus-in-hook #'my-pomodoro-status)

    (defvar my-pomodoro-speak nil)
    (defun my-toggle-pomodoro-speak ()
      (interactive)
      (setq my-pomodoro-speak (not my-pomodoro-speak)))

    (when (memq window-system '(mac ns))
      ;; Mac ユーザ向け．Kyokoさんに指示してもらう
      (defvar pomodoro:with-speak nil)
      (when pomodoro:with-speak
        (add-hook 'pomodoro:finish-work-hook
                  (lambda ()
                    (let ((script
                           (concat "say -v Kyoko "
                                   (number-to-string
                                    (floor pomodoro:rest-time))
                                   "分間，休憩しろ")))
                      (if my-pomodoro-speak
                          (shell-command-to-string script)
                        (message "%s" script)))))

        (add-hook 'pomodoro:finish-rest-hook
                  (lambda ()
                    (let ((script
                           (concat "say -v Kyoko "
                                   (number-to-string
                                    (floor pomodoro:work-time))
                                   "分間，作業しろ")))
                      (if my-pomodoro-speak
                          (shell-command-to-string script)
                        (message "%s" script)))))

        (add-hook 'pomodoro:long-rest-hook
                  (lambda ()
                    (let ((script
                           (concat "say -v Kyoko これから"
                                   (number-to-string
                                    (floor pomodoro:long-rest-time))
                                   "分間の休憩です")))
                      (if my-pomodoro-speak
                          (shell-command-to-string script)
                        (message "%s" script))))))

      (declare-function my-pomodoro-notify "init" nil)
      (defun my-pomodoro-notify ()
        (my-desktop-notification
         "Pomodoro"
         (concat "三三 ﾍ(*ﾟ∇ﾟ)ﾉ   Go #"
                 (format "%s" (1+ pomodoro:work-count))) nil "Glass"))
      (add-hook 'pomodoro:finish-work-hook #'my-pomodoro-notify))))

(with-eval-after-load "pomodoro"
  ;; 追加実装
  (defvar pomodoro:update-work-sign-interval 0.17) ;; work用表示間隔
  (defvar pomodoro:update-rest-sign-interval 0.21) ;; rest用表示間隔
  (defvar pomodoro:update-long-rest-sign-interval 0.36) ;; long-rest用表示間隔

  (setq pomodoro:mode-line-work-sign-list
        '("/  " "// " "///" " //" "  /" "   " "   "))
  (setq pomodoro:mode-line-rest-sign-list
        '(".  " ".. " "..." "..:" ".::" ":::" ":::"
          "::." ":.." "..." " .." "  ." "   " "   "
          ",  " ",, " ",,," ",,;" ",;;" ";;;" ";;;"
          ";;," ";,," ",,," " ,," "  ," "   " "   "))
  (setq pomodoro:mode-line-long-rest-sign-list
        '("   " " | " "|||" "| |" "   "))

  ;; Example.0
  ;; (setq pomodoro:mode-line-work-sign-list
  ;;       '("|  " "|| " "|||" " ||" "  |" "   " "   "
  ;;         "  |" " ||" "|||" "|| " "|  " "   " "   "))
  ;; (setq pomodoro:mode-line-rest-sign-list
  ;;       '(".  " ".. " "..." " .." "  ." "   " "   "
  ;;         "  ." " .." "..." ".. " ".  " "   " "   "))
  ;; (setq pomodoro:mode-line-long-rest-sign-list
  ;;       '("   " " | " "|||" "| |" "   "))

  ;; Example.1
  ;; (defvar pomodoro:mode-line-work-sign-list
  ;;   '("▁" "▂" "▃" "▄" "▅" "▆" "▇" "▇" "▆" "▅" "▄" "▃" "▂" "▁" "▁" ))
  ;; (defvar pomodoro:mode-line-rest-sign-list
  ;;   pomodoro:mode-line-work-sign-list)
  ;; (defvar pomodoro:mode-line-long-rest-sign-list
  ;;   pomodoro:mode-line-work-sign-list)

  ;; Example.2
  ;; (defvar pomodoro:mode-line-work-sign-list
  ;;   '(">   " ">>  " ">>> " ">>>>" " >>>" "  >>" "   >" "    "))
  ;; (defvar pomodoro:mode-line-rest-sign-list
  ;;   '("   <" "  <<" " <<<" "<<<<" "<<< " "<<  " "<   " "    "))
  ;; (defvar pomodoro:mode-line-long-rest-sign-list
  ;;   '("  <>  " " <<>> " "<<<>>>" "<<  >>" "<    >" "      "))

  ;; Example.3
  ;; (setq pomodoro:mode-line-work-sign-list
  ;;       '("▂▁  ▁" "▃▂▁  " "▄▃▂▁ " "▅▄▃▂▁" "▆▅▄▃▂" "▇▆▅▄▃" "▇▇▆▅▄" "▆▇▇▆▅"
  ;;         "▅▆▇▇▆" "▄▅▆▇▇" "▃▄▅▆▇" "▂▃▄▅▆" "▁▂▃▄▅" " ▁▂▃▄" "  ▁▂▃" "▁  ▁▂"))

  ;; Example.4
  ;; (defvar pomodoro:mode-line-work-sign-list
  ;;   '("◤◢◤ ^-^; ◢◤◢"
  ;;     "◤◢◤ ^-^  ◢◤◢"
  ;;     "◤◢◤ ^-^  ◢◤◢"
  ;;     "◤◢◤ ^-^  ◢◤◢"
  ;;     "◤◢◤ ^-^; ◢◤◢"
  ;;     "◤◢◤ ^-^; ◢◤◢"
  ;;     "◢◤◢◤ ^-^; ◢◤"
  ;;     "◤◢◤◢◤ ^-^; ◢"
  ;;     "◢◤◢◤◢◤ ^-^; "
  ;;     " ◢◤◢◤◢◤ ^-^;"
  ;;     "; ◢◤◢◤◢◤ ^-^"
  ;;     "^; ◢◤◢◤◢◤ ^-"
  ;;     "-^; ◢◤◢◤◢◤ ^"
  ;;     "^-^; ◢◤◢◤◢◤ "
  ;;     " ^-^; ◢◤◢◤◢◤"
  ;;     "◤ ^-^; ◢◤◢◤◢"
  ;;     "◢◤ ^-^; ◢◤◢◤"));

  ;; たなこふ氏: https://twitter.com/mattn_jp/status/987203614199263233
  ;; (setq pomodoro:mode-line-work-sign-list
  ;;   '("(´･_･`)´･_･`)"
  ;;     " (´･_･`)_･`)  "
  ;;     "  (´･_･`)`)   "
  ;;     "  ((´･_･`)    "
  ;;     " (´･(´･_･`)  "
  ;;     " (´･_(´･_･`) "
  ;;     "(´･_･`)´･_･`)"
  ;;     " (´･_･`)_･`)  "
  ;;     "  (´･_･`)`)   "
  ;;     "  (´･_･`))    "
  ;;     "   ((´･_･`)   "
  ;;     "  (´･(´･_･`) "
  ;;     " (´･_(´･_･`) "));

  ;; 起動フラグ
  (defvar my-pomodoro-visualize t)

  ;; タイマーを記録
  (defvar pomodoro:update-sign-timer nil)

  ;; 初期状態を登録
  (if my-pomodoro-visualize
      (setq pomodoro:mode-line-work-sign
            (car pomodoro:mode-line-work-sign-list))
    (setq pomodoro:mode-line-work-sign "")
    (setq pomodoro:mode-line-rest-sign "")
    (setq pomodoro:mode-line-long-rest-sign ""))

  ;; utilities
  (defun pomodoro:list-rotate (sign-list)
    (if (listp sign-list)
        (append (cdr sign-list)
                (list (car sign-list)))
      sign-list))

  (defun pomodoro:activate-visual-sign (sign interval)
    (when (timerp pomodoro:update-sign-timer)
      (cancel-timer pomodoro:update-sign-timer))
    (setq pomodoro:update-sign-timer
          (run-at-time t interval sign)))

  (defun pomodoro:visualize-start ()
    (setq my-pomodoro-visualize t)
    (cond ((eq pomodoro:current-state 'rest)
           (pomodoro:update-rest-sign)
           (pomodoro:activate-visual-rest-sign))
          ((eq pomodoro:current-state 'long-rest)
           (pomodoro:update-long-rest-sign)
           (pomodoro:activate-visual-long-rest-sign))
          (t
           (pomodoro:update-work-sign)
           (pomodoro:activate-visual-work-sign))))

  (defun pomodoro:visualize-stop ()
    (setq my-pomodoro-visualize nil)
    (setq pomodoro:mode-line-work-sign "")
    (setq pomodoro:mode-line-rest-sign "")
    (setq pomodoro:mode-line-long-rest-sign "")
    (force-mode-line-update t)
    (when (timerp pomodoro:update-sign-timer)
      (cancel-timer pomodoro:update-sign-timer)))

  (defun ad:pomodoro:start (f &rest minutes)
    "Extensions to stop pomodoro and timers"
    (interactive "P")
    (pomodoro:visualize-start)
    (apply f minutes))

  (defun ad:pomodoro:stop (f &rest do-reset)
    "Extensions to stop pomodoro and timers"
    (interactive)
    (pomodoro:visualize-stop)
    (when (timerp pomodoro:timer)
      (apply f do-reset)))

  (when my-pomodoro-visualize
    (advice-add 'pomodoro:start :around #'ad:pomodoro:start)
    (advice-add 'pomodoro:stop :around #'ad:pomodoro:stop))

  ;; work
  (defun pomodoro:update-work-sign ()
    "Update pomodoro work-sign on modeline."
    (when my-pomodoro-visualize
      (setq pomodoro:mode-line-work-sign
            (car pomodoro:mode-line-work-sign-list))
      (setq pomodoro:mode-line-work-sign-list
            (pomodoro:list-rotate pomodoro:mode-line-work-sign-list))
      (force-mode-line-update t)))

  (defun pomodoro:activate-visual-work-sign ()
    (pomodoro:activate-visual-sign
     'pomodoro:update-work-sign pomodoro:update-work-sign-interval))

  ;; rest
  (defun pomodoro:update-rest-sign ()
    "Update pomodoro rest-sign on modeline."
    (when my-pomodoro-visualize
      (setq pomodoro:mode-line-rest-sign
            (car pomodoro:mode-line-rest-sign-list))
      (setq pomodoro:mode-line-rest-sign-list
            (pomodoro:list-rotate pomodoro:mode-line-rest-sign-list))
      (force-mode-line-update t)))

  (defun pomodoro:activate-visual-rest-sign ()
    (pomodoro:activate-visual-sign
     'pomodoro:update-rest-sign pomodoro:update-rest-sign-interval))

  ;; long rest
  (defun pomodoro:update-long-rest-sign ()
    "Update pomodoro long-rest-sign on modeline."
    (when my-pomodoro-visualize
      (setq pomodoro:mode-line-long-rest-sign
            (car pomodoro:mode-line-long-rest-sign-list))
      (setq pomodoro:mode-line-long-rest-sign-list
            (pomodoro:list-rotate pomodoro:mode-line-long-rest-sign-list))
      (force-mode-line-update t)))

  (defun pomodoro:activate-visual-long-rest-sign ()
    (pomodoro:activate-visual-sign
     'pomodoro:update-long-rest-sign pomodoro:update-long-rest-sign-interval))

  ;; ステータスが切り替わる時に表示を入れ替える
  (when my-pomodoro-visualize
    (add-hook 'pomodoro:finish-rest-hook #'pomodoro:activate-visual-work-sign)
    (add-hook 'pomodoro:finish-work-hook #'pomodoro:activate-visual-rest-sign)
    (add-hook 'pomodoro:long-rest-hook
              #'pomodoro:activate-visual-long-rest-sign)))

(when (autoload-if-found
       '(my-google-this google-this google-this-word)
       "google-this" nil t)

  (global-set-key (kbd "C-c f g") 'my-google-this)

  (with-eval-after-load "google-this"
    (defun my-google-this ()
      (interactive)
      (google-this (current-word) t))))

(when (autoload-if-found
       '(lingr-login my-lingr-login)
       "lingr" nil t)

  (with-eval-after-load "lingr"
    (custom-set-variables
     '(lingr-icon-mode t)
     '(lingr-icon-fix-size 24)
     '(lingr-image-convert-program  (or (executable-find "convert")
                                        lingr-image-convert-program))))

  (when (future-time-p "23:00")
    ;; do not use `run-at-time' at booting since diary-lib.el
    ;; will be loaded. It requires loading cost.
    (run-at-time "23:00" nil 'my-lingr-login)))

(when (autoload-if-found
       '(multi-term)
       "multi-term" nil t)

  (with-eval-after-load "multi-term"
    (setenv "HOSTTYPE" "intel-mac")))

(when (autoload-if-found
       '(osx-lib-say osx-lib-say-region)
       "osx-lib" nil t)

  (with-eval-after-load "osx-lib"
    (custom-set-variables
     '(osx-lib-say-ratio 100)
     '(osx-lib-say-voice "Samantha"))))

(global-set-key (kbd "C-c f t") 'my-open-current-directory-in-terminal)

(when (autoload-if-found
       '(gif-screencast)
       "gif-screencast" nil t)

  (with-eval-after-load "gif-screencast"
    (setq gif-screencast-want-optimized nil)
    (setq gif-screencast-args '("-x"))
    (setq gif-screencast-capture-format "ppm")

    ;; Start... M-x gif-screencast
    (define-key gif-screencast-mode-map (kbd "<f5>") 'gif-screencast-stop)
    (define-key gif-screencast-mode-map (kbd "S-<f5>")
      'gif-screencast-toggle-pause)

    ;; 拡張
    (defcustom gif-screencast-additional-normal-hooks '()
      "A list of hooks. These hooks activate `gif-screencast-capture'."
      :group 'gif-screencast
      :type '(repeat hook))

    (add-to-list 'gif-screencast-additional-normal-hooks
                 'window-size-change-functions) ;; for which-key.el, as of 26.1
    (add-to-list 'gif-screencast-additional-normal-hooks
                 'window-configuration-change-hook) ;; for which-key.el
    (add-to-list 'gif-screencast-additional-normal-hooks 'focus-in-hook)
    (add-to-list 'gif-screencast-additional-normal-hooks 'focus-out-hook)
    (add-to-list 'gif-screencast-additional-normal-hooks 'minibuffer-setup-hook)
    (add-to-list 'gif-screencast-additional-normal-hooks 'minibuffer-exit-hook)
    ;; (add-to-list 'gif-screencast-additional-normal-hooks 'pre-redisplay-functions)
    ;; pre-redisplay-functions はやばい．

    ;; modification-hooks
    (defun ad:gif-screencast ()
      (dolist (hook gif-screencast-additional-normal-hooks)
        (add-hook hook #'gif-screencast-capture)))
    (advice-add 'gif-screencast :after #'ad:gif-screencast)

    (defun ad:gif-screencast-stop ()
      (dolist (hook gif-screencast-additional-normal-hooks)
        (remove-hook hook 'gif-screencast-capture)))
    (advice-add 'gif-screencast-stop :after #'ad:gif-screencast-stop)

    (defun ad:gif-screencast-toggle-pause ()
      (if (memq 'gif-screencast-capture (default-value 'pre-command-hook))
          (dolist (hook gif-screencast-additional-normal-hooks)
            (remove-hook hook 'gif-screencast-capture))
        (dolist (hook gif-screencast-additional-normal-hooks)
          (add-hook hook #'gif-screencast-capture))))
    (advice-add 'gif-screencast-toggle-pause
                :before #'ad:gif-screencast-toggle-pause)

    (defun ad:gif-screencast-opendir ()
      "Open the output directory when screencast is finished."
      (if (not (eq system-type 'darwin))
          (my-gif-screencast-opendir-dired)
        (shell-command-to-string
         (concat "open " gif-screencast-screenshot-directory))
        (shell-command-to-string
         (concat "open " gif-screencast-output-directory))))
    (advice-add 'gif-screencast-stop :before #'ad:gif-screencast-opendir)

    (defun my-gif-screencast-opendir-dired ()
      "Open directories for screenshots and generated GIFs by Dired."
      (interactive)
      (dired gif-screencast-output-directory)
      (dired gif-screencast-screenshot-directory))))

(global-set-key (kbd "C-M--") 'my-cycle-bullet-at-heading)
(global-set-key (kbd "<f12>") 'my-open-file-ring)
;;  (global-set-key (kbd "C-c t") 'my-date)
(global-set-key (kbd "C-c f 4") 'my-window-resizer)

(when (autoload-if-found
       '(manage-minor-mode)
       "manage-minor-mode" nil t)

  (with-eval-after-load "manage-minor-mode"
    (define-key manage-minor-mode-map (kbd "q")
      (lambda () (interactive)
          (delete-window (get-buffer-window "*manage-minor-mode*"))))))

(autoload-if-found '(gitter) "gitter"  nil t)

(if (not (executable-find "pass"))
    (message "--- pass is NOT installed.")
  (global-set-key (kbd "C-c f p") 'helm-pass)
  (autoload-if-found '(helm-pass) "helm-pass" nil t)
  (autoload-if-found '(ivy-pass) "ivy-pass" nil t))

(provide 'late-init)
