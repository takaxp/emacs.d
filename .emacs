;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
;; (load "~/Dropbox/emacs.d/config/el-get-setup.el" nil t)
;; (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el"))
;; ln -s ~/Dropbox/emacs.d/config/.spacemacs ~/
;; git clone https://github.com/syl20bnr/spacemacs ~/.spacemacs.d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 水平方向の自動スクロールを制御
(setq hscroll-margin 40)

;; dired に反映されるのは確認できたが，completion では確認できず．
;; Possible completions
(setq completion-ignored-extensions
      (append completion-ignored-extensions
              '("./" "../" ".xlsx" ".docx" ".pptx" ".DS_Store")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy-rich
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (when (require 'ivy-rich nil t) ;; Heavy?
;;   (ivy-rich-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy/Counsel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-recent-headings
;; org-recent-headings は helm/ivy の共存が難しい．
;; byte-compile しているとなおさら．
;; ~/.emacs.d/org-recent-headings.dat は端末間で共有しないほうがいいかも．
;; 履歴の掃除が必要かもしれない．対象の org ファイルが永続的なら生じない問題．
(when (autoload-if-found
       '(org-recent-headings-ivy org-recent-headings-mode)
       "org-recent-headings" nil t)

  (with-eval-after-load "ivy"
    ;; Originally located in org-recent-headings.el.
    (defun org-recent-headings-ivy ()
      "Choose from recent Org headings with Ivy."
      (interactive)
      (org-recent-headings :completing-read-fn #'ivy-completing-read)))

  ;; for Ivy interface
  (with-eval-after-load "org-recent-headings"
    ;;    (setq helm-source-org-recent-headings nil)
    (setq org-recent-headings-show-entry-function
          'org-recent-headings--show-entry-direct)
    (setq org-recent-headings-advise-functions
          '(org-agenda-goto
            org-agenda-show
            org-agenda-show-mouse
            org-show-entry
            org-reveal
            org-refile
            org-tree-to-indirect-buffer
            org-bookmark-jump))
    (custom-set-variables
     '(org-recent-headings-save-file "~/.emacs.d/org-recent-headings.dat")))

  (with-eval-after-load "postpone"
    (global-set-key (kbd "C-c f r") 'org-recent-headings-ivy)
    (defun ad:org-recent-headings-activate ()
      (interactive)
      (when (and (require 'dash-functional nil t) ;; FIXME
                 (require 'org-recent-headings nil t))
        (org-recent-headings-mode)
        (advice-remove 'org-recent-headings-ivy
                       #'ad:org-recent-headings-activate)))
    (advice-add 'org-recent-headings-ivy :before
                #'ad:org-recent-headings-activate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; counsel-projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load "projectile"
  (when (require 'counsel-projectile nil t)
    ;; M-o z で fzf を呼び出せる．
    ;; https://twitter.com/takaxp/status/1134481340458360832
    (defun counsel-projectile-action-fzf (_project)
      "Search the current directory with fzf."
      (counsel-fzf))
    ;; あれ，これなんで動いてるの？関数に add-to-list できるの？
    ;; 以下の2つの関数は，動的に生成される defcustom で規定される．すごい．
    (add-to-list 'counsel-projectile-switch-project-action
                 '("z" counsel-projectile-action-fzf
                   "search project with fzf") t)
    (add-to-list 'counsel-projectile-find-file-action
                 '("z" counsel-projectile-action-fzf
                   "search project with fzf") t)
    (setq projectile-completion-system 'ivy)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (counsel-projectile-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Swiper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (autoload-if-found '(counsel-M-x counsel-recentf) "counsel" nil t)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-M-z") 'counsel-fzf)
  (global-set-key (kbd "C-M-r") 'counsel-recentf)
  (global-set-key (kbd "C-h b") 'counsel-descbinds)
  ;; counsel-ibuffer の表示項目のカスタマイズは？ ivy-switch-buffer と同じ？
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)) ;; ivy-switch-buffer?

(with-eval-after-load "postpone"
  (global-set-key (kbd "C-M-f") 'counsel-ag)
  (when (require 'swiper nil t)
    ;; こういうの exclude list を実装したい．"^\\*+" に限らず判定したいね．
    (defun ad:swiper-thing-at-point ()
      "`swiper' with `ivy-thing-at-point'."
      (interactive)
      (let ((thing (if (thing-at-point-looking-at "^\\*+")
                       nil
                     (ivy-thing-at-point))))
        (when (use-region-p)
          (deactivate-mark))
        (swiper thing)))
    (advice-add 'swiper-thing-at-point :override #'ad:swiper-thing-at-point)
    (global-set-key (kbd "M-s M-s") 'swiper-thing-at-point))

  (when (and (require 'ivy nil t)
             (require 'counsel nil t))

    (global-set-key (kbd "M-y") 'counsel-yank-pop)
    ;;    (global-set-key (kbd "C-M-l") 'counsel-locate) ;; or counsel-fzf?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; counsel-recentf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; counsel-recentf のリストを "~/" から始める．
    (defun my-counsel-recentf ()
      "Find a file on `recentf-list'."
      (interactive)
      (require 'recentf)
      (recentf-mode)
      (ivy-read "Recentf: "
                (progn
                  (mapcar #'substring-no-properties recentf-list) ;; remove?
                  (mapcar #'abbreviate-file-name recentf-list)) ;; starting "~/"
                :action (lambda (f)
                          (with-ivy-window
                            (find-file f)))
                :require-match t
                :caller 'counsel-recentf))
    (advice-add 'counsel-recentf :override #'my-counsel-recentf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for bm.el
;;; https://www.reddit.com/r/emacs/comments/700xck/ivy_with_bmel_bookmark_manager/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun bm-counsel-get-list (bookmark-overlays)
      (-map (lambda (bm)
              (with-current-buffer (overlay-buffer bm)
                (let* ((line (replace-regexp-in-string
                              "\n$" ""
                              (buffer-substring (overlay-start bm)
                                                (overlay-end bm))))
                       ;; line numbers start on 1
                       (line-num
                        (+ 1 (count-lines (point-min) (overlay-start bm))))
                       (name (format "%s:%d - %s" (buffer-name) line-num line)))
                  `(,name . ,bm))))
            bookmark-overlays))

    (defun bm-counsel-find-bookmark ()
      (interactive)
      (let* ((bm-list (bm-counsel-get-list (bm-overlays-lifo-order t)))
             (bm-hash-table (make-hash-table :test 'equal))
             (search-list (-map (lambda (bm) (car bm)) bm-list)))
        (-each bm-list (lambda (bm)
                         (puthash (car bm) (cdr bm) bm-hash-table)
                         ))
        (ivy-read "Find bookmark: "
                  search-list
                  :require-match t
                  :keymap counsel-describe-map
                  :action (lambda (chosen)
                            (let ((bookmark (gethash chosen bm-hash-table)))
                              (switch-to-buffer (overlay-buffer bookmark))
                              (bm-goto bookmark)
                              ))
                  :sort t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; disable counsel-find-file
;;; https://emacs.stackexchange.com/questions/45929/disable-ivy-for-find-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; completion-in-region-function も一時的にデフォに戻さないと，TAB補完時に
    ;; ivy が有効化されてしまう．
    (defun my-disable-counsel-find-file (&rest args)
      "Disable `counsel-find-file' and use the original `find-file' with ARGS."
      (let ((completing-read-function #'completing-read-default)
            (completion-in-region-function #'completion--in-region))
        (apply #'read-file-name-default args)))
    (setq read-file-name-function #'my-disable-counsel-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 絞り込み過程の単語のハイライト配色
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defface my-ivy-arrow-visible
      '((((class color) (background light))
         :foreground "orange") ;;  :background "#FFF6F6"
        (((class color) (background dark)) :foreground "#EE6363"))
      "Face used by Ivy for highlighting the arrow.")
    ;; FIXME
    (defface my-ivy-arrow-invisible
      '((((class color) (background light)) :foreground "#FFFFFF")
        (((class color) (background dark)) :foreground "#31343F"))
      "Face used by Ivy for highlighting the invisible arrow.")
    (custom-set-faces
     '(ivy-current-match
       ((((class color) (background light))
         :background "#FFF3F3" :distant-foreground "black")
        (((class color) (background dark))
         :background "#4F5353" :distant-foreground "#b03333")))
     '(ivy-minibuffer-match-face-1
       ((((class color) (background light)) :foreground "#d3d3d3")
        (((class color) (background dark)) :foreground "#555555")))
     '(ivy-minibuffer-match-face-2
       ((((class color) (background light)) :foreground "#b00000" :underline t)
        (((class color) (background dark)) :foreground "#b03333" :underline t)))
     '(ivy-minibuffer-match-face-3
       ((((class color) (background light)) :foreground "#bbbbff")
        (((class color) (background dark)) :foreground "#7777ff")))
     '(ivy-minibuffer-match-face-4
       ((((class color) (background light)) :foreground "#ffbbff")
        (((class color) (background dark)) :foreground "#8a498a"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; M-x `find-library' が大量の =./= を表示する問題を回避
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; `find-library' ではなく `counsel-find-library' を直接呼べばOK．
    ;; M-x のリストに出したくないので，advice はダメ．
    ;; find-library から interactive を取ってしまう
    ;; これなら，たとえcounsel-find-library が呼んでいたとしても M-x には出ない
    ;; a. (make-obsolete 'find-library 'counsel-find-library)
    ;; b. (put 'find-library 'disabled t)
    (when (require 'find-func nil t)
      (defun find-library (library)
        "Override the original `find-library' to hide in command list."
        (prog1
            (switch-to-buffer (find-file-noselect (find-library-name library)))
          (run-hooks 'find-function-after-hook))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 選択対象を "" にする（かなり重くなる？）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq ivy-format-function #'ivy-format-function-arrow)
    (when window-system
      (defun ad:ivy-format-function-arrow (cands)
        "Transform CANDS into a string for minibuffer."
        (ivy--format-function-generic
         ;; (lambda (str)
         ;;   (concat (format "%s" (ivy--add-face "\t" 'my-ivy-arrow-visible))
         ;;           (ivy--add-face str 'ivy-current-match)))
         ;; (lambda (str)
         ;;   (concat (format "%-4s" " ")
         ;;           str))
         (lambda (str)
           (concat (ivy--add-face " " 'my-ivy-arrow-visible)
                   (ivy--add-face str 'ivy-current-match)))
         (lambda (str)
           (concat (ivy--add-face " " 'my-ivy-arrow-invisible)
                   str))
         cands
         "\n"))
      (advice-add 'ivy-format-function-arrow
                  :override #'ad:ivy-format-function-arrow))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-M-x と同じ振る舞いにする設定（4点）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; コマンド使用履歴を使って counsel-M-x の候補を表示する（4点目）
    ;; 激重の様子？
    ;; 1. extended-command-history
    (when (require 'smex nil t)
      (setq smex-history-length 35)
      (setq smex-completion-method 'ivy))

    ;; 2. exclude `counsel-M-x'
    (setq ivy-initial-inputs-alist
          '(
            ;; (org-refile . "^")
            (org-agenda-refile . "^")
            (org-capture-refile . "^")
            (counsel-describe-function . "^")
            (counsel-describe-variable . "^")
            (counsel-org-capture . "^")
            (Man-completion-table . "^")
            (woman . "^")))

    ;; 3. ignore-order
    (add-to-list 'ivy-re-builders-alist '(t . ivy--regex-ignore-order))

    ;; 4. sort
    ;; https://github.com/abo-abo/swiper/issues/1294
    (defun ivy--sort-by-len (name candidates)
      "Sort CANDIDATES based on similarity of their length with NAME."
      (let ((name-len (length name))
            (candidates-count (length candidates)))
        (if (< 500 candidates-count)
            candidates
          (seq-sort-by #'length
                       (lambda (a b)
                         (< (abs (- name-len a))
                            (abs (- name-len b))))
                       candidates))))
    ;; 全体に適用すると，counsel-recentf が壊れる．
    ;; counsel-M-x を狙い撃ちで設定する．
    ;; (setf (alist-get 't ivy-sort-matches-functions-alist)
    ;;       #'ivy--sort-by-len)
    (add-to-list 'ivy-sort-matches-functions-alist
                 '(counsel-M-x . ivy--sort-by-len) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flyspell-correct-ivy
;;; https://github.com/d12frosted/flyspell-correct/issues/10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; =M-o= 押下しないと，save option が出てこない．つまり，<F7> M-o s となる．
    ;; <F7><F7> の夢，敗れる．
    (when (require 'flyspell-correct-ivy nil t)
      (defun flyspell-correct-ivy-inline-actions (candidates word)
        "Run `ivy-read' for the given CANDIDATES given by flyspell for the WORD.
Return a selected word to use as a replacement or a tuple
of (command, word) to be used by flyspell-do-correct."
        (let* (result
               (action (lambda (x) (setq result x))))
          (ivy-read (format "Suggestions for \"%s\" in dictionary \"%s\": "
                            word (or ispell-local-dictionary
                                     ispell-dictionary
                                     "Default"))
                    (append
                     (mapcar (lambda (x) (cons x x)) candidates)
                     (list (cons (format "Save \"%s\"" word) (cons 'save word))
                           (cons (format "Accept (session) \"%s\"" word) (cons 'session word))
                           (cons (format "Accept (buffer) \"%s\"" word) (cons 'buffer word))))
                    :action action)
          result))

      (setq flyspell-correct-interface 'flyspell-correct-ivy-inline-actions)
      (global-set-key (kbd "<f7>") 'flyspell-correct-word-generic))

    ;; `ivy-switch-buffer' のリストに recent files と bookmark を含める．
    ;; C-x b のことであり，C-x C-b ではない．
    (setq ivy-use-virtual-buffers t)

    ;; ミニバッファでコマンド発行を認める (t)
    (setq enable-recursive-minibuffers nil) ;
    (minibuffer-depth-indicate-mode 1) ;; 何回層入ったかプロンプトに表示．

    ;; https://github.com/abo-abo/swiper/blob/596461e1ff09431eb417877a9870e53c452e1b62/doc/Changelog.org
    ;; https://github.com/abo-abo/swiper/issues/924
    ;; https://github.com/abo-abo/swiper/issues/309
    (ivy-mode 1)

    ;; (counsel-mode 1) ;; 無くても大きな問題ないかも．
    ;; (global-set-key (kbd "C-h b") 'counsel-descbinds)
    ;; (counsel-mode 1) があれば，'counsel-descbinds の set-key は不要．
    ;; 逆に(counsel-mode 1) があれば， find-file の張替えが必要．
    ;; (counsel-mode 1) を書かずに，(kbd "C-h b")を設定するのでも良い．
    ;; (global-set-key (kbd "C-x C-f") 'find-file) ;; reconfigure
    ;; (counsel-mode 1) 使うと，counsel-find-file を取り返せないので，
    ;; 有効にしない方がいい．
    ;; うまいことすれば，(counsel-mode 1) しつつ，counsel-find-file を無効化できるかも．
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (with-eval-after-load "company"
;;   (when (and (require 'all-the-icons nil t)
;;              (require 'company-box nil t))
;;     (add-hook 'company-mode-hook 'company-box-mode)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (when (require 'ivy-posframe nil t)
;;   (setq ivy-display-function #'ivy-posframe-display)
;;   ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
;;   ;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
;;   ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
;;   ;; (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
;;   ;; (setq ivy-display-function #'ivy-posframe-display-at-point)
;;   (ivy-posframe-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (with-eval-after-load "postpone" (require 'frog-jump-buffer nil t))

;; Trying LSP
;; https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
(when (and (fboundp 'lsp)
           (autoload-if-found '(lsp) "lsp-mode" nil t))

  (add-hook 'c-mode-common-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)

  (with-eval-after-load "lsp"
    (setq lsp-prefer-flymake nil)
    (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error")))

  (with-eval-after-load "lsp-ui"
    (setq lsp-ui-doc-enable t
          lsp-ui-doc-use-childframe t
          lsp-ui-doc-position 'top
          lsp-ui-doc-include-signature t
          lsp-ui-sideline-enable nil
          lsp-ui-flycheck-enable t
          lsp-ui-flycheck-list-position 'right
          lsp-ui-flycheck-live-reporting t
          lsp-ui-peek-enable t
          lsp-ui-peek-list-width 60
          lsp-ui-peek-peek-height 25)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fontawesome 拡張
(when window-system
  (with-eval-after-load "postpone"
    ;; 以下を関数化して，任意の文字コードに対応させる．
    (defface my-face-f0a4 '((t (:foreground "orange")))
      nil :group 'font-lock-highlighting-faces)
    (defface my-face-f088 '((t (:foreground "red")))
      nil :group 'font-lock-highlighting-faces)
    (defface my-face-f087 '((t (:foreground "Seagreen3")))
      nil :group 'font-lock-highlighting-faces)
    (defvar my-face-f0a4 'my-face-f0a4)
    (defvar my-face-f088 'my-face-f088)
    (defvar my-face-f087 'my-face-f087)
    (defadvice font-lock-mode (before my-font-lock-mode1 ())
      (font-lock-add-keywords
       major-mode
       '(("" 0 my-face-f0a4 append)
         ("" 0 my-face-f088 append)
         ("" 0 my-face-f087 append))))
    (ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode1)
    (ad-activate 'font-lock-mode)))

;; (when (require 'hi-lock nil t)
;;   (highlight-phrase "")
;;   (highlight-phrase "")
;;   (highlight-phrase "")
;;   (global-hi-lock-mode))

;; To decrypt old sub trees
;; (advice-add 'epg--check-error-for-decrypt :override 'ignore)

(when (autoload-if-found '(facecheck-at-point) "facecheck" nil t)
  (with-eval-after-load "facecheck"
    (facecheck-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs ends here
