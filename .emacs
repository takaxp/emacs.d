;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
;; (load "~/Dropbox/emacs.d/config/el-get-setup.el" nil t)
;; (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el"))
;; ln -s ~/Dropbox/emacs.d/config/.spacemacs ~/
;; git clone https://github.com/syl20bnr/spacemacs ~/.spacemacs.d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME
;; https://github.com/akirak/ivy-omni-org
;; https://github.com/Kungsgeten/ivy-todo
;; https://github.com/mkcms/ivy-yasnippet
;; https://github.com/squiter/ivy-youtube

(with-eval-after-load "postpone" (my-mode-line-off))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DONE org-tag でタグを空にする．C-c C-c した後，タグを消して，C-M-j で良い．
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; counsel-world-clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load "ivy"
  (require 'counsel-world-clock nil t)) ;; M-x counsel-world-clock

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy/Counsel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 計測上，helm よりも重いかもしれない，ivy/swiper/counsel をしっかり遅延ロードさせるように設定したい
;; org-recent-headings
;; org-recent-headings は helm/ivy の共存が難しい．
;; byte-compile しているとなおさら．
;; ~/.emacs.d/org-recent-headings.dat は端末間で共有しないほうがいいかも．
;; 履歴の掃除が必要かもしれない．対象の org ファイルが永続的なら生じない問題．
;; `org-recent-headings' 内のrealが生成されていないことがわかった．
(with-eval-after-load "org-recent-headings"
  ;; DONE 記録するレベルを限定する．level=2だけとか．
  ;; org-recent-headings--store-heading のハックが必要．
  ;; (defun org-recent-headings--store-heading (&rest ignore)
  ;;   "Add current heading to `org-recent-headings' list."
  ;;         (org-with-wide-buffer
  ;;          (unless (org-before-first-heading-p)
  ;;            (when (< 1 (org-current-level))
  )

(when (autoload-if-found
       '(org-recent-headings-ivy org-recent-headings-mode)
       "org-recent-headings" nil t)

  (with-eval-after-load "ivy"
    ;; デフォルトだと `ivy-string<' が使われてしまい，使用履歴が反映されない．
    ;; うので， recent-headings.dat に記録された順が反映されない．
    (add-to-list 'ivy-sort-functions-alist '(org-recent-headings-ivy . nil))

    ;; Originally located in org-recent-headings.el.
    (defun org-recent-headings-ivy ()
      "Choose from recent Org headings with Ivy."
      (interactive)
      (org-recent-headings :completing-read-fn #'ivy-completing-read)))

  ;; for Ivy interface
  (with-eval-after-load "org-recent-headings"
    (custom-set-variables
     '(org-recent-headings-save-file "~/.emacs.d/org-recent-headings.dat")
     '(org-recent-headings-show-entry-function
       'org-recent-headings--show-entry-direct)
     '(org-recent-headings-advise-functions
       '(org-agenda-goto
         org-agenda-show
         org-agenda-show-mouse
         org-show-entry
         org-reveal
         org-refile
         org-tree-to-indirect-buffer
         org-bookmark-jump))))

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
;; eldoc with ivy
(with-eval-after-load "eldoc"
  ;; ミニバッファの使用中には eldoc を表示させない．
  (defun ad:eldoc-message (f &optional string)
    (unless (active-minibuffer-window)
      (funcall f string)))
  (advice-add 'eldoc-message :around #'ad:eldoc-message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; header-line-format for ivy prompt
(with-eval-after-load "ivy"
  ;; header-line-format は効かない様子．
  (defvar ivy-prompt-headerline t) ;; ime-on/-off に合わせて変えてもOK．
  (defun my-toggle-ivy-prompt-headerline ()
    "Toggle showing a header line before the ivy prompt."
    (interactive)
    (setq ivy-prompt-headerline (not ivy-prompt-headerline)))

  (defun ivy-prompt-headerline ()
    "Return a header line for the ivy prompt."
    (when ivy-prompt-headerline
      (if window-system
          (format "%s\n%s " (make-string (frame-width) ?\x5F) ;; "__", ?\x2D
                  (all-the-icons-faicon "sort-amount-asc")) ;; ""
        (format "%s\n" (make-string (1- (frame-width)) ?\x2F))))) ;; "////"

  (defun ad:ivy--insert-prompt ()
    "Update the prompt according to `ivy--prompt'."
    (when (setq ivy--prompt (ivy-prompt))
      (unless (memq this-command '(ivy-done ivy-alt-done ivy-partial-or-done
                                            counsel-find-symbol))
        (setq ivy--prompt-extra ""))
      (let (head tail)
        (if (string-match "\\(.*?\\)\\(:? ?\\)\\'" ivy--prompt)
            (progn
              (setq head (match-string 1 ivy--prompt))
              (setq tail (match-string 2 ivy--prompt)))
          (setq head ivy--prompt)
          (setq tail ""))
        (let ((inhibit-read-only t)
              (std-props '(front-sticky t rear-nonsticky t field t read-only t))
              (n-str
               (concat
                (if (and (bound-and-true-p minibuffer-depth-indicate-mode)
                         (> (minibuffer-depth) 1))
                    (format "[%d] " (minibuffer-depth))
                  "")
                (concat
                 (if (string-match "%d.*%d" ivy-count-format)
                     (format head
                             (1+ ivy--index)
                             (or (and (ivy-state-dynamic-collection ivy-last)
                                      ivy--full-length)
                                 ivy--length))
                   (format head
                           (or (and (ivy-state-dynamic-collection ivy-last)
                                    ivy--full-length)
                               ivy--length)))
                 ivy--prompt-extra
                 tail)))
              (d-str (if ivy--directory
                         (abbreviate-file-name ivy--directory)
                       "")))
          (save-excursion
            (goto-char (point-min))
            (delete-region (point-min) (minibuffer-prompt-end))
            (let ((len-n (length n-str))
                  (len-d (length d-str))
                  (ww (window-width)))
              (setq n-str
                    (cond ((> (+ len-n len-d) ww)
                           (concat n-str "\n" d-str "\n"))
                          ((> (+ len-n len-d (length ivy-text)) ww)
                           (concat n-str d-str "\n"))
                          (t
                           (concat n-str d-str)))))
            ;; ADDED
            (when ivy-prompt-headerline
              (setq n-str (concat (ivy-prompt-headerline) n-str)))
            ;;
            (when ivy-add-newline-after-prompt
              (setq n-str (concat n-str "\n")))
            (let ((regex (format "\\([^\n]\\{%d\\}\\)[^\n]" (window-width))))
              (while (string-match regex n-str)
                (setq n-str (replace-match
                             (concat (match-string 1 n-str) "\n")
                             nil t n-str 1))))
            (set-text-properties 0 (length n-str)
                                 `(face minibuffer-prompt ,@std-props)
                                 n-str)
            (setq n-str (funcall ivy-set-prompt-text-properties-function
                                 n-str std-props))
            (insert n-str))
          ;; Mark prompt as selected if the user moves there or it is the only
          ;; option left.  Since the user input stays put, we have to manually
          ;; remove the face as well.
          (when ivy--use-selectable-prompt
            (if (or (= ivy--index -1)
                    (= ivy--length 0))
                (ivy-add-face-text-property
                 (minibuffer-prompt-end) (line-end-position) 'ivy-prompt-match)
              (remove-list-of-text-properties
               (minibuffer-prompt-end) (line-end-position) '(face))))
          ;; get out of the prompt area
          (constrain-to-field nil (point-max))))))
  (advice-add 'ivy--insert-prompt :override #'ad:ivy--insert-prompt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; counsel-projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load "projectile"
  (when (require 'counsel-projectile nil t)
    ;; M-o z で fzf を呼び出せる．
    ;; https://twitter.com/takaxp/status/1134481340458360832
    (defun counsel-projectile-action-fzf (_project)
      "Search the current directory with fzf."
      (counsel-fzf nil default-directory))
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
(when (autoload-if-found
       '(counsel-M-x counsel-recentf counsel-fzf)
       "counsel" nil t)
  (global-set-key (kbd "M-x")   'counsel-M-x)
  (global-set-key (kbd "C-M-z") 'counsel-fzf)
  (global-set-key (kbd "C-M-r") 'counsel-recentf)
  ;; counsel-ibuffer の表示項目のカスタマイズは？ ivy-switch-buffer と同じ？
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)) ;; ivy-switch-buffer?

(with-eval-after-load "postpone"
  ;; 選択範囲をすぐに ag 検索対象にしたい．Swiperのように
  (global-set-key (kbd "C-M-f") 'counsel-ag)

  ;; "C-u C-M-f" でディレクトリ指定したい
  (defun ad:counsel-ag (f &optional initial-input initial-directory extra-ag-args ag-prompt caller)
    (apply f (or initial-input (ivy-thing-at-point))
           (or initial-directory default-directory)
           extra-ag-args ag-prompt caller))
  (advice-add 'counsel-ag :around #'ad:counsel-ag)

  (defun ad:counsel-fzf (f &optional initial-input initial-directory fzf-prompt)
    (apply f (or initial-input
                 (when (use-region-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))))
           (or initial-directory default-directory)
           fzf-prompt))
  (advice-add 'counsel-fzf :around #'ad:counsel-fzf)

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
    ;; (setq counsel-find-file-ignore-regexp
    ;;       (regexp-opt '("~/.emacs.d/bookmarks")))
    ;; recentf-exlude をカスタマイズする方が良い．
    ;; :matcher #'counsel--find-file-matcher
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; counsel-recentf のリストを "~/" から始める．
    (defun ad:counsel-recentf ()
      "Find a file on `recentf-list'."
      (interactive)
      (require 'recentf)
      (recentf-mode)
      (ivy-read "Recentf: "
                (progn
                  (mapcar #'substring-no-properties recentf-list) ;; no need?
                  (mapcar #'abbreviate-file-name recentf-list)) ;; starting "~/"
                :action (lambda (f)
                          (with-ivy-window
                            (find-file f)))
                :require-match t
                :caller 'counsel-recentf))
    (advice-add 'counsel-recentf :override #'ad:counsel-recentf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for bm.el
;;; https://www.reddit.com/r/emacs/comments/700xck/ivy_with_bmel_bookmark_manager/
;;; bm-bookmarks の代替で使いたいが，annotation が表示されていない．
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun counsel-bm-get-list (bookmark-overlays)
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

    (defun counsel-bm-show-all ()
      (interactive)
      (let* ((bm-list (counsel-bm-get-list (bm-overlays-lifo-order t)))
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
    (global-set-key (kbd "<S-f10>") 'counsel-bm-show-all)

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
    ;; (counsel-mode 1) を設定しても counsel-find-file が呼ばれないようにする．
    (define-key counsel-mode-map [remap find-file]  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-key counsel-mode-map [remap org-capture]  'counsel-org-capture)

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
;;; 選択対象を "" にする
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; https://github.com/abo-abo/swiper/commit/7feb14f476e108dc10fe5380ee7c33de3b5fe3f1 の変更で，(setq ivy-format-function #'ivy-format-function-arrow) は無意味に．      (advice-add 'ivy-format-function-default :override #'ad:ivy-format-function-arrow)

    (if window-system
        (when (require 'all-the-icons nil t)
          (defun my-ivy-format-function-arrow (cands)
            "Transform CANDS into a string for minibuffer."
            (ivy--format-function-generic
             ;; (lambda (str)
             ;;   (concat (format "%s" (ivy--add-face "\t" 'my-ivy-arrow-visible))
             ;;           (ivy--add-face str 'ivy-current-match)))
             ;; (lambda (str) (concat (format "%-4s" " ") str))
             ;;---
             ;; (lambda (str)
             ;;   (concat (ivy--add-face " " 'my-ivy-arrow-visible)
             ;;           (ivy--add-face str 'ivy-current-match)))
             ;; (lambda (str)
             ;;   (concat (ivy--add-face " " 'my-ivy-arrow-invisible)
             ;;           str))
             ;;---
             (lambda (str)
               (concat (all-the-icons-faicon
                        "hand-o-right"
                        :v-adjust -0.2 :face 'my-ivy-arrow-visible)
                       " " (ivy--add-face str 'ivy-current-match)))
             (lambda (str)
               (concat (all-the-icons-faicon
                        "hand-o-right" :face 'my-ivy-arrow-invisible) " " str))
             cands
             "\n"))
          (setq ivy-format-functions-alist
                '((t . my-ivy-format-function-arrow))))
      (setq ivy-format-functions-alist '((t . ivy-format-function-arrow))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-M-x と同じ振る舞いにする設定（4点）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; コマンド使用履歴を使って counsel-M-x の候補を表示する（4点目）
    ;; 激重の様子？ helm-M-x で培った履歴はそのままでは使えないっぽい．
    ;; 1. extended-command-history
    (when (require 'smex nil t)
      (setq smex-history-length 35)
      (setq smex-completion-method 'ivy))

    ;; 2. exclude `counsel-M-x' to not to show "^" after the ivy prompt
    (setq ivy-initial-inputs-alist
          '(;; (org-refile . "^")
            (org-agenda-refile . "^")
            (org-capture-refile . "^")
            (counsel-describe-function . "^")
            (counsel-describe-variable . "^")
            ;; (counsel-org-capture . "^")
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
    (when (setq enable-recursive-minibuffers t)
      (minibuffer-depth-indicate-mode 1)) ;; 何回層入ったかプロンプトに表示．
    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)

    ;; https://github.com/abo-abo/swiper/blob/596461e1ff09431eb417877a9870e53c452e1b62/doc/Changelog.org
    ;; https://github.com/abo-abo/swiper/issues/924
    ;; https://github.com/abo-abo/swiper/issues/309
    (ivy-mode 1)
    (counsel-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy-rich（ivy導入後のお楽しみ）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (when (require 'ivy-rich nil t) ;; Heavy?
;;   (ivy-rich-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (with-eval-after-load "company"
;;   (when (and (require 'all-the-icons nil t)
;;              (require 'company-box nil t))
;;     (add-hook 'company-mode-hook 'company-box-mode)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy-posframe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (autoload-if-found
       '(ivy-posframe-mode my-toggle-ivy-posframe)
       "ivy-posframe" nil t)

  (defun my-toggle-ivy-posframe ()
    "Toggle `ivy-posframe'."
    (interactive)
    (ivy-posframe-mode (if ivy-posframe-mode -1 1)))

  (with-eval-after-load "ivy-posframe"
    (setq ivy-posframe-border-width 10)
    (custom-set-faces
     '(ivy-posframe
       ((((class color) (background light))
         :foreground "#000000" :background "#FFF3F3" :distant-foreground "black")
        (((class color) (background dark))
         :foreground "#000000" :background "#4F5353" :distant-foreground "#b03333")))
     '(ivy-posframe-border
       ((((class color) (background light))
         :foreground "#000000" :background "#FFF3F3" :distant-foreground "black")
        (((class color) (background dark))
         :foreground "#000000" :background "#4F5353" :distant-foreground "#b03333"))))
    (setq ivy-posframe-display-functions-alist
          '((counsel-M-x . ivy-posframe-display-at-point)
            (t           . ivy-posframe-display)))
    (ivy-posframe-mode 1)))

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
;; To decrypt old sub trees
;; (advice-add 'epg--check-error-for-decrypt :override 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs ends here
