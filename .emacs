;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
;; (load "~/Dropbox/emacs.d/config/el-get-setup.el" nil t)
;; (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el"))
;; ln -s ~/Dropbox/emacs.d/config/.spacemacs ~/
;; git clone https://github.com/syl20bnr/spacemacs ~/.spacemacs.d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dired に反映されるのは確認できたが，completion では確認できず．
;; Possible completions
(setq completion-ignored-extensions
      (append completion-ignored-extensions
              '("./" "../" ".xlsx" ".docx" ".pptx" ".DS_Store")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "postpone"
  (when (and (require 'ivy nil t)
             (require 'counsel nil t))

    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-M-r") 'counsel-recentf)
    (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
    ;;    (global-set-key (kbd "C-M-l") 'counsel-locate) ;; or counsel-fzf?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; for bm.el
    ;; https://www.reddit.com/r/emacs/comments/700xck/ivy_with_bmel_bookmark_manager/
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

    ;; disable counsel-find-file
    ;; https://emacs.stackexchange.com/questions/45929/disable-ivy-for-find-file
    ;; completion-in-region-function も一時的にデフォに戻さないと，TAB補完時に
    ;; ivy が有効化されてしまう．
    (defun my-disable-counsel-find-file (&rest args)
      "Disable `counsel-find-file' and use the original `find-file' with ARGS."
      (let ((completing-read-function #'completing-read-default)
            (completion-in-region-function #'completion--in-region))
        (apply #'read-file-name-default args)))
    (setq read-file-name-function #'my-disable-counsel-find-file)

    ;; 絞り込み過程の単語のハイライト配色
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

    ;; M-x `find-library' が大量の =./= を表示する問題を回避
    ;; `find-library' ではなく `counsel-find-library' を直接呼べばOK．
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

    ;; 選択対象を "" にする（かなり重くなる？） ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq ivy-format-function #'ivy-format-function-arrow)
    (defun ad:ivy-format-function-arrow (cands)
      "Transform CANDS into a string for minibuffer."
      (ivy--format-function-generic
       (lambda (str)
         (concat (ivy--add-face " " 'my-ivy-arrow-visible)
                 (ivy--add-face str 'ivy-current-match)))
       (lambda (str)
         (concat (ivy--add-face " " 'my-ivy-arrow-invisible) str))
       ;; (lambda (str)
       ;;   (concat "  " str))
       cands
       "\n"))
    ;; （かなり重くなる）
    (advice-add 'ivy-format-function-arrow
                :override #'ad:ivy-format-function-arrow)

    ;; helm-M-x と同じ振る舞いにする設定（3点）;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; コマンド使用履歴を使って counsel-M-x の候補を表示する（4点目）
    ;; 激重の様子．
    ;; extended-command-history
    (when (require 'smex nil t)
      (setq smex-history-length 35)
      (setq smex-completion-method 'ivy))

    ;; exclude `counsel-M-x'
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
    ;; ignore-order
    (add-to-list 'ivy-re-builders-alist '(t . ivy--regex-ignore-order))
    ;; sort
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

    ;; https://github.com/d12frosted/flyspell-correct/issues/10
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
      (global-set-key (kbd "<f7>") 'flyspell-correct-word-generic)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode 1))

;; (with-eval-after-load "company"
;;   (when (and (require 'all-the-icons nil t)
;;              (require 'company-box nil t))
;;     (add-hook 'company-mode-hook 'company-box-mode)))

;; (when (require 'ivy-posframe nil t)
;;   (setq ivy-display-function #'ivy-posframe-display)
;;   ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
;;   ;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
;;   ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
;;   ;; (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
;;   ;; (setq ivy-display-function #'ivy-posframe-display-at-point)
;;   (ivy-posframe-enable))

(with-eval-after-load "postpone" (require 'frog-jump-buffer nil t))

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

(with-eval-after-load "org-capture"
  ;; ブックマーク入りのノートをキャプチャするテンプレートを追加
  (setq org-capture-templates
        `(("u" "subtree with a bookmark" entry
           (file+headline "~/Desktop/hoge.org" "INBOX")
           "** TODO %(message \"%s\" my-captured-bookmark-last)\n%?\n%(my-get-org-bookmark)")
          ("U" "subtree with a bookmark" entry
           (file+headline "~/Desktop/hoge.org" "INBOX")
           "** TODO %(message \"%s\" my-captured-bookmark-last)\n%?\n%(my-get-org-bookmark)" :prepend t)
          ("p" "Memos")
          ("p1" "T1 MEMO" entry (file+olp+datetree "~/Desktop/hoge.org" "Memo") "***** %U\n(p1)%?" :prepend t)
          ("p2" "T2 MEMO" entry (file+olp+datetree "~/Desktop/hoge.org" "Memo") "***** %U\n(p2)%?" :prepend t)
          ("M" "MEMO" entry (file+olp+datetree "~/Desktop/hoge.org" "Memo") "***** %U\n(M)%?" :prepend nil)))

  ;; キャプチャ直前に記録するブックマークの名前を記録
  (defvar my-captured-bookmark-last nil)
  (defun my-org-capture-bookmark-set ()
    "Bookmark the location when capture is activated."
    (let ((name (format "%s::%s"
                        (bookmark-buffer-file-name)
                        (buffer-substring-no-properties
                         (point-at-bol) (point-at-eol)))))
      (bookmark-set (setq my-captured-bookmark-last name))))
  (defun my-get-org-bookmark ()
    "Format a link compliant with `ol-bookmark'."
    (if my-captured-bookmark-last
        (format "  [[bookmark:%s][jump to source]]" my-captured-bookmark-last)
      (warn "Nothing is stored in `my-captured-bookmark-last'")))
  (defun ad:org-capture (&optional goto keys)
    (my-org-capture-bookmark-set))
  (advice-add 'org-capture :before #'ad:org-capture)
  (defun ad:org-capture-kill ()
    (when my-captured-bookmark-last
      (bookmark-delete my-captured-bookmark-last)))
  (advice-add 'org-capture-kill :before #'ad:org-capture-kill))

(with-eval-after-load "org-clock"
  (setq org-clocktable-defaults
        (list
         :maxlevel 2
         :lang (or (bound-and-true-p org-export-default-language) "en")
         :scope 'file
         :block nil
         :wstart 1
         :mstart 1
         :tstart nil
         :tend nil
         :step nil
         :stepskip0 nil
         :fileskip0 nil
         :tags nil
         :match nil
         :emphasize nil
         :link nil
         :narrow '40!
         :indent t
         :formula nil
         :timestamp nil
         :level nil
         :tcolumns nil
         :formatter nil
         :reverse nil))

  (defun ad:org-clocktable-steps (params)
    "Create one or more clock tables, according to PARAMS.
Step through the range specifications in plist PARAMS to make
a number of clock tables."
    (let* ((ignore-empty-tables (plist-get params :stepskip0))
           (step (plist-get params :step))
           (step-header
            (pcase step
              (`day "Daily report: ")
              (`week "Weekly report starting on: ")
              (`month "Monthly report starting on: ")
              (`year "Annual report starting on: ")
              (_ (user-error "Unknown `:step' specification: %S" step))))
           (week-start (or (plist-get params :wstart) 1))
           (month-start (or (plist-get params :mstart) 1))
           (range
            (pcase (plist-get params :block)
              (`nil nil)
              (range
               (org-clock-special-range range nil t week-start month-start))))
           ;; For both START and END, any number is an absolute day
           ;; number from Agenda.  Otherwise, consider value to be an Org
           ;; timestamp string.  The `:block' property has precedence
           ;; over `:tstart' and `:tend'.
           (start
            (pcase (if range (car range) (plist-get params :tstart))
              ((and (pred numberp) n)
               (pcase-let ((`(,m ,d ,y) (calendar-gregorian-from-absolute n)))
                 (apply #'encode-time (list 0 0 org-extend-today-until d m y))))
              (timestamp
               (seconds-to-time
                (org-matcher-time (or timestamp
                                      ;; The year Org was born.
                                      "<2003-01-01 Thu 00:00>"))))))
           (end
            (pcase (if range (nth 1 range) (plist-get params :tend))
              ((and (pred numberp) n)
               (pcase-let ((`(,m ,d ,y) (calendar-gregorian-from-absolute n)))
                 (apply #'encode-time (list 0 0 org-extend-today-until d m y))))
              (timestamp (seconds-to-time (org-matcher-time timestamp)))))
           (reverse-point (when (plist-get params :reverse) (point))))
      (while (time-less-p start end)
        (unless (bolp) (insert "\n"))
        (when reverse-point
          (goto-char (1- reverse-point))
          (insert "\n"))
        ;; Insert header before each clock table.
        (insert "\n"
                step-header
                (format-time-string (org-time-stamp-format nil t) start)
                "\n")
        ;; Compute NEXT, which is the end of the current clock table,
        ;; according to step.
        (let* ((next
                (apply #'encode-time
                       (pcase-let
                           ((`(,_ ,_ ,_ ,d ,m ,y ,dow . ,_) (decode-time start)))
                         (pcase step
                           (`day (list 0 0 org-extend-today-until (1+ d) m y))
                           (`week
                            (let ((offset (if (= dow week-start) 7
                                            (mod (- week-start dow) 7))))
                              (list 0 0 org-extend-today-until (+ d offset) m y)))
                           (`month (list 0 0 0 month-start (1+ m) y))
                           (`year (list 0 0 org-extend-today-until 1 1 (1+ y)))))))
               (table-begin (or reverse-point (line-beginning-position 0)))
               (table-end 0)
               (step-time
                ;; Write clock table between START and NEXT.
                (org-dblock-write:clocktable
                 (org-combine-plists
                  params (list :header ""
                               :step nil
                               :block nil
                               :tstart (format-time-string
                                        (org-time-stamp-format t t)
                                        start)
                               :tend (format-time-string
                                      (org-time-stamp-format t t)
                                      ;; Never include clocks past END.
                                      (if (time-less-p end next) end next)))))))
          (when reverse-point
            (unless (ignore-errors (let ((case-fold-search t)) (re-search-forward step-header)))
              (let ((case-fold-search t)) (re-search-forward "^[ \t]*#\\+END:")))
            (end-of-line 0)
            (setq table-end (point)))
          (let ((case-fold-search t)) (re-search-forward "^[ \t]*#\\+END:"))
          ;; Remove the table if it is empty and `:stepskip0' is
          ;; non-nil.
          (when (and ignore-empty-tables (equal step-time 0))
            (delete-region (if reverse-point table-end (line-beginning-position)) table-begin))
          (setq start next))
        (end-of-line 0))))
  (advice-add 'org-clocktable-steps :override #'ad:org-clocktable-steps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fontawesome 拡張
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
  (ad-activate 'font-lock-mode))

;; (when (require 'hi-lock nil t)
;;   (highlight-phrase "")
;;   (highlight-phrase "")
;;   (highlight-phrase "")
;;   (global-hi-lock-mode))

(progn ;; facecheck-mode
  (defvar facecheck-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-1] 'facecheck-at-point)
      map)
    "")

  ;;;###autoload
  (defun facecheck-at-point ()
    (interactive)
    (describe-face (face-at-point)))

  (defvar facecheck--global-hl-line-mode nil)
  (defvar facecheck--mouse-highlight nil)
  (defvar facecheck--mouse-1-click-follows-link nil)

  (defun facecheck--setup ()
    "Init function."
    (setq facecheck--mouse-highlight mouse-highlight
          facecheck--mouse-1-click-follows-link mouse-1-click-follows-link)
    (setq mouse-highlight nil
          mouse-1-click-follows-link nil)
    (when (setq facecheck--global-hl-line-mode global-hl-line-mode)
      (global-hl-line-mode -1)))

  (defun facecheck--abort ()
    "Abort."
    (setq mouse-highlight facecheck--mouse-highlight
          mouse-1-click-follows-link facecheck--mouse-1-click-follows-link)
    (when facecheck--global-hl-line-mode
      (global-hl-line-mode 1)))

  (define-minor-mode facecheck-mode
    ""
    :init-value nil
    :keymap facecheck-mode-map
    :global t
    :require 'facecheck
    :group 'facecheck
    (when window-system
      (if facecheck-mode
          (facecheck--setup)
        (facecheck--abort)))))

;; To decrypt old sub trees
;; (advice-add 'epg--check-error-for-decrypt :override 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs ends here
