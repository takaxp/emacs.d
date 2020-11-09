(when (autoload-if-found
       '(counsel-M-x counsel-recentf counsel-ag counsel-ibuffer)
       "counsel" nil t)

  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-M-z") 'counsel-fzf)
  (global-set-key (kbd "C-M-r") 'counsel-recentf)
  (global-set-key (kbd "C-M-f") 'counsel-ag)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)

  (with-eval-after-load "ivy"
    (when (require 'ivy-hydra nil t)
      ;; M-o を ivy-dispatching-done-hydra に割り当てる．
      ;; ivy-dispatching-done を使う．
      ;; (define-key ivy-minibuffer-map (kbd "M-o") 'ivy-dispatching-done))
      (defun ad:ivy-dispatching-done-hydra (f)
        (when (> ivy--length 0)
          (funcall f)))
      (advice-add 'ivy-dispatching-done-hydra
                  :around #'ad:ivy-dispatching-done-hydra))

    (setq ivy-use-virtual-buffers t)
    (when (setq enable-recursive-minibuffers t)
      (minibuffer-depth-indicate-mode 1))
    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)

    ;; counsel-M-x, see also prescient.el section
    (setq ivy-initial-inputs-alist
          '((org-agenda-refile . "^")
            (org-capture-refile . "^")
            (counsel-describe-function . "^")
            (counsel-describe-variable . "^")
            (Man-completion-table . "^")
            (woman . "^")))

    ;; faces
    (custom-set-faces
     '(ivy-current-match
       ((((class color) (background light))
         :background "#FFF3F3" :distant-foreground "#000000")
        (((class color) (background dark))
         :background "#404040" :distant-foreground "#abb2bf")))
     '(ivy-minibuffer-match-face-1
       ((((class color) (background light)) :foreground "#666666")
        (((class color) (background dark)) :foreground "#999999")))
     '(ivy-minibuffer-match-face-2
       ((((class color) (background light)) :foreground "#c03333" :underline t)
        (((class color) (background dark)) :foreground "#e04444" :underline t)))
     '(ivy-minibuffer-match-face-3
       ((((class color) (background light)) :foreground "#8585ff" :underline t)
        (((class color) (background dark)) :foreground "#7777ff" :underline t)))
     '(ivy-minibuffer-match-face-4
       ((((class color) (background light)) :foreground "#439943" :underline t)
        (((class color) (background dark)) :foreground "#33bb33" :underline t))))

;;; 選択対象を "" にする (requires all-the-icons.el)
    ;; https://github.com/abo-abo/swiper/commit/7feb14f476e108dc10fe5380ee7c33de3b5fe3f1 の変更で，(setq ivy-format-function #'ivy-format-function-arrow) は無意味に．(advice-add 'ivy-format-function-default :override #'ad:ivy-format-function-arrow)
    (defface my-ivy-arrow-visible
      '((((class color) (background light)) :foreground "orange")
        (((class color) (background dark)) :foreground "#EE6363"))
      "Face used by Ivy for highlighting the arrow.")
    (defface my-ivy-arrow-invisible
      '((((class color) (background light)) :foreground "#FFFFFF")
        (((class color) (background dark)) :foreground "#31343F"))
      "Face used by Ivy for highlighting the invisible arrow.")
    (if window-system
        (when (require 'all-the-icons nil t)
          (defun my-ivy-format-function-arrow (cands)
            "Transform CANDS into a string for minibuffer."
            (ivy--format-function-generic
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

;;; ivy-prescient. コマンド履歴を保存．コマンドのイニシャル入力を可能にする．
    (with-eval-after-load "prescient"
      (setq prescient-aggressive-file-save t) ;; Merged!
      (setq prescient-save-file
            (expand-file-name "~/.emacs.d/prescient-save.el"))
      (prescient-persist-mode 1))

    (when (require 'company-prescient nil t)
      (company-prescient-mode 1))

    (when (require 'ivy-prescient nil t)
      (setq ivy-prescient-retain-classic-highlighting t)
      (dolist (command '(counsel-world-clock ;; Merged!
                         counsel-app))
        (add-to-list 'ivy-prescient-sort-commands command))
      (ivy-prescient-mode 1)
      (setf (alist-get 'counsel-M-x ivy-re-builders-alist)
            #'ivy-prescient-re-builder)
      (setf (alist-get t ivy-re-builders-alist) #'ivy--regex-ignore-order))

    (ivy-mode 1))


  (with-eval-after-load "counsel"

    (when (require 'smex nil t)
      (setq smex-history-length 35)
      (setq smex-completion-method 'ivy))

    ;;  https://github.com/abo-abo/swiper/issues/1294
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
    (setf (alist-get 'counsel-M-x ivy-sort-matches-functions-alist)
          #'ivy--sort-by-len)
    ;; (setf (alist-get 'counsel-M-x ivy-re-builders-alist)
    ;;       #'ivy--regex-ignore-order)
    ;; (add-to-list 'ivy-sort-matches-functions-alist
    ;;              '(counsel-M-x . ivy--sort-by-len) t)
    ;; (add-to-list 'ivy-re-builders-alist
    ;;              '(counsel-M-x . ivy--regex-ignore-order))

    ;; Disable counsel-find-file
    ;; https://emacs.stackexchange.com/questions/45929/disable-ivy-for-find-file
    (defun my-disable-counsel-find-file (&rest args)
      "Disable `counsel-find-file' and use the original `find-file' with ARGS."
      (let ((completing-read-function #'completing-read-default)
            (completion-in-region-function #'completion--in-region))
        (apply #'read-file-name-default args)))
    (setq read-file-name-function #'my-disable-counsel-find-file)
    (define-key counsel-mode-map [remap find-file]  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  (define-key counsel-mode-map [remap org-capture]  'counsel-org-capture)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flycheck
;;; see https://github.com/nathankot/dotemacs/blob/master/init.el#L709
;;; prescient.el用に caller を追加
    (defvar counsel-flycheck-history nil
      "History for `counsel-flycheck'")

    (defun counsel-flycheck ()
      (interactive)
      (if (not (bound-and-true-p flycheck-mode))
          (message "Flycheck mode is not available or enabled")
        (ivy-read "Error: "
                  (let ((source-buffer (current-buffer)))
                    (with-current-buffer
                        (or (get-buffer flycheck-error-list-buffer)
                            (progn
                              (with-current-buffer
                                  (get-buffer-create flycheck-error-list-buffer)
                                (flycheck-error-list-mode)
                                (current-buffer))))
                      (flycheck-error-list-set-source source-buffer)
                      (flycheck-error-list-reset-filter)
                      (revert-buffer t t t)
                      (split-string (buffer-string) "\n" t " *")))
                  :action (lambda (s &rest _)
                            (-when-let* ( (error (get-text-property 0 'tabulated-list-id s))
                                          (pos (flycheck-error-pos error)) )
                              (goto-char (flycheck-error-pos error))))
                  :history 'counsel-flycheck-history
                  :caller 'counsel-flycheck)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; M-x `find-library' が大量の =./= を表示する問題を回避
    ;; `find-library' ではなく `counsel-find-library' を直接呼べばOK．
    (when (require 'find-func nil t)
      ;; オリジナルを非インタラクティブ化
      (defun find-library (library)
        "Override the original `find-library' to hide in command list."
        (prog1
            (switch-to-buffer (find-file-noselect (find-library-name library)))
          (run-hooks 'find-function-after-hook))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ivy-rich
    ;; (when (require 'ivy-rich nil t) ;; Heavy? 80桁縛りの人には向かない．
    ;;   (ivy-rich-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; プロンプトをカスタマイズ（モードライン非表示派向け）
    (defvar my-ivy-prompt-prefix t) ;; ime-on/-off に合わせて変えてもOK．
    (defun my-toggle-ivy-prompt-prefix ()
      "Toggle showing a header line before the ivy prompt."
      (interactive)
      (setq my-ivy-prompt-prefix (not my-ivy-prompt-prefix)))

    (defun my-ivy-prompt-prefix ()
      "Return a header line for the ivy prompt."
      (when my-ivy-prompt-prefix
        (if window-system
            (format "%s\n%s "
                    (make-string (frame-width) ?\x5F) ;; "__"
                    (all-the-icons-faicon "sort-amount-asc")) ;; ""
          (format "%s\n" (make-string (1- (frame-width)) ?\x2D))))) ;; "--"

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
              (when my-ivy-prompt-prefix
                (setq n-str (concat (my-ivy-prompt-prefix) n-str)))
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
              (if (= ivy--index -1)
                  (ivy-add-face-text-property
                   (minibuffer-prompt-end) (line-end-position) 'ivy-prompt-match)
                (remove-list-of-text-properties
                 (minibuffer-prompt-end) (line-end-position) '(face))))
            ;; get out of the prompt area
            (constrain-to-field nil (point-max))))))
    (advice-add 'ivy--insert-prompt :override #'ad:ivy--insert-prompt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Common actions for counsel-ag, counsel-fzf, and counsel-recentf
    (defun my-counsel-fzf-in-default-dir (_arg)
      "Search the current directory with fzf."
      (counsel-fzf ivy-text default-directory))
    (defun my-counsel-fzf-in-dir (_arg)
      "Search again with new root directory."
      (counsel-fzf ivy-text
                   (read-directory-name
                    (concat (car (split-string counsel-fzf-cmd))
                            " in directory: "))))
    (defun my-counsel-ag-in-dir (_arg)
      "Search again with new root directory."
      (let ((current-prefix-arg '(4)))
        (counsel-ag ivy-text nil ""))) ;; also disable extra-ag-args

    (ivy-add-actions
     'counsel-ag
     '(("r" my-counsel-ag-in-dir "search in directory")))

    (ivy-add-actions
     'counsel-fzf
     '(("r" my-counsel-fzf-in-dir "search in directory")))

    (ivy-add-actions
     'counsel-recentf
     '(("g" my-counsel-ag-in-dir "switch to ag")
       ("r" my-counsel-fzf-in-dir "switch to fzf (in dir.)")
       ("z" my-counsel-fzf-in-default-dir "switch to fzf (default)")))

;;; counsel-ag
    ;; カーソル位置の単語とバッファのあるディレクトリをデフォルトにする(ag, fzf)
    (defun ad:counsel-ag (f &optional initial-input initial-directory extra-ag-args ag-prompt caller)
      (apply f (or initial-input (ivy-thing-at-point))
             (unless current-prefix-arg
               (or initial-directory default-directory))
             extra-ag-args ag-prompt caller))
    (advice-add 'counsel-ag :around #'ad:counsel-ag)

    (defun ad:counsel-fzf (f &optional initial-input initial-directory fzf-prompt)
      (apply f (or initial-input
                   (ivy-thing-at-point))
             (or initial-directory default-directory)
             fzf-prompt))
    (advice-add 'counsel-fzf :around #'ad:counsel-fzf)

;;; counsel-recentf のリストを "~/" から始める．
    (defun ad:counsel-recentf ()
      "Find a file on `recentf-list'."
      (interactive)
      (require 'recentf)
      (recentf-mode)
      (ivy-read "Recentf: "
                (progn
                  (mapcar #'substring-no-properties recentf-list) ;; no need?
                  (mapcar #'abbreviate-file-name recentf-list)) ;; ~/
                :action (lambda (f)
                          (with-ivy-window
                            (find-file f)))
                :require-match t
                :caller 'counsel-recentf))
    (advice-add 'counsel-recentf :override #'ad:counsel-recentf)

;;; counsel-projectile
    (when (require 'counsel-projectile nil t)
      (add-to-list 'counsel-projectile-switch-project-action
                   '("z" my-counsel-fzf-in-default-dir
                     "switch to fzf") t)
      (add-to-list 'counsel-projectile-find-file-action
                   '("z" my-counsel-fzf-in-default-dir
                     "switch to fzf") t)

      (setq projectile-completion-system 'ivy)
      (setq counsel-projectile-sort-files t) ;; 当該プロジェクト内リストをソート
      (setq counsel-projectile-sort-projects t) ;; プロジェクトリストをソート
      (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
      (counsel-projectile-mode 1))

;;; auto fzf, 0件ヒットの時，1回だけ[y/n]で counsel-fzf に繋ぐか問う
    (defcustom my-nocand-then-fzf-commands '(counsel-recentf
                                             counsel-projectile-find-file
                                             counsel-projectile-switch-project)
      "List of commands for applying extension no candidates then `counsel-fzf'."
      :group 'ivy
      :type '(list symbol))

    (defcustom my-nocand-then-fzf-idle-time 0.8
      "Idle time for showing prompt."
      :group 'ivy
      :type 'float) ;; N[s] 無応答の時[y/n]を出す．

    (defvar my--nocand-then-fzf t)
    (defun my-nocand-then-fzf-reset ()
      (setq my--nocand-then-fzf t))
    (defun my-nocand-then-fzf (prompt)
      (when (= ivy--length 0)
        (if (eq (read-char prompt) ?y) ;; y-or-n-p is not applicable
            (ivy-exit-with-action
             (lambda (x)
               (counsel-fzf ivy-text default-directory)))
          (setq my--nocand-then-fzf nil))))
    (defun ad:fzf:ivy--insert-prompt ()
      (when (and my--nocand-then-fzf
                 (memq (ivy-state-caller ivy-last) my-nocand-then-fzf-commands)
                 (= ivy--length 0))
        (let* ((std-props
                '(front-sticky t rear-nonsticky t field t read-only t))
               (prompt (concat (my-ivy-prompt-prefix)
                               "Switch to Counsel-fzf? [y/n] ")))
          (set-text-properties 0 (length prompt)
                               `(face minibuffer-prompt ,@std-props) prompt)
          (run-with-idle-timer my-nocand-then-fzf-idle-time
                               nil #'my-nocand-then-fzf prompt))))
    (advice-add 'ivy--insert-prompt :before #'ad:fzf:ivy--insert-prompt)
    (add-hook 'minibuffer-setup-hook #'my-nocand-then-fzf-reset)
    (add-hook 'minibuffer-exit-hook #'my-nocand-then-fzf-reset)

;;; org-recent-headings
    (global-set-key (kbd "C-c f r") 'org-recent-headings-ivy)

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

    (when (and nil ;; org HEAD ではうまく動かず．要修正．
               (require 'org-recent-headings nil t))

      ;; デフォルトだと `ivy-string<' が使われてしまい，使用履歴が反映されない．
      ;; つまり， recent-headings.dat に記録された順が反映されない．
      (setf (alist-get 'org-recent-headings-ivy ivy-sort-functions-alist)
            nil)
      ;; (add-to-list 'ivy-sort-functions-alist '(org-recent-headings-ivy . nil))

      ;; Originally located in org-recent-headings.el.
      (defun org-recent-headings-ivy ()
        "Choose from recent Org headings with Ivy."
        (interactive)
        (org-recent-headings :completing-read-fn #'ivy-completing-read))

      (defun ad:org-recent-headings-activate ()
        (interactive)
        (when (and (require 'dash-functional nil t) ;; FIXME
                   (require 'org-recent-headings nil t))
          (org-recent-headings-mode)
          (advice-remove 'org-recent-headings-ivy
                         #'ad:org-recent-headings-activate)))
      (advice-add 'org-recent-headings-ivy :before
                  #'ad:org-recent-headings-activate)

      ;; for Ivy interface
      (custom-set-variables
       '(org-recent-headings-save-file "~/.emacs.d/org-recent-headings.dat")
       '(org-recent-headings-show-entry-function
         'org-recent-headings--show-entry-direct)
       '(org-recent-headings-advise-functions
         '(org-agenda-goto
           org-agenda-show
           org-agenda-show-mouse
           org-show-entry ;; having a bug, so just be disabled
           org-reveal
           org-refile
           org-tree-to-indirect-buffer
           org-bookmark-jump))))

;;; ivy-posframe
    (when (and nil
               (require 'ivy-posframe nil t))
      (defun my-toggle-ivy-posframe ()
        "Toggle `ivy-posframe'."
        (interactive)
        (ivy-posframe-mode (if ivy-posframe-mode -1 1)))
      (setq ivy-posframe-display-functions-alist
            '((counsel-M-x . ivy-posframe-display-at-point)
              (t           . ivy-posframe-display)))
      (ivy-posframe-mode 1))

    ;; activate!
    (counsel-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; end of init-ivy.el
