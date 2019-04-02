;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
;; (load "~/Dropbox/emacs.d/config/el-get-setup.el" nil t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/alphapapa/outshine
;; file:~/Dropbox/org/next.org::*Usage

(with-eval-after-load "org"
  (defun my-insert-org-link-last-captured ()
    (interactive)
    (let ((cb (current-buffer)))
      (org-capture-goto-last-stored)
      (org-store-link nil)
      (sleep-for 1)
      (switch-to-buffer cb))
    (org-insert-link)))
;; (require 'helm-bookmark nil t)
;; (when (require 'org-bookmark-heading nil t)
;;   )

(with-eval-after-load "org-capture"
  ;; ブックマーク入りのノートをキャプチャするテンプレートを追加
  (setq org-capture-templates
        `(("u" "subtree with a bookmark" entry
           (file+headline "~/Desktop/hoge1.org" "INBOX")
           "** TODO %(message \"%s\" my-captured-bookmark-last)\n%?\n%(my-get-org-bookmark)")))

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

;; Testing
(with-eval-after-load "postpone"
  (when (require 'company nil t)
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

    (global-company-mode))

  (when (require 'company-quickhelp nil t)
    (company-quickhelp-mode)))


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

;; org-trello
(with-eval-after-load "postpone"
  (defvar org-trello-current-prefix-keybinding nil) ;; To avoid an error
  ;; 1. TODO/DOING/DONE に trello 側のカードを変えておく．
  ;; 2. M-x org-trello-install-key-and-token
  ;; ~/.emacs.d/.trello/<account>.el が作られる
  ;; 3. M-x org-trello-install-board-metadata
  ;; Trello 側の情報を基にして current-buffer にプロパティブロックが挿入される
  ;; 4. C-u M-x org-trello-sync-buffer で pull
  ;; 5. M-x org-trello-sync-buffer で push
  (add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))
  (defun my-activate-org-trello ()
    (let ((filename (buffer-file-name (current-buffer))))
      (when (and filename
                 (string= "trello" (file-name-extension filename))
                 (require 'org-trello nil t))
        (org-trello-mode))))
  (add-hook 'org-mode-hook #'my-activate-org-trello))

;; FIXME 同期するとカード記述の情報が繰り返しインデントされてしまう．
(with-eval-after-load "org-trello"
  (defun my-push-trello-card () (interactive) (org-trello-sync-card))
  (defun my-pull-trello-card () (interactive) (org-trello-sync-card t))
  (defun my-push-trello () (interactive) (org-trello-sync-buffer))
  (defun my-pull-trello () (interactive) (org-trello-sync-buffer t)))

;; To decrypt old sub trees
;; (advice-add 'epg--check-error-for-decrypt :override 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs ends here
