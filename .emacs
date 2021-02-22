;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
;; (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME

(with-eval-after-load "smartparens"
  ;; To avoid an error
  (defun sp-split-sexp (arg)
    ""
    (interactive "P")
    (cond
     ((equal arg '(4))
      (-when-let ((first-item . rest-items) (sp-get-list-items))
        (sp-get first-item
                (save-excursion
                  (goto-char :end)            (delete-char (- (length :cl)))
                  (--each (nreverse rest-items)
                    (goto-char (sp-get it :end))              (insert :cl)
                    (goto-char (sp-get it :beg))              (insert :op))
                  (goto-char :beg)            (delete-char (length :op))))))
     (t
      (let ((should-split-as-string
             (and sp-split-sexp-always-split-as-string
                  (sp-point-in-string))))
        (-when-let (ok (if should-split-as-string
                           (save-excursion
                             (goto-char (car (sp-get-quoted-string-bounds)))
                             (sp-get-sexp))
                         (sp-get-enclosing-sexp 1)))
          (sp-get ok
                  (sp--run-hook-with-args :op :pre-handlers 'split-sexp)
                  (if should-split-as-string
                      (progn
                        (insert :cl)
                        (save-excursion (insert :op)))
                    (forward-char
                     (- (prog1 (sp-backward-whitespace t) (insert :cl))))
                    (save-excursion (sp-forward-whitespace) (insert :op)))
                  (sp--run-hook-with-args :op :post-handlers 'split-sexp))))))))

(with-eval-after-load "org-tree-slide"
  ;; README (1)
  ;; org-tree-slide.el を読み込み，custom-set-faces すれば，dark/light にそれぞれ色がつく．
  ;; dark <-> light 間で配色を交換しても，期待通りに色が変わる．
  ;; (custom-set-faces
  ;;  '(org-tree-slide-header-overlay-face
  ;;    ((((background dark))
  ;;      :foreground "white" :background "#594d5d" :underline "red")
  ;;     (t
  ;;      :bold t :foreground "red"  :background "purple"))))

  ;; README (2)
  ;; これで期待通りの振る舞いになる
  ;; TODO 別途，title だけ変えられる？
  (defun org-tree-slide-reload-header-face ()
    (face-spec-set 'org-tree-slide-header-overlay-face
                   `((t (:bold t
                               :foreground ,(face-foreground 'default)
                               :background ,(face-background 'default))))))
  (when (require 'ah nil t)
    (add-hook 'ah-after-enable-theme-hook #'org-tree-slide-reload-header-face))
  ;; ということで，dark/light の変換コマンドと，hook がわかれば，
  ;; それにorg-tree-slide-reload-header-faceのような補助関数をぶら下げればOK

  ;; title 等の表示を変えたい場合（dark/light の設定が，テーマ変更時に反映される）
  ;; (custom-set-faces
  ;;  '(org-document-title ((t (:bold t :height 200))))
  ;;  '(org-document-info ((t (:bold t :underline "red")))))
  )

;; https://gitlab.com/matsievskiysv/math-preview
;; https://github.com/dandavison/xenops

(with-eval-after-load "postpone"
  ;; circe.el
  (setq circe-network-options
        '(("Freenode" :tls t :nick "takaxp" :channels ("#emacsconf"))))

  ;; transient-dwim
  (require 'transient-dwim nil t))

(when nil
  ;; https://github.com/chuntaro/emacs-keycaster/issues/4
  (load "~/.emacs.d/27.0.60/el-get/keypression/keypression.el")
  ;; (custom-set-variables
  ;;  '(keycaster-x-offset (+ (frame-pixel-width) 10)))
  (custom-set-variables
   '(keycaster-use-child-frame t)
   '(keycaster-x-offset (+ 576 10)))
  ;; (custom-set-variables
  ;;  '(keycaster-x-offset (+ 476 10)))
  (keypression-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; need to learn the overlay...
(when nil
  (with-eval-after-load "all-the-icons-dired"
    ;; アイコンの backgound が変わらないのは27だから．
    (custom-set-faces
     '(all-the-icons-dired-dir-face
       ((t (:foreground "black" :background "light pink"
                        :underline "OrangeRed2")))))

    (defun all-the-icons-dired--add-overlay (pos string)
      "Add overlay to display STRING at POS."
      (custom-set-faces
       '(all-the-icons-dired-dir-face
         ((t (:foreground "black" :background "light pink"
                          :underline "OrangeRed2")))))

      (let ((ov (make-overlay (1- pos) pos)))
        ;; (overlay-put ov 'priority nil)
        (overlay-put ov 'face all-the-icons-dired-dir-face)
        (overlay-put ov 'all-the-icons-dired-overlay t)
        (overlay-put ov 'after-string string)))

    (defun ad:all-the-icons-dired--refresh ()
      (all-the-icons-dired--remove-all-overlays)
      (save-excursion
        (goto-char (point-min))
        (setq tab-width 1)
        (while (not (eobp))
          (let ((file (dired-get-filename 'verbatim t)))
            (when file
              (let ((icon (if (file-directory-p file)
                              (all-the-icons-icon-for-dir file nil "")
                            (all-the-icons-icon-for-file file :v-adjust all-the-icons-dired-v-adjust)))
                    (matcher (all-the-icons-match-to-alist file all-the-icons-dir-icon-alist)))
                ;;
                ;; (if (file-directory-p file)
                ;;     (setq icon (cond
                ;;                 ((and (fboundp 'tramp-tramp-file-p)
                ;;                       (tramp-tramp-file-p default-directory))
                ;;                  (all-the-icons-octicon "file-directory" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                ;;                 ((file-symlink-p file)
                ;;                  (all-the-icons-octicon "file-symlink-directory" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                ;;                 ((all-the-icons-dir-is-submodule file)
                ;;                  (all-the-icons-octicon "file-submodule" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                ;;                 ((file-exists-p (format "%s/.git" file))
                ;;                  (all-the-icons-octicon "repo" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                ;;                 (t (apply (car matcher) (list (cadr matcher) :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust)))))
                ;;   ;; (setq icon (concat icon "\t"))
                ;;   )

                ;; ここで適用するオーバーレイの問題だ．例えば "\t" が真っ白（背景色）．
                ;; all-the-icons-dired--add-overlay にoverlay-put ov 'face hl-line-face を加えると，全体の "\t" に色が着く．
                ;; しかし，overlay-put で 'face を追加してもアイコンの 'face（背景）は変更されない．
                (if (member file '("." ".."))
                    (all-the-icons-dired--add-overlay (point) "\t")
                  (all-the-icons-dired--add-overlay (point) (concat icon "\t")))

                )))
          (dired-next-line 1))))
    ;;(advice-add 'all-the-icons-dired--refresh :override #'ad:all-the-icons-dired--refresh)
    ))

(when nil
  (with-eval-after-load "postpone"
    (setq mac-default-input-source "com.apple.inputmethod.Kotoeri.Japanese"))

  ;; Shall be updated for Kotoeri
  (defun ns-insert-marked-text (pos len)
    "Insert contents of `ns-working-text' as UTF-8 string and mark with
  `ns-working-overlay' and `ns-marked-overlay'.  Any previously existing
  working text is cleared first. The overlay is assigned the faces
  `ns-working-text-face' and `ns-marked-text-face'."
    (ns-delete-working-text)
    (let ((start (point)))
      (when (<= pos (length ns-working-text))
        ;; (put-text-property pos len 'face 'ns-working-text-face ns-working-text)
        ;; (insert ns-working-text)
        ;; (if (= len 0)
        ;;     (overlay-put (setq ns-working-overlay
        ;;                        (make-overlay start (point) (current-buffer) nil t))
        ;;                  'face 'ns-working-text-face)
        ;;   (overlay-put (setq ns-working-overlay
        ;;                      (make-overlay start (point) (current-buffer) nil t))
        ;;                'face 'ns-unmarked-text-face)
        ;;   (overlay-put (setq ns-marked-overlay
        ;;                      (make-overlay (+ start pos) (+ start pos len)
        ;;                                    (current-buffer) nil t))
        ;;                'face 'ns-marked-text-face))
        ;; (goto-char (+ start pos))

        (if (= len 0)
            (overlay-put (setq ns-working-overlay
                               (make-overlay start (point) (current-buffer) nil t))
                         'after-string
                         (propertize ns-working-text 'face 'ns-working-text-face))
          (overlay-put (setq ns-working-overlay
                             (make-overlay start (point) (current-buffer) nil t))
                       'after-string
                       (propertize (substring ns-working-text 0 len)
                                   'face 'ns-marked-text-face))
          (overlay-put (setq ns-marked-overlay
                             (make-overlay (point) (+ (point) pos len)
                                           (current-buffer) nil t))
                       'before-string
                       (propertize (substring ns-working-text len
                                              (length ns-working-text))
                                   'face 'ns-unmarked-text-face))
          (goto-char (+ start pos)))
        ))))

(when nil
  (unless (version< emacs-version "27.0")
    (when (autoload-if-found
           '(keycaster-mode)
           "keycaster" nil t)

      (add-hook 'keycaster-mode-hook #'dimmer-permanent-off)
      (with-eval-after-load "keycaster"

        ;; 日本語入力のガード
        ;; 表示位置の調整
        (defun keycaster--set-frame-string (i string)
          (aset keycaster--strings i string)
          (with-current-buffer (aref keycaster--buffers i)
            (erase-buffer)
            (insert string))
          (let ((window-resize-pixelwise t)
                (frame-resize-pixelwise t)
                (window-min-width 0)
                (window-min-height 0)
                (frame (aref keycaster--frames i)))
            (if keycaster-use-child-frame
                (fit-frame-to-buffer frame nil 0 nil 0)
              (set-frame-width frame (string-width string)))))

        (setq keycaster-frames-maxnum 8
              ;; keycaster-frame-justify 'keycaster-left-justified
              keycaster-use-child-frame nil
              keycaster-x-offset 1
              keycaster-font "Monaco"
              keycaster-fade-out-delay 2.0
              keycaster-y-offset 20))
      )))

;; (with-eval-after-load "postpone"
;;   (when (eq system-type 'darwin)
;;     (add-hook 'focus-in-hook 'mac-ime-update-title)))

;; (with-eval-after-load "postpone"
;;   (setq ns-alerter-command nil)) ;; due to broken of alerter command

;; Shiftを使って大文字を入力する時，IME的にはASCIIにHLINEがかわるから
;; 表示が乱れる．Shiftモードに入る時のフックと抜ける時のフックが必要
;; hook があれば，そこに custom-set-faces を当てられる．
;; それか，let で回避するか．
;; それか，ShiftでIME入力を継続させる時は，IMEONを継続させるとか？
;; /usr/local/bin/gpg へ gpg2 からシンボリックリンクを貼るでOK．OK
;; %04y を %Y にしろと怒られた

;; input-method-activate-hook, input-method-deactivate-hook
;; activate-mark-hook, deactivate-mark-hook
;; minibuffer-setup-hook, minibuffer-exit-hook
;; isearch-mode-hook, isearch-mode-end-hook

(with-eval-after-load "org"
  (when (require 'backline nil t)
    (advice-add 'outline-flag-region :after 'backline-update)))

;; ns-inline-patch
(custom-set-faces
 '(ns-marked-text-face
   ((t (:foreground "black" :background "light pink" :underline "OrangeRed2"))))
 '(ns-unmarked-text-face
   ((t (:foreground "black" :background "light sky blue" :underline "royal blue")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;; (advice-add 'epg--check-error-for-decrypt :override 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load "postpone"
  (defun ad:message (f FORMAT-STRING &rest ARGS)
    (let ((str (concat (make-string (frame-width) ?\x5F) "\n" FORMAT-STRING)))
      (apply f str ARGS)))
  ;; (advice-add 'message :around #'ad:message)
  ;; (advice-remove 'message #'ad:message)
  )
