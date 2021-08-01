;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond (nil ;; To test the latest org
       (add-to-list 'load-path (expand-file-name "~/devel/git/org-mode/lisp"))
       (setq org-agenda-files '("~/Desktop/hoge.org")))
      (t ;; Normal mode. see also init-eval.el
       (load "~/Dropbox/emacs.d/config/init-env.el" nil t)))
(with-eval-after-load "postpone"
  (when (require 'mlscroll nil t)
    ;; (setq mlscroll-in-color "salmon1")
    ;;(setq mlscroll-in-color "#BBBBBB")
    (setq mlscroll-in-color "#FFA07A")
    (setq mlscroll-out-color "#FFFFE0")
    (mlscroll-mode 1)))
(when (version< "28.0" emacs-version)
  (setq native-comp-async-report-warnings-errors nil)
  (defmacro define-obsolete-variable-alias (obsolete-name
                                            current-name
					                                  &optional when docstring)
    ""
    (declare (doc-string 4)
             (advertised-calling-convention
              (obsolete-name current-name when &optional docstring) "23.1"))
    `(progn
       (defvaralias ,obsolete-name ,current-name ,docstring)
       (dolist (prop '(saved-value saved-variable-comment))
         (and (get ,obsolete-name prop)
              (null (get ,current-name prop))
              (put ,current-name prop (get ,obsolete-name prop))))
       (make-obsolete-variable ,obsolete-name ,current-name ,when)))
  (defmacro define-obsolete-function-alias (obsolete-name
                                            current-name
					                                  &optional when docstring)
    ""
    (declare (doc-string 4)
             (advertised-calling-convention
              (obsolete-name current-name when &optional docstring) "23.1"))
    `(progn
       (defalias ,obsolete-name ,current-name ,docstring)
       (make-obsolete ,obsolete-name ,current-name ,when)))
  )
;; (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME

;;(add-to-list 'load-path "~/.local/share/icons-in-terminal/")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ns-inline-patch
(custom-set-faces
 '(ns-marked-text-face
   ((t (:foreground "black"
                    :background "light pink" :underline "OrangeRed2"))))
 '(ns-unmarked-text-face
   ((t (:foreground "black"
                    :background "light sky blue" :underline "royal blue")))))

(when nil
  (with-eval-after-load "postpone"
    (setq mac-default-input-source "com.apple.inputmethod.Kotoeri.Japanese")

    ;; Shall be updated for Kotoeri
    (defun ns-insert-marked-text (pos len)
      "Insert contents of `ns-working-text' as UTF-8 string and mark with
  `ns-working-overlay' and `ns-marked-overlay'.  Any previously existing
  working text is cleared first. The overlay is assigned the faces
  `ns-working-text-face' and `ns-marked-text-face'."
      (ns-delete-working-text)
      (let ((start (point)))
        (when (<= pos (length ns-working-text))
          (put-text-property pos len 'face 'ns-working-text-face ns-working-text)
          (insert ns-working-text)
          ;; (if (= len 0)
          ;;     (overlay-put (setq ns-working-overlay
          ;;                        (make-overlay start (point)
          ;;                                      (current-buffer) nil t))
          ;;                  'face 'ns-working-text-face)
          ;;   (overlay-put (setq ns-working-overlay
          ;;                      (make-overlay start (point)
          ;;                                    (current-buffer) nil t))
          ;;                'face 'ns-unmarked-text-face)
          ;;   (overlay-put (setq ns-marked-overlay
          ;;                      (make-overlay
          ;;                       (+ start pos) (+ start pos len)
          ;;                       (current-buffer) nil t))
          ;;                'face 'ns-marked-text-face))
          (goto-char (+ start pos))

          (if (= len 0)
              (overlay-put (setq ns-working-overlay
                                 (make-overlay start (point)
                                               (current-buffer) nil t))
                           'after-string
                           (propertize ns-working-text
                                       'face 'ns-working-text-face))
            (overlay-put (setq ns-working-overlay
                               (make-overlay start (point)
                                             (current-buffer) nil t))
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
            (goto-char (+ start pos))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;;(advice-add 'epg--check-error-for-decrypt :override 'ignore)
