;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME

(with-eval-after-load "el-get-byte-compile"

  (defun el-get-byte-compile-file (el &optional warnings)
    "Byte compile the EL file, and skips unnecessary compilation.

Specifically, if the compiled elc file already exists and is
newer, then compilation is skipped."
    (let ((elc (concat (file-name-sans-extension el) ".elc"))
          (byte-compile-warnings warnings)
          ;; Byte-compile runs emacs-lisp-mode-hook; disable it
          emacs-lisp-mode-hook)
      (when (or (not (file-exists-p elc))
                (not (file-newer-than-file-p elc el)))
        (when (file-exists-p elc)
          ;; Delete the old elc to make sure that if the compilation fails to
          ;; generate a new one, there will be no discrepancy between them.
          (delete-file elc))
        (condition-case err
            (progn
              (message "--- Compiling...%s" el)
              (byte-compile-file el)
              (native-compile el)
              )
          ((debug error) ;; catch-all, allow for debugging
           (message "%S" (error-message-string err)))))))

  )



;; (unless (getenv "LIBRARY_PATH")
;;   (setenv "LIBRARY_PATH"
;;           (string-join
;;            '("/opt/homebrew/opt/gcc/lib/gcc/13"
;;              "/opt/homebrew/opt/libgccjit/lib/gcc/13"
;;              "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin23/13")
;;            ":")))

;; (with-eval-after-load "nerd-icons"
;;   ;;  
;;   (nerd-icons-octicon "nf-oct-fold")

;;   (setq mode-line-modes
;;         (mapcar
;;          (lambda (entry)
;;            (if (equal entry "%n")
;;                '(:eval (progn
;;                          ;; org が widen を乱発するのでこちらをトリガーにする．
;;                          ;; 色の変更
;;                          (my-update-modeline-color)
;;                          ;; "Narrow" を "N" に短縮表示
;;                          (if (and (buffer-narrowed-p)
;;                                   (fboundp 'nerd-icons-octicon))
;;                              (concat " " (nerd-icons-octicon
;;                                           "nf-oct-fold" :v-adjust 0.0)) "")))
;;              entry))
;;          mode-line-modes))
;;   (setq mode-line-position-line-format
;;         `(,(nerd-icons-mdicon "nf-md-square_edit_outline") "%3l"))
;; ;;  󰤌
;;   )

(with-eval-after-load "icons-in-terminal"
  ;;  
  )

(with-eval-after-load "org"
  ;; (advice-add 'org-assert-version :override #'ignore)
  ;; (require 'org-phscroll nil t)

  (defun my-org-pin-subtree ()
    "Pin the subtree with \"pin\" tag."
    (interactive)
    (save-excursion
      (save-restriction
        (unless (org-at-heading-p)
          (org-previous-visible-heading 1))
        (unless (org-before-first-heading-p)
          (org-set-property "VISIBILITY" "children")
          (org-toggle-tag "pin" 'on)
          (message "Pinned")))))

  (defun my-org-unpin-subtree ()
    "Unpin the subtree with \"pin\" tag."
    (interactive)
    (save-excursion
      (save-restriction
        (unless (org-at-heading-p)
          (org-previous-visible-heading 1))
        (unless (org-before-first-heading-p)
          (when (org-element-property :VISIBILITY (org-element-at-point))
            (org-delete-property "VISIBILITY")
            (org-toggle-tag "pin" 'off)
            (message "Unpinned"))))))

  (defun my-toggle-org-pin-subtree ()
    "Toggle \"VISIBILITY\" of the current tree."
    (interactive)
    (save-excursion
      (save-restriction
        (unless (org-at-heading-p)
          (org-previous-visible-heading 1))
        (unless (org-before-first-heading-p)
          (let ((element (org-element-at-point)))
            (cond ((org-element-property :VISIBILITY element)
                   (org-delete-property "VISIBILITY")
                   (org-toggle-tag "pin" 'off)
                   (message "Unpinned"))
                  (t
                   (org-set-property "VISIBILITY" "children")
                   (org-toggle-tag "pin" 'on)
                   (message "Pinned"))))))))
  )

;; Boot mode selection
(cond
 ;; minimal boot or DOOM Emacs (use toggle-doom.sh to switch)
 (nil
  (when (boundp 'ns-command-modifier) (setq ns-command-modifier 'meta))
  (when (and (memq window-system '(ns mac))
             (fboundp 'mac-get-current-input-source))
    ;; "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese" for Big Sur
    (custom-set-variables
     '(mac-default-input-source "com.google.inputmethod.Japanese.base"))
    (mac-input-method-mode 1)
    (global-set-key (kbd "M-SPC") 'mac-ime-toggle)
    (global-set-key (kbd "S-SPC") 'mac-ime-toggle)))
 ;; To test the latest org
 (nil
  (add-to-list 'load-path (expand-file-name "~/devel/git/org-mode/lisp"))
  (add-to-list 'load-path (expand-file-name "~/devel/git/org-tree-slide"))
  (setq org-agenda-files '("~/Desktop/test/hoge.org")))
 ;; minimum
 (nil (load (concat user-emacs-directory "min/init.el")))
 ;; Spacemacs
 (nil (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el")))
 ;; Normal mode. see also init-eval.el
 (t (load "~/Dropbox/emacs.d/config/init-env.el" nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;; (advice-add 'epg--check-error-for-decrypt :override 'ignore)
;; (package-initialize) ;; keep this line here for previous versions
