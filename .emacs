;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME

(defun my-hugo-export-upload ()
  "Export subtree for Hugo and upload the engty."
  (when (member (buffer-name) '("imadenale.org" "archive.org"))
    (if (not (org-entry-is-done-p))
        (message "The state of the entry is not \"DONE\" yet.")
      (my-org-replace-punc-in-tree)
      (save-buffer)
      ;; (let ((outfile (org-hugo-export-wim-to-md)))
      ;;   (sit-for 2)
      ;;   (when (and outfile
      ;;              (file-exists-p outfile))
      ;;     (switch-to-buffer
      ;;      (find-file-noselect outfile)
      ;;      (my-org-replace-punc-in-buffer))))
      (org-hugo-export-wim-to-md)
      (let ((command "/Users/taka/Dropbox/scripts/push-hugo.sh")
            (filename (org-entry-get (point) "EXPORT_FILE_NAME"))
            (exported (format "[ox-hugo] \"%s\" has been exported."
                              (nth 4 (org-heading-components)))))
        (when filename
          ;; (when (file-exists-p (concat outfile ".md"))
          ;;   (switch-to-buffer
          ;;    (find-file-noselect (concat outfile ".md"))
          ;;    (my-org-replace-punc-in-buffer)
          ;;    (save-buffer)))
          (save-excursion
            (save-restriction
              (outline-up-heading 1)
              (setq filename
                    (concat (nth 4 (org-heading-components)) "/" filename))
              (setq command (concat command " -e " (downcase filename)))))
          (message "[hugo] %s" command)
          (message "%s\nUploading..." exported)
          (message "%s" (shell-command-to-string command))
          (message "%s" command)
          (message "%s\nUploading...done" exported))))))

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
