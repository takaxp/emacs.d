;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME

;; (profiler-start 'cpu)
;; (setq my-suppress-message-p nil)
;; (setq measure-exec-time-list '(my-show-org-buffer
;;                                my-private-conf-activate
;;                                my-org-babel-load-activate
;;                                my-org-modules-activate
;;                                my-org-agenda-prepare-buffers
;;                                ))

(with-eval-after-load "org"
  ;; outline-flag-region and backline
  (when (require 'backline nil t)
    (advice-add 'outline-flag-region :after 'backline-update))

  (defun my-hugo-export-upload ()
    "Export subtree for Hugo and upload the engty."
    (when (member (buffer-name) '("imadenale.org" "archive.org"))
      (if (not (org-entry-is-done-p))
          (message "The state of the entry is not \"DONE\" yet.")
        (my-org-replace-punc-in-tree)
        (save-buffer)
        (org-hugo-export-wim-to-md)
        (let ((command "/Users/taka/Dropbox/scripts/push-hugo.sh")
              (filename (org-entry-get (point) "EXPORT_FILE_NAME"))
              (exported (format "[ox-hugo] \"%s\" has been exported."
                                (nth 4 (org-heading-components)))))
          (when filename
            (save-excursion
              (save-restriction
                (outline-up-heading 1)
                (setq filename
                      (concat (nth 4 (org-heading-components)) "/" filename))
                (setq command (concat command " -e " (downcase filename)))))
            (message "%s\nUploading..." exported)
            (message "--- %s" command)
            ;; commad='/Users/taka/Dropbox/scripts/push-hugo.sh -e 2023/0301-macmini2018-transfer'
            (message "%s" (shell-command-to-string command))
            ;; (call-process "/Users/taka/Dropbox/scripts/push-hugo.sh"
            ;;               nil
            ;;               "-e"
            ;;               (downcase filename))
            (message "%s\nUploading...done" exported))))))

  ;; (setq epg-pinentry-mode nil)
  (when (string= (system-name) "water.local")
    (setq org-show-notification-handler nil))

  (defun my-pin-subtree ()
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
  (defun my-unpin-subtree ()
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
  (defun my-toggle-subtree-pin ()
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

  ;; (defmacro org-assert-version ()
  ;;   "Assert compile time and runtime version match."
  ;;   t)
  (advice-add 'org-assert-version :override #'ignore))

;; (with-eval-after-load "org"
;;   (require 'org-phscroll nil t))

(defun my-my-native-comp-packages-done ()
  (message "Native Compilation...done"))

(defun my-native-comp-packages ()
  (interactive)
  (add-hook 'native-comp-async-all-done-hook
            #'my-my-native-comp-packages-done)
  (let ((native-comp-async-jobs-number 8)
        (native-comp-speed 3))
    (message "Native Compilation...")
    (native-compile-async "~/.emacs.d/29.0.50/el-get" 'recursively)))
;; (remove-hook 'native-comp-async-all-done-hook #'my-my-native-comp-packages-done)

;; Boot mode selection
(cond
 (nil ;; minimal boot or DOOM Emacs (use toggle-doom.sh to switch)
  (when (boundp 'ns-command-modifier) (setq ns-command-modifier 'meta))
  (when (and (memq window-system '(ns mac))
             (fboundp 'mac-get-current-input-source))
    ;; "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese" for Big Sur
    (custom-set-variables
     '(mac-default-input-source "com.google.inputmethod.Japanese.base"))
    (mac-input-method-mode 1)
    (global-set-key (kbd "M-SPC") 'mac-ime-toggle)
    (global-set-key (kbd "S-SPC") 'mac-ime-toggle)))
 (nil ;; To test the latest org
  (add-to-list 'load-path (expand-file-name "~/devel/git/org-mode/lisp"))
  (setq org-agenda-files '("~/Desktop/test/hoge.org")))
 (nil ;; minimum
  (load (concat user-emacs-directory "min/init.el")))
 (nil ;; Spacemacs
  (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el")))
 (t ;; Normal mode. see also init-eval.el
  (load "~/Dropbox/emacs.d/config/init-env.el" nil t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "postpone"
  ;; At least in Big Sur, this setting shall be used with side car for moom.el.
  ;; Without side car in Big Sur, the following setting is also correct.
  ;; Then what about other macOSs?

  ;; (when (string= "Big Sur" (macos-name (macos-version)))
  ;;   (setq moom--common-margin '(0 0 0 0)))

  ;;(profiler-report)
  )

;; (require 'postpone)
;; (my-show-org-buffer "next.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;;(advice-add 'epg--check-error-for-decrypt :override 'ignore)
;; (package-initialize) ;; keep this here for previous versions
