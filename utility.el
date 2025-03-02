;; utility.el --- My utility.el -*- lexical-binding: t -*-
;; "my-" and "ad:" functions associated with my 'init.el'
(unless (featurep 'postpone)
  (call-interactively 'postpone-pre))
(unless noninteractive
  (defvar my-utility-start (current-time)))

;;;###autoload
(defun my--measure-exec-time (f &rest arg)
  "If `measure-exec-time-list' is non-nil, measure exe time for each function."
  (if measure-exec-time-list
      (let ((inhibit-message nil)
            (message-log-max 5000)
            (begin (current-time)))
        (apply f arg)
        (message
         (format "--- %.3f[ms] %S"
                 (* 1000 (float-time (time-subtract (current-time) begin)))
                 (if (byte-code-function-p f)
                     nil ;; not includes closure
                   f)))) ;; FIXME
    (apply f arg)))

;;;###autoload
(defun my--do-after-load-evaluation (abs-file)
  "Evaluate all `eval-after-load' forms, if any, for ABS-FILE.
ABS-FILE, a string, should be the absolute true name of a file just loaded.
This function is called directly from the C code."
  ;; Run the relevant eval-after-load forms.
  (dolist (a-l-element after-load-alist)
    (when (and (stringp (car a-l-element))
               (string-match-p (car a-l-element) abs-file))
      ;; discard the file name regexp
      (mapc #'funcall (cdr a-l-element))))
  ;; Complain when the user uses obsolete files.
  (when (string-match-p "/obsolete/[^/]*\\'" abs-file)
    ;; Maybe we should just use display-warning?  This seems yucky...
    (let* ((file (file-name-nondirectory abs-file))
           (package (intern (substring file 0
                                       (string-match "\\.elc?\\>" file))
                            obarray))
           (msg (unless (memq package my-exclude-deprecated-packages)
                  (format "Package %s is deprecated" package)))
           (fun (lambda (msg) (message "%s" msg))))
      (when (or (not (fboundp 'byte-compile-warning-enabled-p))
                (byte-compile-warning-enabled-p 'obsolete package))
        (cond
         ((bound-and-true-p byte-compile-current-file)
          ;; Don't warn about obsolete files using other obsolete files.
          (unless (and (stringp byte-compile-current-file)
                       (string-match-p "/obsolete/[^/]*\\'"
                                       (expand-file-name
                                        byte-compile-current-file
                                        byte-compile-root-dir)))
            (byte-compile-warn "%s" msg)))
         (noninteractive (funcall fun msg)) ;; No timer will be run!
         (t (run-with-idle-timer 0 nil fun msg))))))

  ;; Finally, run any other hook.
  (run-hook-with-args 'after-load-functions abs-file))

;;;###autoload
(defun future-time-p (time)
  "Return non-nil if provided TIME formed of \"10:00\" is the future time."
  (not (time-less-p
        (apply 'encode-time
               (let ((t1 (decode-time))
                     (t2 (parse-time-string time)))
                 (setf (nth 0 t1) 0)
                 (setf (nth 1 t1) (nth 1 t2))
                 (setf (nth 2 t1) (nth 2 t2))
                 t1))
        (current-time))))

;; For instance,
;; (when (future-time-p "10:00") (run-at-time...))

;; (eval-when-compile
;;   (message "Loading gcmh... %s" (featurep 'gcmh))
;;   (require 'gcmh))

(defvar my-gcmh-idlegc-p nil)

;;;###autoload
(defun my--garbage-collect (f)
  (unless my-gcmh-idlegc-p
    (message "[gcmh] Garbage collecting...")
    (message "[gcmh] Garbage collecting...done (%.3fs)"
             (gcmh-time (funcall f)))))

;;;###autoload
(defun my--gcmh-idle-garbage-collect (f)
  (let ((my-gcmh-idlegc-p t))
    (funcall f)))

;;;###autoload
(defun my-gcmh-activate ()
  (cancel-timer my-gcmh-timer)
  (gcmh-mode 1))

;;;###autoload
(defun my-native-comp-p ()
  (when (fboundp 'native-comp-available-p)
    (native-comp-available-p)))

;;;###autoload
(defun my-native-comp-packages-done ()
  (message "Native Compilation...done"))

;;;###autoload
(defun my-org-hide-drawers-all ()
  (when (eq major-mode 'org-mode)
    (org-cycle-hide-drawers 'all)))

;;;###autoload
(defun my-auto-revert-activate ()
  (global-auto-revert-mode 1)
  (remove-hook 'find-file-hook #'my-auto-revert-activate))

;; see also a configuration of `directory-abbrev-alist'
;;;###autoload
(defun my-shorten-default-directory ()
  "Enforce to replace \"/home/...\" with \"~/\"."
  (setq default-directory (abbreviate-file-name default-directory)))

;;;###autoload
(defun my-private-conf-activate ()
  (cancel-timer my-private-conf-timer)
  ;; (require 'epa)
  (when (and (file-exists-p "~/Dropbox/config/private.el.gpg")
             (eq system-type 'darwin)
             (not (featurep 'private)))
    (unless (ignore-errors (require 'private "private.el.gpg" t))
      (user-error "GPG decryption error (private.el)"))))

;;;###autolaod
(defun my-lock-secret-buffer (&optional file)
  (when (and (stringp file)
             (buffer-live-p (get-buffer file)))
    (kill-buffer file)
    (let ((message-log-max nil))
      (message "--- %s is locked." file))))

;;;###autoload
(defun my-start-autolock-secret-buffer ()
  (interactive)
  (my-stop-autolock-secret-buffer)
  (setq my-secret-close-timer
        (run-with-idle-timer
         my-secret-autolock-time t
         #'my-lock-secret-buffer my-secret-org-file)))

;;;###autoload
(defun my-stop-autolock-secret-buffer ()
  (interactive)
  (when (timerp my-secret-close-timer)
    (cancel-timer my-secret-close-timer)))

;;;###autoload
(defun my-isearch-ime-deactivate-sticky ()
  (unless (region-active-p)
    (mac-ime-deactivate-sticky)))

;;;###autoload
(defun my-toggle-ime-ns ()
  "Toggle IME."
  (interactive)
  (if (my-ime-active-p) (my-ime-off) (my-ime-on)))

;;;###autoload
(defun my-working-text-face-on ()
  (if (or isearch-mode
          (minibufferp))
      (custom-set-faces
       '(ns-working-text-face nil))
    (custom-set-faces
     '(ns-working-text-face
       ((((background dark))
         :background "#594d5d" :underline "LightSlateBlue")
        (t (:background "#fff0de" :underline "gray20")))))))

;;;###autoload
(defun my-working-text-face-off ()
  (if (or isearch-mode
          (minibufferp))
      (custom-set-faces
       '(ns-working-text-face nil))
    (custom-set-faces
     '(ns-working-text-face
       ((((background dark)) :background "#484c5c" :underline "white")
        (t (:background "#DEEDFF" :underline "DarkOrchid3")))))))

;;;###autoload
(defun my-ns-org-heading-auto-ascii ()
  "IME off, when the cursor on org headings."
  ;; (message "%s" (frame-focus-state (selected-frame)))
  (when (and
         (fboundp 'frame-focus-state)
         (frame-focus-state)
         (eq major-mode 'org-mode)
         (boundp 'org-agenda-buffer-name)
         (or (looking-at org-heading-regexp)
             (equal (buffer-name) org-agenda-buffer-name))
         (my-ime-active-p))

    (my-ime-off)))

;;;###autoload
(defun my--mark-sexp (f &optional arg allow-extend)
  "Set mark ARG sexps from point.
When the cursor is at the end of line or before a whitespace, set ARG -1."
  (interactive "P\np")
  (funcall f (if (and (not (bolp))
                      (not (eq (preceding-char) ?\ ))
                      (not (memq (following-char) '(?\( ?\< ?\[ ?\{)))
                      (or (eolp)
                          (eq (following-char) ?\ )
                          (memq (preceding-char) '(?\) ?\> ?\] ?\}))))
                 -1 arg)
           allow-extend))

;;;###autoload
(defun my--er:mark-sexp (f &optional arg allow-extend)
  "If the cursor is on a symbol, expand the region along the symbol."
  (interactive "P\np")
  (if (and (not (use-region-p))
           (symbol-at-point)
           (not (memq (following-char) '(?\( ?\< ?\[ ?\{)))
           (not (memq (preceding-char) '(?\) ?\> ?\] ?\}))))
      (er/mark-symbol)
    (funcall f arg allow-extend)))

;;;###autoload
(defun my-bm-save-all ()
  (bm-buffer-save-all)
  (bm-repository-save))

;;;###autoload
(defun my-toggle-bm ()
  "bm-toggle with updating history"
  (interactive)
  (let ((bm (concat
             (buffer-name) "::"
             (if (and (equal major-mode 'org-mode)
                      (not (org-before-first-heading-p)))
                 (nth 4 (org-heading-components))
               (format "%s" (line-number-at-pos))))))
    (if (bm-bookmark-at (point))
        (bookmark-delete bm)
      (bookmark-set bm)))
  (bm-toggle)
  (bm-buffer-save-all)
  (bm-repository-save))

;;;###autoload
(defun my-bm-next ()
  "bm-next with org-mode"
  (interactive)
  (bm-next)
  (when (and (equal major-mode 'org-mode)
             (not (org-before-first-heading-p)))
    (widen)
    (org-overview)
    (org-reveal)
    (org-cycle-hide-drawers 'all)
    (org-show-entry)
    (show-children)
    (org-show-siblings)))

;;;###autoload
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

;;;###autoload
(defun counsel-bm ()
  (interactive)
  (let* ((bm-list (counsel-bm-get-list (bm-overlays-lifo-order t)))
         (bm-hash-table (make-hash-table :test 'equal))
         (search-list (-map (lambda (bm) (car bm)) bm-list)))
    (-each bm-list (lambda (bm)
                     (puthash (car bm) (cdr bm) bm-hash-table)
                     ))
    (ivy-read "Find bookmark(bm.el): "
              search-list
              :require-match t
              :keymap counsel-describe-map
              :action (lambda (chosen)
                        (let ((bookmark (gethash chosen bm-hash-table)))
                          (switch-to-buffer (overlay-buffer bookmark))
                          (bm-goto bookmark)
                          ))
              :sort t)))

;;;###autoload
(defun my--bm-show-mode ()
  "Enable truncate mode when showing bm list."
  (toggle-truncate-lines 1))

;;;###autoload
(defun my-centered-cursor-activate () (centered-cursor-mode 1))

;;;###autoload
(defun my-centered-cursor-deactivate () (centered-cursor-mode -1))

;;;###autoload
(defun my-smart-mark-activate ()
  (smart-mark-mode 1)
  (remove-hook 'find-file-hook #'my-smart-mark-activate))

;;;###autoload
(defun my--smart-mark-restore-cursor ()
  "Restore cursor position saved just before mark."
  (when smart-mark-point-before-mark
    (when (> smart-mark-point-before-mark 1)
      ;; To avoid to jump to the beginning of the buffer
      (goto-char smart-mark-point-before-mark))
    (setq smart-mark-point-before-mark nil)))

;;;###autoload
(defun my--smart-mark-set-restore-before-mark (&rest _arg)
  (unless (memq this-command
                '(er/expand-region er/mark-symbol er/contract-region))
    (setq smart-mark-point-before-mark (point))))

;;;###autoload
(defun my--er:keyboard-quit ()
  (when (memq last-command '(er/expand-region er/contract-region))
    (when smart-mark-point-before-mark
      (goto-char smart-mark-point-before-mark))))

;;;###autoload
(defun my--er:pre:keyboard-quit ()
  (when (memq last-command '(er/expand-region er/contract-region))
    (er/contract-region 0)
    ;; (when (> smart-mark-point-before-mark 1) ;; FIXME
    ;;   (goto-char smart-mark-point-before-mark))
    ))

;;;###autoload
(defun my--syntax-subword-activate (&rest arg)
  (unless (featurep 'syntax-subword)
    (global-syntax-subword-mode 1))
  (advice-remove 'forward-word #'my--syntax-subword-activate)
  (advice-remove 'backward-word #'my--syntax-subword-activate)
  arg)

;;;###autoload
(defun my--syntax-subword-kill (&optional n)
  "Replace `kill-region' with `delete-region'."
  (interactive "^p")
  (let ((beg (point))
        (end (save-excursion (syntax-subword-forward n) (point))))
    (delete-region beg end)))

;;;###autoload
(defun my-time-stamp ()
  (setq time-stamp-format
        (if (eq major-mode 'org-mode)
            "[%Y-%02m-%02d %3a]" ;; "%04y %02H:%02M"
          "%Y-%02m-%02d"))
  (if (boundp 'org-tree-slide-mode)
      (unless org-tree-slide-mode
        (time-stamp))
    (time-stamp)))

;;;###autoload
(defun my--isearch-mode (f forward &optional regexp op-fun recursive-edit
                          regexp-function)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        (funcall f forward regexp op-fun recursive-edit regexp-function)
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    (funcall f forward regexp op-fun recursive-edit regexp-function)))

;;;###autoload
(defun my-orgalist-activate ()
  (when (require 'orgalist nil t)
    (orgalist-mode 1))) ;; originally orgstruct-mode

;;;###autoload
(defun my--add-change-log-entry-other-window ()
  (when view-mode
    (View-exit-and-edit)))

;;;###autoload
(defun org-info-ja (&optional node)
  "(Japanese) Read documentation for Org-mode in the info system.
    With optional NODE, go directly to that node."
  (interactive)
  (info (format "(org-ja)%s" (or node ""))))

;;;###autoload
(defun my-json-mode-beautify ()
  (when (eq major-mode 'json-mode)
    (json-mode-beautify (point-min) (point-max))))

;;;###autoload
(defun my-json-pretty-print-buffer ()
  (when (eq major-mode 'json-mode)
    (json-pretty-print-buffer)))

;;;###autoload
(defun my-auto-view ()
  "Open a file with `view-mode'."
  (when (file-exists-p buffer-file-name)
    (when (and my-auto-view-regexp
         (string-match my-auto-view-regexp buffer-file-name))
      (view-mode 1))
    (dolist (dir my-auto-view-dirs)
      (when (eq 0 (string-match (expand-file-name dir) buffer-file-name))
        (view-mode 1)))))

;;;###autoload
(defun my-org-view-next-heading ()
  (interactive)
  (if (and (derived-mode-p 'org-mode)
           (org-at-heading-p))
      (org-next-visible-heading 1)
    (next-line)))

;;;###autoload
(defun my-org-view-previous-heading ()
  (interactive)
  (if (and (derived-mode-p 'org-mode)
           (org-at-heading-p))
      (org-previous-visible-heading 1)
    (previous-line)))

;;;###autoload
(defun my-view-tab ()
  (interactive)
  (if (and (derived-mode-p 'org-mode)
           (or (org-at-heading-p)
               (org-at-property-drawer-p)))
      (let ((view-mode nil))
        (org-cycle))
    (when (require 'origami nil t)
      (origami-toggle-node (current-buffer) (point)))))

;;;###autoload
(defun my-view-shifttab ()
  (interactive)
  (if (derived-mode-p 'org-mode)
      (let ((view-mode nil))
        (org-shifttab))
    (when (require 'origami nil t)
      (origami-toggle-all-nodes (current-buffer)))))

;;;###autoload
(defun my-unlock-view-mode ()
  (when view-mode
    (View-exit-and-edit)))

;;;###autoload
(defun my-view-exit ()
  (interactive)
  (if (use-region-p) (my-eval-region) (View-exit)))

;;;###autoload
(defun my--view--enable () (my-mode-line-on))

;;;###autoload
(defun my--view--disable () (my-mode-line-off))

;;;###autoload
(defun my--switch-to-buffer (&rest _arg)
  (when (and (not view-mode)
             (member (buffer-name) my-auto-view-buffers))
    (view-mode 1)))

;;;###autoload
(defun my-web-indent-fold ()
  (interactive)
  (web-mode-fold-or-unfold)
  (web-mode-buffer-indent)
  (indent-for-tab-command))

;;;###autoload
(defun my-emacs-lisp-mode-indent-conf ()
  (interactive)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 8)
  (setq indent-line-function 'lisp-indent-line))

;;;###autoload
(defun my-flyspell-ignore-nonascii (beg end _info)
  "incorrect判定をASCIIに限定"
  (string-match "[^!-~]" (buffer-substring beg end)))
(add-hook 'flyspell-incorrect-hook #'my-flyspell-ignore-nonascii)

;;;###autoload
(defun my-flyspell-on ()
  (cond
   ((memq major-mode major-mode-with-flyspell)
    (turn-on-flyspell))
   ((memq major-mode major-mode-with-flyspell-prog)
    (flyspell-prog-mode))
   (t
    nil)))

;;;###autoload
(defun my-flyspell-off ()
  (when (memq major-mode my-flyspell-target-modes)
    (turn-off-flyspell)))

;;;###autoload
(defun my--YaTeX-insert-begin-end (env region-mode)
  "Insert \\begin{mode-name} and \\end{mode-name}.
This works also for other defined begin/end tokens to define the structure."
  (setq YaTeX-current-completion-type 'begin)
  (let*((ccol (current-column)) beg beg2 exchange
        (_arg region-mode)		;for old compatibility
        (indent-column (+ ccol YaTeX-environment-indent))(_i 1) _func)
    (if (and region-mode (> (point) (mark)))
        (progn (exchange-point-and-mark)
               (setq exchange t
                     ccol (current-column)
                     indent-column (+ ccol YaTeX-environment-indent))))
    ;;VER2 (insert "\\begin{" env "}" (YaTeX-addin env))
    (setq beg (point))
    (YaTeX-insert-struc 'begin env)
    (setq beg2 (point))
    (insert "\n")
    (indent-to indent-column)
    (save-excursion
      ;;indent optional argument of \begin{env}, if any
      (while (> (point-beginning-of-line) beg)
        (skip-chars-forward "\\s " (point-end-of-line))
        (indent-to indent-column)
        (forward-line -1)))
    (require 'yatexenv)
    (if region-mode
        ;;if region-mode, indent all text in the region
        (save-excursion
          (if (fboundp (intern-soft (concat "YaTeX-enclose-" env)))
              (funcall (intern-soft (concat "YaTeX-enclose-" env))
                       (point) (mark))
            (while (< (progn (forward-line 1) (point)) (mark))
              (if (eolp) nil
                (skip-chars-forward " \t\n")
                (indent-to indent-column))))))
    (if region-mode (exchange-point-and-mark))
    (indent-to ccol)
    ;;VER2 (insert "\\end{" env "}\n")
    (YaTeX-insert-struc 'end env)
    (YaTeX-reindent ccol)
    (if region-mode
        (progn
          (insert "\n")
          (or exchange (exchange-point-and-mark)))
      (goto-char beg2)
      (YaTeX-intelligent-newline nil)
      (YaTeX-indent-line))
    (YaTeX-package-auto-usepackage env 'env)
    (if YaTeX-current-position-register
        (point-to-register YaTeX-current-position-register))))

;;;###autoload
(defun my-smartparens-mode ()
  (smartparens-global-mode)
  (remove-hook 'yatex-mode-hook #'my-smartparens-mode)
  (remove-hook 'org-mode-hook #'my-smartparens-mode))

;;;###autoload
(defun my-activate-selected ()
  (require 'transient nil t)
  (selected-global-mode 1)
  (selected--on) ;; must call expclitly here
  (remove-hook 'activate-mark-hook #'my-activate-selected))

;;;###autoload
(defun my-helpful-variable ()
  (interactive)
  (let ((thing (symbol-at-point)))
    (if (helpful--variable-p thing)
        (helpful-variable thing)
      (call-interactively 'helpful-variable))))

(defvar my-eval-result "*eval-result*")

;;;###autoload
(defun my-eval-region ()
  (interactive)
  (when (use-region-p)
    (eval-region (region-beginning) (region-end)
                 (get-buffer-create my-eval-result))
    ;; Copy the result to kill-ring and print it
    (with-current-buffer (get-buffer-create my-eval-result)
      (delete-char -1)
      (goto-char (point-min))
      (delete-blank-lines)
      (mark-whole-buffer)
      (kill-ring-save (point-min) (point-max))
      (message "%s" (car kill-ring))
      (erase-buffer))
    ;; Jump to the end of the region
    (goto-char (max (or (mark) 0) (point)))
    (deactivate-mark)))

;;;###autoload
(defun my-eval-region-as-function ()
  (interactive)
  (when (use-region-p)
    (let ((region (intern (buffer-substring-no-properties
                           (region-beginning) (region-end)))))
      (funcall region))))

;;;###autoload
(defun my-describe-selected-keymap ()
  (interactive)
  (describe-keymap 'selected-keymap))

;;;###autoload
(defun my-update-modeline-face ()
  (setq my-selected-window-last (frame-selected-window))
  ;; (message "--- %s" my-selected-window-last)
  (unless (minibufferp)
    (my-modeline-face (buffer-narrowed-p))))

;;;###autoload
(defun my-modeline-face (buffer-narrowed)
  "Update modeline color.
If BUFFER-NARROWED is nil, then change the color to indicating `widen'.
Otherwise, indicating narrowing."
  (unless (eq my-buffer-narrowed-last
              buffer-narrowed) ;; block unnecessary request
    (setq my-buffer-narrowed-last buffer-narrowed)
    ;; (message "--- %s %s %s" this-command last-command buffer-narrowed)
    (when (not (memq this-command '(save-buffer))) ;; FIXME
      (if buffer-narrowed
          (custom-set-faces
           `(mode-line ((t (:background
                            ,(nth 0 my-narrow-modeline)
                            :foreground
                            ,(nth 1 my-narrow-modeline))))))
        (custom-set-faces '(mode-line ((t nil))))))))

;;;###autoload
(defun my-update-modeline-color ()
  "Update modeline face of the current selected window.
Call this function at updating `mode-line-mode'."
  (when (eq my-selected-window-last (frame-selected-window))
    (my-modeline-face (buffer-narrowed-p))))

;;;###autoload
(defun my-reload-mlscroll ()
  (mlscroll-mode -1)
  (setq mlscroll-border (ceiling (/ moom-font--size 4.0)))
  (mlscroll-mode 1))

;;;###autoload
(defun my-mode-line-vc-mode-nerd-icons ()
  (if (string-match "^ Git:" vc-mode) ;; nf-oct-git_branch
      (replace-regexp-in-string
       "^ Git:" (propertize " " 'face 'mode-line-vc-modified-face) vc-mode)
    (replace-regexp-in-string
     "^ Git-" (propertize " " 'face 'mode-line-vc-normal-face) vc-mode)))

;;;###autoload
(defun my-mode-line-vc-mode-icons-in-terminal ()
  (if (string-match "^ Git:" vc-mode)
      (replace-regexp-in-string
       "^ Git:" (propertize " " 'face 'mode-line-vc-modified-face) vc-mode)
    (replace-regexp-in-string
     "^ Git-" (propertize " " 'face 'mode-line-vc-normal-face) vc-mode)))

;;;###autoload
(defun my-open-scratch ()
  "Switch the current buffer to \*scratch\* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

;;;###autoload
(defun my--split-window-below (&optional _size)
  "An extention to switch to \*scratch\* buffer after splitting window."
  (my-open-scratch))

;;;###autoload
(defun my-change-window-divider ()
  (interactive)
  (let ((display-table (or buffer-display-table
                           standard-display-table
                           (make-display-table))))
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table)))

;;;###autoload
(defun my-update-display-line-numbers-face ()
  (custom-set-faces
   `(line-number-current-line
     ((t (:bold t :background ,(face-attribute 'hl-line :background)))))))

;;;###autoload
(defun my-display-line-numbers-width ()
  (when (< display-line-numbers-width 5)
    (setq display-line-numbers-width 5))
  (setq moom-display-line-numbers-width (+ 2 display-line-numbers-width)))

;;;###autoload
(defun my-display-line-numbers-mode-on ()
  "Trun on `display-line-numbers'."
  (interactive)
  (if (fboundp 'global-display-line-numbers-mode) ;; 26.1 or later
      (unless global-display-line-numbers-mode
        (global-display-line-numbers-mode 1)
        (line-number-mode -1))
    (user-error "The display-line-numbers is NOT supported")))

;;;###autoload
(defun my-display-line-numbers-mode-off ()
  "Trun off `display-line-numbers'."
  (interactive)
  (if (fboundp 'global-display-line-numbers-mode) ;; 26.1 or later
      (when global-display-line-numbers-mode
        (global-display-line-numbers-mode -1)
        (line-number-mode 1))
    (user-error "The display-line-numbers is NOT supported")))

;;;###autoload
(defun my-toggle-display-line-numbers-mode ()
  "Toggle variable `global-display-line-numbers-mode'."
  (interactive)
  (if (fboundp 'global-display-line-numbers-mode) ;; 26.1 or later
      (let ((flag (if global-display-line-numbers-mode -1 1)))
        (global-display-line-numbers-mode flag)
        (line-number-mode (- flag)))
    (user-error "The display-line-numbers is NOT supported")))

;;;###autoload
(defun my-mic-paren-activate ()
  (paren-activate)
  (show-paren-mode -1)
  (remove-hook 'find-file-hook #'my-mic-paren-activate))

;;;###autoload
(defun my--mic-paren-highlight (f)
  (if (active-minibuffer-window)
      (let ((paren-display-message 'never))
        (funcall f)
        paren-display-message)
    (funcall f)))

;;;###autoload
(defun my--font-lock-mode (&optional _ARG)
  (unless (memq major-mode '(vterm-mode))
    (font-lock-add-keywords major-mode
                            ;; "[\t]+$" 行末のタブ
                            '(("　" 0 'my-face-b-1 append)
                              ("[ ]+$" 0 'my-face-b-3 append)
                              ("[\t]+$" 0 'my-face-b-2 append)))))

;; 文字エンコーディングの文字列表現

;;;###autoload
(defun my-coding-system-name-mnemonic (coding-system)
  (let* ((base (coding-system-base coding-system))
         (name (symbol-name base)))
    (cond ((string-prefix-p "utf-8" name) "U8")
          ((string-prefix-p "utf-16" name) "U16")
          ((string-prefix-p "utf-7" name) "U7")
          ((string-prefix-p "japanese-shift-jis" name) "SJIS")
          ((string-match "cp\\([0-9]+\\)" name) (match-string 1 name))
          ((string-match "japanese-iso-8bit" name) "EUC")
          (t "???"))))

;;;###autoload
(defun my-coding-system-bom-mnemonic (coding-system)
  (let ((name (symbol-name coding-system)))
    (cond ((string-match "be-with-signature" name) "[BE]")
          ((string-match "le-with-signature" name) "[LE]")
          ((string-match "-with-signature" name) "[BOM]")
          (t ""))))

;;;###autoload
(defun my-mode-line-icon-lock-icons-in-terminal ()
  (if view-mode
      (concat (icons-in-terminal-faicon
               "lock" :face '(:foreground "#FF0000")) " ") ""))

;;;###autoload
(defun my-mode-line-icon-lock-nerd-icons ()
  (if view-mode
      (concat (nerd-icons-mdicon
               "nf-md-file_lock" :face '(:foreground "#FF0000")) " ") ""))

;;;###autoload
(defun my-mode-line-icon-for-file ()
  (cond ((require 'nerd-icons nil t)
         (nerd-icons-icon-for-file
          (buffer-name) :v-adjust 0.03 :face 'mode-line-file-icon-face))
        ((require 'icons-in-terminal nil t)
         (icons-in-terminal-icon-for-file
          (buffer-name) :v-adjust 0.03 :face 'mode-line-file-icon-face))))

;;;###autoload
(defun my-buffer-coding-system-mnemonic ()
  "Return a mnemonic for `buffer-file-coding-system'."
  (let* ((code buffer-file-coding-system)
         (name (my-coding-system-name-mnemonic code))
         (bom (my-coding-system-bom-mnemonic code)))
    (if (version< emacs-version "29.0")
        (format "%s %s%s" (my-mode-line-icon-for-file) name bom )
      (format "%s%s" name bom ))))

;;;###autoload
(defun my-delight-activate ()
  (require 'delight nil t)
  (remove-hook 'find-file-hook #'my-delight-activate))

;;;###autoload
(defun my-migemo-activate ()
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (add-hook 'isearch-mode-hook #'migemo-init)
    (migemo-init))
  (remove-hook 'isearch-mode-hook #'my-migemo-activate))

(autoload 'calendar-iso-from-absolute "cal-iso" nil t)
(autoload 'calendar-absolute-from-gregorian "calendar" nil t)

;;;###autoload
(defun my-get-week-number ()
  "Return the current week number."
  (format "%02d"
          (car
           (calendar-iso-from-absolute
            (calendar-absolute-from-gregorian
             (list (string-to-number (format-time-string "%m"))
                   (string-to-number (format-time-string "%d"))
                   (string-to-number (format-time-string "%y"))))))))

;;;###autoload
(defun my-week-number ()
  "Show the current week number."
  (interactive)
  (message "w%s" (my-get-week-number)))

;;;###autoload
(defun my-empty-booting-header-line ()
  (with-current-buffer "*scratch*"
    (let ((week (format "W%s: " (my-get-week-number)))
          (date (format-time-string "%Y-%m-%d %a.")))
      (setq header-line-format
            (concat
             " No day is a good day.                                       "
             week
             date
             (propertize " "
                         'display
                         `(space . (:align-to
                                    ,(- (frame-width)
                                        (length week)
                                        (length date))))))))))

;;;###autoload
(defun my-calendar-mark-selected ()
  (org-eval-in-calendar '(setq cursor-type nil) t))

;;;###autoload
(defun my--elisp-eldoc (_callback)
  "Avoid hiding `hl-line' in `emacs-lisp-mode'."
  (when (fboundp 'hl-line-highlight)
    (hl-line-highlight)))

;;;###autoload
(defun my--eldoc-message (f &optional string)
  (unless (active-minibuffer-window)
    (funcall f string)))

;;;###autoload
(defun my-seq-sort-by (function pred sequence)
  "Sort SEQUENCE using PRED as a comparison function.
Elements of SEQUENCE are transformed by FUNCTION before being
sorted.  FUNCTION must be a function of one argument."
  (seq-sort (lambda (a b)
        (funcall pred
           (funcall function a)
           (funcall function b)))
      sequence))

;;;###autoload
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

;;;###autoload
(defun my-disable-counsel-find-file (&rest args)
  "Disable `counsel-find-file' and use the original `find-file' with ARGS."
  (let ((completing-read-function #'completing-read-default)
        (completion-in-region-function #'completion--in-region))
    (apply #'read-file-name-default args)))

;; Common actions for counsel-ag, counsel-fzf, and counsel-recentf

;;;###autoload
(defun my-counsel-fzf-in-default-dir (_arg)
  "Search the current directory with fzf."
  (counsel-fzf ivy-text default-directory))

;;;###autoload
(defun my-counsel-fzf-in-dir (_arg)
  "Search again with new root directory."
  (counsel-fzf ivy-text
         (read-directory-name
    (concat (car (split-string counsel-fzf-cmd))
      " in directory: "))))

;;;###autoload
(defun my-counsel-ag-in-dir (_arg)
  "Search again with new root directory."
  (let ((current-prefix-arg '(4)))
    (counsel-ag ivy-text nil ""))) ;; also disable extra-ag-args

;;;###autoload
(defun my--counsel-mark-ring ()
  "Browse `mark-ring' interactively.
Obeys `widen-automatically', which see."
  (interactive)
  (let* ((counsel--mark-ring-calling-point (point))
   (marks (copy-sequence mark-ring))
   (marks (delete-dups marks))
   (marks
    ;; mark-marker is empty?
    (if (equal (mark-marker) (make-marker))
        marks
      (cons (copy-marker (mark-marker)) marks)))
   (candidates (counsel-mark--get-candidates marks)))
    (delete-dups candidates) ;; [added] remove duplicated lines
    (if candidates
  (counsel-mark--ivy-read "Mark: " candidates 'counsel-mark-ring)
      (message "Mark ring is empty"))
    counsel--mark-ring-calling-point)) ;; To avoid an warning on lexical val.

;;;###autoload
(defun my-pre-prompt-function ()
  (cond (window-system
         (format "%s%s "
                 (if my-toggle-modeline-global "" ;; FIXME
                   (concat (make-string (frame-width) ?\x5F) "\n")) ;; "__"
                 (cond ((require 'nerd-icons nil t)
                        (nerd-icons-mdicon "nf-md-playlist_check")) ;; 󰗇
                       ((require 'icons-in-terminal nil t)
                        (icons-in-terminal-material "playlist_add_check"))
                       ((require 'all-the-icons nil t)
                        (all-the-icons-material "playlist_add_check"))
                       (t ""))))
        ;; ((eq system-type 'windows-nt)
        ;;	(format "%s%s "
        ;;					(if my-toggle-modeline-global "" ;; FIXME
        ;;						(concat (make-string (frame-width) ?\x5F) "\n")) ;; "__"
        ;;					">>"))
        (t
         (format "%s\n" (make-string (1- (frame-width)) ?\x2D)))))

;;;###autoload
(defun my--truncate-lines-activate ()
  "Truncate lines on `imenu-list' buffer."
  (toggle-truncate-lines 1))

;;;###autoload
(defun my--imenu-list-update ()
  "Expand frame width by `moom-change-frame-width'."
  (when (and (memq imenu-list-position '(right left))
             (not (get-buffer-window imenu-list-buffer-name t)))
    (moom-change-frame-width (+ (frame-width) imenu-list-size))))

;;;###autoload
(defun my--imenu-list-quit-window ()
  "Shrink frame width by `moom-change-frame-width'."
  (when (and (memq imenu-list-position '(right left))
             (not (get-buffer-window imenu-list-buffer-name t)))
    (moom-change-frame-width (- (frame-width) imenu-list-size))))

;;;###autoload
(defun my-command-log-mode-activate ()
  (interactive)
  (keypression-mode 1)
  (global-command-log-mode 1)
  (when (require 'moom nil t)
    (moom-delete-windows)
    (moom-change-frame-width 140)
    (moom--stay-in-region)
    (clm/open-command-log-buffer)))

;;;###autoload
(defun my-command-log-mode-deactivate ()
  (interactive)
  (keypression-mode -1)
  (global-command-log-mode -1)
  (when (require 'moom nil t)
    (moom-delete-windows)))

;;;###autoload
(defun my-enable-tree-sitter ()
  (unless (featurep 'tree-sitter)
    (require 'tree-sitter)
    (require 'tree-sitter-hl)
    (require 'tree-sitter-debug)
    (require 'tree-sitter-query)
    (require 'tree-sitter-langs))
  (tree-sitter-hl-mode))

;;;###autoload
(defun my--swiper-thing-at-point ()
  "`swiper' with `ivy-thing-at-point'."
  (interactive)
  (let ((thing (if (thing-at-point-looking-at "^\\*+") ;; org heading を除外
                   nil
                 (ivy-thing-at-point))))
    (when (use-region-p)
      (deactivate-mark))
    (swiper thing)))

;;;###autoload
(defun my-update-nerd-icons-ivy-rich-display-transformers-list (command config)
  "If update config during a session, call `nerd-icons-ivy-rich-reload'
  to enable the config."
  (if (plist-get nerd-icons-ivy-rich-display-transformers-list command)
      (plist-put nerd-icons-ivy-rich-display-transformers-list
                 command config)
    (user-error "`%s' is not listed in `nerd-icons-ivy-rich-display-transformers-list'." command)))

;;;###autoload
(defun my-toggle-dimmer ()
  (interactive)
  (if (setq my-dimmer-mode (not my-dimmer-mode))
      (dimmer-on) (dimmer-off)))

;;;###autoload
(defun dimmer-permanent-off ()
  (setq my-dimmer-mode nil)
  (dimmer-off))

;;;###autoload
(defun dimmer-off ()
  (dimmer-process-all)
  (dimmer-mode -1))

;;;###autoload
(defun dimmer-on ()
  (when my-dimmer-mode
    (dimmer-mode 1)
    (dimmer-process-all)))

;;;###autoload
(defun my-dimmer-update ()
  (if (frame-focus-state) (dimmer-on) (dimmer-off)))

;;;###autoload
(defun my--dimmer-org-agenda--quit (&optional _bury)
  (when (fboundp 'dimmer-on)
    (setq my-dimmer-mode t)
    (dimmer-on)
    (redraw-frame)))

;;;###autoload
(defun my-dimmer-activate ()
  (setq my-dimmer-mode (dimmer-mode 1))
  (remove-hook 'window-configuration-change-hook #'my-dimmer-activate));; FIXME

;;;###autoload
(defun my-recentf-save-list-silence ()
  (interactive)
  (if shutup-p
      (shut-up (recentf-save-list))
    (let ((message-log-max nil))
      (recentf-save-list)))
  (message ""))

;;;###autoload
(defun my-recentf-cleanup-silence ()
  (interactive)
  (when (file-exists-p "/Volumes/orzHDn")
    (if shutup-p
        (shut-up (recentf-cleanup))
      (let ((message-log-max nil))
        (recentf-cleanup)))
    (message "")))

;;;###autoload
(defun my-counsel-recentf-action (file)
  (cond ((string-match "\\.numbers$\\|\\.xlsx$" file)
         (eval `(with-ivy-window (org-open-file ,file))))
        (t
         (eval `(with-ivy-window (find-file ,file))))))

;;;###autoload
(defun my--counsel-recentf ()
  "Find a file on `recentf-list'."
  (interactive)
  (require 'recentf)
  (recentf-mode)
  (ivy-read "Recentf: "
            (mapcar (lambda (x) (abbreviate-file-name  ;; ~/
                                 (substring-no-properties x)))
                    recentf-list)
            :action #'my-counsel-recentf-action
            :require-match t
            :caller 'counsel-recentf))

(defvar my-cg-bookmark "c-g-point-last")
;;;###autoload
(defun my-cg-bookmark ()
  (push-mark)
  (when (and buffer-file-name
             (eq major-mode 'org-mode)
             (not (org-before-first-heading-p))
             (> (org-current-level) 1)) ;; レベル1の heading を除外
    (bookmark-set my-cg-bookmark)
    (save-buffer)))

;;;###autoload
(defun crux-copy-file-preserve-attributes (visit)
  "[crux.el]
Copy the current file-visiting buffer's file to a destination.

This function prompts for the new file's location and copies it
similar to cp -p. If the new location is a directory, and the
directory does not exist, this function confirms with the user
whether it should be created. A directory must end in a slash
like `copy-file' expects. If the destination is a directory and
already has a file named as the origin file, offers to
overwrite.

If the current buffer is not a file-visiting file or the
destination is a non-existent directory but the user has elected
to not created it, nothing will be done.

When invoke with C-u, the newly created file will be visited.
"
  (interactive "p")
  (let ((current-file (buffer-file-name)))
    (when current-file
      (let* ((new-file (read-file-name "Copy file to: "))
             (abs-path (expand-file-name new-file))
             (create-dir-prompt "%s is a non-existent directory, create it? ")
             (is-dir? (string-match "/" abs-path (1- (length abs-path))))
             (dir-missing? (and is-dir? (not (file-exists-p abs-path))))
             (create-dir? (and is-dir?
                               dir-missing?
                               (y-or-n-p
                                (format create-dir-prompt new-file))))
             (destination (concat (file-name-directory abs-path)
                                  (file-name-nondirectory current-file))))
        (unless (and is-dir? dir-missing? (not create-dir?))
          (when (and is-dir? dir-missing? create-dir?)
            (make-directory abs-path))
          (condition-case nil
              (progn
                (copy-file current-file abs-path nil t t t)
                (message "Wrote %s" destination)
                (when visit
                  (find-file-other-window destination)))
            (file-already-exists
             (when (y-or-n-p (format "%s already exists, overwrite? " destination))
               (copy-file current-file abs-path t t t t)
               (message "Wrote %s" destination)
               (when visit
                 (find-file-other-window destination))))))))))

;;;###autoload
(defun crux-rename-file-and-buffer ()
  "[crux.el]
Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (when-let* ((filename (buffer-file-name))
              (new-name (or (read-file-name "New name: " (file-name-directory filename) nil 'confirm)))
              (containing-dir (file-name-directory new-name)))
    ;; make sure the current buffer is saved and backed by some file
    (when (or (buffer-modified-p) (not (file-exists-p filename)))
      (if (y-or-n-p "Can't move file before saving it.  Would you like to save it now?")
          (save-buffer)))
    (if (get-file-buffer new-name)
        (message "There already exists a buffer named %s" new-name)
      (progn
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename)
          ;; vc-rename-file seems not able to cope with remote filenames?
          (let ((vc-filename (if (tramp-tramp-file-p filename) (tramp-file-local-name filename) filename))
                (vc-new-name (if (tramp-tramp-file-p new-name) (tramp-file-local-name filename) new-name)))
            (vc-rename-file vc-filename vc-new-name)))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;;;###autoload
(defun crux-delete-file-and-buffer ()
  "[crux.el]
Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;;;###autoload
(defun crux-open-with (arg)
  "[crux.el]
Open visited file in default external program.
When in dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (derived-mode-p 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (recentf-add-file current-file-name) ;; Add the file to list of recentf
    (call-process program nil 0 nil current-file-name)))

;;;###autoload
(defun my-backup-recentf ()
  (interactive)
  (my-backup recentf-save-file) ;; "~/.emacs.d/recentf"
  (my-backup (expand-file-name "~/.histfile")))

;;;###autoload
(defun my-auto-backup ()
  (unless (equal (buffer-name) "recentf")
    (backup-each-save)))

;;;###autoload
(defun my--backup-each-save-compute-location (filename)
  (let* ((containing-dir (file-name-directory filename))
         (basename (file-name-nondirectory filename))
         (backup-container
          (format "%s/%s"
                  backup-each-save-mirror-location
                  ;; "c:" is not allowed
                  (replace-regexp-in-string ":" "" containing-dir))))
    (when (not (file-exists-p backup-container))
      (make-directory backup-container t))
    (format "%s/%s-%s" backup-container basename
            (format-time-string backup-each-save-time-format))))

;;;###autoload
(defun my-dired-activate ()
  (unless (require 'init-dired nil t)
    (user-error "init-dired.el doesn't exist")))

;;;###autoload
(defun my-super-save-predicates-p ()
  "Return nil, if the buffer should not be saved."
  (not
   (cond ((memq major-mode '(undo-tree-visualizer-mode diff-mode)) t)
         ((when (eq major-mode 'org-mode)
            ;; when activating org-capture
            (or (bound-and-true-p org-capture-mode)
                (and (fboundp 'org-entry-get)
                     (equal "" (org-entry-get (point)
                                              "EXPORT_FILE_NAME"))))) t)
         ((let ((pt (point)))
            ;; .gpg で半角スペースの後ろのブリッツでは自動保存しない．
            ;; FIXME 半角スペース
            (when (and (string-match ".gpg" (buffer-name))
                       (not (eq pt 1))
                       (not (eq pt (point-min))))
              (string-match (buffer-substring (- pt 1) pt) " "))) t))))

;;;###autoload
(defun my--super-save-buffers-command ()
  "Save the buffer if needed.
see https://github.com/bbatsov/super-save/pull/20/files."
  (save-mark-and-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (when (and buffer-file-name
                 (buffer-modified-p (current-buffer))
                 (file-writable-p buffer-file-name)
                 (if (file-remote-p buffer-file-name)
                     super-save-remote-files t))
        (save-buffer)))))

;;;###autoload
(defun my-super-save-activate ()
  (unless noninteractive
    (super-save-mode 1))
  (remove-hook 'find-file-hook #'my-super-save-activate))

;;;###autoload
(defun my--neotree-show ()
  "Extension to support change frame width when opening neotree."
  (unless (neo-global--window-exists-p)
    (when (and (require 'moom nil t)
               (not my-neo-activated))
      (setq moom-frame-width-single
            (+ moom-frame-width-single my-neo-adjusted-window-width))
      (setq moom-frame-width-double
            (+ moom-frame-width-double my-neo-adjusted-window-width)))
    (set-frame-width nil (+ (frame-width) my-neo-adjusted-window-width))
    (setq my-neo-activated t)))

;;;###autoload
(defun my--neotree-hide ()
  "Extension to support change frame width when closing neotree."
  (when (neo-global--window-exists-p)
    (when (and (require 'moom nil t)
               my-neo-activated)
      (setq moom-frame-width-single
            (- moom-frame-width-single my-neo-adjusted-window-width))
      (setq moom-frame-width-double
            (- moom-frame-width-double my-neo-adjusted-window-width)))
    (set-frame-width nil (- (frame-width) my-neo-adjusted-window-width))
    (when (> 80 (frame-width)) ;; fail safe
      (set-frame-width nil 80))
    (setq my-neo-activated nil)))

;;;###autoload
(defun my--helpful-at-point ()
  (deactivate-mark))

;;;###autoload
(defun my--keyfreq-show ()
  "Extension to make the buffer view-only."
  (interactive)
  (if shutup-p
      (shut-up (view-buffer keyfreq-buffer))
    (view-buffer keyfreq-buffer)))

;;;###autoload
(defun my--counsel-ag (f &optional initial-input initial-directory extra-ag-args ag-prompt caller)
  (apply f (or initial-input
               (and (not (thing-at-point-looking-at "^\\*+"))
                    (ivy-thing-at-point)))
         (unless current-prefix-arg
           (or initial-directory default-directory))
         extra-ag-args ag-prompt caller))

;;;###autoload
(defun my--counsel-fzf (f &optional initial-input initial-directory fzf-prompt)
  (funcall f (or initial-input
               (if (thing-at-point-looking-at "^\\*+") ;; org heading を除外
                   nil
                 (ivy-thing-at-point)))
         (or initial-directory (funcall counsel-fzf-dir-function))
         fzf-prompt))

;;;###autoload
(defun my-decimal-to-hex ()
  (interactive)
  (0xc-convert 16 (word-at-point)))

;;;###autoload
(defun my-hex-to-decimal ()
  (interactive)
  (0xc-convert 10 (word-at-point)))

;;;###autoload
(defun my-uuid-string ()
  (interactive)
  (insert (uuid-string)))

;;;###autoload
(defun my-projectile-activate ()
  (interactive)
  (setq projectile-keymap-prefix (kbd "C-c p"))
  (projectile-mode 1)
  (remove-hook 'find-file-hook #'my-projectile-activate))

;;;###autoload
(defun my--neotree-dir (path)
  "Extension to change the frame width automatically."
  (interactive "DDirectory: ")
  (unless (neo-global--window-exists-p)
    (neotree-show))
  (neo-global--open-dir path)
  (neo-global--select-window))

;;;###autoload
(defun my--projectile-visit-project-tags-table ()
  "Extensions to skip calling `visit-tags-table'."
  nil)

;;;###autoload
(defun my-counsel-projectile-ag ()
  "Use `counsel-projectile-ag' in a projectile project except when `dired'.
Otherwise, use `counsel-ag'."
  (interactive)
  (if (or (and (eq projectile-require-project-root 'prompt)
               (not (projectile-project-p)))
          (eq major-mode 'dired-mode))
      (counsel-ag)
    (counsel-projectile-ag)))

;;;###autoload
(defun my--magit-mode-bury-buffer (&optional _bury)
  (when (fboundp 'dimmer-on)
    (setq my-dimmer-mode t)
    (dimmer-on)
    (redraw-frame)))

;;;###autoload
(defun my-editorconfig-activate ()
  (if (and (executable-find "editorconfig")
           (require 'editorconfig nil t)
           (require 'editorconfig-core nil t)  )
      (editorconfig-mode 1)
    (message "Editorconfig is not installed."))
  (remove-hook 'find-file-hook #'my-editorconfig-activate))

;;;###autoload
(defun my--minibuffer-complete (f)
  "Enforce to use `completion--in-region' when completing in minibuffer."
  (let ((completion-in-region-function #'completion--in-region))
    (funcall f)))

;;;###autoload
(defun my-advice-minibuffer-complete ()
  (advice-add 'minibuffer-complete :around #'my--minibuffer-complete)
  (remove-hook 'minibuffer-setup-hook #'my-advice-minibuffer-complete))

;;;###autoload
(defun my-load-cape-modules-for-org ()
  ;; 1st: begin_src emacs-lisp..end_src 内でelispを補完可能にする．
  (add-hook 'completion-at-point-functions #'cape-elisp-block -2 'local)
  ;; 2nd: システムのファイルパスを補完可能にする
  (add-hook 'completion-at-point-functions #'cape-file -1 'local)
  ;; 3rd: 辞書 FIXME should be done by manually?
  (add-hook 'completion-at-point-functions #'cape-dict nil 'local))

;;;###autoload
(defun my--corfu-insert-separator (ARG)
  "Use C-SPC to insert the separator."
  (interactive "P")
  (if (corfu--continue-p) ;; (> corfu--total 0)
      (insert corfu-separator)
    (set-mark-command ARG)))

;;;###autoload
(defun my-org-modules-activate ()
  (interactive)
  (if (and (featurep 'org-tempo)
           (featurep 'org-id))
      (message "org-modules are previously loaded.")
    (message "Loading org-modules...")
    (setq org-modules my-org-modules) ;; revert to the original value
    ;; モジュールの追加
    (add-to-list 'org-modules 'org-id)
    (with-eval-after-load "org-agenda"
      ;; org-agenda を読んでしまうので org-mode 開始時には読み込ませない
      (add-to-list 'org-modules 'org-habit)) ;; require org and org-agenda
    (when (version< "9.1.4" (org-version))
      (add-to-list 'org-modules 'org-tempo))
    (when (require 'ol-bookmark nil t)
      ;; [[bookmark:hoge][hogehoge]] 形式のリンクを有効化
      (add-to-list 'org-modules 'ol-bookmark)
      (setq bookmark-save-flag 4) ;; N回 bookmark を操作したら保存
      ;; `bookmark-default-file' の読み込み
      (bookmark-maybe-load-default-file))

    ;; 不必要なモジュールの読み込みを停止する
    (delq 'ol-bbdb org-modules)
    (delq 'ol-irc org-modules)
    (delq 'ol-mhe org-modules)
    (delq 'ol-docview org-modules)
    ;; Reload
    (org-load-modules-maybe t)
    (org-element-cache-reset 'all) ;; FIXME use `custom-set-variables'
    (message "Loading org-modules...done")))

;;;###autoload
(defun my-open-default-org-file ()
  (interactive)
  (my-show-org-buffer "next.org"))

;;;###autoload
(defun my--org-last-repeat (&optional _arg)
  (when (and (org-get-repeat)
             (org-entry-is-todo-p))
    (org-entry-put nil "LAST_REPEAT" (format-time-string
                                      (org-time-stamp-format t t)))))

;;;###autoload
(defun my--org-modules-activate (&optional _arg)
  (interactive)
  (my-org-modules-activate)
  (advice-remove 'org-cycle #'my--org-modules-activate))

;;;###autoload
(defun my-desktop-notification (title message &optional sticky sound timeout)
  "Show a message by `alerter' command."
  (if (eq ns-alerter-command 'script)
      (ns-do-applescript
       (format "display notification \"%s\" with title \"%s\""
               title message))
    (start-process
     "notification" "*notification*"
     ns-alerter-command
     "-title" title
     "-message" message
     "-sender" "org.gnu.Emacs"
     "-timeout" (format "%s" (if sticky 0 (or timeout 7)))
     "-sound" (or sound ns-default-notification-sound))))

;;;###autoload
(defun my-desktop-notification-handler (message)
  (my-desktop-notification "Message from org-mode" message t))

(defun my--org-reveal (f &optional siblings)
  (interactive "P")
  (if (org-at-heading-p)
      (org-show-subtree)
    (funcall f siblings)))

;;;###autoload
(defun my-org-move-item-begin ()
  "Move the current item to the beginning of the list."
  (interactive)
  (unless (org-at-item-p) (error "Not at an item"))
  (let* ((col (current-column))
         (item (point-at-bol))
         (struct (org-list-struct))
         (prevs (org-list-prevs-alist struct))
         (prev-item (org-list-get-prev-item (point-at-bol) struct prevs)))
    (unless prev-item
      (user-error "Cannot move this item further up"))
    (setq struct (org-list-send-item item 'begin struct))
    (goto-char item)
    (org-list-write-struct struct (org-list-parents-alist struct))
    (org-move-to-column col)))

;;;###autoload
(defun my-org-move-item-end ()
  "Move the current item to the end of the list."
  (interactive)
  (unless (org-at-item-p) (error "Not at an item"))
  (let* ((col (current-column))
         (item (point-at-bol))
         (struct (org-list-struct))
         (prevs (org-list-prevs-alist struct))
         (next-item (org-list-get-next-item (point-at-bol) struct prevs)))
    (unless next-item
      (user-error "Cannot move this item further down"))
    (setq struct (org-list-send-item item 'end struct))
    (goto-char item)
    (org-list-write-struct struct (org-list-parents-alist struct))
    (org-move-to-column col)))

;;;###autoload
(defun my-org-agenda-prepare-buffers ()
  (unless (featurep 'org-agenda)
    (when (require 'org-agenda nil t)
      (unless (and (featurep 'org-id)
                   (featurep 'org-tempo))
        (my-org-modules-activate)) ;; FIXME
      (unless (featurep 'ob-http) (my-org-babel-load-activate)) ;; FIXME
      (org-agenda-prepare-buffers org-agenda-files)
      (message "Building agenda buffers...done"))))

;;;###autoload
(defun my-recenter-top-bottom-top ()
  "Recenter the current line to the top of window."
  (set-window-start (get-buffer-window) (line-beginning-position)))

;; org-agenda の内容をアラームに登録する

;; 重複実行の抑制用フラグ
(defvar my-org-agenda-to-appt-ready t)

;;;###autoload
(defun my-org-agenda-to-appt (&optional force)
  "Update `appt-time-mag-list'.  Use `async' if possible."
  (interactive)
  (unless (featurep 'org)
    (require 'org))
  (if (or (not (require 'async nil t))
          (not my-org-agenda-to-appt-async))
      (unless (active-minibuffer-window)
        ;; (org-agenda-to-appt t '((headline "TODO")))
        (org-agenda-to-appt t)
        (appt-check))
    (when force
      (setq my-org-agenda-to-appt-ready t))
    (if (not my-org-agenda-to-appt-ready)
        (message "[appt] Locked")
      (setq my-org-agenda-to-appt-ready nil)
      ;; (message "-------------------------")
      ;; (message "parent: %s"
      ;;          (format-time-string "%H:%M:%S.%3N" (current-time)))
      (async-start
       `(lambda ()
          (setq load-path ',load-path)
          (require 'org)
          (require 'appt)
          (setq org-agenda-files ',org-agenda-files)
          ;; (org-agenda-to-appt t '((headline "TODO")))
          (org-agenda-to-appt t)
          (appt-check) ;; remove past events
          ;; Remove tags
          (let ((msgs appt-time-msg-list))
            (setq appt-time-msg-list nil)
            (dolist (msg msgs)
              (add-to-list 'appt-time-msg-list
                           (let ((match (string-match
                                         org-tag-group-re (nth 1 msg))))
                             (if match
                                 (list (nth 0 msg)
                                       (org-trim (substring-no-properties
                                                  (nth 1 msg)
                                                  0 match))
                                       (nth 2 msg))
                               msg)
                             ) t))
            ;; just for sure
            (delq nil appt-time-msg-list)))
       `(lambda (result)
          ;; (message "child: %s"
          ;;          (format-time-string "%H:%M:%S.%3N" (current-time)))
          (setq appt-time-msg-list result) ;; nil means No event
          ;; (my-add-prop-to-appt-time-msg-list)
          (unless (active-minibuffer-window)
            (let ((cnt (length appt-time-msg-list))
                  (message-log-max nil))
              (if (eq cnt 0)
                  (message "[async] No event to add")
                (message "[async] Added %d event%s for today"
                         cnt (if (> cnt 1) "s" "")))))
          (setq my-org-agenda-to-appt-ready t))))))

;;;###autoload
(defun my-org-babel-load-activate ()
  (if (featurep 'ob-http)
      (message "org-babel language packages are previously loaded.")
    (message "Loading org-babel language packages...")
    (require 'ob-http nil t)
    (require 'ob-gnuplot nil t)
    (require 'ob-octave nil t)
    (require 'ob-go nil t)
    (require 'ob-async nil t)
    (custom-set-variables ;; will call `org-babel-do-load-languages'
     '(org-babel-load-languages '((emacs-lisp . t)
                                  (dot . t)
                                  (C . t)
                                  (ditaa . t)
                                  (perl . t)
                                  (shell . t)
                                  (latex . t)
                                  (sqlite . t)
                                  (R . t)
                                  (python . t))))
    (message "Loading org-babel language packages...done")))

(defvar my-org-delete-saved-item-timer nil)

;;;###autoload
(defun my-delete-last-saved-string ()
  (setq kill-ring (cdr kill-ring)))

;;;###autoload
(defun my-get-content-with-decrypt ()
  (interactive)
  (if (not (org-at-encrypted-entry-p))
      (echo "--- Do nothing, the subtree is NOT encrypted.")
    (outline-hide-subtree) ;; FIXME
    (org-decrypt-entry)
    (unless (org-at-heading-p)
      (org-back-to-heading))
    (org-end-of-meta-data t)
    (kill-ring-save (point)
                    (org-element-property :end (org-element-at-point)))
    (org-encrypt-entry)
    (outline-hide-subtree) ;; FIXME
    (org-back-to-heading)
    (if (org-at-encrypted-entry-p)
        (message "--- secured.")
      (error "Not secured"))
    (when (timerp my-org-delete-saved-item-timer)
      (cancel-timer my-org-delete-saved-item-timer))
    ;; FIXME should also run cancel-timer when yank only one time.
    (setq my-org-delete-saved-item-timer
          (run-with-timer 5 nil #'my-delete-last-saved-string))))

;;;###autoload
(defun my-add-ox-hugo-lastmod ()
  "Add `lastmod' property with the current time."
  (interactive)
  (org-set-property "EXPORT_HUGO_LASTMOD"
                    (format-time-string "[%Y-%m-%d %a %H:%M]")))

;;;###autoload
(defun my--ox-hugo:org-todo (&optional ARG)
  "Export subtree for Hugo if the TODO status in ARG is changing to DONE."
  (when (and (equal (buffer-name) "imadenale.org")
             ;; FIXME C-c C-t d に反応しない．speed command はOK．
             (or (eq ARG 'done)
                 (equal ARG "DONE")))
    (org-hugo-export-wim-to-md)
    (message "[ox-hugo] \"%s\" has been exported."
             (nth 4 (org-heading-components)))
    (let ((command "/Users/taka/Dropbox/local/scripts/push-hugo.sh"))
      (if (require 'async nil t)
          (async-start
           `(lambda () (shell-command-to-string ',command)))
        (shell-command-to-string command)))))

;;;###autoload
(defun my-push-trello-card () (interactive) (org-trello-sync-card))

;;;###autoload
(defun my-pull-trello-card () (interactive) (org-trello-sync-card t))

;;;###autoload
(defun my-push-trello () (interactive) (org-trello-sync-buffer))

;;;###autoload
(defun my-pull-trello () (interactive) (org-trello-sync-buffer t))

;;;###autoload
(defun my-activate-org-trello ()
  (let ((filename (buffer-file-name (current-buffer))))
    (when (and filename
               (string= "trello" (file-name-extension filename))
               (require 'org-trello nil t))
      (org-trello-mode))))

;;;###autoload
(defun my-toggle-org-show-emphasis-markers ()
  (interactive)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (font-lock-flush)))))

;;;###autoload
(defun my--org-recent-headings-activate ()
  (interactive)
  (when (require 'org-recent-headings nil t)
    (org-recent-headings-mode 1) ;; one time activate
    (advice-remove 'org-recent-headings
                   #'my--org-recent-headings-activate)))

;;;###autoload
(defun my-ime-invisible-cursor ()
  (interactive)
  (setq cursor-type (plist-get my-cur-type-ime :invisible)))

;;;###autoload
(defun my-ime-on ()
  (interactive)
  (if (fboundp 'mac-ime-activate)
      (mac-ime-activate)
     (activate-input-method default-input-method))
  (setq my-ime-last t))

;;;###autoload
(defun my-ime-off ()
  (interactive)
  (if (fboundp 'mac-ime-deactivate)
      (mac-ime-deactivate)
    (deactivate-input-method))
  (setq my-ime-last nil))

;;;###autoload
(defun my-ime-on-sticky ()
  (when my-ime-before-action
    (my-ime-on)))

;;;###autoload
(defun my-ime-off-sticky ()
  (when (setq my-ime-before-action (my-ime-active-p))
    (my-ime-off)))

;;;###autoload
(defun my--make-frame (&optional _parameters)
  (when (and (display-graphic-p)
             (called-interactively-p 'interactive))
    (message "--- Creating a frame.")
    (my-theme)
    (setq-default cursor-type
                  (if (my-ime-active-p)
                      (plist-get my-cur-type-ime :on)
                    (plist-get my-cur-type-ime :off)))
    (when (and (require 'moom-font nil t)
               (require 'moom nil t))
      (moom-font-resize))))

;;;###autoload
(defun my-toggle-modeline-global ()
  (interactive)
  (setq my-toggle-modeline-global (not my-toggle-modeline-global))
  (if my-toggle-modeline-global
      (my-mode-line-on)
    (my-mode-line-off)))

;;;###autoload
(defun my-mode-line-off ()
  "Turn off mode line."
  (when (fboundp 'dimmer-on)
    (dimmer-on))
  (when (fboundp 'pomodoro:visualize-stop)
    (pomodoro:visualize-stop))
  (when mode-line-format
    (setq my-mode-line-format mode-line-format))
  (setq mode-line-format nil))

;;;###autoload
(defun my-mode-line-on ()
  "Turn on mode line."
  (when (fboundp 'dimmer-off)
    (dimmer-off))
  (when (fboundp 'pomodoro:visualize-start)
    (pomodoro:visualize-start))
  (unless my-mode-line-format
    (error "Invalid value: %s" my-mode-line-format))
  (setq mode-line-format my-mode-line-format)
  (redraw-frame))

;;;###autoload
(defun my-toggle-mode-line ()
  "Toggle mode line."
  (interactive)
  (if mode-line-format
      (my-mode-line-off)
    (my-mode-line-on))
  (message "%s" (if mode-line-format "( ╹ ◡╹)ｂ ON !" "( ╹ ^╹)ｐ OFF!")))

;;;###autoload
(defun my--moom-toggle-frame-maximized ()
  (when (eq major-mode 'org-mode)
    (org-redisplay-inline-images))
  (when (and mode-line-format
             (not my-toggle-modeline-global))
    (my-mode-line-off)))

;;;###autoload
(defun my-modeline-activate ()
  (unless my-toggle-modeline-global
    (if shutup-p
        (shut-up (my-mode-line-off))
      (my-mode-line-off))))

;;;###autoload
(defun my--winner:delete-window (&optional _window)
  (message "Undo? M-x winner-undo or type \"C-x g\""))

;;;###autoload
(defun my-shackle-activate ()
  (shackle-mode 1)
  ;; (remove-hook 'window-configuration-change-hook #'my-shackle-activate)
  (remove-hook 'find-file-hook #'my-shackle-activate))

;;;###autoload
(defun my-delete-checkdoc-window ()
  (interactive)
  (let ((checkdoc-window (get-buffer-window "*Checkdoc Status*")))
    (when checkdoc-window
      (delete-window checkdoc-window)))
  (checkdoc-minor-mode -1))

;;;###autoload
(defun my--checkdoc ()
  (interactive)
  (keymap-set checkdoc-minor-mode-map "q" 'my-delete-checkdoc-window)
  (keymap-set checkdoc-minor-mode-map "C-g" 'my-delete-checkdoc-window)
  (checkdoc-minor-mode 1))

;;;###autoload
(defun my--doom-modeline-buffer-file-state-icon
    (icon &optional text face height voffset)
  "Displays an ICON with FACE, HEIGHT and VOFFSET.
TEXT is the alternative if it is not applicable.
Uses `all-the-icons-material' to fetch the icon."
  (if doom-modeline-icon
      (when icon
        (doom-modeline-icon-material
         icon
         :face face
         :height (or height 0.85) ;; 1.1
         :v-adjust (or voffset -0.225))) ;; -0.225
    (when text
      (propertize text 'face face))))

;;;###autoload
(defun my-generic-x-activate ()
  (require 'generic-x nil t)
  (remove-hook 'find-file-hook #'my-generic-x-activate))

;;;###autoload
(defun my-ime-off-hline ()
  (my-hl-line-enable)
  (let ((dark (eq (frame-parameter nil 'background-mode) 'dark)))
    (set-face-background hl-line-face (if dark "#484c5c" "#DEEDFF")))
  (run-hooks 'my-ime-off-hline-hook))

;;;###autoload
(defun my-ime-on-hline ()
  (my-hl-line-enable)
  (let ((dark (eq (frame-parameter nil 'background-mode) 'dark)))
    (set-face-background hl-line-face (if dark "#594d5d" "#fff0de")))
  (run-hooks 'my-ime-on-hline-hook))

;;;###autoload
(defun my-hl-line-update ()
  (if (frame-focus-state) (my-hl-line-enable) (my-hl-line-disable)))

;;;###autoload
(defun my-hl-line-disable ()
  "Disable `hl-line'."
  (hl-line-mode -1))

;; (eval-when-compile
;;   (message "Loading hl-line...")
;;   (require 'hl-line))

;;;###autoload
(defun my-hl-line-activate ()
  (when (require 'hl-line nil t)
    (add-hook 'ah-after-move-cursor-hook #'my-hl-line-enable))
  (remove-hook 'ah-after-move-cursor-hook #'my-hl-line-activate))

;;;###autoload
(defun my-hl-line-enable () ;; Hard to move this under utility.el
  "Enable `hl-line'."
  (unless (or hl-line-mode
              (minibufferp)
              (memq major-mode my-hl-permanent-disabled))
    (hl-line-mode 1))
  (setq my-hl-disabled-by-timer nil))

;; 1) Monaco, Hiragino/Migu 2M : font-size=12, -apple-hiragino=1.2
;; 2) Inconsolata, Migu 2M     : font-size=14,
;; 3) Inconsolata, Hiragino    : font-size=14, -apple-hiragino=1.0
(defconst my-font-size 12)
(defconst my-ja-font "Migu 2M") ;; "Hiragino Maru Gothic Pro"
(defconst my-ascii-font "Monaco") ;; "Inconsolata", Monaco
;; (defconst my-ja-font "Hiragino Maru Gothic Pro") ;; "Hiragino Maru Gothic Pro"
;; (defconst my-ascii-font "Inconsolata") ;; "Inconsolata", Menlo, "Ricty Diminished"

;;;###autoload
(defun my-ja-font-setter (spec)
  (set-fontset-font nil 'japanese-jisx0208 spec)
  (set-fontset-font nil 'katakana-jisx0201 spec)
  (set-fontset-font nil 'japanese-jisx0212 spec)
  (set-fontset-font nil '(#x0080 . #x024F) spec)
  (set-fontset-font nil '(#x0370 . #x03FF) spec)
  (set-fontset-font nil 'mule-unicode-0100-24ff spec)
  (set-fontset-font t 'unicode spec nil 'prepend))

;;;###autoload
(defun my-ascii-font-setter (spec)
  (set-fontset-font nil 'ascii spec))

;;;###autoload
(defun my-unicode-font-setter (spec)
  (set-fontset-font t 'unicode spec nil 'prepend))

;;;###autoload
(defun my-font-icons-setter ()
  (cond ((require 'nerd-icons nil t)
         (my-unicode-font-setter
          (font-spec :family (nerd-icons-mdicon-family)))
         (my-unicode-font-setter
          (font-spec :family (nerd-icons-faicon-family)))
         (my-unicode-font-setter
          (font-spec :family (nerd-icons-octicon-family)))
         (my-unicode-font-setter
          (font-spec :family (nerd-icons-devicon-family)))
         (my-unicode-font-setter
          (font-spec :family (nerd-icons-wicon-family))))
        ((require 'icons-in-terminal nil t)
         (my-unicode-font-setter
          (font-spec :family (icons-in-terminal-faicon-family)))
         (my-unicode-font-setter
          (font-spec :family (icons-in-terminal-fileicon-family)))
         (my-unicode-font-setter
          (font-spec :family (icons-in-terminal-material-family)))
         (my-unicode-font-setter
          (font-spec :family (icons-in-terminal-octicon-family)))
         (my-unicode-font-setter
          (font-spec :family (icons-in-terminal-wicon-family))))))

;;;###autoload
(defun my-font-config (&optional size ascii ja)
  "Font config.
- SIZE: font size for ASCII and Japanese (default: 12)
- ASCII: ascii font family (default: \"Monaco\")
- JA: Japanese font family (default: \"Migu 2M\")
"
  (when (memq window-system '(mac ns))
    (let ((font-size (or size my-font-size))
          (ascii-font (or ascii my-ascii-font))
          (ja-font (or ja my-ja-font)))
      (cond ((require 'nerd-icons nil t)
             ;; https://github.com/ryanoasis/nerd-fonts/wiki/Glyph-Sets-and-Code-Points
             ;; Material Design Icons (f0001-f1af0)
             (set-fontset-font t '(#Xf0001 . #Xf1af0) "Symbols Nerd Font Mono")
             (set-fontset-font t '(#X2300 . #X2bff) "Symbols Nerd Font Mono")
             (set-fontset-font t '(#Xe000 . #Xf5ff) "Symbols Nerd Font Mono")
             ;;  (hand-o-right in icons-in-terminal)
             (set-fontset-font t '(#Xe17b . #Xe17b) "icons-in-terminal"))
            ((require 'icons-in-terminal nil t)
             (set-fontset-font t '(#Xe000 . #Xf8ff) "icons-in-terminal"))
            ((require 'all-the-icons nil t)
             (set-fontset-font t '(#Xe0a0 . #Xeea0) "all-the-icons")))
      (my-ja-font-setter (font-spec :family ja-font :size font-size))
      (my-ascii-font-setter (font-spec :family ascii-font :size font-size)))))

;;;###autoload
(defun my-setup-font ()
  (cond
   ;; CocoaEmacs
   ((memq window-system '(mac ns))
    (when (>= emacs-major-version 23)

      ;; Fix ratio provided by set-face-attribute for fonts display
      (setq face-font-rescale-alist
            '(("^-apple-hiragino.*" . 1.0) ; 1.2
              (".*Migu.*" . 1.2)
              (".*Ricty.*" . 1.0)
              (".*Inconsolata.*" . 1.0)
              (".*osaka-bold.*" . 1.0)     ; 1.2
              (".*osaka-medium.*" . 1.0)   ; 1.0
              (".*courier-bold-.*-mac-roman" . 1.0) ; 0.9
              ;; (".*monaco cy-bold-.*-mac-cyrillic" . 1.0)
              ;; (".*monaco-bold-.*-mac-roman" . 1.0) ; 0.9
              ("-cdac$" . 1.0))))) ; 1.3
   ;; (my-font-config) ;; see `my-theme'

   ((eq window-system 'ns)
    ;; Anti aliasing with Quartz 2D
    (when (boundp 'mac-allow-anti-aliasing)
      (setq mac-allow-anti-aliasing t)))

   ((eq window-system 'w32) ;; Windows
    (let ((font-size 14)
          (font-height 100)
          (ascii-font "Inconsolata")
          (ja-font "Migu 2M")) ;; Meiryo UI, メイリオ
      (set-fontset-font t '(#Xe000 . #Xf8ff) "icons-in-terminal")
      (my-ja-font-setter
       (font-spec :family ja-font :size font-size :height font-height))
      (my-ascii-font-setter (font-spec :family ascii-font :size font-size))
      (setq face-font-rescale-alist '((".*Inconsolata.*" . 1.0))))) ; 0.9

   ((eq window-system 'x) ; for SuSE Linux 12.1
    (let
        ((font-size 14)
         (font-height 100)
         (ascii-font "Inconsolata")
         ;; (ja-font "MigMix 1M")
         (ja-font "Migu 2M"))
      (set-fontset-font t '(#Xe000 . #Xf8ff) "icons-in-terminal")
      (my-ja-font-setter
       (font-spec :family ja-font :size font-size :height font-height))
      (my-ascii-font-setter (font-spec :family ascii-font :size font-size)))
    (setq face-font-rescale-alist '((".*Migu.*" . 2.0)
                                    (".*MigMix.*" . 2.0)
                                    (".*Inconsolata.*" . 1.0))))) ; 0.9
  )

;;;###autoload
(defun my-linespacing ()
  (unless (minibufferp)
    (setq-local line-spacing 2)))

;;;###autoload
(defun my-hl-todo-activate ()
  (global-hl-todo-mode) ;; FIXME
  (remove-hook 'find-file-hook #'my-hl-todo-activate))

;;;###autoload
(defun my-hl-todo-reload ()
  (interactive)
  (global-hl-todo-mode -1)
  (global-hl-todo-mode))

;;;###autoload
(defun my-hl-todo-light-theme ()
  (setq hl-todo-exclude-modes nil) ;; also apply to a case when org-mode
  (setq hl-todo-keyword-faces
  '(("TODO" . "red1")
    ("DONE" . "ForestGreen")
    ("HOLD" . "#d0bf8f")
    ("NEXT" . "#dca3a3")
    ("THEM" . "#dc8cc3")
    ("PROG" . "#7cb8bb")
    ("OKAY" . "#7cb8bb")
    ("DONT" . "#5f7f5f")
    ("FAIL" . "#8c5353")
    ("DONE" . "SeaGreen")
    ("NOTE"   . "#d0bf8f")
    ("KLUDGE" . "#d0bf8f")
    ("HACK"   . "#d0bf8f")
    ("TEMP"   . "#d0bf8f")
    ("FIXME"  . "#3030FF")
    ("XXX+"   . "#cc9393")
    ("\\?\\?\\?+" . "#cc9393")
    ("" . "orange")
    ("" . "red")
    ("" . "Seagreen3")))
  (my-hl-todo-reload))

;;;###autoload
(defun my-hl-todo-dark-theme ()
  (setq hl-todo-keyword-faces
  '(("TODO" . "red1")
    ("DONE" . "ForestGreen")
    ("HOLD" . "#d0bf8f")
    ("NEXT" . "#dca3a3")
    ("THEM" . "#dc8cc3")
    ("PROG" . "#7cb8bb")
    ("OKAY" . "#7cb8bb")
    ("DONT" . "#5f7f5f")
    ("FAIL" . "#8c5353")
    ("NOTE"   . "#d0bf8f")
    ("KLUDGE" . "#d0bf8f")
    ("HACK"   . "#d0bf8f")
    ("TEMP"   . "#d0bf8f")
    ("FIXME"  . "DodgerBlue1")
    ("XXX+"   . "#cc9393")
    ("\\?\\?\\?+" . "#cc9393")
    ("" . "orange")
    ("" . "red")
    ("" . "Seagreen3")))
  (my-hl-todo-reload))

;; (declare-function my-daylight-theme "init" nil)
;; (declare-function my-night-theme "init" nil)
;; (declare-function my-terminal-theme "init" nil)
(defvar my-light-theme-hook nil)
(defvar my-dark-theme-hook nil)

;;;###autoload
(defun my-terminal-theme ()
  (interactive)
  (when (require 'terminal-theme nil t)
    (mapc 'disable-theme custom-enabled-themes)
    (load-theme 'terminal t)
    (plist-put my-cur-color-ime :on "#FF9300")
    (run-hooks 'my-dark-theme-hook)))

;;;###autoload
(defun my-daylight-theme ()
  (when (require 'daylight-theme nil t)
    (mapc 'disable-theme custom-enabled-themes)
    (load-theme 'daylight t)
    (plist-put my-cur-color-ime :on "#FF9300")
    (setq default-frame-alist
          (delete (assoc 'ns-appearance default-frame-alist)
                  default-frame-alist))
    (setq default-frame-alist
          (delete (assoc 'ns-transparent-titlebar default-frame-alist)
                  default-frame-alist))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . light))
    (modify-frame-parameters nil '((ns-transparent-titlebar . t)
                                   (ns-appearance . light)))
    (run-hooks 'my-light-theme-hook)))

;;;###autoload
(defun my-night-theme ()
  (when (require 'night-theme nil t) ;; atom-one-dark-theme
    (mapc 'disable-theme custom-enabled-themes)
    (load-theme 'night t)
    (plist-put my-cur-color-ime :on "RosyBrown") ;; #cebcfe
    (setq default-frame-alist
          (delete (assoc 'ns-appearance default-frame-alist)
                  default-frame-alist))
    (setq default-frame-alist
          (delete (assoc 'ns-transparent-titlebar default-frame-alist)
                  default-frame-alist))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (modify-frame-parameters nil '((ns-transparent-titlebar . t)
                                   (ns-appearance . dark)))
    (run-hooks 'my-dark-theme-hook)))

(defvar my-frame-appearance nil) ;; {nil, 'dark, 'light}

;;;###autoload
(defun my-theme (&optional type)
  (interactive "MType (light or dark): ")
  (if (display-graphic-p)
      (let ((theme (cond ((member type '("light" "l")) 'light)
                         ((member type '("dark" "d")) 'dark)
                         (t my-frame-appearance))))
        (cond ((eq theme 'dark) (my-night-theme))
              ((eq theme 'light) (my-daylight-theme))
              (t (let ((night-time-in 22)
                       (night-time-out 5))
                   (if (my-night-time-p
                        (* night-time-in 60) (* night-time-out 60))
                       (my-night-theme)
                     (my-daylight-theme))))))
    (my-terminal-theme))

  (unless noninteractive
    ;; remove unintentional colored frame border
    (select-frame-set-input-focus (selected-frame))
    (my-font-config (when (featurep 'moom-font) moom-font--size))
    (my-apply-cursor-config)
    ;; (when type
    ;;   (moom-move-frame-to-edge-top)
    ;;   (moom-fill-height))
    ))

;;;###autoload
(defun my-night-time-p (begin end)
  (let* ((ch (string-to-number (format-time-string "%H" (current-time))))
         (cm (string-to-number (format-time-string "%M" (current-time))))
         (ct (+ cm (* 60 ch))))
    (if (> begin end)
        (or (<= begin ct) (<= ct end))
      (and (<= begin ct) (<= ct end)))))

;;;###autoload
(defun my-update-theme-timers () ;; FIXME: it makes frame blink
  (my-theme)
  (dolist (triger '(2 6 10 14 18 22))
    (let ((tm (format "%02d:00" triger)))
      (when (future-time-p tm)
        (run-at-time tm nil #'my-theme)))))

;;;###autoload
(defun my-ivy-format-function-arrow-ni (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
      ;; nf-fa-hand-point_right, nf-fa-hand_o_right
     (concat (nerd-icons-faicon
              "nf-fa-hand_o_right"
              :v-adjust -0.1
              :face 'my-ivy-arrow-visible
              :height 0.8)
             " " (ivy--add-face (concat str "\n")
                                'ivy-current-match)))
   (lambda (str)
     (concat (nerd-icons-faicon
              "nf-fa-hand_o_right"
              :v-adjust -0.1
              :face 'my-ivy-arrow-invisible
              :height 0.8)
             " " (concat str "\n")))
   cands
   ""))

;;;###autoload
(defun my-ivy-format-function-arrow-iit (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat (icons-in-terminal-faicon
              "hand-o-right"
              :v-adjust -0.1
              :face 'my-ivy-arrow-visible
              :height 0.8)
             " " (ivy--add-face (concat str "\n")
                                'ivy-current-match)))
   (lambda (str)
     (concat (icons-in-terminal-faicon
              "hand-o-right"
              :v-adjust -0.1
              :face 'my-ivy-arrow-invisible
              :height 0.8)
             " " (concat str "\n")))
   cands
   ""))

;;;###autoload
(defun my-ivy-format-function-arrow-ati (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat (all-the-icons-faicon
              "hand-o-right"
              :v-adjust -0.2 :face 'my-ivy-arrow-visible)
             " " (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat (all-the-icons-faicon
              "hand-o-right" :face 'my-ivy-arrow-invisible)
             " " str))
   cands
   "\n"))

;; ふわっとエフェクトの追加（ペースト時の色 => カーソル色 => 本来色）
;;;###autoload
(defun my-vhl-change-color ()
  (interactive)
  (when (boundp 'vhl/.hl-lst)
    (let ((next 0.2)
          (reset 0.5)
          (colors '("#F8D3D7" "#F2DAE1" "#EBE0EB" "#E5E7F5" "#DEEDFF")))
      (dolist (color colors)
        (run-at-time next nil
                     'set-face-attribute
                     'vhl/default-face
                     nil :foreground "#FF3333" :background color)
        (setq next (+ 0.05 next)))
      (run-at-time reset nil 'vhl/clear-all))
    (set-face-attribute 'vhl/default-face
                        nil :foreground "#FF3333"
                        :background "#FFCDCD")))

;;;###autoload
(defun my-yank (&optional ARG)
  (interactive)
  (yank ARG)
  (when window-system
    (my-vhl-change-color)))

;;;###autoload
(defun my-org-yank ()
  (interactive)
  (org-yank)
  (when window-system
    (my-vhl-change-color)))

;;;###autoload
(defun my--vhl-activate (&optional arg)
  (require 'volatile-highlights nil t) ;; will take 40-50[ms]
  (advice-remove 'kill-region
                 #'vhl/.advice-callback-fn/.make-vhl-on-kill-region)
  (advice-remove 'my-yank #'my--vhl-activate)
  (advice-remove 'my-org-yank #'my--vhl-activate)
  arg)

;;;###autoload
(defun my-find-missing-packages (&optional defer)
  (interactive)
  (my-async-locate-libraries my-required-libraries (or defer 0)))

;;;###autoload
(defun my-async-locate-libraries (libraries &optional defer)
  "Check the library listed in `LIBRARIES'."
  (if (require 'async nil t)
      (async-start
       `(lambda ()
          (sleep-for (or ',defer 10))
          (setq load-path ',load-path)
          (let ((alist nil))
            (mapc (lambda (library)
                    (let ((path (locate-library library)))
                      (unless path
                        (add-to-list 'alist (format "%s" library)))))
                  (if (listp ',libraries)
                      ',libraries
                    (list ',libraries)))
            alist))
       (lambda (result)
         (let ((inhibit-message nil)
               (message-log-max 5000))
           (when result
             (unless (active-minibuffer-window)
               (let ((count 0))
                 (dolist (r result)
                   (setq count (1+ count))
                   (message ">> %s (missing)" r))
                 (message (concat (format "[async] %s package" count)
                                  (if (> count 1) "s are" " is")
                                  " NOT installed."))))))))
    (error "missing async.el")))

;;;###autoload
(defun my-delete-old-backup (&optional defer)
  (if (not (require 'async nil t)) ;; 5[ms]
      (recursive-delete-backup-files 7)
    (async-start ;; do not call this from byte compiled code directory
     `(lambda ()
        (sleep-for (or ',defer 5))
        (when (load (concat (getenv "HOME") "/.emacs") t)
          (setq load-path ',load-path)
          (require 'init-autoloads)
          (recursive-delete-backup-files 7)
          t))
     (lambda (result)
       (if result
           (let ((inhibit-message nil)
                 (message-log-max 5000))
             (unless (eval '(active-minibuffer-window))
               (message "[async] Deleting old backup files...done")))
         (error "[async] Failed to delete backup files."))))))

;;;###autoload
(defun my-google-this ()
  (interactive)
  (google-this (current-word) t))

;;;###autoload
(defun my-gif-screencast-opendir-dired ()
  "Open directories for screenshots and generated GIFs by Dired."
  (interactive)
  (dired gif-screencast-output-directory)
  (dired gif-screencast-screenshot-directory))

;;;###autoload
(defun my--gif-screencast ()
  (dolist (hook gif-screencast-additional-normal-hooks)
    (add-hook hook #'gif-screencast-capture)))

;;;###autoload
(defun my--gif-screencast-stop ()
  (dolist (hook gif-screencast-additional-normal-hooks)
    (remove-hook hook 'gif-screencast-capture)))

;;;###autoload
(defun my--gif-screencast-opendir ()
  "Open the output directory when screencast is finished."
  (if (not (eq system-type 'darwin))
      (my-gif-screencast-opendir-dired)
    (shell-command-to-string
     (concat "open " gif-screencast-screenshot-directory))
    (shell-command-to-string
     (concat "open " gif-screencast-output-directory))))

;;;###autoload
(defun my--gif-screencast-toggle-pause ()
  (if (memq 'gif-screencast-capture (default-value 'pre-command-hook))
      (dolist (hook gif-screencast-additional-normal-hooks)
        (remove-hook hook 'gif-screencast-capture))
    (dolist (hook gif-screencast-additional-normal-hooks)
      (add-hook hook #'gif-screencast-capture))))

;;;###autoload
(defun my-nocand-then-fzf-reset ()
  (setq my--nocand-then-fzf t))

;;;###autoload
(defun my-nocand-then-fzf (prompt)
  (when (= ivy--length 0)
    (if (eq (read-char prompt) ?y) ;; y-or-n-p is not applicable
        (ivy-exit-with-action
         (lambda (_x)
           (counsel-fzf ivy-text default-directory)))
      (setq my--nocand-then-fzf nil))))

;;;###autoload
(defun my--fzf:ivy--insert-prompt ()
  (when (and my--nocand-then-fzf
             (memq (ivy-state-caller ivy-last) my-nocand-then-fzf-commands)
             (= ivy--length 0))
    (let* ((std-props
            '(front-sticky t rear-nonsticky t field t read-only t))
           (prompt (concat (my-pre-prompt-function)
                           "Switch to Counsel-fzf? [y/n] ")))
      (set-text-properties 0 (length prompt)
                           `(face minibuffer-prompt ,@std-props) prompt)
      (run-with-idle-timer my-nocand-then-fzf-idle-time
                           nil #'my-nocand-then-fzf prompt))))

;; https://en.wikipedia.org/wiki/Darwin_(operating_system)
;;;###autoload
(defun macos-name (version)
  "Return macOS name according to the VERSION number."
  (if (stringp version)
      (cond ((version<= "23.0" version) "Sonoma")
            ((version<= "22.0" version) "Ventura")
            ((version<= "21.0" version) "Monterey")
            ((version<= "20.0" version) "Big Sur")
            ((version<= "19.0" version) "Catalina")
            ((version<= "18.0" version) "Mojave")
            ((version<= "17.0" version) "High Sierra")
            ((version<= "16.0" version) "Sierra")
            ((version<= "15.0" version) "El Capitan")
            ((version<= "14.0" version) "Yosemite")
            ((version<= "13.0" version) "Mavericks")
            ((version<= "12.0" version) "Mountain Lion")
            ((version<= "11.0" version) "Lion")
            ((version<= "10.0" version) "Snow Leopard")
            ((version<= "9.0" version) "Leopard")
            ((version<= "8.0" version) "Tiger")
            ((version<= "7.0" version) "Panther")
            (t "undefined"))
    nil))

;;;###autoload
(defun macos-version ()
  (let ((macos-type-version (nth 2 (split-string system-configuration "-"))))
    (string-match "darwin\\(.*\\)" macos-type-version)
    (match-string 1 macos-type-version)))

;;;###autoload
(defun my-cmd-to-open-iterm2 (&optional arg)
  (interactive "P")
  (shell-command-to-string
   (concat "open -a iTerm.app "
           (when arg default-directory))))

(defvar my-kyoko-mad-mode nil)
;;;###autoload
(defun my-kyoko-mad-mode-toggle ()
  (interactive)
  (setq my-kyoko-mad-mode (not my-kyoko-mad-mode))
  (message (concat "Kyoko mad mode: "
                   (if my-kyoko-mad-mode "ON" "OFF"))))
;;;###autoload
(defun my-kyoko-mad ()
  (interactive)
  (when my-kyoko-mad-mode
    (shell-command-to-string
     "say -v Kyoko おいおまえ，遊んでないで，仕事しろ")))

;; She will be mad if you do nothing within 10 min.
(run-with-idle-timer 600 t 'my-kyoko-mad)

(defcustom open-current-directory-console-program "iTerm2.app"
  "Specify a console program"
  :type 'string
  :group 'takaxp-mac)

;;;###autoload
(defun my-open-current-directory-in-terminal ()
  " Open Current Directory for macOS
  0) Put this function in your .emacs
  1) M-x open-current-directory
  2) Terminal will open automatically
  3) Type M-v to paste and move to a path to the current directory in Emacs"
  (interactive)
  (let ((file-path (buffer-file-name (current-buffer))))
    (unless (string= file-path nil)
      (let ((directory
             (substring file-path 0
                        (-
                         (length file-path)
                         (length (buffer-name (current-buffer)))))))
        (message "%s" directory)
        (shell-command-to-string (concat "echo cd " directory " |pbcopy"))
        (shell-command-to-string
         (concat "open -a " open-current-directory-console-program))))))

;;;###autoload
(defun my-update-alarms-from-file ()
  (interactive)
  (let ((bname (buffer-name)))
    (when (string= bname "daily.org")
      (my-set-alarms-from-file (concat "~/Dropbox/org/db/" bname)))))

(defun my-set-alarms-from-file (file)
  "Make alarms from org-mode tables. If you have an org-mode file
     with tables with the following format:
     |------+-------+--------------------|
     | Flag |  Time | Content            |
     |------+-------+--------------------|
     |      | 07:00 | Wakeup             |
     |      |       | Read papers        |
     | X    | 12:00 | Clean up your desk |
     When it is 7:00 and 12:00, Growl notify with a message which is specified
     content column from the table. \"Read papers\" will be ignored.
     \"Clean up your desk\" will be shown by sticky mode"
  (let
      ((lines (read-line file)))
    (cancel-function-timers 'my-desktop-notify) ;; clear existing timers
    (while lines
      (my--set-alarm-from-line (decode-coding-string (car lines) 'utf-8))
      (setq lines (cdr lines)))))

(defun my--set-alarm-from-line (line)
  (let
      ((hour nil)
       (min nil)
       (current-hour nil)
       (current-min nil)
       (action nil))
    (when (string-match "\\([0-2]?[0-9]\\):\\([0-5][0-9]\\)" line)
      (setq hour (substring line (match-beginning 1) (match-end 1)))
      (setq min (substring line (match-beginning 2) (match-end 2)))
      (when (string-match
             "\|\\s-*\\([^\|]+[^ ]\\)\\s-*\|" line (match-end 2))
        (setq action
              (substring line (match-beginning 1) (match-end 1)))))
    (when (and (and hour min) action)
      ;;        (message "[%s:%s] => %s" hour min action)
      (setq current-hour (format-time-string "%H" (current-time)))
      (setq current-min (format-time-string "%M" (current-time)))
      (when (> (+ (* (string-to-number hour) 60)
                  (string-to-number min))
               (+ (* (string-to-number current-hour) 60)
                  (string-to-number current-min)))
        (let ((s nil))
          (when (string-match "^\|\\s-*X\\s-*\|" line)
            (setq s 'sticky))
          (my--set-notify-macos hour min action s))))))

;;;###autoload
(defun my--set-notify-macos (hour min action sticky)
  "`alerter' is required."
  (run-at-time (format "%s:%s" hour min) nil
               'my-desktop-notify
               "macos" "Org Mode" hour min action sticky))

(declare-function my-desktop-notification "init-org")

;;;###autoload
(defun my-desktop-notify (type title hour min action sticky)
  "An interface to `my-desktop-notification'."
  (cond
   ((and (display-graphic-p)
         (string= type "macos"))
    (my-desktop-notification
     title (format "%s:%s %s" hour min action) sticky))))

;;;###autoload
(defun read-line (file)
  "Make a list from a file, which is divided by LF code"
  (with-temp-buffer
    (insert-file-contents-literally file)
    (split-string
     (buffer-string) "\n" t)))

(defvar my-file-ring nil)

;;;###autoload
(defun my-make-file-ring (files)
  (setq my-file-ring (copy-sequence files)))
;;    (setf (cdr (last my-file-ring)) my-file-ring))
(my-make-file-ring
 '("~/Dropbox/org/tr/work.org" "~/Dropbox/org/db/daily.org"
   "~/Dropbox/org/minutes/wg1.org" "~/Dropbox/org/tr/work.org"
   "~/Dropbox/org/academic.org" "~/Dropbox/org/org2ja.org"
   "~/Dropbox/org/db/article.org" "~/Dropbox/emacs.d/config/init.org"))

;;;###autoload
(defun my-open-file-ring ()
  (interactive)
  (find-file (car my-file-ring))
  (setq my-file-ring
        (append (cdr my-file-ring)
                (list (car my-file-ring)))))

;;    (setq my-file-ring (cdr my-file-ring)))

;;;###autoload
(defun my-show-org-buffer (file)
  "Show an org-file on the current buffer."
  (interactive)
  (let ((tbuffer (get-buffer file))
        (cbuffer (current-buffer))
        (orgfile (concat (getenv "SYNCROOT") "/org/" file))
        (afile (expand-file-name file))
        ;; (message-log-max nil)
        )
    (when (and (fboundp 'my-org-agenda-to-appt)
               (not (eq cbuffer tbuffer)))
      (my-org-agenda-to-appt 'force))
    (if (cond (tbuffer (switch-to-buffer tbuffer))
              ((file-exists-p orgfile) (find-file orgfile))
              ((file-exists-p afile) (find-file afile)))
          (message "%s" file)
      (message "No buffer or file is shown."))))

(declare-function org-end-of-line "org")

;;;###autoload
(defun insert-org-file-header-template ()
  (interactive)
  (when (string= major-mode 'org-mode)
    (let ((title "#+title:\t\n")
          (date "#+date: \t\n")
          (author "#+author:\tTakaaki ISHIKAWA <takaxp@ieee.org>\n")
          (option "#+options:\t\\n:t\n")
          (other "\n"))
      (goto-char 0)
      (save-excursion
        (insert title date author option other))
      (when (require 'org nil t)
        (org-end-of-line)))))

;;;###autoload
(defun my-insert-empty-pgp-tree ()
  (interactive)
  (insert "** TODO hoge\n")
  (insert "-----BEGIN PGP MESSAGE-----\n\n-----END PGP MESSAGE-----\n")
  (forward-line -2))

;;;###autoload
(defun my-insert-enc2me-pgp-tree ()
  (interactive)
  (insert "** TODO share with me\n")
  (insert "   :PROPERTIES:\n")
  (insert "   :CRYPTKEY: takaxp@ieee.org\n")
  (insert "   :END:\n")
  (insert "\n")
  (forward-line -1))

;;;###autoload
(defun insert-minutes-template ()
  (interactive)
  (when (string= major-mode 'org-mode)
    (let ((date "日時：\n")
          (place "場所：\n")
          (attendance "出席者：\n")
          (documents "資料：\n\n"))
      (save-excursion
        (insert date place attendance documents)))))

;;;###autoload
(defun my-get-random-string (length)
  "Get a string contain the length digit number with random selection"
  (interactive)
  (random t)
  (cond ((> length 0)
         (let
             ((count length)
              (string nil)
              (tmp nil))
           (while (< 0 count)
             (setq count (1- count))
             (setq tmp string)
             (setq string
                   (concat tmp (number-to-string (random 10)))))
           (message "%s" string)))
        (t "0")))

(when (autoload-if-found '(browse-url)
                         "browse-url" nil t)
  (with-eval-after-load "browse-url"
    (cond
     ((eq window-system 'ns)
      (custom-set-variables
       '(browse-url-generic-program 'google-chrome)))
     ((eq window-system 'mac)
      (custom-set-variables
       '(browse-url-browser-function 'browse-url-generic)
       '(browse-url-generic-program
         "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
       ))
     (t
      nil))))
;;(setq browse-url-browser-function 'browse-url-default-macosx-browser)
;;(setq browse-url-browser-function 'browse-url-default-windows-browser)
;;(setq browse-url-browser-function 'browse-url-chrome)

;; find ~/.emacs.d/backup  -type f -name '*15-04-24_*' -print0 | while read -r -d '' file; do echo -n " \"$file\""; done | xargs -0

;;;###autoload
(defun recursive-delete-backup-files (days)
  (if (= days 1)
      1
    (recursive-delete-backup-files (1- days)))
  (delete-backup-files days))

;;;###autoload
(defun delete-backup-files (&optional day-shift)
  "Delete backup files created in yesterday.
  > find ~/.emacs.d/backup -type f -name '*YY-MM-DD_*' -print0 | xargs -0"
  (interactive)
  (unless day-shift
    (setq day-shift 1))
  (let* ((backup-dir "~/.emacs.d/backup")
         (cmd (concat "find " backup-dir "  -type f -name \'*"
                      (format-time-string
                       "%y-%m-%d_"
                       (time-subtract (current-time)
                                      (seconds-to-time
                                       (* day-shift (* 24 3600)))))
                      "*\' -print0 | while read -r -d \'\' file; "
                      " do echo -n \" \\\"$file\\\"\"; done | xargs -0"))
         (files (shell-command-to-string cmd)))
    ;; (message "%s" cmd)
    (unless (string= (chomp files) "")
      (message "%s" (chomp files))
      (let ((trash (if (eq system-type 'darwin)
                       " ~/.Trash" "~/.local/share/Trash")))
        (shell-command-to-string (concat "mv -v " (chomp files) trash))))))

;;;###autoload
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

;;;###autoload
(defun my-backup (files &optional dropbox)
  "Backup a file to `Dropbox/backup' directory.
If `dropbox' option is provided then the value is uased as a root directory."
  (interactive "P")
  (let ((dir (concat (expand-file-name (or dropbox (getenv "SYNCROOT"))) "/backup/" (system-name))))
    (if (file-directory-p dir)
        (mapc
         (lambda (file)
           (if (and (stringp file)
                    (file-readable-p (or file (expand-file-name file))))
               (shell-command-to-string
                (concat "cp -f " file " " dir "/"))
             (warn (format "--- backup failure: %s" file))))
         (if (listp files)
             files
           (list files)))
      (user-error (format "--- backup-dir does not exist: %s" dir)))))

;;;###autoload
(defun mac:delete-files-in-trash-bin ()
  (interactive)
  (do-applescript
   (concat
    "tell application \"Finder\"\n"
    "set itemCount to count of items in the trash\n"
    "if itemCount > 0 then\n"
    "empty the trash\n"
    "end if\n"
    "end tell\n"))
  (my-desktop-notification "Emacs" "Empty the trash, done."))

;;;###autoload
(defun my-kill-emacs ()
    (switch-to-buffer "*Messages*")
    (message "3: %s" kill-emacs-hook)
    (y-or-n-p "..."))

;;;###autoload
(defun my-kill-emacs-hook-show ()
  "Test Emacs killing sequence."
  (add-hook 'after-init-hook
            (lambda () (message "1: %s" kill-emacs-hook)) t)
  (with-eval-after-load "postpone"
    (message "2: %s" kill-emacs-hook))
  (add-hook 'kill-emacs-hook #'my-kill-emacs))

;;;###autoload
(defun my-setup-package-el ()
  "Setting up for installing packages via built-in package.el.
Downloaded packages will be stored under ~/.eamcs.d/elpa."
  (when (and (require 'package nil t)
             (boundp 'package-archives))
    (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                        (not (gnutls-available-p))))
           (proto (if no-ssl "http" "https")))
      (add-to-list 'package-archives
                   (cons "melpa" (concat proto "://melpa.org/packages/")) t)
      (add-to-list 'package-archives
                   (cons "takaxp" "~/devel/git/melpa/packages/") t))
    (package-initialize)))

(declare-function org-babel-tangle "org-babel")

;;;###autoload
(defun my-eval-org-buffer ()
  "Load init.org/utility.org and tangle init.el/utility.el."
  (interactive)
  (if (and (require 'org nil t)
           (eq major-mode 'org-mode)
           (member (buffer-name) '("init.org" "utility.org")))
      (progn
        (org-babel-tangle)
        (let ((tangled-file
               (concat (file-name-sans-extension (buffer-file-name)) ".el")))
          (when (file-exists-p tangled-file)
            (byte-compile-file tangled-file))))
    (message "Nothing to do for this buffer.")))

(defvar my-org-bullet-re
  "\\(^[ \t]*[-\\+\\*][ \t]\\|^[ \t]*[a-z0-9A-Z]*[\\.)][ \t]\\)")

(defvar my-org-bullet-with-checkbox-re
  (concat my-org-bullet-re "\\[.\\][ \t]+"))

;;;###autoload
(defun my-org-insert-bullet (begin end)
  (interactive "r")
  (unless mark-active
    (setq begin (line-beginning-position))
    (setq end (line-end-position)))
  (let* ((bullet "- ")
         (len (string-width bullet)))
    (goto-char begin)
    (while (and (re-search-forward (concat "\\(^[ \t]*\\)") end t)
                (not (looking-at "[-\\+\\*][ \t]\\|[a-z0-9A-Z]*[\\.)][ \t]"))
                (not (equal (point) end)))
      (replace-match (concat "\\1" bullet) nil nil)
      (setq end (+ end len)))
    (goto-char begin)))

;;;###autoload
(defun my-org-delete-bullet (begin end)
  (interactive "r")
  (unless mark-active
    (setq begin (line-beginning-position))
    (setq end (line-end-position)))
  (goto-char begin)
  (while (and (re-search-forward
               "^[ \t]*\\([-\\+\\*][ \t]\\|[a-z0-9A-Z]*[\\.)][ \t]\\)" end t)
              (not (looking-at "\\[.\\][ \t]+")))
    (let ((len (- (match-end 0) (match-beginning 0))))
      (replace-match "" nil nil)
      (setq end (- end len))))
  (goto-char begin))

;;;###autoload
(defun my-org-toggle-checkbox (begin end)
  (interactive "r")
  (unless mark-active
    (setq begin (line-beginning-position))
    (setq end (line-end-position)))
  (goto-char begin)
  (if (re-search-forward
       my-org-bullet-with-checkbox-re (point-at-eol) t)
      (my-org-delete-checkbox-from-bullet begin end)
    (my-org-insert-checkbox-into-bullet begin end)))

;;;###autoload
(defun my-org-insert-checkbox-into-bullet (begin end)
  (interactive "r")
  (unless mark-active
    (setq begin (line-beginning-position))
    (setq end (line-end-position)))
  (let* ((checkbox "[ ] ")
         (len (string-width checkbox)))
    (goto-char begin)
    (while (and (re-search-forward my-org-bullet-re end t)
                (not (looking-at "\\[.\\][ \t]+")))
      (replace-match (concat "\\1" checkbox) nil nil)
      (setq end (+ end len)))
    (goto-char begin)))

;;;###autoload
(defun my-org-delete-checkbox-from-bullet (begin end)
  (interactive "r")
  (unless mark-active
    (setq begin (line-beginning-position))
    (setq end (line-end-position)))
  (let ((len (string-width "[ ] ")))
    (goto-char begin)
    (while (re-search-forward my-org-bullet-with-checkbox-re end t)
      (replace-match "\\1" nil nil)
      (setq end (- end len)))
    (goto-char begin)))

;;;###autoload
(defun my-org-insert-bullet-and-checkbox (begin end)
  (interactive "r")
  (unless mark-active
    (setq begin (line-beginning-position))
    (setq end (line-end-position)))
  (let* ((bullet "- ")
         (checkbox "[ ] ")
         (blen (string-width bullet))
         (clen (string-width checkbox)))
    (goto-char begin)
    (while (and (re-search-forward (concat "\\(^[ \t]*\\)") end t)
                (not (looking-at "[-\\+\\*][ \t]\\|[a-z0-9A-Z]*[\\.)][ \t]"))
                (not (equal (point) end)))
      (replace-match (concat "\\1" bullet checkbox) nil nil)
      (setq end (+ end blen clen)))
    (goto-char begin)))

;;;###autoload
(defun my-org-delete-bullet-and-checkbox (begin end)
  (interactive "r")
  (unless mark-active
    (setq begin (line-beginning-position))
    (setq end (line-end-position)))
  (goto-char begin)
  (while (re-search-forward my-org-bullet-with-checkbox-re end t)
    (let ((len (- (match-end 0) (match-beginning 0))))
      (replace-match "" nil nil)
      (setq end (- end len))))
  (goto-char begin))

;;;###autoload
(defun my-cycle-bullet-at-heading (arg)
  "Add a bullet of \" - \" if the line is NOT a bullet line."
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (let ((bullet "- ")
          (point-at-eol (point-at-eol)))
      (cond
       ((re-search-forward
         my-org-bullet-with-checkbox-re point-at-eol t)
        (replace-match (if arg "" "\\1") nil nil))
       ((re-search-forward
         "\\(^[ \t]*[-\\+\\*][ \t]\\|^[ \t]*[a-z0-9A-Z]*[\\.)][ \t]\\)"
         point-at-eol t)
        (replace-match (if arg "" (concat "\\1[ ] ")) nil nil))
       ((re-search-forward
         (concat "\\(^[ \t]*\\)") point-at-eol t)
        (replace-match (concat "\\1" bullet) nil nil))
       (t nil)))))

;;;###autoload
(defun my-replace-punctuation-to-normal ()
  (interactive)
  (my--replace-punctuation 'normal))

;;;###autoload
(defun my-replace-punctuation-to-scientific ()
  (interactive)
  (my--replace-punctuation 'scientific))

(defun my--replace-punctuation (to)
  (let ((pos (point))
        (source (cond ((eq to 'normal) "\\(，\\)\\|\\(．\\)")
                      ((eq to 'scientific) "\\(、\\)\\|\\(。\\)"))))
    (if (not source)
        (error "Target punctuation is wrong")
      (goto-char (point-min))
      (while (re-search-forward source nil :noerror)
        (let ((w (match-string-no-properties 0)))
          (cond ((equal w "，") (replace-match "、"))
                ((equal w "．") (replace-match "。"))
                ((equal w "、") (replace-match "，"))
                ((equal w "。") (replace-match "．")))))
      (goto-char pos))))

(defvar my-garbage-collect-height max-mini-window-height)

;;;###autoload
(defun my-garbage-collect-activate ()
  (setq max-mini-window-height 16)
  (add-hook 'pre-command-hook #'my-garbage-collect-deactivate))

;;;###autoload
(defun my-garbage-collect-deactivate ()
  (setq max-mini-window-height my-garbage-collect-height)
  (remove-hook 'pre-command-hook #'my-garbage-collect-deactivate))

;;;###autoload
(defun my-garbage-collect ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (my-garbage-collect-activate)
  (message
   (concat
    (format "\n%-12s\t%-6s + %-6s = %s\n" "type" "used" "free" "total")
    (make-string (frame-width) ?-)
    (cl-loop
     for (type size used free) in (garbage-collect)
     for used1 = (* used size)
     for free1 = (* (or free 0) size)
     for total = (file-size-human-readable (+ used1 free1))
     for used2 = (file-size-human-readable used1)
     for free2 = (file-size-human-readable free1)
     concat
     (format "\n%-12s\t%-6s + %-6s = %s" type used2 free2 total)))))

;;; Test function from GNU Emacs (O'REILLY, P.328)
;;;###autoload
(defun count-words-buffer ()
  "Count the number of words in the current buffer"
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (forward-word 1)
        (setq count (1+ count)))
      (message "buffer contains %d words." count))))

;;; Test function for AppleScript
;;; Cite: http://sakito.jp/emacs/emacsobjectivec.html
;;;###autoload
(defun do-test-applescript ()
  (interactive)
  (do-applescript
   (format
    (concat
     "display dialog \"Hello world!\" \r"))))

;;;###autoload
(defun describe-timer ()
  "see http://masutaka.net/chalow/2009-12-05-1.html"
  (interactive)
  (pop-to-buffer (get-buffer-create "*timer*"))
  (view-mode -1)
  (erase-buffer)
  (insert
   (concat "TIME           FUNCTION (" (format "%s " (length timer-list)) "timers)\n")
   "-------------- ----------------------\n")
  (dolist (timer timer-list)
    (insert
     (concat
      (format-time-string "%m/%d %T"
                          (list (aref timer 1)
                                (aref timer 2)
                                (aref timer 3)))
      " "
      (let ((name (aref timer 5)))
        (if (symbolp name)
            (symbol-name name)
          "...undefined..."))
      "\n")))
  (view-mode 1))

;; (defun insert-formatted-current-date (arg)
;;   "Insert a timestamp at the cursor position. C-u will add [] brackets."
;;   (interactive "p")
;;   (cl-case
;;       (4 (if (equal major-mode 'org-mode)
;;              (org-time-stamp-inactive)
;;            (insert (format-time-string "[%Y-%m-%d]"))))
;;     (t (insert (format-time-string "%Y-%m-%d")))))

;;;###autoload
(defun insert-formatted-current-date ()
  "Insert a timestamp at the cursor position."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;;;###autoload
(defun insert-formatted-current-time ()
  (interactive)
  (insert (format-time-string "%H:%M")))

;;;###autoload
(defun insert-formatted-signature ()
  (interactive)
  (insert (concat (format-time-string "%Y-%m-%d") "  " user-full-name
                  "  <" user-mail-address ">")))

;;;###autoload
(defun org2dokuwiki-cp-kill-ring ()
  "Convert the current org-file to dokuwiki text, and copy it to kill-ring."
  (interactive)
  (when (eq major-mode 'org-mode)
    (cond (buffer-file-name
           (kill-new
            (shell-command-to-string
             (concat "cat " buffer-file-name "| perl "
                     (expand-file-name "~/Dropbox/local/scripts/org2dokuwiki.pl"))))
           (minibuffer-message "Copying %s ... done" buffer-file-name))
          (t (message "There is NOT such a file.")))))

;;;###autoload
(defun my-kill-all-file-buffers ()
  "Kill all buffers visiting files."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (or (and (buffer-live-p buffer)
                   (buffer-file-name buffer))
              (and (switch-to-buffer buffer)
                   (eq major-mode 'dired-mode)
                   (file-directory-p (dired-current-directory))))
      (kill-buffer buffer)))
  (delete-windows-on)
  (scratch-buffer)
  (message "Quit Emacs? (C-c C-x)"))

;;;###autoload
(defun my-kill-emacs-when-scratch-buffer ()
  (interactive)
  (when (equal "*scratch*" (buffer-name))
    (save-buffers-kill-emacs)))

;;;###autoload
(defun my--format-emacs-lisp-buffer ()
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (my-emacs-lisp-mode-indent-conf)
    (save-excursion
      (save-restriction
        (widen)
        (untabify (point-min) (point-max))
        (tabify (point-min) (point-max))
        (indent-region (point-min) (point-max))))))

;;;###autoload
(defun my--format-emacs-lisp-for-org-buffer ()
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (my-org-mode-indent-conf)
    (save-excursion
      (save-restriction
        (widen)
        (untabify (point-min) (point-max))
        (indent-region (point-min) (point-max))))))

;;;###autoload
(defun my-window-resizer ()
  "Control separated window size and position.
   Type {j,k,l,m} to adjust windows size."
  (interactive)
  (let (
;;        (window-obj (selected-window))
;;        (current-width (window-width))
;;        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-event (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))

(when nil
  (unless noninteractive
    (let ((inhibit-message t))
      (message "Loading utility.el...done (%4d [ms])"
         (* 1000
      (float-time (time-subtract
             (current-time)
             my-utility-start)))))))
(provide 'utility)
