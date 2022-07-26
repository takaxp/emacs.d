;; utility.el --- My utility.el -*- lexical-binding: t -*-
;; "my-" and "ad:" functions associated with my 'init.el'

;;;###autoload
(defun ad:do-after-load-evaluation (abs-file)
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
      ;; Cribbed from cl--compiling-file.
      (when (or (not (fboundp 'byte-compile-warning-enabled-p))
                (byte-compile-warning-enabled-p 'obsolete package))
        (cond
	       ((and (boundp 'byte-compile--outbuffer)
	             (bufferp (symbol-value 'byte-compile--outbuffer))
	             (equal (buffer-name (symbol-value 'byte-compile--outbuffer))
		                  " *Compiler Output*"))
	        ;; Don't warn about obsolete files using other obsolete files.
	        (unless (and (stringp byte-compile-current-file)
		                   (string-match-p "/obsolete/[^/]*\\'"
				                               (expand-file-name
					                              byte-compile-current-file
					                              byte-compile-root-dir)))
	          (byte-compile-warn "%s" msg)))
         ((and msg
               noninteractive (funcall fun msg))) ;; No timer will be run!
	       (t (when msg
              (run-with-idle-timer 0 nil fun msg)))))))

  ;; Finally, run any other hook.
  (run-hook-with-args 'after-load-functions abs-file))

;;;###autoload
(defun my-emacs-lisp-mode-conf ()
  ;; (setq indent-tabs-mode t)
  ;; (setq tab-width 8)
  (setq indent-line-function 'lisp-indent-line))

;;;###autoload
(defun my-org-hide-drawers-all ()
  (when (eq major-mode 'org-mode)
    (org-cycle-hide-drawers 'all)))

;;;###autoload
(defun my-private-conf-activate ()
  (cancel-timer my-private-conf-timer)
  (when (and (file-exists-p "~/Dropbox/config/private.el.gpg")
             (eq system-type 'darwin)
             (not (boundp 'my-private-conf-loaded)))
    (unless (ignore-errors
              (if shutup-p
                  (shut-up (require 'private "private.el.gpg" t))
                (require 'private "private.el.gpg" t)))
      (warn "GPG decryption error (private.el)")))
  (remove-hook 'find-file-hook #'my-private-conf-activate))

;;;###autoload
(defun ad:epa-file-write-region (f start end file &optional append visit
                                   lockname mustbenew)
  (let ((message-log-max nil))
    (funcall f start end file append visit lockname mustbenew)))

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
    (if (fboundp 'mac-ime-toggle) (mac-ime-deactivate) (my-ime-off) )))

;;;###autoload
(defun ad:mark-sexp (f &optional arg allow-extend)
  "Set mark ARG sexps from point.
When the cursor is at the end of line or before a whitespace, set ARG -1."
  (interactive "P\np")
  (funcall f (if (and (not (bolp))
                      (not (eq (preceding-char) ?\ ))
                      (or (eolp)
                          (eq (following-char) ?\ )
                          (memq (preceding-char) '(?\) ?\> ?\] ?\}))))
                 -1 arg)
           allow-extend))

;;;###autoload
(defun ad:er:mark-sexp (f &optional arg allow-extend)
  "If the cursor is on a symbol, expand the region along the symbol."
  (interactive "P\np")
  (if (and (not (use-region-p))
           (symbol-at-point))
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
(defun ad:bm-show-mode ()
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
(defun ad:smart-mark-restore-cursor ()
  "Restore cursor position saved just before mark."
  (when smart-mark-point-before-mark
    (when (> smart-mark-point-before-mark 1)
      ;; To avoid to jump to the beginning of the buffer
      (goto-char smart-mark-point-before-mark))
    (setq smart-mark-point-before-mark nil)))

;;;###autoload
(defun ad:smart-mark-set-restore-before-mark (&rest _arg)
  (unless (memq this-command
                '(er/expand-region er/mark-symbol er/contract-region))
    (setq smart-mark-point-before-mark (point))))

;;;###autoload
(defun ad:er:keyboard-quit ()
  (when (memq last-command '(er/expand-region er/contract-region))
    (when smart-mark-point-before-mark
      (goto-char smart-mark-point-before-mark))))

;;;###autoload
(defun ad:er:pre:keyboard-quit ()
  (when (memq last-command '(er/expand-region er/contract-region))
    (er/contract-region 0)
    ;; (when (> smart-mark-point-before-mark 1) ;; FIXME
    ;;   (goto-char smart-mark-point-before-mark))
    ))

;;;###autoload
(defun my-syntax-subword-activate ()
  (global-syntax-subword-mode 1)  
  (remove-hook 'find-file-hook #'my-syntax-subword-activate))

;;;###autoload
(defun my-time-stamp ()
  (setq time-stamp-format
        (if (eq major-mode 'org-mode)
            "[%Y-%02m-%02d %3a %02H:%02M]" ;; "%04y"
          "%Y-%02m-%02d"))
  (if (boundp 'org-tree-slide-mode)
      (unless org-tree-slide-mode
        (time-stamp))
    (time-stamp)))

;;;###autoload
(defun my-orgalist-activate ()
  (when (require 'orgalist nil t)
    (orgalist-mode 1))) ;; originally orgstruct-mode

;;;###autoload
(defun ad:add-change-log-entry-other-window ()
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
(defun ad:view--enable () (my-mode-line-on))

;;;###autoload
(defun ad:view--disable () (my-mode-line-off))

;;;###autoload
(defun my-web-indent-fold ()
  (interactive)
  (web-mode-fold-or-unfold)
  (web-mode-buffer-indent)
  (indent-for-tab-command))

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
(defun ad:YaTeX-insert-begin-end (env region-mode)
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
    (goto-char (max (mark) (point)))
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
(defun my-mode-line-vc-mode-icon ()
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
(defun ad:split-window-below (&optional _size)
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
  (remove-hook 'find-file-hook #'my-mic-paren-activate))

;;;###autoload
(defun ad:mic-paren-highlight (f)
  (if (active-minibuffer-window)
      (let ((paren-display-message 'never))
        (funcall f)
        paren-display-message)
    (funcall f)))

;;;###autoload
(defun ad:font-lock-mode (&optional _ARG)
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
(defun my-mode-line-icon-lock ()
  (if view-mode
      (concat (icons-in-terminal-faicon
               "lock" :face '(:foreground "#FF0000")) " ") ""))

;;;###autoload
(defun my-mode-line-icon-for-file ()
  (icons-in-terminal-icon-for-file
   (buffer-name) :v-adjust 0.03 :face 'mode-line-file-icon-face))

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
  (when (and (locate-library "migemo");; overhead but should be checked here
	     (executable-find "cmigemo")
	     (require 'migemo nil t))
    (add-hook 'isearch-mode-hook #'migemo-init))
  (remove-hook 'find-file-hook #'my-migemo-activate))

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
(defun my:elisp-eldoc (_callback)
  "Avoid hiding `hl-line' in `emacs-lisp-mode'."
  (when (fboundp 'my-hl-line-enable)
    (my-hl-line-enable)))

;;;###autoload
(defun ad:eldoc-message (f &optional string)
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
(defun my-pre-prompt-function ()
  (cond (window-system
         (format "%s%s "
                 (if my-toggle-modeline-global "" ;; FIXME
                   (concat (make-string (frame-width) ?\x5F) "\n")) ;; "__"
                 (cond ((require 'icons-in-terminal nil t)
                        (icons-in-terminal-material "playlist_add_check"))
                       ((require 'all-the-icons nil t)
                        (all-the-icons-material "playlist_add_check"))
                       (t ""))))
        ;; ((eq system-type 'windows-nt)
        ;;  (format "%s%s "
        ;;          (if my-toggle-modeline-global "" ;; FIXME
        ;;            (concat (make-string (frame-width) ?\x5F) "\n")) ;; "__"
        ;;          ">>"))
        (t
         (format "%s\n" (make-string (1- (frame-width)) ?\x2D)))))

;;;###autoload
(defun my-truncate-lines-activate ()
  "Truncate lines on `imenu-list' buffer."
  (toggle-truncate-lines 1))

;;;###autoload
(defun my-imenu-list-update ()
  "Expand frame width by `moom-change-frame-width'."
  (when (and (memq imenu-list-position '(right left))
             (not (get-buffer-window imenu-list-buffer-name t)))
    (moom-change-frame-width (+ (frame-width) imenu-list-size))))

;;;###autoload
(defun my-imenu-list-quit-window ()
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
(defun ad:swiper-thing-at-point ()
  "`swiper' with `ivy-thing-at-point'."
  (interactive)
  (let ((thing (if (thing-at-point-looking-at "^\\*+") ;; org heading を除外
                   nil
                 (ivy-thing-at-point))))
    (when (use-region-p)
      (deactivate-mark))
    (swiper thing)))

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
(defun ad:dimmer-org-agenda--quit (&optional _bury)
  (when (fboundp 'dimmer-on)
	  (setq my-dimmer-mode t)
	  (dimmer-on)
	  (redraw-frame)))

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
  (eval `(with-ivy-window (find-file ,file))))

;;;###autoload
(defun ad:counsel-recentf ()
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
(defun my-backup-recentf ()
  (interactive)
  (when (require 'utility nil t)
    (my-backup recentf-save-file) ;; "~/.emacs.d/recentf"
    (my-backup (expand-file-name "~/.histfile"))))

;;;###autoload
(defun my-auto-backup ()
  (unless (equal (buffer-name) "recentf")
    (backup-each-save)))

;;;###autoload
(defun my-backup-each-save-compute-location (filename)
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
(defun my-ox-hugo-auto-saving-p ()
  (when (eq major-mode 'org-mode)
    (or (bound-and-true-p org-capture-mode) ;; when activating org-capture
        (and (fboundp 'org-entry-get)
             (equal "" (org-entry-get (point) "EXPORT_FILE_NAME"))))))

;;;###autoload
(defun my-auto-save-buffers ()
  (cond ((memq major-mode '(undo-tree-visualizer-mode diff-mode)) nil)
        ((string-match "Org Src" (buffer-name)) nil)
        ((let ((pt (point)))
           ;; .gpg で半角スペースの後ろのブリッツでは自動保存しない．FIXME 半角スペース
           (when (and (string-match ".gpg" (buffer-name))
                      (not (eq pt 1))
                      (not (eq pt (point-min))))
             (string-match (buffer-substring (- pt 1) pt) " ")))
         nil)
        ((my-ox-hugo-auto-saving-p) nil)
        (t
         (auto-save-buffers))))

;;;###autoload
(defun ad:neotree-show ()
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
(defun ad:neotree-hide ()
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
(defun ad:helpful-at-point ()
  (deactivate-mark))

;;;###autoload
(defun ad:keyfreq-show ()
  "Extension to make the buffer view-only."
  (interactive)
  (if shutup-p
      (shut-up (view-buffer keyfreq-buffer))
    (view-buffer keyfreq-buffer)))

;;;###autoload
(defun ad:counsel-ag (f &optional initial-input initial-directory extra-ag-args ag-prompt caller)
  (apply f (or initial-input
               (and (not (thing-at-point-looking-at "^\\*+"))
                    (ivy-thing-at-point)))
         (unless current-prefix-arg
           (or initial-directory default-directory))
         extra-ag-args ag-prompt caller))

;;;###autoload
(defun ad:counsel-fzf (f &optional initial-input initial-directory fzf-prompt)
  (apply f (or initial-input
               (if (thing-at-point-looking-at "^\\*+") ;; org heading を除外
                   nil
                 (ivy-thing-at-point)))
         (or initial-directory default-directory)
         fzf-prompt))

(eval-when-compile
  (require 'dash))

;;;###autoload
(defun counsel-flycheck-action (obj &rest _)
  (-when-let* ((err (get-text-property 0 'tabulated-list-id obj))
               (pos (flycheck-error-pos err)) )
    (goto-char (flycheck-error-pos err))))

(defvar counsel-flycheck-history nil "History for `counsel-flycheck'")

;;;###autoload
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
              :action 'counsel-flycheck-action ;; (lambda (s &rest _))
              :history 'counsel-flycheck-history
              :caller 'counsel-flycheck)))

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
(defun ad:neotree-dir (path)
  "Extension to change the frame width automatically."
  (interactive "DDirectory: ")
  (unless (neo-global--window-exists-p)
    (neotree-show))
  (neo-global--open-dir path)
  (neo-global--select-window))

;;;###autoload
(defun ad:projectile-visit-project-tags-table ()
  "Extensions to skip calling `visit-tags-table'."
  nil)

;;;###autoload
(defun ad:magit-mode-bury-buffer (&optional _bury)
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
(defun my-company-activate ()
  (remove-hook 'emacs-lisp-mode-hook #'my-company-activate)
  (remove-hook 'org-mode-hook #'my-company-activate)
  (require 'company nil t))

;;;###autoload
(defun ad:company-idle-begin (f buf win tick pos)
  (unless (and (boundp 'ns-put-text-p) ns-put-text-p)
    (funcall f buf win tick pos)))

;;;###autoload
(defun ad:company-pseudo-tooltip--ujofwd-on-timer (f command)
  (unless (and (boundp 'ns-put-text-p) ns-put-text-p)
    (funcall f command)))

;;;###autoload
(defun my-desktop-notification (title message &optional sticky sound timeout)
  "Show a message by `alerter' command."
  (if ns-alerter-command
      (start-process
       "notification" "*notification*"
       ns-alerter-command
       "-title" title
       "-message" message
       "-sender" "org.gnu.Emacs"
       "-timeout" (format "%s" (if sticky 0 (or timeout 7)))
       "-sound" (or sound ns-default-notification-sound))
    (message "--- ns-alerter-command is %s." ns-alerter-command)))

;;;###autoload
(defun my-desktop-notification-handler (message)
  (my-desktop-notification "Message from org-mode" message t))

;;;###autoload
(defun my-org-agenda-prepare-buffers ()
  (unless (featurep 'org-agenda)
    (when (require 'org-agenda nil t)
      (message "Building agenda buffers...")
      (org-agenda-prepare-buffers org-agenda-files)
      (message "Building agenda buffers...done"))))

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
(defun ad:org-recent-headings-activate ()
  (interactive)
  (when (require 'org-recent-headings nil t)
    (org-recent-headings-mode 1) ;; one time activate
    (advice-remove 'org-recent-headings
                   #'ad:org-recent-headings-activate)))

;;;###autoload
(defun my-ime-on ()
  (interactive)
  (if (fboundp 'mac-toggle-input-method)
	    (progn
	      (mac-toggle-input-method t)
	      (run-hooks 'input-method-activate-hook))
	  (activate-input-method default-input-method))
  (setq my-ime-last t))

;;;###autoload
(defun my-ime-off ()
  (interactive)
  (if (fboundp 'mac-toggle-input-method)
	    (progn
	      (mac-toggle-input-method nil)
	      (run-hooks 'input-method-deactivate-hook))
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
(defun ad:make-frame (&optional _parameters)
  (when (display-graphic-p)
    (my-theme)
    ;; (my-apply-cursor-config)
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
(defun ad:moom-toggle-frame-maximized ()
    (when (eq major-mode 'org-mode)
      (org-redisplay-inline-images))
    (when (and mode-line-format
               (not my-toggle-modeline-global))
      (my-mode-line-off)))

;;;###autoload
(defun ad:winner:delete-window (&optional _window)
  (message "Undo? M-x winner-undo or type \"C-x g\""))

;;;###autoload
(defun my-delete-checkdoc-window ()
  (interactive)
  (let ((checkdoc-window (get-buffer-window "*Checkdoc Status*")))
    (when checkdoc-window
      (delete-window checkdoc-window)))
  (checkdoc-minor-mode -1))

;;;###autoload
(defun ad:checkdoc ()
  (interactive)
  (define-key checkdoc-minor-mode-map (kbd "q") 'my-delete-checkdoc-window)
  (define-key checkdoc-minor-mode-map (kbd "C-g") 'my-delete-checkdoc-window)
  (checkdoc-minor-mode 1))

;;;###autoload
(defun ad:doom-modeline-buffer-file-state-icon
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
  (set-fontset-font nil 'mule-unicode-0100-24ff spec))

;;;###autoload
(defun my-ascii-font-setter (spec)
  (set-fontset-font nil 'ascii spec))

;;;###autoload
(defun my-unicode-font-setter (spec)
  (set-fontset-font t 'unicode spec nil 'prepend))

;;;###autoload
(defun my-all-the-icons-setter ()
  (when (require 'icons-in-terminal nil t)
    (my-unicode-font-setter
     (font-spec :family (icons-in-terminal-faicon-family)))
    (my-unicode-font-setter
     (font-spec :family (icons-in-terminal-fileicon-family)))
    (my-unicode-font-setter
     (font-spec :family (icons-in-terminal-material-family)))
    (my-unicode-font-setter
     (font-spec :family (icons-in-terminal-octicon-family)))
    (my-unicode-font-setter
     (font-spec :family (icons-in-terminal-wicon-family)))))

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
      (set-fontset-font t '(#Xe000 . #Xf8ff) "icons-in-terminal")
      ;;(set-fontset-font t '(#Xe0a0 . #Xeea0) "icons-in-terminal")
      (my-ascii-font-setter (font-spec :family ascii-font :size font-size))
      (my-ja-font-setter (font-spec :family ja-font :size font-size)))))

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
      (my-ascii-font-setter (font-spec :family ascii-font :size font-size))
      (my-ja-font-setter
       (font-spec :family ja-font :size font-size :height font-height))
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
(defun my-hl-todo-reload ()
  (interactive)
  (global-hl-todo-mode -1)
  (global-hl-todo-mode))

;;;###autoload
(defun my-hl-todo-light-theme ()
  (setq hl-todo-keyword-faces
        '(("HOLD" . "#d0bf8f")
          ("TODO" . "#FF0000")
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
        '(("HOLD" . "#d0bf8f")
          ("TODO" . "#cc9393")
          ("NEXT" . "#dca3a3")
          ("THEM" . "#dc8cc3")
          ("PROG" . "#7cb8bb")
          ("OKAY" . "#7cb8bb")
          ("DONT" . "#5f7f5f")
          ("FAIL" . "#8c5353")
          ("DONE" . "#afd8af")
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
    (plist-put my-cur-color-ime :on "#FF9300")))

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

(defvar my-frame-appearance nil) ;; {nil, 'dark, 'light} see init-env.el

;;;###autoload
(defun my-theme (&optional type)
  (interactive "MType (light or dark): ")
  (setq my-frame-appearance
        (cond ((member type '("light" "l")) 'light)
              ((member type '("dark" "d")) 'dark)
              (t
               my-frame-appearance)))
  (if (display-graphic-p)
      (cond ((eq my-frame-appearance 'dark)
             (my-night-theme))
            ((eq my-frame-appearance 'light)
             (my-daylight-theme))
            (t
             (let ((night-time-in 23)
                   (night-time-out 5))
               (if (my-night-time-p
                    (* night-time-in 60) (* night-time-out 60))
                   (my-night-theme)
                 (my-daylight-theme)))))
    (my-terminal-theme))

  (unless noninteractive
    ;; remove unintentional colored frame border
    (select-frame-set-input-focus (selected-frame))
    (my-font-config)
    (my-apply-cursor-config)
    (when type
      (moom-move-frame-to-edge-top)
      (moom-fill-height))))

;;;###autoload
(defun my-night-time-p (begin end)
  (let* ((ch (string-to-number (format-time-string "%H" (current-time))))
         (cm (string-to-number (format-time-string "%M" (current-time))))
         (ct (+ cm (* 60 ch))))
    (if (> begin end)
        (or (<= begin ct) (<= ct end))
      (and (<= begin ct) (<= ct end)))))

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
(defun ad:gif-screencast ()
  (dolist (hook gif-screencast-additional-normal-hooks)
    (add-hook hook #'gif-screencast-capture)))

;;;###autoload
(defun ad:gif-screencast-stop ()
  (dolist (hook gif-screencast-additional-normal-hooks)
    (remove-hook hook 'gif-screencast-capture)))

;;;###autoload
(defun ad:gif-screencast-opendir ()
  "Open the output directory when screencast is finished."
  (if (not (eq system-type 'darwin))
      (my-gif-screencast-opendir-dired)
    (shell-command-to-string
     (concat "open " gif-screencast-screenshot-directory))
    (shell-command-to-string
     (concat "open " gif-screencast-output-directory))))

;;;###autoload
(defun ad:gif-screencast-toggle-pause ()
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
(defun ad:fzf:ivy--insert-prompt ()
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
      (cond ((version<= "21.0" version) "Monterey")
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
   ((string= type "macos")
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
  (message "%s" file)
  (let ((tbuffer (get-buffer file))
        (cbuffer (current-buffer)))
    (if tbuffer
        (switch-to-buffer tbuffer)
      (find-file (concat (getenv "SYNCROOT") "/org/" file)))
    (when (and (fboundp 'my-org-agenda-to-appt)
               (not (eq cbuffer tbuffer)))
      (my-org-agenda-to-appt 'force))))

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

(defvar ox-icalendar-activate nil)

;;;###autoload
(defun my-ox-icalendar-activate ()
  (setq ox-icalendar-activate (frame-focus-state)))
(with-eval-after-load "org"
  (when (eq system-type 'ns)
    (run-with-idle-timer 180 t 'my-reload-ical-export)
    ;;    (run-with-idle-timer 1000 t 'org-mobile-push)
    (add-function :after after-focus-change-function
                  #'my-ox-icalendar-activate)))

(declare-function my-ox-upload-icalendar "init.org")
;;;###autoload
(defun my-reload-ical-export ()
  "Export org files as an iCal format file"
  (interactive)
  (when (and (string= major-mode 'org-mode)
             ox-icalendar-activate)
    (my-ox-upload-icalendar)))

(when (autoload-if-found
       '(browse-url)
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
  (let ((system (system-name))
        (rootdir (or dropbox (getenv "SYNCROOT"))))
    (if (and system
             (stringp rootdir)
             (file-directory-p (or rootdir (expand-file-name rootdir))))
        (mapc
         (lambda (file)
           (if (and (stringp file)
                    (file-readable-p (or file (expand-file-name file))))
               (shell-command-to-string
                (concat "cp -f " file " " rootdir "/backup/" system "/"))
             (warn (format "--- backup failure: %s" file))))
         (if (listp files)
             files
           (list files)))
      (user-error (format "--- backup-dir does not exist: %s" rootdir)))))

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
    (y-or-n-p "Sure? "))

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
  (let ((tl timer-list)
        (timer nil))
    (pop-to-buffer (get-buffer-create "*timer*"))
    (erase-buffer)
    (insert
     "TIME           FUNCTION\n"
     "-------------- ----------------------\n")
    (while tl
      (setq timer (car tl))
      (insert
       (concat
        (format-time-string "%m/%d %T"
                            (list (aref timer 1)
                                  (aref timer 2)
                                  (aref timer 3)))
        " "
        (symbol-name (aref timer 5))
        "\n"))
      (setq tl (cdr tl)))
    (read-only-mode 1)))

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
                     (expand-file-name "~/Dropbox/scripts/org2dokuwiki.pl"))))
           (minibuffer-message "Copying %s ... done" buffer-file-name))
          (t (message "There is NOT such a file.")))))

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

(provide 'utility)
