;; init.el --- My init.el -*- lexical-binding: t -*-
;; Configurations for Emacs
;;                                         Takaaki ISHIKAWA  <takaxp@ieee.org>
;; see also https://takaxp.github.io/init.html
(require 'utility-autoloads nil t)

(with-eval-after-load "postpone"
  (require 'late-init nil t)
  (require 'init-org nil t)) ;; loading all with-eval-after-load for Org

(unless noninteractive
  (with-eval-after-load "org"
    (require 'postpone nil t)
    (require 'init-org nil t)))

(defconst my-before-load-init-time (current-time))
;;;###autoload
(defun my-load-init-time ()
  "Loading time of user init files including time for `after-init-hook'."
  (let ((time1 (float-time
                (time-subtract after-init-time my-before-load-init-time)))
        (time2 (float-time
                (time-subtract (current-time) my-before-load-init-time))))
    (message (concat "Loading init files: %.0f [msec], "
                     "of which %.f [msec] for `after-init-hook'.")
             (* 1000 time1) (* 1000 (- time2 time1)))))

(add-hook 'after-init-hook #'my-load-init-time t)

(defvar my-tick-previous-time my-before-load-init-time)
;;;###autoload
(defun my-tick-init-time (msg)
  "Tick boot sequence at loading MSG."
  (when my-loading-profile-p
    (let ((ctime (current-time)))
      (message "---- %5.2f[ms] %s"
               (* 1000 (float-time
                        (time-subtract ctime my-tick-previous-time)))
               msg)
      (setq my-tick-previous-time ctime))))

;;;###autoload
(defun my-emacs-init-time ()
  "Emacs booting time in msec."
  (message "Emacs booting time: %.0f [msec] = `emacs-init-time'."
           (* 1000
              (float-time (time-subtract
                           after-init-time
                           before-init-time)))))

(add-hook 'after-init-hook #'my-emacs-init-time)

(setq gc-cons-threshold (* 128 1024 1024)) ;; 128MB
(setq garbage-collection-messages t)

;; (setq byte-compile-warnings '(obsolete))
;; Suppress warning on cl.el loading
(defvar my-exclude-deprecated-packages '(cl tls))
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
(advice-add 'do-after-load-evaluation :override #'ad:do-after-load-evaluation)

(setq save-silently t) ;; No need shut-up.el for saving files.

;;;###autoload
(defun my-load-package-p (file)
  (let ((enabled t))
    (when (boundp 'my-loading-packages)
      (dolist (package my-loading-packages)
        (let ((name (car package))
              (flag (cdr package)))
          (when (and (stringp name)
                     (equal file name)
                     (not flag))
            (setq enabled nil)
            (message "--- `%s' was NOT loaded intentionally" name)))))
    enabled))

(defvar my-skip-check-autoload-file t)
(when (bound-and-true-p my-loading-packages)
  (setq my-skip-check-autoload-file nil))

;;;###autoload
(defun autoload-if-found (functions file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (when (boundp 'my-required-libraries)
    (add-to-list 'my-required-libraries file))
  (when (or my-skip-check-autoload-file
            (and (my-load-package-p file)
                 (locate-library file)))
    (dolist (f functions)
      (autoload f file docstring interactive type))
    t))

(if (not (locate-library "postpone"))
    (message "postpone.el is NOT installed.")
  (autoload 'postpone-kicker "postpone" nil t)
  (defvar postpone-init-time 0)
  (defun my-postpone-kicker ()
    (interactive)
    (when (eq postpone-init-time 0)
      (unless (memq this-command ;; specify commands for exclusion
                    '(self-insert-command
                      newline
		                  forward-char
		                  backward-char
                      delete-char
		                  delete-backward-char
                      save-buffer
                      save-buffers-kill-terminal
                      electric-newline-and-maybe-indent
                      exit-minibuffer))
        (message "Activating postponed packages...")
        (let ((t1 (current-time)))
          (postpone-kicker 'my-postpone-kicker)
          (setq postpone-init-time (float-time
                                    (time-subtract (current-time) t1))))
        (message "Activating postponed packages...done (%.3f seconds)"
                 postpone-init-time))))

  ;; 起動後，最初のアクションでキック
  (add-hook 'pre-command-hook #'my-postpone-kicker)

  ;; 起動後X秒何もしない場合は自動でキック (related to setting on org-agenda)
  (run-with-idle-timer 8 nil #'my-postpone-kicker))

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
;; (when (future-time-p "10:00") (run-at-time...))

;;;###autoload
(defun library-p (libraries)
  "Return t when every specified library can be located. "
  (let ((result t))
    (mapc (lambda (library)
            (unless (locate-library library)
              (message "--- NOT FOUND: %s" library)
              (setq result nil)))
          (if (listp libraries)
              libraries
            (list libraries)))
    result))

;;;###autoload
(defun my-native-comp-p ()
  (when (fboundp 'native-comp-available-p)
    (native-comp-available-p)))

(my-tick-init-time "startup")

(prefer-coding-system 'utf-8-unix)
;; (set-language-environment "Japanese") ;; will take 20-30[ms]
(set-locale-environment "en_US.UTF-8") ; "ja_JP.UTF-8"
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
(when (eq system-type 'windows-nt)
  (set-clipboard-coding-system 'utf-16le) ;; enable copy-and-paste correctly
  (setq system-time-locale "C")) ;; format-time-string %a, not 日 but Sun

(when (fboundp 'mac-add-key-passed-to-system)
  (setq default-input-method "macOS")
  (mac-add-key-passed-to-system 'shift))

(when (eq system-type 'gnu/linux)
  (global-set-key (kbd "<hiragana-katakana>") 'toggle-input-method)
  (push "/usr/share/emacs/site-lisp/anthy" load-path)
  (push "/usr/share/emacs/site-lisp/emacs-mozc" load-path)
  (set-language-environment "Japanese")

  (if (require 'mozc nil t)
      (progn
        (setq default-input-method "japanese-mozc")
        (custom-set-variables
         '(mozc-candidate-style 'overlay)))

    (when (require 'anthy nil t) ;; sudo yum install emacs-anthy-el
      ;; if get error
      (load-file "/usr/share/emacs/site-lisp/anthy/leim-list.el")
      (setq default-input-method 'japanese-anthy))))

(when (eq system-type 'darwin)
  (when (boundp 'ns-command-modifier)
    (setq ns-command-modifier 'meta))
  (when (boundp 'ns-alternate-modifier)
    (setq ns-alternate-modifier 'super))
  (when (boundp 'ns-pop-up-frames)
    (setq ns-pop-up-frames nil))
  (global-set-key (kbd "M-v") 'yank)
  (global-set-key [ns-drag-file] 'ns-find-file))
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Limit the final word to a line break code (automatically correct)
(setq require-final-newline t)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)

(defun my-emacs-lisp-mode-conf ()
  ;; (setq indent-tabs-mode t)
  ;; (setq tab-width 8)
  (setq indent-line-function 'lisp-indent-line))
(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-conf)

(when (version< "27.0" emacs-version)
  (defun ad:find-file-read-args (f prompt mustmatch)
    (when (equal default-directory "/")
      (setq default-directory "~/"))
    (funcall f prompt mustmatch))
  (advice-add 'find-file-read-args :around #'ad:find-file-read-args))

(my-tick-init-time "core")

(my-tick-init-time "point")

(global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)

(when (autoload-if-found
       '(modern-c++-font-lock-mode)
       "modern-cpp-font-lock" nil t)
  (push '("\\.[hm]$" . c++-mode) auto-mode-alist)
  (add-hook 'c-mode-hook #'modern-c++-font-lock-mode)
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(when (autoload-if-found
       '(yaml-mode)
       "yaml-mode" nil t)

  (push '("\\.yml$" . yaml-mode) auto-mode-alist))

(when (autoload-if-found
       '(json-mode)
       "json-mode" nil t)

  (push '("\\.json$" . json-mode) auto-mode-alist)
  (with-eval-after-load "json-mode"
    (defun my-json-mode-beautify ()
      (when (eq major-mode 'json-mode)
        (json-mode-beautify (point-min) (point-max))))
    (defun my-json-pretty-print-buffer ()
      (when (eq major-mode 'json-mode)
        (json-pretty-print-buffer)))
    (add-hook 'before-save-hook #'my-json-mode-beautify)
    (add-hook 'after-save-hook #'my-json-pretty-print-buffer)))

(when (autoload-if-found
       '(csv-mode)
       "csv-mode" nil t)

  (push '("\\.csv$" . csv-mode) auto-mode-alist))

(when (autoload-if-found
       '(cc-mode)
       "cc-mode" nil t)

  (push '("\\.pde$" . java-mode) auto-mode-alist) ;; Processing
  (push '("\\.java$" . java-mode) auto-mode-alist))

(when (autoload-if-found
       '(es-mode)
       "es-mode" nil t)

  (push '("\\.es$" . es-mode) auto-mode-alist))

(when (autoload-if-found
       '(markdown-mode)
       "markdown-mode" nil t)

  (push '("\\.markdown$" . markdown-mode) auto-mode-alist)
  (push '("\\.md$" . markdown-mode) auto-mode-alist))

(when (autoload-if-found
       '(logview-mode)
       "logview" nil t)
  (push '("\\.log$" . logview-mode) auto-mode-alist))

(when (autoload-if-found
       '(web-mode)
       "web-mode" "web-mode" t)

  ;; web-mode で開くファイルの拡張子を指定
  (push '("\\.phtml\\'" . web-mode) auto-mode-alist)
  (push '("\\.tpl\\.php\\'" . web-mode) auto-mode-alist)
  (push '("\\.jsp\\'" . web-mode) auto-mode-alist)
  (push '("\\.as[cp]x\\'" . web-mode) auto-mode-alist)
  (push '("\\.erb\\'" . web-mode) auto-mode-alist)
  (push '("\\.mustache\\'" . web-mode) auto-mode-alist)
  (push '("\\.djhtml\\'" . web-mode) auto-mode-alist)
  (push '("\\.html?\\'" . web-mode) auto-mode-alist)

  (with-eval-after-load "web-mode"
    (define-key web-mode-map (kbd "S-<tab>") 'my-web-indent-fold)

    (defun my-web-indent-fold ()
      (interactive)
      (web-mode-fold-or-unfold)
      (web-mode-buffer-indent)
      (indent-for-tab-command))

    ;; indent
    (setq web-mode-markup-indent-offset 1)

    ;; 色の設定
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(web-mode-comment-face ((t (:foreground "#D9333F"))))
     '(web-mode-css-at-rule-face ((t (:foreground "#FF7F00"))))
     '(web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))
     '(web-mode-css-rule-face ((t (:foreground "#A0D8EF"))))
     '(web-mode-doctype-face ((t (:foreground "#82AE46"))))
     '(web-mode-html-attr-name-face ((t (:foreground "#C97586"))))
     '(web-mode-html-attr-value-face ((t (:foreground "#82AE46"))))
     '(web-mode-html-tag-face ((t (:foreground "##4682ae" :weight bold))))
     '(web-mode-server-comment-face ((t (:foreground "#D9333F")))))))

;;(autoload 'po-mode "po-mode+" nil nil)
;;(autoload 'po-mode "po-mode" nil t)
(when (autoload-if-found
       '(po-mode)
       "po-mode" nil t)

  (push '("\\.po[tx]?\\'\\|\\.po\\$" . po-mode) auto-mode-alist))

(when (autoload-if-found
       '(yatex-mode)
       "yatex" "Yet Another LaTeX mode" t)

  (push '("\\.tex$" . yatex-mode) auto-mode-alist)

  ;; Disable auto line break
  (add-hook 'yatex-mode-hook
            (lambda ()
              (setq auto-fill-function nil)))

  (with-eval-after-load "yatex"
    ;; 1=Shift JIS, 2=JIS, 3=EUC, 4=UTF-8
    ;; (setq YaTeX-kanji-code nil)
    (modify-coding-system-alist 'file "\\.tex$'" 'utf-8)
    (define-key YaTeX-mode-map (kbd "C-M-SPC") 'mark-sexp)
    (define-key YaTeX-mode-map (kbd "C-M-@") 'mark-sexp)))

(autoload-if-found '(embark-act) "embark" nil t)

(my-tick-init-time "editing")

(with-eval-after-load "vc-hooks"
  (setcdr (assq 'vc-mode mode-line-format)
          '((:eval (replace-regexp-in-string "^ Git" "" vc-mode)))))

(with-eval-after-load "icons-in-terminal"
  ;; 変更がアリ時は赤アイコン，そうでない時に緑アイコンをモードラインに表示
  (make-face 'mode-line-vc-normal-face)
  (make-face 'mode-line-vc-modified-face)
  (set-face-attribute 'mode-line-vc-normal-face nil :foreground "#AFFFAF")
  (set-face-attribute 'mode-line-vc-modified-face nil :foreground "#EEAFAF")
  (defun my-mode-line-vc-mode-icon ()
    (if (string-match "^ Git:" vc-mode)
        (replace-regexp-in-string
         "^ Git:" (propertize " " 'face 'mode-line-vc-modified-face) vc-mode)
      (replace-regexp-in-string
       "^ Git-" (propertize " " 'face 'mode-line-vc-normal-face) vc-mode)))
  (setcdr (assq 'vc-mode mode-line-format)
          '((:eval (my-mode-line-vc-mode-icon)))))

;; mode-line
(set-face-attribute 'mode-line nil
                    :foreground "#FFFFFF"
                    :background "#a46398"
                    ;; :overline "#9d5446"
                    :box nil)
;; mode-line-inactive
(set-face-attribute 'mode-line-inactive nil
                    :foreground "#FFFFFF"
                    :background "#c8a1b7"
                    ;; :overline "#FFFFFF"
                    :box nil)
;; Terminal
(unless (display-graphic-p)
  (set-face-foreground 'mode-line "#96CBFE")
  (set-face-background 'mode-line "#21252B"))

;;  (setq visible-bell nil) ;; default=nil
(setq ring-bell-function 'ignore)

(when (require 'empty-booting nil t)
  ;; (setq initial-buffer-choice t) ;; 引数付き起動すると画面分割される
  (setq initial-scratch-message nil)
  (setq initial-major-mode 'empty-booting-mode)
  ;;  :underline "#203e6f"
  (set-face-foreground 'header-line "#FFFFFF") ;; "#203e6f" #333333 "#FFFFFF"
  (set-face-background 'header-line "#a46398") ;; "#ffb08c" "#7e59b5" ##5F7DB7
  (set-face-attribute 'header-line nil
                      :inherit nil
                      :overline nil
                      :underline nil)
  )

;; Disable to show the tool bar.
(when (and (boundp 'early-init-file)
	   (not early-init-file)
	   (display-graphic-p))
  (tool-bar-mode -1))

(when (and (boundp 'early-init-file)
	   (not early-init-file)
	   (or (not (display-graphic-p))
	       (eq system-type 'windows-nt)))
  (menu-bar-mode -1))

;; Disable to show the splash window at startup
(setq inhibit-startup-screen t)

(setq inhibit-default-init t)

(setq line-number-display-limit-width 100000)

;; モードラインの行数表示の前にアイコンを追加
(with-eval-after-load "icons-in-terminal"
  (setq mode-line-position-line-format
        `(,(icons-in-terminal-material "edit") "%3l")))

(with-eval-after-load "selected"
  (transient-define-prefix my-org-bullet-and-checkbox ()
    "Commands to handle bullet and checkbox"
    [["Bullet"
      ("i" "insert" my-org-insert-bullet)
      ("d" "delete" my-org-delete-bullet)]
     ["Checkbox"
      ("[" "insert" my-org-insert-checkbox-into-bullet)
      ("]" "delete" my-org-delete-checkbox-from-bullet)
      ;;("a" "toggle checkbox" my-org-toggle-checkbox)
      ;;("h" "cycle" my-cycle-bullet-at-heading) ;; single line
      ]
     ["Bullet and Checkbox"
      ("I" "insert" my-org-insert-bullet-and-checkbox)
      ("D" "delete" my-org-delete-bullet-and-checkbox)]])
  )

(my-tick-init-time "presentation")

(my-tick-init-time "media")

(setq history-length 2000)

(setq undo-outer-limit nil)

;; *.~
(setq make-backup-files nil)
;; .#*
(setq auto-save-default nil)
;; auto-save-list
(setq auto-save-list-file-prefix nil)

(defun ad:find-file-noselect (_filename &optional _nowarn _rawfile _wildcards)
  (unless (require 'init-dired nil t)
    (user-error "init-dired.el doesn't exist"))
  (advice-remove 'find-file-noselect #'ad:find-file-noselect)
  (advice-remove 'dired #'ad:dired-activate))

(defun ad:dired-activate (_dirname &optional _switches)
  (unless (require 'init-dired nil t)
    (user-error "init-dired.el doesn't exist"))
  (advice-remove 'find-file-noselect #'ad:find-file-noselect)
  (advice-remove 'dired #'ad:dired-activate))

(advice-add 'find-file-noselect :before #'ad:find-file-noselect)
(advice-add 'dired :before #'ad:dired-activate)

(when (autoload-if-found
       '(session-initialize)
       "session" nil t)

  (unless noninteractive
    (add-hook 'after-init-hook #'session-initialize))

  (with-eval-after-load "session"
    (add-to-list 'session-globals-include 'ivy-dired-history-variable)
    (add-to-list 'session-globals-exclude 'org-mark-ring)
    (setq session-set-file-name-exclude-regexp "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|[/\\]COMMIT_EDITMSG")
    ;; Change save point of session.el
    (setq session-save-file
          (expand-file-name (concat (getenv "SYNCROOT") "/emacs.d/.session")))
    (setq session-initialize '(de-saveplace session keys menus places)
          session-globals-include '((kill-ring 100)
                                    (session-file-alist 100 t)
                                    (file-name-history 200)
                                    ivy-dired-history-variable
                                    search-ring
                                    regexp-search-ring))
    (setq session-undo-check -1)))

;; FIXME
;;  (setq session-set-file-name-exclude-regexp
;;        "^/private/\\.\\*"))
;;          "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|^/private\\.*\\|^/var/folders\\.*"))

(my-tick-init-time "history")

(if (not (executable-find "gtags"))
    (message "--- global is NOT installed in this system.")

  (when (autoload-if-found
         '(ggtags-mode)
         "ggtags" nil t)

    (with-eval-after-load "ggtags"
      ;; (setq ggtags-completing-read-function t) ;; nil for helm
      (define-key ggtags-mode-map (kbd "M-]") nil))

    (dolist (hook (list 'c-mode-common-hook 'python-mode-hook))
      (add-hook hook (lambda () (ggtags-mode 1)))))

  (when (autoload-if-found
         '(counsel-gtags-mode)
         "counsel-gtags" nil t)
    (dolist (hook '(c-mode-hook c++-mode-hook))
      (add-hook hook 'counsel-gtags-mode))
    (with-eval-after-load "counsel-gtags"
      (custom-set-variables
       '(counsel-gtags-update-interval-second 10)))))

(my-tick-init-time "development")

(cond
 ;; for Macintosh
 ((memq window-system '(mac ns))
  (setq initial-frame-alist
	      (append
	       '((top . 23)
	         (left . 0)
	         ;; (alpha . (100 90))
	         ;; (vertical-scroll-bars . nil)
	         ;; (internal-border-width . 20)
	         ;; (outer-border-width . 20)
	         ;; (ns-appearance . nil) ;; 26.1 {light, dark}
	         (ns-transparent-titlebar . t)) ;; 26.1
	       initial-frame-alist)))

 ;; for Linux
 ((eq window-system 'x)
  (setq initial-frame-alist
	      (append
	       '((vertical-scroll-bars . nil)
	         (top . 0)
	         (left . 0)
	         (width . 80)
	         (height . 38))
	       initial-frame-alist)))

 ((eq window-system nil)
  nil)

 ;; for Windows
 (t (setq initial-frame-alist
	        (append
	         '((vertical-scroll-bars . nil)
	           (top . 0)
	           (left . 0)
	           (width . 80)
	           (height . 26))
	         initial-frame-alist))))

;; Apply the initial setting to default
(setq default-frame-alist initial-frame-alist)
(with-eval-after-load "postpone"
  (set-face-foreground 'vertical-border (face-foreground 'default))
  (set-face-background 'vertical-border (face-background 'default)))
(set-face-background 'fringe (face-background 'default)) ;; 10-20[ms]

;; カーソルの色
(defconst my-cur-color-ime '(:on "#FF9300" :off "#91C3FF"))
(defconst my-cur-type-ime '(:on (bar . 2) :off (bar . 2) :invisible nil))
(defvar my-ime-last nil)

;; (defun my-ime-active-p ()
;;   (if (fboundp 'mac-get-current-input-source)
;;       (not (string-match "\\.\\(Roman\\|US\\|ABC\\)$"
;; 			 (mac-get-current-input-source)))
;;     (if current-input-method t nil)))

(defun my-ime-active-p ()
  (mac-ime-active-p))

(defun my-ime-on-cursor ()
  (interactive)
  (setq cursor-type (plist-get my-cur-type-ime :on))
  (set-cursor-color (plist-get my-cur-color-ime :on)))
(defun my-ime-off-cursor ()
  (interactive)
  (setq cursor-type (plist-get my-cur-type-ime :off))
  (set-cursor-color (plist-get my-cur-color-ime :off)))
(defun my-ime-invisible-cursor ()
  (interactive)
  (setq cursor-type (plist-get my-cur-type-ime :invisible)))
(add-hook 'input-method-activate-hook #'my-ime-on-cursor)
(add-hook 'input-method-deactivate-hook #'my-ime-off-cursor)

(if (version< emacs-version "27.0")
    (defun my-apply-cursor-config ()
      (interactive)
      (when (display-graphic-p)
	      (if (my-ime-active-p) (my-ime-on-cursor) (my-ime-off-cursor))))

  (defun my-apply-cursor-config ()
    (interactive)
    (when (and (display-graphic-p)
	             (fboundp 'mac-ime-active-p))
      (if (mac-ime-active-p) (my-ime-on-cursor) (my-ime-off-cursor))))
  (add-hook 'buffer-list-update-hook #'my-apply-cursor-config))

;; (unless (and (memq window-system '(ns mac))
;;              noninteractive)
;;   ;; ensure IME off when starting Emacs except macOS
;;   (toggle-input-method)
;;   (toggle-input-method nil nil))

;; for init setup
(setq-default cursor-type (plist-get my-cur-type-ime :on))
(my-apply-cursor-config)

;; could be deleted
(when (memq window-system '(ns x))
  (with-eval-after-load "postpone"
    (run-with-idle-timer 3 t #'my-apply-cursor-config)))

(with-eval-after-load "postpone"
  (cond
   ((memq window-system '(ns x))
    ;; モードラインにアイコンを出す
    (make-face 'mode-line-ime-on-face)
    (set-face-attribute 'mode-line-ime-on-face
                        nil :foreground (plist-get my-cur-color-ime :on))
    (when (fboundp 'mac-set-input-method-parameter)
      (mac-set-input-method-parameter
       "com.google.inputmethod.Japanese.base" 'title
       (concat
	      (if (require 'icons-in-terminal nil t)
            (icons-in-terminal-octicon "keyboard"
                                       :v-adjust 0.0
                                       :face 'mode-line-ime-on-face)
          "") " "))) ;; FIXME (the color is NOT changed, patch is wrong?)

    (declare-function my-ime-on "init" nil)
    (declare-function my-ime-off "init" nil)
    (declare-function my-ime-active-p "init" nil)

    ;; for private patch
    (when (boundp 'mac-ime-cursor-type)
      (setq mac-ime-cursor-type (plist-get my-cur-type-ime :on)))

    (setq my-ime-last (my-ime-active-p))
    (defun my-ime-on ()
      (interactive)
      (if (fboundp 'mac-toggle-input-method)
	        (progn
	          (mac-toggle-input-method t)
	          (run-hooks 'input-method-activate-hook))
	      (activate-input-method default-input-method))
      (setq my-ime-last t))
    (defun my-ime-off ()
      (interactive)
      (if (fboundp 'mac-toggle-input-method)
	        (progn
	          (mac-toggle-input-method nil)
	          (run-hooks 'input-method-deactivate-hook))
	      (deactivate-input-method))
      (setq my-ime-last nil))

    (defvar my-ime-before-action nil)
    (defun my-ime-on-sticky ()
      (when my-ime-before-action
	      (my-ime-on)))
    (defun my-ime-off-sticky ()
      (when (setq my-ime-before-action (my-ime-active-p))
	      (my-ime-off)))

    (if (version< emacs-version "27.0")
	      (progn
	        ;; For selected.el
	        (add-hook 'activate-mark-hook #'my-ime-off-sticky)
	        (add-hook 'deactivate-mark-hook #'my-ime-on-sticky)
	        ;; 「M-x あ」対策
	        (add-hook 'minibuffer-setup-hook #'my-ime-off-sticky)
	        (add-hook 'minibuffer-exit-hook #'my-ime-on-sticky))
      ;; For selected.el
      (add-hook 'activate-mark-hook #'mac-ime-deactivate-sticky)
      (add-hook 'deactivate-mark-hook #'mac-ime-activate-sticky))

    ;; (defun ad:find-file (FILENAME &optional WILDCARDS)
    ;;   "Extension to find-file as before-find-file-hook."
    ;;   (message "--- ad:findfile")
    ;;   (apply FILENAME WILDCARDS))
    ;; (advice-add #'find-file :around #'ad:find-file)

    ;; http://tezfm.blogspot.jp/2009/11/cocoa-emacs.html
    ;; バッファ切替時に input method を切り替える
    ;; (with-eval-after-load "postpone"
    ;;   (when (and (fboundp 'mac-handle-input-method-change)
    ;;              (require 'cl nil t))
    ;;     (add-hook
    ;;      'post-command-hook
    ;;      (lexical-let ((previous-buffer nil))
    ;;        (message "Change IM %S -> %S" previous-buffer (current-buffer))
    ;;        (lambda ()
    ;;            (unless (eq (current-buffer) previous-buffer)
    ;;              (when (bufferp previous-buffer)
    ;;                (mac-handle-input-method-change))
    ;;              (setq previous-buffer (current-buffer))))))))
    )


   ;; EMP: Emacs Mac Port
   ((eq window-system 'mac)
    (when (fboundp 'mac-input-source)
      (defun my-mac-keyboard-input-source () ;; Need update
	      (if (string-match "\\.Roman$" (mac-input-source))
	          (progn
	            (setq cursor-type (plist-get my-cur-type-ime :off))
	            (add-to-list 'default-frame-alist
			                     `(cursor-type . ,(plist-get my-cur-type-ime :off)))
	            (set-cursor-color (plist-get my-cur-color-ime :off)))
	        (progn
	          (setq cursor-type (plist-get my-cur-type-ime :on))
	          (add-to-list 'default-frame-alist
			                   `(cursor-type . ,(plist-get my-cur-type-ime :on)))
	          (set-cursor-color (plist-get my-cur-color-ime :on)))))

      (when (fboundp 'mac-auto-ascii-mode)
	      ;; (mac-auto-ascii-mode 1)
	      ;; IME ON/OFF でカーソルの種別や色を替える
	      (add-hook 'mac-selected-keyboard-input-source-change-hook
		              #'my-mac-keyboard-input-source)
	      ;; IME ON の英語入力＋決定後でもカーソルの種別や色を替える
	      ;; (add-hook 'mac-enabled-keyboard-input-sources-change-hook
	      ;;           #'my-mac-keyboard-input-source)
	      (declare-function my-mac-keyboard-input-source "init" nil)
	      (my-mac-keyboard-input-source))))

   (t nil)))

(declare-function my-font-config "init" nil)
(defconst moom-autoloads
  '(moom-cycle-frame-height
    moom-move-frame-to-edge-top moom-move-frame my-frame-reset
    moom-toggle-frame-maximized moom-move-frame-to-edge-right
    moom-move-frame-to-center moom-move-frame-right moom-move-frame-left
    moom-fill-display-band moom-move-frame-to-edge-right moom-fill-band
    moom-change-frame-width moom-change-frame-width-double
    moom-change-frame-width-single moom-change-frame-width-half-again
    moom-cycle-monitors))

(when (autoload-if-found
       moom-autoloads
       "moom" nil t)

  (global-set-key (kbd "C-1") 'moom-move-frame-to-edge-top)
  (global-set-key (kbd "C-2") 'moom-cycle-frame-height)
  (global-set-key (kbd "M-2") 'moom-move-frame-to-center)
  (global-set-key (kbd "M-9") 'moom-cycle-monitors)

  (with-eval-after-load "moom"
    (add-hook 'moom-split-window-hook #'dimmer-permanent-off)
    (add-hook 'moom-delete-window-hook #'dimmer-on)
    (add-hook 'moom-after-select-monitor-hook #'moom-move-frame-to-center)

    ;; (define-key moom-mode-map (kbd "C-c C-<") 'moom-move-frame-to-edge-left)
    ;; (define-key moom-mode-map (kbd "C-c C->") 'moom-move-frame-to-edge-right)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Trying... (2022-01-28)
    (defun moom-recommended-keybindings (options)
      "Apply pre defined keybindings.
OPTIONS is a list of moom API types.  If you want to set all recommemded
keybindings, put the following code in your init.el.
 (with-eval-after-load \"moom\"
   (moom-recommended-keybindings 'all))
'all is identical to '(move fit expand fill font reset undo).
If OPTIONS includes 'wof, then each binding is configured not to use fn key.
If you give only '(reset) as the argument, then \\[moom-reset] is activated.
The keybindings will be assigned only when Emacs runs in GUI."
      (when window-system
        (when (memq 'all options)
          (if (memq 'wof options)
              (setq options '(move fit expand fill font reset undo wof))
            (setq options '(move fit expand fill font reset undo))))
        (when (memq 'move options)
          (define-key moom-mode-map (kbd "M-0") 'moom-move-frame)
          (define-key moom-mode-map (kbd "M-1") 'moom-move-frame-left)
          (define-key moom-mode-map (kbd "M-2") 'moom-move-frame-to-center)
          (define-key moom-mode-map (kbd "M-3") 'moom-move-frame-right))
        (when (memq 'fit options)
          (cond ((memq 'wof options)
                 (define-key moom-mode-map (kbd "C-c e l") 'moom-move-frame-to-edge-left)
                 (define-key moom-mode-map (kbd "C-c e r") 'moom-move-frame-to-edge-right)
                 (define-key moom-mode-map (kbd "C-c e t") 'moom-move-frame-to-edge-top)
                 (define-key moom-mode-map (kbd "C-c e b") 'moom-move-frame-to-edge-bottom))
                (t
                 (define-key moom-mode-map (kbd "M-<f1>") 'moom-move-frame-to-edge-left)
                 (define-key moom-mode-map (kbd "M-<f3>") 'moom-move-frame-to-edge-right)
                 (define-key moom-mode-map (kbd "<f1>") 'moom-move-frame-to-edge-top)
                 (define-key moom-mode-map (kbd "S-<f1>") 'moom-move-frame-to-edge-bottom))))
        (define-key moom-mode-map (kbd "C-c f c l")
          'moom-move-frame-to-centerline-from-left)
        (define-key moom-mode-map (kbd "C-c f c r")
          'moom-move-frame-to-centerline-from-right)
        (define-key moom-mode-map (kbd "C-c f c t")
          'moom-move-frame-to-centerline-from-top)
        (define-key moom-mode-map (kbd "C-c f c b")
          'moom-move-frame-to-centerline-from-bottom))
      (when (memq 'expand options)
        (cond ((memq 'wof options)
               (define-key moom-mode-map (kbd "C-2") 'moom-cycle-frame-height))
              (t
               (define-key moom-mode-map (kbd "<f2>") 'moom-cycle-frame-height)))
        (define-key moom-mode-map (kbd "C-c f s") 'moom-change-frame-width-single)
        (define-key moom-mode-map (kbd "C-c f d") 'moom-change-frame-width-double)
        (define-key moom-mode-map (kbd "C-c f S") 'moom-delete-windows)
        (define-key moom-mode-map (kbd "C-c f D") 'moom-split-window)
        (define-key moom-mode-map (kbd "C-c f a")
          'moom-change-frame-width-half-again))
      (when (memq 'fill options)
        (define-key moom-mode-map (kbd "C-c f f t") 'moom-fill-top)
        (define-key moom-mode-map (kbd "C-c f f b") 'moom-fill-bottom)
        (define-key moom-mode-map (kbd "C-c f f l") 'moom-fill-left)
        (define-key moom-mode-map (kbd "C-c f f r") 'moom-fill-right)
        (define-key moom-mode-map (kbd "C-c f f 1") 'moom-fill-top-left)
        (define-key moom-mode-map (kbd "C-c f f 2") 'moom-fill-top-right)
        (define-key moom-mode-map (kbd "C-c f f 3") 'moom-fill-bottom-left)
        (define-key moom-mode-map (kbd "C-c f f 4") 'moom-fill-bottom-right)
        (define-key moom-mode-map (kbd "C-c f f m") 'moom-fill-band)
        (define-key moom-mode-map (kbd "C-c f f w") 'moom-fill-width)
        (define-key moom-mode-map (kbd "C-c f f h") 'moom-fill-height)
        (cond ((memq 'wof options)
               (define-key moom-mode-map (kbd "C-c f m") 'moom-toggle-frame-maximized))
              (t
               (define-key moom-mode-map (kbd "M-<f2>") 'moom-toggle-frame-maximized))))
      (when (memq 'font options)
        (define-key moom-mode-map (kbd "C--") 'moom-font-decrease)
        (define-key moom-mode-map (kbd "C-=") 'moom-font-increase)
        (define-key moom-mode-map (kbd "C-0") 'moom-font-size-reset))
      (when (memq 'reset options)
        (define-key moom-mode-map (kbd "C-c C-0") 'moom-reset))
      (when (memq 'undo options)
        (define-key moom-mode-map (kbd "C-c C-/") 'moom-undo))
      (when (and moom-verbose
                 options)
        (message "[Moom] Key defined for APIs of %s." options)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (custom-set-variables
     '(moom-command-with-centering nil)
     '(moom-lighter "M")
     '(moom-verbose t))
    (moom-recommended-keybindings '(all wof))
    (moom-mode 1)
    (my-font-config)))  ;; this could increase `postpone-init-time'.

(when (autoload-if-found
       '(moom-font-increase
         moom-font-decrease moom-font-size-reset moom-font-resize)
       "moom-font" nil t)

  (add-hook 'moom-font-after-resize-hook #'moom-move-frame-to-edge-top)
  (add-hook 'moom-font-after-resize-hook #'moom-fill-height)

  (with-eval-after-load "moom-font"
    (custom-set-variables
     '(moom-scaling-gradient (/ (float 50) 30))
     '(moom-font-table
       '((50 30) (49 29) (48 29) (47 28) (46 28) (45 27) (44 26) (43 26)
         (42 25) (41 25) (40 24) (39 23) (38 23) (37 22) (36 22) (35 21)
         (34 20) (33 20) (32 19) (31 19) (30 18) (29 17) (28 17) (27 16)
         (26 16) (25 15) (24 14) (23 14) (22 13) (21 13) (20 12) (19 11)
         (18 11) (17 10) (16 10) (15 9) (14 8) (13 8) (12 7) (11 7) (10 6)
         (9 5) (8 5) (7 4) (6 4) (5 3))))))

(my-tick-init-time "frame and window")

;; 1) Monaco, Hiragino/Migu 2M : font-size=12, -apple-hiragino=1.2
;; 2) Inconsolata, Migu 2M     : font-size=14,
;; 3) Inconsolata, Hiragino    : font-size=14, -apple-hiragino=1.0
(defconst my-font-size 12)
(defconst my-ja-font "Migu 2M") ;; "Hiragino Maru Gothic Pro"
(defconst my-ascii-font "Monaco") ;; "Inconsolata", Monaco
;; (defconst my-ja-font "Hiragino Maru Gothic Pro") ;; "Hiragino Maru Gothic Pro"
;; (defconst my-ascii-font "Inconsolata") ;; "Inconsolata", Menlo, "Ricty Diminished"
(defun my-ja-font-setter (spec)
  (set-fontset-font nil 'japanese-jisx0208 spec)
  (set-fontset-font nil 'katakana-jisx0201 spec)
  (set-fontset-font nil 'japanese-jisx0212 spec)
  (set-fontset-font nil '(#x0080 . #x024F) spec)
  (set-fontset-font nil '(#x0370 . #x03FF) spec)
  (set-fontset-font nil 'mule-unicode-0100-24ff spec))
(defun my-ascii-font-setter (spec)
  (set-fontset-font nil 'ascii spec))
(defun my-unicode-font-setter (spec)
  (set-fontset-font t 'unicode spec nil 'prepend))
(defun my-all-the-icons-setter ()
  (when (require 'icons-in-terminal nil t)
    (my-unicode-font-setter (font-spec :family (icons-in-terminal-faicon-family)))
    (my-unicode-font-setter (font-spec :family (icons-in-terminal-fileicon-family)))
    (my-unicode-font-setter (font-spec :family (icons-in-terminal-material-family)))
    (my-unicode-font-setter (font-spec :family (icons-in-terminal-octicon-family)))
    (my-unicode-font-setter (font-spec :family (icons-in-terminal-wicon-family)))))
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

(defun my-setup-font ()
  (interactive)
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
(my-setup-font)

;; set-default で global 指定すると，ミニバッファの message で制御不能になる
;; propertize で拡大できるが，global の値以下に縮小できなくなる．
;; (set-default 'line-spacing 2)
(defun my-linespacing ()
  (unless (minibufferp)
    (setq-local line-spacing 2)))
(add-hook 'buffer-list-update-hook #'my-linespacing)
(add-hook 'org-src-mode-hook #'my-linespacing)
(add-hook 'debugger-mode-hook #'my-linespacing)

(with-eval-after-load "org-agenda"
  (defun my-org-agenda (&optional _arg _org-keys _restriction)
    (my-linespacing))
  (advice-add 'org-agenda :after #'my-org-agenda)
  (defun my-org-agenda-redo (&optional _all)
    (my-linespacing))
  (advice-add 'org-agenda-redo :after #'my-org-agenda-redo))

(declare-function my-daylight-theme "init" nil)
(declare-function my-night-theme "init" nil)
(declare-function my-terminal-theme "init" nil)
(defvar my-light-theme-hook nil)
(defvar my-dark-theme-hook nil)
(if (not (display-graphic-p))
    (defun my-terminal-theme ()
      (interactive)
      (when (require 'terminal-theme nil t)
        (mapc 'disable-theme custom-enabled-themes)
        (load-theme 'terminal t)
        (plist-put my-cur-color-ime :on "#FF9300")))

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
      (run-hooks 'my-dark-theme-hook))))

(declare-function my-font-config "init" nil)
;;;###autoload
(defun my-night-time-p (begin end)
  (let* ((ch (string-to-number (format-time-string "%H" (current-time))))
         (cm (string-to-number (format-time-string "%M" (current-time))))
         (ct (+ cm (* 60 ch))))
    (if (> begin end)
        (or (<= begin ct) (<= ct end))
      (and (<= begin ct) (<= ct end)))))

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

;; init. This may override or reset font setting
(with-eval-after-load "postpone"
  (my-theme))

;; (with-eval-after-load "postpone"
;;   (run-at-time "21:00" 86400 'my-theme)
;;   (run-at-time "05:00" 86400 'my-theme)) ;; FIXME: it makes frame blink

(my-tick-init-time "font")

(when (autoload-if-found
       '(my-cmd-to-open-iterm2)
       "utility" nil t)

  (global-set-key (kbd "C-M-i") #'my-cmd-to-open-iterm2)

  (with-eval-after-load "flyspell"
    (define-key flyspell-mode-map (kbd "C-M-i") #'my-cmd-to-open-iterm2))

  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-M-i") #'my-cmd-to-open-iterm2)))

(when (autoload-if-found
       '(counsel-osx-app)
       "counsel-osx-app" nil t)

  (global-set-key (kbd "C-M-1") 'counsel-osx-app)

  (with-eval-after-load "counsel-osx-app"
    (custom-set-variables
     '(counsel-osx-app-location
       '("/Applications" "/Applications/Utilities"
         "/System/Applications"
         "/System/Applications/Utilities"
         "/Applications/Microsoft Remote Desktop.localized")))))

(my-tick-init-time "utility")
(provide 'init)
