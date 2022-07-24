;; init.el --- My init.el -*- lexical-binding: t -*-
;; Configurations for Emacs
;;                                         Takaaki ISHIKAWA  <takaxp@ieee.org>
;; see also https://takaxp.github.io/init.html
(require 'init-autoloads nil t)

(with-eval-after-load "postpone"
  (require 'late-init nil t)
  (require 'init-org nil t)) ;; loading all with-eval-after-load for Org

(unless noninteractive
  (with-eval-after-load "org"
    (require 'postpone nil t)
    (require 'init-org nil t)))

(defconst my-before-load-init-time (current-time))
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
(defun my-tick-init-time (msg)
  "Tick boot sequence at loading MSG."
  (when my-loading-profile-p
    (let ((ctime (current-time)))
      (message "---- %5.2f[ms] %s"
               (* 1000 (float-time
                        (time-subtract ctime my-tick-previous-time)))
               msg)
      (setq my-tick-previous-time ctime))))

(defun my-emacs-init-time ()
  "Emacs booting time in msec."
  (message "Emacs booting time: %.0f [msec] = `emacs-init-time'."
           (* 1000
              (float-time (time-subtract
                           after-init-time
                           before-init-time)))))

(add-hook 'after-init-hook #'my-emacs-init-time)

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

(when (require 'postpone-pre nil t)
  (setq postpone-pre-exclude
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
  (run-with-idle-timer 8 nil #'postpone-pre))

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

(when (fboundp 'mac-add-key-passed-to-system)
  (setq default-input-method "macOS")
  (mac-add-key-passed-to-system 'shift))

(when (eq system-type 'darwin)
  (when (boundp 'ns-command-modifier) (setq ns-command-modifier 'meta))
  (when (boundp 'ns-alternate-modifier) (setq ns-alternate-modifier 'super))
  (when (boundp 'ns-pop-up-frames) (setq ns-pop-up-frames nil))
  (global-set-key (kbd "M-v") 'yank)
  (global-set-key [ns-drag-file] 'ns-find-file))
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Limit the final word to a line break code (automatically correct)
(setq require-final-newline t)

(when (version< "27.0" emacs-version)
  (defun ad:find-file-read-args (f prompt mustmatch)
    (when (equal default-directory "/")
      (setq default-directory "~/"))
    (funcall f prompt mustmatch))
  (advice-add 'find-file-read-args :around #'ad:find-file-read-args))

(my-tick-init-time "core")

(my-tick-init-time "point")

(global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)

(my-tick-init-time "editing")

(if (not (display-graphic-p))
    (progn ;; Terminal
      (set-face-foreground 'mode-line "#96CBFE")
      (set-face-background 'mode-line "#21252B"))

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
                      :box nil))

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
                      :underline nil))

;; Disable to show the splash window at startup
(setq inhibit-startup-screen t)

(setq inhibit-default-init t)

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

(my-tick-init-time "history")

(my-tick-init-time "development")

(cond
 ((memq window-system '(mac ns)) ;; for macOS
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

 ((eq window-system 'x) ;; for Linux
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

 (t ;; for Windows
  (setq initial-frame-alist
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
;;(set-face-background 'fringe (face-background 'default)) ;; 10-20[ms]
(set-face-background 'fringe "#FFFFFF") ;; 10-20[ms]

;; カーソルの色
(defconst my-cur-color-ime '(:on "#FF9300" :off "#91C3FF"))
(defconst my-cur-type-ime '(:on (bar . 2) :off (bar . 2) :invisible nil))
(defvar my-ime-last nil)

(if (fboundp 'mac-ime-active-p)
    (defalias 'my-ime-active-p 'mac-ime-active-p)
  (defun my-ime-active-p () current-input-method))

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

(defun my-apply-cursor-config ()
  (interactive)
  (when (display-graphic-p)
	  (if (my-ime-active-p) (my-ime-on-cursor) (my-ime-off-cursor))))

;; for init setup
(setq-default cursor-type (plist-get my-cur-type-ime :on))
(unless noninteractive
  (add-hook 'buffer-list-update-hook #'my-apply-cursor-config)
  (my-apply-cursor-config))
(add-hook 'input-method-activate-hook #'my-ime-on-cursor)
(add-hook 'input-method-deactivate-hook #'my-ime-off-cursor)

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
  (global-set-key (kbd "C-!") 'moom-move-frame-to-edge-bottom)
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
   (moom-recommended-keybindings \='all))
=\'all is identical to =\'(move fit expand fill font reset undo).
If OPTIONS includes =\'wof, then each binding is configured not to use fn key.
If you give only =\'(reset) as the argument, then \\[moom-reset] is activated.
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
    (my-font-config)))  ;; this could increase `postpone-pre-init-time'.

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

(my-tick-init-time "font")

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

(global-set-key (kbd "C-c 0") 'insert-formatted-current-date)
(global-set-key (kbd "C-c 9") 'insert-formatted-current-time)

(my-tick-init-time "utility")
(provide 'init)
