;; init.el --- My init.el -*- lexical-binding: t -*-
;; Configurations for Emacs
;;																				 Takaaki ISHIKAWA	 <takaxp@ieee.org>
;; see also https://takaxp.github.io/init.html
(require 'init-autoloads nil t)
(when (and (boundp my-profiler-p)
		       my-profiler-p)
	(profiler-start 'cpu+mem))
(when (and (boundp my-profiler-p)
		       my-ad-require-p)
	(load "~/Dropbox/emacs.d/config/init-ad.el" nil t))

(with-eval-after-load "postpone"
  (require 'late-init nil t)
  ;; only top-level setting will be loaded. This will not actually load `org' so settings in `with-eval-after-load' will not be loaded.
  (require 'init-org nil t))

(unless noninteractive
  (with-eval-after-load "org"
    (require 'init-org nil t)))

(defun my-emacs-init-time ()
  "Emacs booting time in msec."
  (let ((inhibit-message t))
    (message "Emacs booting time: %4d [ms] = `emacs-init-time'."
             (* 1000
                (float-time (time-subtract
                             after-init-time
                             before-init-time))))))

(add-hook 'after-init-hook #'my-emacs-init-time)

(defconst my-before-load-init-time (current-time)
  "Starting point to calculate Emacs booting time.  see `my-load-init-time'.")
(defun my-load-init-time ()
  "Loading time of user init files including time for `after-init-hook'."
  (let ((t-init-files (time-subtract after-init-time my-before-load-init-time))
        (t-after-init (time-subtract (current-time) after-init-time))
        (t-others (time-subtract my-before-load-init-time before-init-time))
        (t-early-init (time-subtract my-early-end my-early-start))
        (inhibit-message t))
    (message (concat
              "  Loading init files: %4d [ms]\n"
              "  Loading early-init: %4d [ms]\n"
              "  Others(GUI etc.):   %4d [ms] includes `before-init-hook'\n"
              "(`after-init-hook': %4d [ms])")
             (* 1000 (float-time t-init-files))
             (* 1000 (float-time t-early-init))
             (* 1000 (- (float-time t-others) (float-time t-early-init)))
             (* 1000 (float-time t-after-init)))))

(add-hook 'after-init-hook #'my-load-init-time t)

(defvar my-tick-previous-time my-before-load-init-time)
(defun my-tick-init-time (msg)
  "Tick boot sequence at loading MSG."
  (when my-loading-profile-p
    (let ((ctime (current-time)))
      (message "---- %4d[ms] %s"
               (* 1000 (float-time
                        (time-subtract ctime my-tick-previous-time)))
               msg)
      (setq my-tick-previous-time ctime))))

(defvar my-suppress-message-p t)
(defun ad:suppress-message (f &rest arg)
  (if my-suppress-message-p
      (let ((inhibit-message t)
            (message-log-max nil))
        (apply f arg))
    (apply f arg)))

;; Suppress printing "Waiting for git..." from version.el
(advice-add 'emacs-repository-branch-git :around #'ad:suppress-message)
(advice-add 'emacs-repository-version-git :around #'ad:suppress-message)

(when (version< emacs-version "29.0")
  (load "~/Dropbox/emacs.d/config/init-compat.el" nil t))

(defun my-load-package-p (file)
  (let ((enabled t))
    (when (boundp 'my-disabled-packages)
      (dolist (package my-disabled-packages)
        (let ((name (car package))
              (flag (cdr package)))
          (when (and (stringp name)
                     (equal file name)
                     (not flag))
            (setq enabled nil)
            (message "--- `%s' was NOT loaded intentionally" name)))))
    enabled))

(defvar my-skip-check-autoload-file t)
(defvar my-required-libraries nil)
(when (bound-and-true-p my-disabled-packages)
  (setq my-skip-check-autoload-file nil))

(defun autoload-if-found (functions file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (when (boundp 'my-required-libraries)
    (add-to-list 'my-required-libraries file)) ;; collect packages TBC later
  (when (or my-skip-check-autoload-file
            (and (my-load-package-p file)
                 (locate-library file))) ;; takes time here
    (dolist (f functions)
      (autoload f file docstring interactive type))
    t))

;; Copied from postpone-pre.el for speed up -- begin ;;;;;;;;;;;;;;;;;;;;;
(defvar postpone-pre-init-time nil
  "A variable to store the duration of loading postponed packages.")

(defcustom postpone-pre-exclude '(self-insert-command
                                  save-buffers-kill-terminal
                                  exit-minibuffer)
  "A list of commands not to activate `postpone-mode'."
  :type 'sexp
  :group 'postpone)

;;;###autoload
(defun postpone-pre ()
  (interactive)
  (unless (or my-secure-boot
              (memq this-command postpone-pre-exclude)
              postpone-pre-init-time)
    (message "Activating postponed packages...")
    (let ((t1 (current-time)))
      (postpone-kicker 'postpone-pre)
      (setq postpone-pre-init-time (float-time
                                    (time-subtract (current-time) t1))))
    (message "Activating postponed packages...done ( %4d [ms])"
             (* postpone-pre-init-time 1000))))

(autoload 'postpone-kicker "postpone" nil t)
(add-hook 'pre-command-hook #'postpone-pre) ;; will be removed in postpone.el.
;; Copied from postpone-pre.el for speed up -- end ;;;;;;;;;;;;;;;;;;;;;;;

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

;; 起動後X秒何もしない場合は自動でキック (related to setting on org-agenda)
(defvar my-default-loading-delay 5) ;; [s]
(unless (or noninteractive my-secure-boot)
  (run-with-idle-timer (+ 5 my-default-loading-delay) nil #'postpone-pre))

;; Native Compiling の最終のワーニング等をウィンドウに出さない
(setq native-comp-async-report-warnings-errors nil)

;; Suppress exporting of custom-set-variables (25.1 or later)
(setq custom-file (locate-user-emacs-file "custom.el"))

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
  (keymap-global-set "M-v" 'yank)
  (keymap-global-set "<ns-drag-file>" 'ns-find-file))
(keymap-global-set "<delete>" 'delete-char)
(keymap-global-set "<kp-delete>" 'delete-char)

(when (version< "27.0" emacs-version)
  (with-eval-after-load "files"
    (defun ad:find-file-read-args (f prompt mustmatch)
      (when (equal default-directory "/")
        (setq default-directory "~/"))
      (funcall f prompt mustmatch))
    (advice-add 'find-file-read-args :around #'ad:find-file-read-args)))

(run-with-idle-timer 60 t #'my-lock-secret-buffer "secret.org.gpg")

(keymap-global-set "M-SPC" 'my-toggle-ime-ns)
(keymap-global-set "S-SPC" 'my-toggle-ime-ns)
;; (keymap-set isearch-mode-map "M-SPC" 'my-toggle-ime-ns)
;; (keymap-set isearch-mode-map "S-SPC" 'my-toggle-ime-ns)
(when (fboundp 'mac-ime-toggle) ;; using ns-inline-patch
  (defalias 'my-toggle-ime-ns 'mac-ime-toggle)
  (defalias 'my-ime-active-p 'mac-ime-active-p)) ;; FIXME

(my-tick-init-time "core")

(keymap-global-set "C-M-t" 'beginning-of-buffer)
(keymap-global-set "C-M-b" 'end-of-buffer)
;; Backward page scrolling instead of M-v
(keymap-global-set "C-t" 'scroll-down)
;; Frontward page scrolling instead of C-v
;; (keymap-global-set "M-n" 'scroll-up)
;; Move cursor to a specific line
(keymap-global-set "C-c g" 'goto-line)

(keymap-global-set "C-M-p" (lambda () (interactive) (other-window -1)))
(keymap-global-set "C-M-n" (lambda () (interactive) (other-window 1)))

(keymap-global-set "M-]" 'bs-cycle-next)
(when (display-graphic-p)
  (keymap-global-set "M-[" 'bs-cycle-previous))

(my-tick-init-time "point")

(keymap-global-set "RET" 'electric-newline-and-maybe-indent)

(keymap-global-set "M-=" 'count-words)

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

(defun empty-booting-mode ()
  "Minimum mode for quick booting"
  (interactive)
  (setq mode-name "Empty")
  (setq major-mode 'empty-booting-mode)
  (setq header-line-format " No day is a good day.")
  ;;  (setq buffer-mode-map (make-keymap))
  ;;  (use-local-map buffer-mode-map)
  (run-hooks 'empty-booting-hook))
;; (setq initial-buffer-choice t) ;; 引数付き起動すると画面分割される
(setq initial-scratch-message nil)
(setq initial-major-mode 'empty-booting-mode)
(set-face-foreground 'header-line "#FFFFFF") ;; "#203e6f" #333333 "#FFFFFF"
(set-face-background 'header-line "#a46398") ;; "#ffb08c" "#7e59b5" ##5F7DB7
(set-face-attribute 'header-line nil
                    :inherit nil
                    :overline nil
                    :underline nil)

;; (advice-add 'split-window-below :after #'ad:split-window-below)
(if (< emacs-major-version 29)
    (keymap-global-set "C-M-s" #'my-open-scratch)
  (keymap-global-set "C-M-s" #'scratch-buffer))

;; Disable to show the splash window at startup
(setq inhibit-startup-screen t)

(setq inhibit-default-init t)

(autoload 'org-eval-in-calendar "org" nil t)

(my-tick-init-time "presentation")

;; (my-tick-init-time "media")

(setq history-length 2000)

(setq undo-outer-limit nil)

(when (autoload-if-found '(counsel-recentf)
                         "counsel" nil t)
  (keymap-global-set "C-M-r" 'counsel-recentf))

;; see https://github.com/mattfidler/EmacsPortable.App/issues/7
(when (eq system-type 'darwin)
  ;; Dropbox のエイリアスを展開されないようにする
  ;; find-file での表示も短縮される．
  (let ((provider (expand-file-name "~/Library/CloudStorage/")))
    (setq directory-abbrev-alist
	        `((,(concat "\\`" provider "Dropbox") . "~/Dropbox")))))

;; *.~
(setq make-backup-files nil)
;; .#*
(setq auto-save-default nil)
;; auto-save-list
(setq auto-save-list-file-prefix nil)

(when (autoload-if-found '(session-initialize)
                         "session" nil t)
  (unless (or noninteractive my-secure-boot)
    (add-hook 'after-init-hook #'session-initialize))
  (with-eval-after-load "session"
    (add-to-list 'session-globals-include 'ivy-dired-history-variable)
    (add-to-list 'session-globals-exclude 'org-mark-ring)
    (setq session-set-file-name-exclude-regexp "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|[/\\]COMMIT_EDITMSG")
    ;; Change save point of session.el
    (setq session-save-file
          (expand-file-name (concat (getenv "SYNCROOT") "/emacs.d/.session"))
          session-initialize '(de-saveplace session keys menus places)
          session-globals-include '((kill-ring 100)
                                    (session-file-alist 100 t)
                                    (file-name-history 200)
                                    ivy-dired-history-variable
                                    search-ring
                                    regexp-search-ring))
    (setq session-undo-check -1)))

(my-tick-init-time "history")

(keymap-global-set "C-;" 'comment-dwim) ;; M-; is the defualt
(keymap-global-set "C-c c" 'compile)

;; ホームポジション的な Orgファイルを一発で開きます．
(keymap-global-set "C-M-o" #'my-open-default-org-file)

(my-tick-init-time "development")

(cond
 ((memq window-system '(mac ns)) ;; for macOS
  (setq initial-frame-alist
	      (append
	       '((alpha . (100 95))
           ;; (top . 23)
	         ;; (left . 0)
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
(set-face-background 'fringe "#FFFFFF") ;; 10-20[ms]

;; カーソルの色
(defconst my-cur-color-ime '(:on "#FF9300" :off "#91C3FF"))
(defconst my-cur-type-ime '(:on (bar . 2) :off (bar . 2) :invisible nil))

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

(defun my-apply-cursor-config ()
  (interactive)
  (when (display-graphic-p)
    (if (my-ime-active-p) (my-ime-on-cursor) (my-ime-off-cursor))))

;; for init setup
(setq-default cursor-type (plist-get my-cur-type-ime :on))
(unless noninteractive
  ;; (2024-02-05) org から calendar を呼ぶときにカーソルが残ってしまうので，しばらく下記のhookを停止する
  ;; (add-hook 'buffer-list-update-hook #'my-apply-cursor-config)
  (my-apply-cursor-config))
(add-hook 'input-method-activate-hook #'my-ime-on-cursor)
(add-hook 'input-method-deactivate-hook #'my-ime-off-cursor)

(defconst moom-autoloads
  '(moom-cycle-frame-height
    moom-move-frame-right moom-move-frame-left moom-move-frame
    moom-move-frame-to-edge-top moom-move-frame-to-edge-bottom
    my-frame-reset moom-move-frame-to-center
    moom-fill-band moom-cycle-monitors))

(when (autoload-if-found moom-autoloads
                         "moom" nil t)
  (keymap-global-set "C-1" 'moom-move-frame-to-edge-top)
  (keymap-global-set "C-!" 'moom-move-frame-to-edge-bottom)
  (keymap-global-set "C-2" 'moom-cycle-frame-height)
  (keymap-global-set "M-2" 'moom-move-frame-to-center)
  (keymap-global-set "M-9" 'moom-cycle-monitors)
  (keymap-global-set "C-c f m" 'moom-fill-band)

  (autoload 'moom-transient-dispatch "moom-transient" nil t)
  (keymap-global-set "C-c o" #'moom-transient-dispatch)

  (with-eval-after-load "moom-transient"
    (moom-transient-hide-cursor)
    (setopt moom-transient-dispatch-sticky nil)
    (advice-add 'moom-transient-dispatch :after #'my-ime-off)) ;; FIXME

  (with-eval-after-load "moom"
    (add-hook 'moom-split-window-hook #'dimmer-permanent-off)
    (add-hook 'moom-delete-window-hook #'dimmer-on)
    (add-hook 'moom-after-select-monitor-hook #'moom-move-frame-to-center)

    (setopt moom-command-with-centering nil
            moom-lighter "M"
            moom-verbose t)
    (moom-recommended-keybindings '(all wof))
    (moom-mode 1)
    (my-font-config))) ;; this could increase `postpone-pre-init-time'.

(my-tick-init-time "frame and window")

;; (my-tick-init-time "font")

(when (autoload-if-found '(counsel-osx-app)
                         "counsel-osx-app" nil t)
  (keymap-global-set "C-M-1" 'counsel-osx-app)
  (with-eval-after-load "counsel-osx-app"
    (setq counsel-osx-app-location
          '("/Applications" "/Applications/Utilities"
            "/System/Applications" "/System/Applications/Utilities"
            "/Applications/Microsoft Remote Desktop.localized"))))

(keymap-global-set "C-c 0" 'insert-formatted-current-date)
(keymap-global-set "C-c 9" 'insert-formatted-current-time)

;; (my-tick-init-time "utility")
(when (and (boundp my-profiler-p)
		       my-profiler-p)
	(profiler-report))

(provide 'init)
