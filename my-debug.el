(setq make-backup-files nil)
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq save-buffer-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)
(set-locale-environment "en_US.UTF-8") ; "ja_JP.UTF-8"
(when (eq window-system 'ns)
        ;; Paste command by Command+v 
  (global-set-key (kbd "M-v") 'yank)
  (setq ns-command-modifier (quote meta))
  (set ns-alternate-modifier (quote super))
  ;; Drag and Drop config. for Emacs 23 (2010-07-20)
  (global-set-key [ns-drag-file] 'ns-find-file))
(setq next-line-add-newlines nil)
(setq require-final-newline t)
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)
(setq scroll-conservatively 1000)
(setq scroll-step 1)
(setq scroll-margin 0) ; default=0
(setq next-screen-context-lines 1)
(require 'time-stamp nil t)
(add-hook 'before-save-hook 'time-stamp)
(eval-after-load "time-stamp"
  '(progn
     (setq time-stamp-active t)
     (setq time-stamp-start "Last Update: ")
     (setq time-stamp-format "%04y-%02m-%02d@%02H:%02M")
     (setq time-stamp-end "$")
     (setq time-stamp-line-limit 10)))
(autoload 'word-count-mode "word-count" "Minor mode to count words." t)
(add-hook 'change-log-mode-hook
          '(lambda() (setq tab-width 4) (setq left-margin 4)))
(cua-mode t)
(setq cua-enable-cua-keys nil)
(setq visible-bell t)
(defcustom echo-area-bell-string " BEEP ";
  "Message displayed in mode-line by `echo-area-bell' function."
  :group 'user)
(defcustom echo-area-bell-delay 0.1
  "Number of seconds `echo-area-bell' displays its message."
  :group 'user)
;; internal variables
(defvar echo-area-bell-cached-string nil)
(defvar echo-area-bell-propertized-string nil)
(defun echo-area-bell ()
  "Briefly display a highlighted message in the echo-area.
   The string displayed is the value of `echo-area-bell-string',
   with a red background; the background highlighting extends to the
   right margin.  The string is displayed for `echo-area-bell-delay'
   seconds.
   This function is intended to be used as a value of `ring-bell-function'."
  (unless (equal echo-area-bell-string echo-area-bell-cached-string)
    (setq echo-area-bell-propertized-string
          (propertize
           (concat
            (propertize
             "x"
             'display
             `(space :align-to (- right ,(+ 2 (length echo-area-bell-string)))))
            echo-area-bell-string)
           ;; 'face '(:background "red")
           ;; 'face '(:foreground "#FFFFFF" :background "#FF4040")
           ;; 'face '(:foreground "#FFFFFF" :background "#C1252D")
           ;; 'face '(:foreground "#FFFFFF" :background "#FD8A4B")
           'face '(:foreground "#FFFFFF" :background "#FF7D7D")))
    (setq echo-area-bell-cached-string echo-area-bell-string))
  (message echo-area-bell-propertized-string)
  (sit-for echo-area-bell-delay)
  (message ""))
(setq ring-bell-function 'echo-area-bell)
(line-number-mode t)
(set-frame-width (selected-frame) 81)
(set-frame-width (selected-frame) 80)
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key (kbd "C-;") 'comment-dwim) ;; M-; is the defualt
(global-set-key (kbd "C-M-t") 'beginning-of-buffer)
(global-set-key (kbd "C-M-b") 'end-of-buffer)
(global-set-key (kbd "M-p") 'scroll-down)
(global-set-key (kbd "M-n") 'scroll-up)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-M-p") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-M-n") '(lambda () (interactive) (other-window 1)))
(global-set-key (kbd "M-]") 'cycle-buffer)
(global-set-key (kbd "M-[") 'cycle-buffer-backward)
(global-set-key (kbd "M-+") 'word-count-mode)
(provide 'my-debug)
