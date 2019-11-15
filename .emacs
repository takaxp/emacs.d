;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
;; (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el"))
;;   ln -s ~/Dropbox/emacs.d/config/.spacemacs ~/
;;   git clone https://github.com/syl20bnr/spacemacs ~/.spacemacs.d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME



;; For blog entry
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-10/msg00245.html
;; 以下の事例は， M-x はこれで影響を受けるが，(message "1\n2") は無視される．
;; (defun my-minibuffer-line-spacing ()
;;   (setq line-spacing 1.5))
;; (add-hook 'minibuffer-setup-hook #'my-minibuffer-line-spacing)
;; (propertize
;; (concat
;;  (propertize (concat
;;               (when my-bs-separater
;;                 (concat (make-string (frame-width) ?_) "\n"))
;;               "Next buffers: ")
;;              'face 'minibuffer-prompt)
;;  (or (my-bs-cycle-list) "this buffer"))
;; 'line-spacing nil)

(with-eval-after-load "bs"
  (defvar my-bs-max-height 9)
  (defvar my-bs-slant t)
  (defvar my-bs-separater t)

  ;; To avoid conflict on echo area
  (defun my-flyspell-deactivate ()
    "Deactivate `flyspell' till moving the cursor."
    (flyspell-mode -1)
    (add-hook 'ah-after-move-cursor-hook #'my-flyspell-activate))
  (defun my-flyspell-activate ()
    "Activate `flyspell'."
    (flyspell-mode 1)
    (remove-hook 'ah-after-move-cursor-hook #'my-flyspell-activate))
  (defun my-paren-deactivate ()
    "Deactivate `mic-paren' till moving the cursor."
    (paren-deactivate)
    (add-hook 'ah-after-move-cursor-hook #'my-paren-activate))
  (defun my-paren-activate ()
    "Activate `mic-paren'."
    (paren-activate)
    (remove-hook 'ah-after-move-cursor-hook #'my-paren-activate))

  (defun my-bs-cycle-list (&optional reverse)
    (if (cdr bs--cycle-list)
        (let* ((bs-list (if reverse
                            (reverse (cdr bs--cycle-list))
                          (cdr bs--cycle-list)))
               (len (min my-bs-max-height
                         (length bs-list)
                         (/ (frame-height) 6))) ;; FIXME
               (bs-limit-list nil)
               (str nil))
          (while (> len 0)
            (push (format "%d. %s%s"
                          (1+ (length bs-limit-list))
                          ;; (if my-bs-slant (make-string (1- len) ?\s) "")
                          (if my-bs-slant (make-string
                                           (length bs-limit-list) ?\s) "")
                          (car bs-list))
                  bs-limit-list)
            (setq bs-list (cdr bs-list))
            (setq len (1- len)))
          (unless reverse
            (setq bs-limit-list (reverse bs-limit-list)))
          (setq str (format "%s" (mapcar
                                  (lambda (buffer)
                                    (format "\n%s" buffer))
                                  bs-limit-list)))
          (substring str 1 (1- (length str))))
      nil))

  (defun my-bs-cycle-next ()
    "Select next buffer defined by buffer cycling.
The buffers taking part in buffer cycling are defined
by buffer configuration `bs-cycle-configuration-name'."
    (interactive)
    (let ((bs--buffer-coming-from (current-buffer))
          (bs-dont-show-regexp   bs-dont-show-regexp)
          (bs-must-show-regexp   bs-must-show-regexp)
          (bs-dont-show-function bs-dont-show-function)
          (bs-must-show-function bs-must-show-function)
          (bs--show-all          nil))
      (bs-set-configuration (or bs-cycle-configuration-name bs-default-configuration))
      (let ((bs-buffer-sort-function nil)
            (bs--current-sort-function nil))
        (let* ((tupel (bs-next-buffer (if (or (eq last-command
                                                  'bs-cycle-next)
                                              (eq last-command
                                                  'bs-cycle-previous))
                                          bs--cycle-list)))
               (next (car tupel))
               (cycle-list (cdr tupel)))
          ;; We don't want the frame iconified if the only window in the frame
          ;; happens to be dedicated.
          (bury-buffer (current-buffer))
          (switch-to-buffer next nil t)
          (setq bs--cycle-list (append (cdr cycle-list)
                                       (list (car cycle-list))))
          (my-paren-deactivate)
          (my-flyspell-deactivate)
          (bs-message-without-log
           (concat
            (propertize (concat
                         (when my-bs-separater
                           (concat (make-string (frame-width) ?_) "\n"))
                         "Next buffers: ")
                        'face 'minibuffer-prompt)
            (or (my-bs-cycle-list) "this buffer")))))))

  (defun my-bs-cycle-previous ()
    "Select previous buffer defined by buffer cycling.
The buffers taking part in buffer cycling are defined
by buffer configuration `bs-cycle-configuration-name'."
    (interactive)
    (let ((bs--buffer-coming-from (current-buffer))
          (bs-dont-show-regexp   bs-dont-show-regexp)
          (bs-must-show-regexp   bs-must-show-regexp)
          (bs-dont-show-function bs-dont-show-function)
          (bs-must-show-function bs-must-show-function)
          (bs--show-all          nil))
      (bs-set-configuration (or bs-cycle-configuration-name bs-default-configuration))
      (let ((bs-buffer-sort-function nil)
            (bs--current-sort-function nil))
        (let* ((tupel (bs-previous-buffer (if (or (eq last-command
                                                      'bs-cycle-next)
                                                  (eq last-command
                                                      'bs-cycle-previous))
                                              bs--cycle-list)))
               (prev-buffer (car tupel))
               (cycle-list (cdr tupel)))
          (switch-to-buffer prev-buffer nil t)
          (setq bs--cycle-list (append (last cycle-list)
                                       (reverse (cdr (reverse cycle-list)))))
          (my-paren-deactivate)
          (my-flyspell-deactivate)
          (bs-message-without-log
           (concat
            (propertize (concat
                         (when my-bs-separater
                           (concat (make-string (frame-width) ?_) "\n"))
                         "Previous buffers: ")
                        'face 'minibuffer-prompt)
            (or (my-bs-cycle-list t) "this buffer")))))))

  (defun my-bs-message-without-log (&rest args)
    "Like `message' but don't log it on the message log.
All arguments ARGS are transferred to function `message'."
    (let ((message-log-max nil))
      (apply 'minibuffer-message args)))

  (advice-add 'bs-cycle-next :override #'my-bs-cycle-next)
  (advice-add 'bs-cycle-previous :override #'my-bs-cycle-previous)
  (setq minibuffer-message-timeout 3)
  (advice-add 'bs-message-without-log :override #'my-bs-message-without-log))

;; see init-env.el
;; (when (require 'benchmark-init-modes nil t)
;;   (benchmark-init/show-durations-tabulated))

;; posframe - testing

;; (set-frame-parameter (selected-frame) 'internal-border-width 10)
;; (set-face-background 'internal-border "#FF00FF")
;; Could be useful.
;; (when (require 'mini-modeline nil t)
;;   (mini-modeline-mode 1))

(with-eval-after-load "posframe"
  (custom-set-faces
   '(internal-border
     ((((background dark)) :background "#FF0000")
      (t (:background "#FF0000")))))

  (with-eval-after-load "frame"
    (advice-remove 'make-frame #'ad:make-frame))

  (setq my-string "hoge.
hoge.")

  (when (and nil
             (require 'posframe nil t)
             (posframe-workable-p))
    (posframe-show
     "test"
     :string my-string
     :background-color "black"
     :foreground-color "green"
     :internal-border-width 1
     :internal-border-color "red"))
  )


;; ivy-posframe
(when (and nil
           (require 'ivy-posframe nil t))
  (defun my-toggle-ivy-posframe ()
    "Toggle `ivy-posframe'."
    (interactive)
    (ivy-posframe-mode (if ivy-posframe-mode -1 1)))
  (setq ivy-posframe-border-width
        (* 2 (cdr (assoc 'internal-border-width (frame-geometry)))))
  (setq ivy-posframe-hide-minibuffer nil)
  ;;  (setq ivy-posframe-border ((t (:background "#6272a4"))))
  (setq ivy-posframe-parameters
        '((left-fringe . (frame-parameter nil 'left-fringe))
          (right-fringe . (frame-parameter nil 'right-fringe))))
  (setq ivy-posframe-display-functions-alist
        '((counsel-M-x . ivy-posframe-display-at-point)
          (t           . ivy-posframe-display)))
  (set-face-background 'internal-border "#FF0000")
  (set-frame-parameter (selected-frame) 'internal-border-width 10)
  (ivy-posframe-mode 1)
  (set-face-background 'internal-border "#FF0000")
  (set-frame-parameter (selected-frame) 'internal-border-width 10))

(when nil
  (set-face-background 'fringe "green")
  (set-face-background 'default "#999999")
  (set-face-foreground 'default "#FFFFFF")
  (frame-parameter (selected-frame) 'foreground-color)
  (frame-parameter (selected-frame) 'background-color)

  (set-frame-parameter (selected-frame) 'border-width '20)
  (frame-parameter (selected-frame) 'border-width)

  (set-frame-parameter (selected-frame) 'outer-border-width '30)
  (frame-parameter (selected-frame) 'outer-border-width)
  p
  (frame-parameter (selected-frame) 'frame-width)

  (set-frame-parameter (selected-frame) 'border-color "#FF0000")
  (frame-parameter (selected-frame) 'border-color)

  (frame-parameter (selected-frame) 'internal-border-width)
  (frame-parameter (selected-frame) 'internal-frame-width)

  (modify-frame-parameters (selected-frame) '((border-width . 20)))

  (insert (format "%s" (frame-parameters))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (with-eval-after-load "company"
;;   (when (and (require 'all-the-icons nil t)
;;              (require 'company-box nil t))
;;     (add-hook 'company-mode-hook 'company-box-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trying LSP
;; https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
(when (and (fboundp 'lsp)
           (autoload-if-found '(lsp) "lsp-mode" nil t))

  (add-hook 'c-mode-common-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)

  (with-eval-after-load "lsp"
    (setq lsp-prefer-flymake nil)
    (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error")))

  (with-eval-after-load "lsp-ui"
    (setq lsp-ui-doc-enable t
          lsp-ui-doc-use-childframe t
          lsp-ui-doc-position 'top
          lsp-ui-doc-include-signature t
          lsp-ui-sideline-enable nil
          lsp-ui-flycheck-enable t
          lsp-ui-flycheck-list-position 'right
          lsp-ui-flycheck-live-reporting t
          lsp-ui-peek-enable t
          lsp-ui-peek-list-width 60
          lsp-ui-peek-peek-height 25)))

;; Trying... but...
;; (when (require 'dumb-jump nil t)
;;   (dumb-jump-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;; (advice-add 'epg--check-error-for-decrypt :override 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load "postpone"
  (defun ad:message (f FORMAT-STRING &rest ARGS)
    (let ((str (concat (make-string (frame-width) ?\x5F) "\n" FORMAT-STRING)))
      (apply f str ARGS)))
  ;; (advice-add 'message :around #'ad:message)
  ;; (advice-remove 'message #'ad:message)
  )

;; .emacs ends here
(put 'narrow-to-region 'disabled nil)
