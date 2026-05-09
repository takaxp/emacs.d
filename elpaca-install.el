;; elpaca-install.el --- -*- lexical-binding: t; -*-

;; Constructing `load-path' for normal emacs session
(load (concat user-emacs-directory "early-init.el"))

;; Disable NativeComp
(let ((enable nil))
  (setq native-comp-jit-compilation enable
	native-comp-enable-subr-trampolines enable))

;; Disable package loading by package.el
(setq package-enable-at-startup nil)

;; Changing the frame width to view elpaca-log window
(when (display-graphic-p)
  (set-frame-width nil 155)
  (set-frame-position nil 0 0)
  (raise-frame))

;;(add-to-list 'elpaca-recipe-functions
;;             (lambda (recipe)
;;               (list :build '(:not elpaca--byte-compile elpaca-activate))))

(setq elpaca-busy-interval 300)
(setq elpaca-queue-limit 16)
;; [250 packages]
;; 100 1:58
;; 64  1:13
;; 32  1:12, 1:05
;; 28  1:01
;; 24  0:59, 1:00
;; 20  0:59
;; 16  1:07, 1:05
;; 12  1:18
;; 8   1:57

;; kill-emacs when the process is completed. see elpaca-config.el
;; (add-hook 'elpaca-post-queue-hook #'my-elpaca-post-process)
(defun my-elpaca-post-process ()
  (interactive)
  ;; (message "elpaca--waiting: %s" elpaca--waiting)
  ;; (message "elpaca--queues: %s" (length elpaca--queues))
  (when (fboundp 'my-elpaca-save-load-path)
    (message "--- saving load-path")
    (my-elpaca-save-load-path))
  ;; If you kill emacs here, do not use ":wait t" anywhere
  ;; (kill-emacs)
  )
(add-hook 'kill-emacs-hook #'my-elpaca-post-process)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elpaca installer
(defvar elpaca-installer-version 0.12)
;; `elpaca-directory' is defined in my early-init.el
;; (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el"
						(:exclude "extensions"))
			      :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process
				 `("git" nil ,buffer t "clone"
                                   ,@(when-let*
					 ((depth (plist-get order :depth)))
                                       (list (format "--depth=%d" depth)
					     "--no-single-branch"))
                                   ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process
			   emacs nil buffer nil "-Q" "-L" "." "--batch"
                           "--eval"
			   "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
;; (add-hook 'after-init-hook #'elpaca-process-queues) ;; moved to bottom
(elpaca `(,@elpaca-order))

(provide 'elpaca-install)
