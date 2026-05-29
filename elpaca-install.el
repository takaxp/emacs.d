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
  (select-frame-set-input-focus (selected-frame)))

;; (setq elpaca-busy-interval 300)
(setq elpaca-queue-limit 16)

;; |     |      v0.12 | v0.2 |
;; |     |        250 |  174 |
;; |-----+------------+------|
;; | 100 |       1:58 | 1:03 |
;; |  64 |       1:13 | 1:00 |
;; |  32 | 1:12, 1:05 | 0:57 |
;; |  28 |       1:01 | 1:05 |
;; |  24 | 0:59, 1:00 | 0:57 |
;; |  20 |       0:59 | 0:52 |
;; |  16 | 1:07, 1:05 | 0:52 |
;; |  12 |       1:18 | 0:52 |
;; |   8 |       1:57 | 1:12 |

(defun my-elpaca-post-process ()
  (interactive)
  (when (fboundp 'my-elpaca-save-load-path)
    (message "--- saving load-path")
    (my-elpaca-save-load-path)))
(add-hook 'kill-emacs-hook #'my-elpaca-post-process)

(defvar my-elpaca-kill-emacs-count 4) ;; Number. If nil, skip `kill-emacs'.
(defun my-elpaca-kill-emacs1 ()
  "partially taken from `elpaca-ui--progress-bar'."
  (when my-elpaca-kill-emacs-count
    (cl-loop
     with total = 0 with finalized = 0
     for s in '(finished blocked failed other)
     for plen = (elpaca-alist-get s elpaca--status-counts 0)
     do
     (setq total (+ total plen))
     (when (memq s '(finished failed))
       (cl-incf finalized plen))
     ;; (message "total:%s finalized:%s plen:%s counts:%s" total finalized plen elpaca--status-counts)
     (when (and (equal total finalized)
		(eq (car (car elpaca--status-counts)) 'finished)
		(eq plen 0))
       (cancel-timer my-elpaca-kill-emacs-timer)
       (dotimes (count my-elpaca-kill-emacs-count)
	 (message "%s" (- my-elpaca-kill-emacs-count count))
	 (sleep-for 1))
       (kill-emacs)))))

(defun my-elpaca-kill-emacs ()
  "For v0.2 or later. partially taken from `elpaca-ui--progress-bar'."
  (when my-elpaca-kill-emacs-count
    (cl-loop
     with total = 0 with finalized = 0
     for s in (sort (mapcar #'car elpaca--status-counts))
     for plen = (elpaca-alist-get s elpaca--status-counts 0)
     do
     (setq total (+ total plen))
     (when (memq s '(finished failed))
       (cl-incf finalized plen))
     ;; (message "total:%s finalized:%s plen:%s counts:%s" total finalized plen elpaca--status-counts)
     (when (and (eq (car (car elpaca--status-counts)) 'finished)
		(equal total finalized))
       (cancel-timer my-elpaca-kill-emacs-timer)
       (dotimes (count my-elpaca-kill-emacs-count)
	 (message "%s" (- my-elpaca-kill-emacs-count count))
	 (sleep-for 1))
       (kill-emacs)))))

(defun my-elpaca-post-queue () (message "--- done (queue)"))
(defun my-elpaca-after-init ()
  (message "--- %s done (elpaca after init)" elpaca-after-init-time))
;; (add-hook 'elpaca-post-queue-hook #'my-elpaca-post-queue)
;; (add-hook 'elpaca-after-init-hook #'my-elpaca-after-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the elpaca installer --- https://github.com/progfolio/elpaca#installer
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
