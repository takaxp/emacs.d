;; recentf (C-M-r) / helm-swoop (M-s M-s) / isearch (C-s) / bm <f10>, C-<f10>
;; helm-locate (C-M-l) / org-grep (C-M-g) / ag (C-M-f) / google-this (C-c f g)
;; fullscreen <f11> / double-width (C-c f d)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst before-load-init-time (current-time)) ;; see my:load-init-time
(defconst profiler nil)
(defconst ad-require nil)
(defconst loading-packages nil)
(defconst debug nil)
(setq debug-on-error nil)

(when ad-require
  (defadvice load (around require-benchmark activate)
    (let* ((before (current-time))
           (result ad-do-it)
           (after  (current-time))
           (time (+ (* (- (nth 1 after) (nth 1 before)) 1000)
                    (/ (- (nth 2 after) (nth 2 before)) 1000))))
      (message "--- %04d [ms]: (loading) %s" time (ad-get-arg 0))))
  (defadvice require (around require-benchmark activate)
    "http://memo.sugyan.com/entry/20120105/1325756767"
    (let* ((before (current-time))
           (result ad-do-it)
           (after  (current-time))
           (time (+ (* (- (nth 1 after) (nth 1 before)) 1000)
                    (/ (- (nth 2 after) (nth 2 before)) 1000))))
      (message "--- %04d [ms]: %s" time (ad-get-arg 0)))))

(when profiler (profiler-start 'cpu+mem))

;; Suppress exporting of custom-set-variables (from 25.1)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; or you can use `exec-path-from-shell'.
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; Utility // M-x list-load-path-shadows ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-path-setter (path-list target-path)
  (dolist (x path-list) (add-to-list target-path (file-name-as-directory x))))

;; (1) theme-path
(defconst cask-package-dir (format "~/.emacs.d/.cask/package/%s" emacs-version))
(load-path-setter `(,cask-package-dir) 'custom-theme-load-path)

;; (2) exec-path
(load-path-setter
 `("/usr/bin" "/usr/local/bin"
   ,(expand-file-name ".cask/bin")
   ,(expand-file-name "devel/git/tern/bin")
   ,(expand-file-name "Dropbox/emacs.d/bin")
   "/Applications/UpTex.app/teTeX/bin"
   "/Applications/LibreOffice.app/Contents/MacOS/")
 'exec-path)

;; (3) load-path for { debug | normal } booting
(defconst init-load-path nil)
(cond
 (debug
  (load-path-setter '("~/Dropbox/config") 'load-path)
  (require 'my-debug))
 (t
  (let*
      ((p "~/Dropbox/emacs.d/")
       (g "~/devel/git/")
       ;; (od "org-mode")
       (od "org-9.0")
       (l `("~/Dropbox/emacs.d/config"
            "~/Dropbox/config"
            ,p
            ,(concat p "livemirror")
            ,(concat g "git-complete")
            ,(concat g od "/lisp")
            ,(concat g od "/contrib/lisp"))))
    (load-path-setter l 'load-path)
    (load-path-setter `(,cask-package-dir) 'load-path)
    (setq init-load-path load-path))

  (require 'init nil t)         ;; Less than 500[ms], Cocoa: 1000[ms]
  (require 'my-eshell nil t)    ;; 0[ms]
  (require 'my-mail nil t)      ;; 0[ms]
  (require 'private nil t)
  (when profiler (profiler-report)))) ;; 0[ms] This package depends on init.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my:list-packages ()
  (interactive)
  (my:setup-cask)
  (if (fboundp 'paradox-list-packages)
      (paradox-list-packages nil)
    (list-packages nil)))

(defun my:setup-cask ()
  (interactive)
  (when (or (require 'cask "/usr/local/opt/cask/cask.el" t)
            (require 'cask "~/.cask/cask.el" t))
    (setq load-path (cask-load-path (cask-initialize)))
    (when (require 'pallet nil t)
      (pallet-mode t))))

(defun my:reset-load-path ()
  (interactive)
  (shell-command-to-string "~/Dropbox/emacs.d/bin/update-cask.sh link")
  (setq load-path init-load-path)
  (message "--- Reverted to the original `load-path'."))

(with-eval-after-load "paradox"
  (defun advice:paradox-quit-and-close (kill)
    (my:reset-load-path))
  (advice-add 'paradox-quit-and-close :after #'advice:paradox-quit-and-close))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff を取る前に tabify スペースをタブに変換する．今は全てスペース置換中．
;; この設定でファイルを一度編集後に，M-x tabify しないとだめ．
;; (add-hook 'emacs-lisp-mode-hook
;;           '(lambda ()
;;              (setq indent-tabs-mode t)
;;              (setq tab-width 8)
;;              (setq indent-line-function 'lisp-indent-line)))

