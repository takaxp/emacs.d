;;                                          Takaaki ISHIKAWA <takaxp@ieee.org>
;;                                          https://takaxp.github.io/init.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              TODO/DONE/FIXME
(cond (nil ;; To test the latest org
       (add-to-list 'load-path (expand-file-name "~/devel/git/org-mode/lisp"))
       (setq org-agenda-files '("~/Desktop/hoge.org")))
      (t ;; Normal mode. see also init-eval.el
       (load "~/Dropbox/emacs.d/config/init-env.el" nil t)))
;; (load (concat (setq user-emacs-directory "~/.spacemacs.d/") "init.el"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://en.wikipedia.org/wiki/Darwin_(operating_system)
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
(defun macos-version ()
  (let ((macos-type-version (nth 2 (split-string system-configuration "-"))))
		(string-match "darwin\\(.*\\)" macos-type-version)
		(match-string 1 macos-type-version)))

;; At least in Big Sur, this setting shall be used with side car for moom.el.
;; Without side car in Big Sur, the following setting is also correct.
;; Then what about other macOSs?
(when (string= "Big Sur" (macos-name (macos-version)))
  (setq moom--common-margin '(0 0 0 0)))

(with-eval-after-load "org-tree-slide"
  (defvar my-hide-org-meta-line-p nil)
  (defun my-hide-org-meta-line ()
    (interactive)
    (setq my-hide-org-meta-line-p t)
    (set-face-attribute 'org-meta-line nil
			                  :foreground (face-attribute 'default :background)))
  (defun my-show-org-meta-line ()
    (interactive)
    (setq my-hide-org-meta-line-p nil)
    (set-face-attribute 'org-meta-line nil :foreground nil))

  (defun my-toggle-org-meta-line ()
    (interactive)
    (if my-hide-org-meta-line-p
	      (my-show-org-meta-line) (my-hide-org-meta-line)))

  (add-hook 'org-tree-slide-play-hook #'my-hide-org-meta-line)
  (add-hook 'org-tree-slide-stop-hook #'my-show-org-meta-line)

  ;; Option
  (defun my-update-org-meta-line ()
    (interactive)
    (when my-hide-org-meta-line-p
      (my-hide-org-meta-line)))
  (add-hook 'ah-after-enable-theme-hook #'my-update-org-meta-line))

(with-eval-after-load "keypression"
  ;; (setq keypression-frame-origin 'keypression-origin-bottom-right)
  (setq keypression-frame-origin 'keypression-origin-top-left)
  (setq keypression-x-offset (- (frame-pixel-width) 100))
  (setq keypression-y-offset 10))

(with-eval-after-load "postpone"
  ;; elfeed, elfeed-org,elfeed-web
  (when (autoload-if-found
         '(elfeed elfeed-web-start)
         "elfeed" nil t)

    (with-eval-after-load "elfeed"
      (when (require 'elfeed-org nil t)
        (elfeed-org)
        (setq rmh-elfeed-org-files (list "~/Dropbox/org/elfeed.org")))
      ;; これで elfeed-feeds が更新される
      ;; その後，M-x elfeed, M-x elfeed-update する
      (when (require 'elfeed-web nil t)
        (setq elfeed-web-data-root (concat my-elget-package-dir "/web"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To decrypt old sub trees
;;(advice-add 'epg--check-error-for-decrypt :override 'ignore)
