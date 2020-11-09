;; (load "c:/cygwin64/home/takaaki/init-win.el" nil t)

(when (eq system-type 'windows-nt)
  (let ((base-system "cygwin64")) ;; msys64
    (setenv "HOME" (format "C:\\%s\\home\\taka" base-system))
    (setenv "PATH"
            (concat (getenv "PATH")
                    (format ";C:\\%s\\usr\\local\\bin" base-system)
                    (format ";C:\\%s\\opt\\bin" base-system)
                    (format ";C:\\%s\\usr\\bin" base-system)
                    (format ";C:\\%s\\bin" base-system)
                    (format ";C:\\%s\\mingw64\\bin" base-system)))
    (setq shell-file-name "C:/cygwin64/bin/bash")))
(load "~/Dropbox/emacs.d/config/init-env.el" nil t) ;; see also init-eval.el
(push '("\\.txt$" . org-mode) auto-mode-alist)
(menu-bar-mode -1)
(tool-bar-mode -1)
