;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking installed libraries
;; (benchmark-run 100 (my-async-locate-libraries '("org")))  ;; 2[ms]

;;;###autoload
(defun my-async-locate-libraries (libraries &optional defer)
  "Check the library listed in `LIBRARIES'."
  (if (require 'async nil t)
      (async-start
       `(lambda ()
          (sleep-for (or ',defer 10))
          (setq load-path ',load-path)
          (let ((alist nil))
            (mapc (lambda (library)
                    (let ((path (locate-library library)))
                      (unless path
                        (add-to-list 'alist (format "%s" library)))))
                  (if (listp ',libraries)
                      ',libraries
                    (list ',libraries)))
            alist))
       (lambda (result)
         (let ((inhibit-message nil)
               (message-log-max 5000))
           (when result
             (unless (active-minibuffer-window)
               (let ((count 0))
                 (dolist (r result)
                   (setq count (1+ count))
                   (message ">> %s (missing)" r))
                 (message (concat (format "[async] %s package" count)
                                  (if (> count 1) "s are" " is")
                                  " NOT installed."))))))))
    (error "missing async.el")))

;;;###autoload
(defun my-find-missing-packages (&optional defer)
  (interactive)
  (my-async-locate-libraries my-required-libraries (or defer 0)))

;;;###autoload
(defun my-delete-old-backup (&optional defer)
  (if (not (require 'async nil t)) ;; 5[ms]
      (recursive-delete-backup-files 7)
    (async-start ;; do not call this from byte compiled code directory
     `(lambda ()
        (sleep-for (or ',defer 5))
        (when (load (concat (getenv "HOME") "/.emacs") t)
          (recursive-delete-backup-files 7)
          t))
     (lambda (result)
       (if result
           (let ((inhibit-message nil)
                 (message-log-max 5000))
             (unless (eval '(active-minibuffer-window))
               (message "[async] Deleting old backup files...done")))
         (error "[async] Failed to delete backup files."))))))

(provide 'init-async)
;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8
;; End:
;;; init-async.el ends here
