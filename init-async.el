;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking installed libraries
;; (benchmark-run 100 (my-async-locate-libraries '("org")))  ;; 2[ms]
(defun my-async-locate-libraries1 (libraries)
  "Check the library listed in `LIBRARIES'.
This function could create many sub processes."
  (message "Missing libraries in this system.")
  (if (require 'async nil t)
      (mapc (lambda (library)
              (async-start
               `(lambda ()
                  (setq load-path ',load-path)
                  (let ((path (locate-library ',library)))
                    (if path
                        ;;                        (format "[ OK ] %s" path)
                        nil
                      (format ">> %s (missing)" ',library))))
               (lambda (result)
                 (when result
                   (message "%s" result)))))
            (if (listp libraries)
                libraries
              (list libraries)))
    (error "missing async.el"))
  (message "done."))

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
         (when (and result
                    (not (active-minibuffer-window)))
           (let ((count 0))
             (dolist (r result)
               (setq count (1+ count))
               (message ">> %s (missing)" r))
             (message (concat (format "[async] %s package" count)
                              (if (> count 1) "s are" " is")
                              " NOT installed."))))))
    (error "missing async.el")))

(defun my-find-missing-packages (&optional defer)
  (interactive)
  (my-async-locate-libraries my-required-libraries (or defer 0)))

(defun my-delete-old-backup (&optional defer)
  (if (not (require 'async nil t)) ;; 5[ms]
      (recursive-delete-backup-files 7)
    (async-start ;; do not call this from byte compiled code directory
     `(lambda ()
        (sleep-for (or ',defer 5))
        (message "[async] Deleting old backup files...")
        (when (load (concat (getenv "HOME") "/.emacs") t)
          (recursive-delete-backup-files 7)
          t))
     (lambda (result)
       (if result
           (unless (active-minibuffer-window)
             (message "[async] Deleting old backup files...done"))
         (error "[async] Failed to delete backup files."))))))

(provide 'init-async)
;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8
;; End:
;;; init-async.el ends here
