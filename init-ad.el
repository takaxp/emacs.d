(defvar my-minimum-time-to-print 0.5)
(defun my--print-loading-time (f &rest args)
  "https://memo.sugyan.com/entry/20120105/1325756767"
  (let* ((before (current-time))
         (result (apply f args))
         (after (current-time))
         (file-or-feature (car args))
         (time (+ (* (- (nth 1 after) (nth 1 before)) 1000.0)
                  (/ (- (nth 2 after) (nth 2 before)) 1000.0))))
    (unless (or (memq file-or-feature '(cl-lib macroexp))
                (> my-minimum-time-to-print time))
      (message "--- %04d [ms]: %s%s" time
               (if (equal (subr-name f) "load") "(loading) " "")
               file-or-feature))
    result))
(advice-add 'load :around #'my--print-loading-time)
(advice-add 'require :around #'my--print-loading-time)

(provide 'init-ad)
