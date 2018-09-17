;; advice of load function
(defadvice load (around require-benchmark activate)
  (let* ((before (current-time))
         (result ad-do-it)
         (after  (current-time))
         (time (+ (* (- (nth 1 after) (nth 1 before)) 1000)
                  (/ (- (nth 2 after) (nth 2 before)) 1000)))
         (arg (ad-get-arg 0)))
    (message "--- %04d [ms]: (loading) %s" time arg)))

;; advice of require function
(defadvice require (around require-benchmark activate)
  "http://memo.sugyan.com/entry/20120105/1325756767"
  (let* ((before (current-time))
         (result ad-do-it)
         (after  (current-time))
         (time (+ (* (- (nth 1 after) (nth 1 before)) 1000.0)
                  (/ (- (nth 2 after) (nth 2 before)) 1000.0)))
         (arg (ad-get-arg 0)))
    (unless (memq arg '(cl-lib macroexp))
      (message "--- %04d [ms]: %s" time arg))))

(provide 'init-ad)

