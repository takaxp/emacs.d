(when (version< emacs-version "26.0")
  (defvar definition-prefixes (make-hash-table :test 'equal))
  (defun register-definition-prefixes (file prefixes)
    "Register that FILE uses PREFIXES."
    (dolist (prefix prefixes)
      (puthash prefix (cons file (gethash prefix definition-prefixes))
               definition-prefixes))))

(when (version< emacs-version "27.0")
  (defmacro if-let* (varlist then &rest else)
    (declare (indent 2)
             (debug ((&rest [&or symbolp (symbolp form) (form)])
                     body)))
    (if varlist
        `(let* ,(setq varlist (internal--build-bindings varlist))
           (if ,(caar (last varlist))
               ,then
             ,@else))
      `(let* () ,then)))
  (defun internal--build-binding (binding prev-var)
    "Check and build a single BINDING with PREV-VAR."
    (setq binding
          (cond
           ((symbolp binding)
            (list binding binding))
           ((null (cdr binding))
            (list (make-symbol "s") (car binding)))
           (t binding)))
    (when (> (length binding) 2)
      (signal 'error
              (cons "`let' bindings can have only one value-form" binding)))
    (let ((var (car binding)))
      `(,var (and ,prev-var ,(cadr binding)))))
  (defun internal--build-bindings (bindings)
    "Check and build conditional value forms for BINDINGS."
    (let ((prev-var t))
      (mapcar (lambda (binding)
                (let ((binding (internal--build-binding binding prev-var)))
                  (setq prev-var (car binding))
                  binding))
              bindings)))
  (defmacro if-let* (varlist then &rest else)
    (declare (indent 2)
             (debug ((&rest [&or symbolp (symbolp form) (form)])
                     body)))
    (if varlist
        `(let* ,(setq varlist (internal--build-bindings varlist))
           (if ,(caar (last varlist))
               ,then
             ,@else))
      `(let* () ,then)))
  (defmacro if-let (spec then &rest else)
    (declare (indent 2)
             (debug ([&or (symbolp form)  ; must be first, Bug#48489
                          (&rest [&or symbolp (symbolp form) (form)])]
                     body)))
    (when (and (<= (length spec) 2)
               (not (listp (car spec))))
      ;; Adjust the single binding case
      (setq spec (list spec)))
    (list 'if-let* spec then (macroexp-progn else)))
  (defmacro when-let (spec &rest body)
    (declare (indent 1) (debug if-let))
    (list 'if-let spec (macroexp-progn body))))
