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

(when (version< emacs-version "29.0")
  ;; setopt を init.el で使う．別途 keymap.el を load-path に配置すること．
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
  (require 'keymap)

  ;; define-obsolete-variable-alias の上書き補正
  (defmacro define-obsolete-variable-alias (obsolete-name
                                            current-name
					    &optional when docstring)
    ""
    (declare (doc-string 4)
             (advertised-calling-convention
              (obsolete-name current-name when &optional docstring) "23.1"))
    `(progn
       (defvaralias ,obsolete-name ,current-name ,docstring)
       (dolist (prop '(saved-value saved-variable-comment))
	 (and (get ,obsolete-name prop)
              (null (get ,current-name prop))
              (put ,current-name prop (get ,obsolete-name prop))))
       (make-obsolete-variable ,obsolete-name ,current-name ,when)))
  ;; define-obsolete-function-alias の上書き補正
  (defmacro define-obsolete-function-alias (obsolete-name
                                            current-name
					    &optional when docstring)
    ""
    (declare (doc-string 4)
             (advertised-calling-convention
              (obsolete-name current-name when &optional docstring) "23.1"))
    `(progn
       (defalias ,obsolete-name ,current-name ,docstring)
       (make-obsolete ,obsolete-name ,current-name ,when))))
