
;;; Macro System
;;; ========================
;;; TODO need to implement multiple arguments e.g. '...' in Scheme

;;; Hash table for storing all the macro patterns
(defvar *macro-table* (make-hash-table :test #'equal))

;TODO move these to local scope
(defvar *symbols* ())
(defvar *exprs* ())

;;; Tests if a given expr is a macro by checking to see if the (car expr) is in the table
(defun scheme-macro (expr)
	(multiple-value-setq (a b) (gethash expr *macro-table*))
	b)

;;; Expands the pattern into the template
(defun macro-expand (expr)
	(setf *symbols* ())
	(setf *exprs* ())
	(let* ((macro (gethash (car expr) *macro-table*))
				(syntax-rules (get-syntax macro)))
		(tree-matching syntax-rules expr)
		(let ((body (syntax-set *exprs* *symbols* (get-template macro)))) ;; need to match expressions to variables in the list
			(print body)
			(evall body))))
			;(car (last (mapcar #'evall body))))))

;;; Inserts the expressions in place of the arguments in the template of the macro
(defun syntax-set (exprs args body)
	(if (or (equal nil exprs) (equal nil args))
		body
		(syntax-set (cdr exprs) (cdr args) (subst (car exprs) (car args) body))))

;;; Sets up a new macro
(defun store-macro (macro)
	(setf (gethash (car macro) *macro-table*) macro))

(defun define-syntax (name pattern body)
	(store-macro (list name pattern body)))

(defun get-syntax (macro) 
	(cadr macro))

(defun get-template (macro)
	(caddr macro))

;;; Takes two syntax trees matching a value in one tree to an expression in anothor
;;; Returns two lists such that the first list is a list of expressions '((+ 1 1) (+ 2 2))
;;; The second list is the list of symbols the expressions replace '(a b)
(defun tree-matching (first-tree second-tree)
	(if (not (consp first-tree))
		t
		(progn
			(if (not (consp (car first-tree)))
				(progn
					(push (car first-tree) *symbols*)
					(push (car second-tree) *exprs*))
				(progn
					(mapcar #'(lambda (node lst) (push node lst)) (car first-tree) *symbols*)
					(mapcar #'(lambda (node lst) (push node lst)) (car second-tree) *exprs*)))
			(tree-matching (car first-tree) (car second-tree))
			(tree-matching (cdr first-tree) (cdr second-tree)))))

(defun push-wrapper (node lst)
	(push node lst))
	
;;; Set up some common macros 
(define-syntax 'let 
							 '(let ((var value-expr)) body-expr)
							 '((lambda (var) body-expr) value-expr))

(define-syntax 'or2 '(or2 a b) '(if a a b))


