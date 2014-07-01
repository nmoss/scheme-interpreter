;;; Nicholas Moss
;;; Basic Scheme Interpreter
;;; ========================

;;; Read-eval-print loop
;;; read takes s-expressions which are evaluated by evall,
;;; the initial environment and symbol table is also set up
(defun revalp ()
	(set-functions)
	(if (equal nil *envts*)
		(push *sym-table* *envts*))
	(format t "~% -->")
	(print (evall (read)))
	(revalp))

;;; Returns true if the function is a number, else returns nil 
(defun atomp (x)
	(if (numberp x) t nil))

;;; Reads a Scheme expression, if the expression is a number (atom) it returns that number
;;; If not it looks up the associated function (value of the symbol), and then evaluates the arguments which may themselves be functions.
;;; (eval-2 (+ 3 4 (+ 4 5)))
;;; (+ (evall 3) (evall 4) (evall (+ 4 5)))
;;; (+ 3 4 (+ (evall 4) (evall 5)))
;;; (+ 3 4 (+ 4 5))
;;; (+ 3 4 9)
;;; --> 16
;;; TODO maybe change the whole structure to a cond?
;(defun evall (expr)
;	(if (atomp expr)
;		expr
;		(progn
;			(if (consp expr) ;; change which one is running depending on if a function call or variable
;				(multiple-value-setq (sym fl) (get-variable (car expr) *envts*)) ;; to handle (f 3) for example 
;				(multiple-value-setq (sym fl) (get-variable expr *envts*)))
;			(if (eql t fl)
;				(if (not (consp sym))
;					sym ;; what the expr evaluates to
;					(lambda-run (car expr) sym (mapcar #'evall (cdr expr)))) ; evaluates a user defined closure
;				(cond ((equal 'if (car expr)) (if-eval expr))
;							((equal 'set! (car expr)) (set-eval expr))
;							((equal 'quote (car expr)) (quote-eval expr))
;							((equal 'lambda (car expr)) (lambda-eval expr))
;							(t 
;								(multiple-value-setq (op flag) (get-function (car expr)))
;								(if (eql flag t)
;									(apply op (mapcar #'evall (rest expr))))))))))

(defun evall (expr)
	(cond
		((atomp expr) expr) ;; if it's an atom should evaluate to itself
		((not (consp expr)) (multiple-value-setq (sym fl) (get-variable expr *envts*))
												(if (eql t fl)
													sym)) ;; should be variable that maps to a value x -> 3 for example
		((consp expr) (multiple-value-setq (sym fl) (get-variable (car expr) *envts*)) ;;; expr is an s-expression so could be any of the s-expression forms
									(cond
										((eql fl t)(lambda-run (car expr) sym (mapcar #'evall (cdr expr)))) ;; if expr is a cons like (f 2) and f is in the environment, than f must be a function call 
										((equal 'if (car expr)) (if-eval expr)) ;; start of special forms checking
										((equal 'set! (car expr)) (set-eval expr))
										((equal 'quote (car expr)) (quote-eval expr))
										((equal 'begin (car expr)) (begin-eval expr))
										((equal 'lambda (car expr)) (lambda-eval expr))
										((equal t (macro-p expr)) (macro-expand expr))
										(t ;; must be a built in function 
											(multiple-value-setq (op flag) (get-function (car expr)))
											(if (eql flag t)
												(apply op (mapcar #'evall (rest expr)))))))))

;;; Creates a closure object given a lambda expression
;;; Doesn't run the function because it has no actual arguments only defines the function to be called later
;;; A copy of *envts* at the time the function was called
(defun lambda-eval (expr)
	(make-closure (cadr expr) (cddr expr)))

;;; Runs a closure object as a function by matching formal parameters with actual parameters
(defun lambda-run (sym closure args)
	(setq TEMP (copy-tree *envts*)) ;;; needs to be a deep copy
	(setf *envts* (get-envt sym *envts*))
	(push (make-envt) *envts*) ;;; make a local environment
	(let ((formal-args (get-formal closure)) 
				(body-exprs (get-body closure)))
		(if (not (eql (length args) (length formal-args)))
			(print "Error args not matching.")
			(progn
				(mapcar #'set-variable formal-args args)
				(let ((result (mapcar #'evall body-exprs)))
					(setf *envts* TEMP) ;;; restore regular scope
					(pop *envts*)
					(nth (- (length result) 1) result))))))

;;; Creates a new closure with formal being the formal parameters of the lambda expression,
;;; and the body being expressions to be evaluated
(defun make-closure (formal body )
	(list 'closure formal body))

;;; Restores *envts* to the state it was in when the function was defined
(defun get-envt (sym envts)
	(if (not (listp envts))
		envts ;; top-level
		(progn
			(multiple-value-setq (op flag) (gethash sym (car envts)))
			(if (equal t flag)
				envts
				(progn 
					(pop envts)
					(get-envt sym envts))))))

;;; Makes an environment for when the lambda function is running
;;; All parameters matched to formal arguments will be placed in this environment
(defun make-envt ()
	(make-hash-table :test #'equal))

;;; Returns the formal arguments of a given closure (lambda expression)
(defun get-formal (closure)
	(car (cdr closure)))

;;; Returns the body of a closure (lambda expression)
(defun get-body (closure)
	(car (cdr (cdr  closure))))

;;; Returns true if the expr is a closure, nil otherwise
(defun closure-p (expr)
	(if (equal 'closure (car expr))
		t
		nil))

;;; Returns the value of an expression preceded by quote
;;; (quote x)
;;; --> X
(defun quote-eval (expr)
	(cadr expr))

;;; (if (= 1 1) (begin 
;;; 							(+ 1 1)
;;; 							(+ 2 2))
;;; 							2)
;;; Evaluates begin by making a list of exprs in the begin block and evaluating them
(defun begin-eval (expr)
	(car (last (mapcar #'evall (cdr expr)))))

;;; Evaluates an IF expression
;;; (if-eval (if (= 1 1) 1 2)
;;; (if (evall (= 1 1)) 1 2)
;;; --> 1
(defun if-eval (expr)
	(if (eql t (evall (cadr expr)))
		(evall (caddr expr))
		(evall (cadddr expr))))

;;; Sets a variable to given value which is either an atom or a function, 
;;; (set! var 2)
;;; (set! var (lambda (x) (* x x))) for example
(defun set-eval (expr)
	(set-variable (cadr expr) (evall (caddr expr))))

;;; Sets up the starting environment
(defvar *envts* ())
	
;;; Hash table for holding user defined functions and variables 
(defvar *sym-table* (make-hash-table :test #'equal))

;;; Saves a variable in the current scope,
;;; *envts* should be moved depending on where SET! is called
(defun set-variable (sym val)
	(setf (gethash sym (car *envts*)) val))

;;; Returns the value of sym that is sym being a variable,
;;; starts with the most local environment and moves to the top-level until it finds a value
;;; the envts passed in should start in the scope that the variable sym was defined in.
(defun get-variable (sym envts)
	(if (equal nil envts)
		(values nil nil)
		(progn
			(if (listp envts)
				(multiple-value-setq (var flag) (gethash sym (car envts)))
				(multiple-value-setq (var flag) (gethash sym envts)))
			(if (eql t flag)
				(values var t)
				(get-variable sym (cdr envts))))))

(defvar *func-table* (make-hash-table :test #'equal))

;;; Sets up the built in primitives from the underlying lisp implementation
(defun set-functions ()
	(setf (gethash '+ *func-table*) #'+)
	(setf (gethash '- *func-table*) #'-)
	(setf (gethash '* *func-table*) #'*)
	(setf (gethash '/ *func-table*) #'/)
	(setf (gethash '= *func-table*) #'=)
	(setf (gethash '> *func-table*) #'>)
	(setf (gethash '< *func-table*) #'<)
	(setf (gethash '<= *func-table*) #'<=)
	(setf (gethash '>= *func-table*) #'>=)
	(setf (gethash 'equal *func-table*) #'equal)
	(setf (gethash 'list *func-table*) #'list)
	(setf (gethash 'car *func-table*) #'car)
	(setf (gethash 'cdr *func-table*) #'cdr)
	(setf (gethash 'cons *func-table*) #'cons)
	(setf (gethash 'define-syntax *func-table*) #'define-syntax)
	(setf (gethash 'and *func-table*) #'scheme-and)
	(setf (gethash 'or *func-table*) #'scheme-or)
	(setf (gethash 'not *func-table*) #'scheme-not))

(defun get-function (op)
	(gethash op *func-table*))

(defun scheme-and (expr1 expr2)
	(if (and (eql t expr1) (eql t expr2))
		t 
		nil))

(defun scheme-or (expr1 expr2)
	(if (or (eql t expr1) (eql t expr2))
		t 
		nil))

(defun scheme-not (expr)
	(if (eql t expr)
		nil
		t))

;;; Macro System
;;; ========================
;;; TODO need to implement multiple arguments e.g. '...' in Scheme

;;; Hash table for storing all the macro patterns
(defvar *macro-table* (make-hash-table :test #'equal))

;TODO move these to local scope
(defvar *symbols* ())
(defvar *exprs* ())

;;; Tests if a given expr is a macro by checking to see if the (car expr) is in the table
(defun macro-p (expr)
	(multiple-value-setq (a b) (gethash (car expr) *macro-table*))
	b)

;;; Expands the pattern into the template
(defun macro-expand (expr)
	(setf *symbols* ())
	(setf *exprs* ())
	(let* ((macro (gethash (car expr) *macro-table*))
				(syntax-rules (get-syntax macro)))
		(tree-matching syntax-rules expr)
		(let ((body (syntax-set *exprs* *symbols* (get-template macro)))) ;; going to be a list of some kind need to match expressions to variables in the list
			(print body)
			;;;(evall body))))
			(car (last (mapcar #'evall body))))))

;(defun flat-p (body)
;	(if (equal nil body)
;		t
;		(if (consp (car body))
;			nil
;			(flat-p (cdr body)))))

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
					(mapcar #'push-wrapper (car first-tree) *symbols*)
					(mapcar  #'push-wrapper (car second-tree) *exprs*)))
			(tree-matching (car first-tree) (car second-tree))
			(tree-matching (cdr first-tree) (cdr second-tree)))))

(defun push-wrapper (node lst)
	(push node lst))
	
;;; Set up some common macros 
(define-syntax 'let 
							 '(let ((var value-expr)) body-expr)
							 '((set! nnnnn (lambda (var) body-expr)) (nnnnn value-expr)))

(define-syntax 'or2 '(or2 a b) '((if a a b)))



