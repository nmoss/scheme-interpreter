;;; Nicholas Moss
;;; Basic Scheme Interpreter
;;; Tail call optimized version
;;; ========================

(defun define-syntax () nil)

(load "Macro.lisp")

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

(defun evall (expr)
	(prog ()
		:EVALL
		(return
			(cond
				((atomp expr) expr) ;; if it's an atom should evaluate to itself
				((not (consp expr)) (multiple-value-setq (sym fl) (get-variable expr *envts*))
														(if (eql t fl)
															sym)) ;; should be variable that maps to a value x -> 3 for example
				((consp expr) (multiple-value-setq (sym fl) (get-variable (car expr) *envts*)) ;;; expr is an s-expression
											(cond
												((eql fl t)(lambda-run (car expr) sym (mapcar #'(lambda (x) (evall x)) (cdr expr)))) ;; f must be a function call
												((consp (car expr)) (evall `(set! __INTERNTEMP ,(car expr))) ;; ((lambda (x) (* x x)) 2) -> 2
																						(setq ans (evall `(__INTERNTEMP ,@(mapcar #'(lambda (x) (evall x)) (cdr expr)))))
																						(remhash '__INTERNTEMP *sym-table*)
																						ans)
												((equal 'if (car expr)) (setf expr (if (evall (second expr))
																														 (third expr) 
																														 (fourth expr)))
																								(go :EVALL))
												((equal 'set! (car expr)) (set-eval expr))
												((equal 'quote (car expr)) (second expr))
												((equal 'begin (car expr)) (pop expr)
																									 (loop while (rest expr) do (evall (pop expr)))
																									 (setf expr (first expr))
																									 (go :EVALL))
												((equal 'lambda (car expr)) (lambda-eval expr))
												((scheme-macro (first expr)) (setf expr (macro-expand expr)) (go :EVALL))
												(t ;; must be a built in function 
													(multiple-value-setq (op flag) (get-function (car expr)))
													(if (eql flag t)
														(apply op (mapcar #'(lambda (x) (evall x)) (rest expr)))))))))))

;;; Creates a closure object given a lambda expression
;;; Doesn't run the function because it has no actual arguments only defines the function to be called later
;;; A copy of *envts* at the time the function was called
(defun lambda-eval (expr)
	(make-closure (cadr expr) (cddr expr)))

;;; Runs a closure object as a function by matching formal parameters with actual parameters
(defun lambda-run (sym closure args)
	(prog ()
				:TAIL-CALL
		(return
			(progn
				(setq TEMP (copy-tree *envts*)) ;;; needs to be a deep copy
				(setf *envts* (get-envt sym *envts*))
				(push (make-envt) *envts*) ;;; make a local environment
				(let ((formal-args (get-formal closure)) 
							(body-exprs (get-body closure)))
					(if (not (eql (length args) (length formal-args)))
						(print "Error args not matching.")
						(progn
							(mapcar #'set-variable formal-args args)
							(setq tailform (car (last body-exprs)))
							(when (consp tailform)
								(if (equal 'if (car tailform))
									(setf tailform (if (evall (second tailform))
																	 (third tailform)
																	 (fourth tailform)))))
							(if (consp tailform)
								(when (equal sym (car tailform))
								  (setf body-exprs (remove (car (last body-exprs)) body-exprs))))
							(let ((result (mapcar #'evall body-exprs)))
								(when (consp tailform)
									(when (equal sym (car tailform))
										(setf args (mapcar #'evall (rest tailform)))
										(go :TAIL-CALL)))
								(setf *envts* TEMP) ;;; restore regular scope
								(pop *envts*)
								(nth (- (length result) 1) result)))))))))

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



