;;; Nicholas Moss
;;; Basic Scheme Interpreter
;;; ========================

;;; Read-eval-print
(defun revalp ()
	(set-functions)
	(if (equal nil *envts*)
		(push *sym-table* *envts*))
	(format t "~% -->")
	(print (evall (read)))
	(revalp))

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
(defun evall (expr)
	;(print "EVALL")
	;(print expr)
	(if (atomp expr)
		expr
		(progn
			(if (consp expr) ;; change which one is running depending on if a function call or variable
				(multiple-value-setq (sym fl) (get-variable (car expr) *envts*)) ;; to handle (f 3) for example 
				(multiple-value-setq (sym fl) (get-variable expr *envts*)))
			(if (eql t fl)
				(if (not (consp sym))
					sym ;; what the expr evaluates to
					(lambda-run (car expr) sym (mapcar #'evall (cdr expr)))) ; evaluates a user defined closure
				(cond ((equal 'if (car expr)) (if-eval expr))
							((equal 'set! (car expr)) (set-eval expr))
							((equal 'quote (car expr)) (quote-eval expr))
							((equal 'lambda (car expr)) (lambda-eval expr))
							(t 
								(multiple-value-setq (op flag) (get-function (car expr)))
								;(print "last...")
								(if (eql flag t)
									(apply op (mapcar #'evall (rest expr))))))))))

;;; Creates a closure object given a lambda expression
;;; Doesn't run the function because it has no actual arguments only defines the function to be called later
;;; A copy of *envts* at the time the function was called
(defun lambda-eval (expr)
	;(print (cadr expr))
	;(print (cddr expr))
	(make-closure (cadr expr) (cddr expr)))

;;; Runs a closure object as a function by matching formal parameters with actual parameters
(defun lambda-run (sym closure args)
	;(print "lambda-run")
	(setq TEMP (copy-tree *envts*)) ;;; needs to be a deep copy
	(setf *envts* (get-envt sym *envts*))
	;(print *envts*)
	(push (make-envt) *envts*) ;;; make a local environment
	(let ((formal-args (get-formal closure)) 
				(body-exprs (get-body closure)))
		(if (not (eql (length args) (length formal-args)))
			(print "Error args not matching.")
			(progn
				(mapcar #'set-variable formal-args args)
				(let ((result (mapcar #'evall body-exprs)))
					;(print *envts*)
					(setf *envts* TEMP) ;;; restore regular scope
					(pop *envts*)
					(nth (- (length result) 1) result))))))

;;; Creates a new closure with the envts being a deep copy of the list of current environments do a copy tree or something
;;; needs to be a deep copy for lexical scope preservation
;;; A copy of *envts* at the time the closure was defined
(defun make-closure (formal body )
	(list 'closure formal body))

;;; Restores *envts* to the state it was in when the function was defined
(defun get-envt (sym envts)
	;(print "get-evnts")
	;(print envts)
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

(defun get-formal (closure)
	(car (cdr closure)))

(defun get-body (closure)
	(car (cdr (cdr  closure))))

(defun closure-p (expr)
	(if (equal 'closure (car expr))
		t
		nil))

;;; Returns the value of an expression preceded by quote
;;; (quote x)
;;; --> X
(defun quote-eval (expr)
	(cadr expr))

;;; Evaluates an IF expression
;;; (if-eval (if (= 1 1) 1 2)
;;; (if (evall (= 1 1)) 1 2)
;;; --> 1
(defun if-eval (expr)
	(if (eql t (evall (cadr expr)))
		(evall (caddr expr))
		(evall (cadddr expr))))

;;; Sets a variable to given value which is either an atom or a function, TODO add environment/scope later
;;; (set! var 2)
;;; (set! var (lambda (x) (* x x))) for example
;;; TODO remove the seperate case for functions there shouldn't be a need for a seperate case
;;; envt is the current environment that the set needs to modify
(defun set-eval (expr)
	;(print "set-eval")
	;(print (caddr expr))
	(set-variable (cadr expr) (evall (caddr expr))))

;;; Sets up the starting environment
(defvar *envts* ())
	
;;; Hash table for holding user defined functions and variables 
(defvar *sym-table* (make-hash-table :test #'equal))

(defun set-variable (sym val)
	(setf (gethash sym (car *envts*)) val))

(defun get-variable (sym envts)
	;(print "get-variable")
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
	(setf (gethash 'list *func-table*) #'list)
	(setf (gethash 'car *func-table*) #'car)
	(setf (gethash 'cdr *func-table*) #'cdr)
	(setf (gethash 'cons *func-table*) #'cons)
	(setf (gethash 'and *func-table*) #'scheme-and)
	(setf (gethash 'or *func-table*) #'scheme-or)
	(setf (gethash 'not *func-table*) #'scheme-not))

(defun get-function (op)
	(gethash op *func-table*))

;;; TODO add &rest to handle arbitrary number of arguments
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

