;;; Nicholas Moss
;;; Basic Scheme Interpreter
;;; ========================

;;; Read-eval-print
(defun revalp ()
	(set-functions)
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
	(if (atomp expr)
		expr
		(progn
			(multiple-value-setq (sym fl) (get-variable expr))
			(if (eql t fl)
				sym ;; what the expr evaluates to
				(cond ((equal 'if (car expr)) (if-eval expr))
							((equal 'set! (car expr)) (set-eval expr))
							(t 
								(multiple-value-setq (op flag) (get-function (car expr)))
								(if (eql flag t)
								(apply op (mapcar #'evall (rest expr))))))))))

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
(defun set-eval (expr)
	(set-variable (cadr expr) (evall (caddr expr))))
	
;;; Hash table for holding user defined functions and variables 
(defvar *sym-table* (make-hash-table :test #'equal))

(defun set-variable (sym val)
	(setf (gethash sym *sym-table*) val))

(defun get-variable (sym)
	(gethash sym *sym-table*))

(defvar *func-table* (make-hash-table :test #'equal))

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

(defun get-function (op)
	(gethash op *func-table*))
	



