;;; Nicholas Moss
;;; Basic Scheme Interpreter
;;; ========================

;;; Read-eval-print
(defun revalp ()
	(set-functions)
	(format t "~% -->")
	(print (evall (read)))
	(revalp))

(defun evall (expr)
	(if (consp expr)
		(eval-compound expr)
		(if (atomp expr)
			expr)))

(defun atomp (x)
	(if (numberp x) t nil))

;;; evaluates a scheme expression
;;; need to change the order of evaluation
;;; need to evaluate the arguments, then the function itself
(defun eval-compound (expr)
	(let ((op (car expr))
				(og (car expr))
				(numbers (rest expr))
				(flag nil))
		(multiple-value-setq  (op flag) (get-function og))
		(if (eql 1 (length numbers))
			(car numbers)
			(when (eql flag t)  
				(if (consp (car numbers))
					(setf (car numbers) (eval-compound (car numbers)))) ; checks to make sure first arg is not a list itself
				(if (consp (cadr numbers))
					(setf (cadr numbers) (eval-compound (cadr numbers)))) ; checks second arg 
				(setf (car numbers) (apply op (list (car numbers) (cadr numbers)))) 
				(if (< 1 (length numbers))
					(setf numbers (remove (cadr numbers) numbers)))
				(eval-compound `(,og ,@numbers)))))) 

(defvar *func-table* (make-hash-table :test #'equal))

(defun set-functions ()
	(setf (gethash '+ *func-table*) #'+)
	(setf (gethash '- *func-table*) #'-)
	(setf (gethash '* *func-table*) #'*)
	(setf (gethash '/ *func-table*) #'/)
	(setf (gethash '= *func-table*) #'=)
	(setf (gethash 'list *func-table*) #'list)
	(setf (gethash 'car *func-table*) #'car)
	(setf (gethash 'cdr *func-table*) #'cdr))

(defun get-function (op)
	(gethash op *func-table*))
	



