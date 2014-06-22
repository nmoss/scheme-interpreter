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
(defun evall (expr)
	;(print (car expr))
	(if (atomp expr)
		expr
		(progn
			(if (equal 'if (car expr))
				(if-eval expr)
				(progn
					(multiple-value-setq (op flag) (get-function (car expr)))
					(if (eql flag t)
						(apply op (mapcar #'evall (rest expr)))))))))

;;; Evaluates an IF expression
(defun if-eval (expr)
	(if (eql t (evall (cadr expr)))
		(evall (caddr expr))
		(evall (cadddr expr))))

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
	(setf (gethash 'cons *func-table*) #'cons))

(defun get-function (op)
	(gethash op *func-table*))
	



