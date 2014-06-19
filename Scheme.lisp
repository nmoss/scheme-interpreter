;;; Nicholas Moss
;;; Basic Scheme Interpreter
;;; ========================

;;; Read-eval-print
(defun revalp ()
	(set-functions)
	(format t "~% -->")
	(print (eval-2 (read)))
	(revalp))

(defun atomp (x)
	(if (numberp x) t nil))

(defun eval-2 (expr)
	(if (atomp expr)
		expr
		(progn
			(multiple-value-setq (op flag) (get-function (car expr)))
			(if (eql flag t)
				(apply op (mapcar #'eval-2 (rest expr)))
				(car expr)))))


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
	



