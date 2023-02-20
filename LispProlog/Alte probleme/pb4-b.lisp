;pb4-b
;(((A B) C) (D E)) => (A B C D E)
(defun b(l)
	(cond
		((null l) nil)
		((atom (car l)) (cons (car l) (b(cdr l))))
		(t (append (b(car l)) (b(cdr l))))
	)
)