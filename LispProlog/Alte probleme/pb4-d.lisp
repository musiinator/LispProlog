;pb4-d
;maximul unei liste la nivel superficial
;(1 2 3 (4 5) 2) => 3
(defun d(l)
	(cond
		((null l)-1)
		((listp (car l)) (d(cdr l)))
		((> (car l)(d(cdr l))) (car l))
		(t (d(cdr l)))
	)
)