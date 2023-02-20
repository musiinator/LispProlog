;Nivelul unui nod x in arbore
;nivNod(l1...ln, x, nivel) = { [], n=0
;						     { nivel, l1 = x
;						     { nivNod(l2, x, nivel+1), nivNod(l2, x, nivel) != []
;						     { nivNod(l3, x, nivel+1), altfel
(defun nivNod(l x nivel)
	(cond
		( (null l) nil )
		( (equal (car l) x) nivel )
		( (not (equal (nivNod(cadr l) x nivel) nil)) (nivNod (cadr l) x (+ nivel 1) ) )
		( t (nivNod (caddr l) x (+ nivel 1) ) )
	)
)

(defun main(l x)
	(nivNod l x 0)
)