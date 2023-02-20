;Sa se det nr total al nodurilor de pe nivele pare ale unui
;arbore n-ar reprezentat sub forma (rad (subarb1) (subarb2) ... (subarbn))

;ex: (A (B (D (E)(F))) (C)) => 4
;model matematic



; nr_nivel_par(l nivel) =	1, l = atom && nivel e par
;							0, l = atom && nivel e impar
;							suma(nr_nivel_par(li nivel+1)) i=(1,n), l=l1..ln

(defun nr_nivel_par (l nivel)
	(cond
		(and(atom l)(evenp nivel) 1)
		(and(atom l)(oddp nivel) 0)
		(t (apply '+(mapcar #'(lambda (x) (nr_nivel_par x (+1 nivel)) l))))
	)
)