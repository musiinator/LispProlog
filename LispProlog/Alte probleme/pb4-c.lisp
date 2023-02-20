;pb4-c
;ex: (a b c (d (e f) g h i)) => (c b a (d (f e) i h g))
(defun rev(l)
    (cond
        ((null l) nil)
        (t (append (rev (cdr l)) (list (car l))))
	)
)
(defun c(v aux)
	(cond
		((AND(null v)(not (null aux))) (rev aux))
		((AND(null v)(null aux)) nil)
		((atom (car v)) (c(cdr v)(append aux (list (car v)))))
		(t (append (rev aux) (cons (c(car v)nil) (c(cdr v)nil))))
    )
)