;pb4-a
;v=(2 9 4 3)
;k=(1 2 5)
;R=(3 11 9 3)
(defun suma(v k)
	(cond
		( (AND (null v) (not (null k))) (cons (car k)(suma nil (cdr k))) )
		( (AND (not (null v)) (null k)) (cons (car v)(suma (cdr v) nil)) )
		( (AND (null v) (null k)) nil)
		( t (cons (+ (car v)(car k))(suma (cdr v)(cdr k))))
	)
)
