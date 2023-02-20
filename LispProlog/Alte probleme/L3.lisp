;Definiti o functie care substituie un element prin altul
;la toate nivelurile unei liste date.
;ex: (inlocuire '(1 2 3 (2 (4) (3 (2 5))) 2) 2 A) => (1 A 3 (A (4) (3 (A 5))) A)

;model matematic
;inlocuire(l, e, subs) = { 	  l, l e atom si l != e
;						 { subs, l e atom si l = e
;						 {inlocuire(l1) U ... U inlocuire (ln), altfel
;inlocuire(l:lista, e:element, subs: element)
(defun inlocuire(l e subs)
	(cond
		( (and (atom l) (not(equal l e))) l )
		( (and (atom l) (equal l e)) subs )
		( t (mapcar #'(lambda (x)
					 (inlocuire x e subs)
					  )l
			)
		)
	)
)