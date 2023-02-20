(defun postordine (arbore)
  (cond
	( (null arbore) nil)
	( t (append (postordine (cadr arbore) ) (postordine(caddr arbore) ) (list (car arbore) ) ) )
  )
)