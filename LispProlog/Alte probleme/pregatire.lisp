;Sa se insereze intr-o lista liniara un atom a dat dupa al 2-lea,
;al 4-lea, al 6-lea,....element
(defun inserareDupaPozPare(l a poz)
	(cond
	( (null l) nil)
	( (evenp poz) (append (list (car l) a)(inserareDupaPozPare(cdr l) a (+ poz 1))) )
	( t (cons (car l)(inserareDupaPozPare(cdr l) a (+ poz 1))))
	)
)

;----------------------------------------------------------------------------
;Definiti o functie care obtine dintr-o lista data lista tuturor atomilor
;care apar, pe orice nivel, dar in ordine inversa. De exemplu:(((A B) C)(D E)) -->
;(E D C B A)
(defun inversareLista(l)
	(cond
	( (null l) nil)
	( t (append (inversareLista(cdr l)) (list(car l))))
	)
)

(defun ListaAtomi(l)
	(cond
	( (null l) nil)
	( (atom (car l)) (cons (car l)(ListaAtomi(cdr l))))
	( t (append (ListaAtomi(car l))(ListaAtomi(cdr l))))
	)
)

(defun inversareListaAtomi(l)
	(cond
	( (null l) nil)
	( t (inversareLista(ListaAtomi l)))
	)
)

;----------------------------------------------------------------------------------
;Definiti o functie care intoarce cel mai mare divizor comun al numerelor
;dintr-o lista neliniara.
(defun cmmdc(a b)
	(cond
	((not (numberp a))nil)
	((not (numberp b))nil)
	((equal a b) a)
	( (> a b) (cmmdc (- a b)b) )
	( t (cmmdc a (- b a)) )
	)
)

(defun cmmdcList(l)
  (cond
    ((and (atom (car l)) (null (cdr l))) (car l))
    ((listp (car l)) (cmmdcList (car l)) )
    (t (cmmdc (car l) (cmmdcList (cdr l)) ))
  )
)

;-----------------------------------------------------------------------------------
;Sa se scrie o functie care determina numarul de aparitii
;ale unui atom dat intr-o lista neliniara.
(defun nrAparitii(l a)
	(cond
	( (null l) 0)
	( (equal (car l) a) (+ 1 (nrAparitii (cdr l) a) ) )
	( (listp (car l)) ( + (nrAparitii (car l) a) (nrAparitii (cdr l) a) ) )
	( t (nrAparitii (cdr l) a) )
	)
)

;-----------------------------------------------------------------
;Definiti  o  functie  care  selecteaza  al  n-lea  element
;al  unei  liste,  sau NIL, daca nu exista.
(defun selectN(l n)
	(cond
	( (null l) nil )
	( (equal n 1) (car l) )
	( t (selectN (cdr l) (- n 1)) )
	)
)

;---------------------------------------------------------------------
;Sa se construiasca o functie care verifica daca un atom e membru
;al unei liste nu neaparat liniara.
(defun membru(l a)
	(cond
	( (null l) nil )
	( (equal (car l) a) t)
	( (listp (car l)) (membru (car l) a) )
	( t (membru (cdr l) a) )
	)
)

;----------------------------------------------------------------------
;Sa se construiasca lista tuturor sublistelor unei liste.
;Prin sublista se intelege fie lista insasi, fie un element de pe
;orice nivel, care este lista. Exemplu: (1 2 (3 (4 5) (6 7)) 8 (9 10))
;=> ( (1 2 (3 (4 5) (6 7)) 8 (9 10)) (3 (4 5) (6 7)) (4 5) (6 7) (9 10) ).

(defun sublist(l)
	(cond
	( (null l) nil)
	( (listp (car l)) (cons (car l) ( append(sublist(car l))(sublist(cdr l)) )  ))
	( t (sublist (cdr l)))
	)
)
(defun copy(l)
	(cons l (sublist l))
)

;--------------------------------------------------------------------------------
;Sa se scrie o functie care transforma o lista liniara intr-o multime.
(defun apartineMultime(l a)
  (cond
    ((null l) nil)
    ((equal (car l) a) t)
    (t (apartineMultime (cdr l) a))))

(defun multime(l)
  (cond
    ((null l) nil)
    ((apartineMultime (cdr l) (car l)) (multime (cdr l)))
    (t (cons (car l) (multime (cdr l))))
  )
)

;-------------------------------------------------------------------------
;Definiti o functie care intoarce produsul a doi vectori.
(defun prodVec(v1 v2)
	(cond
	( ( and (null v1) (null v2) ) nil)
	( (and (not (null v1)) (not (null v2)) ) (cons (* (car v1) (car v2)) (prodVec (cdr v1)(cdr v2))) )
	( (null v1) (cons (car v2) (prodVec nil (cdr v2) ) ) )
	( t (cons (car v1) (prodVec (cdr v1) nil ) ) )
	)
)

;--------------------------------------------------------------------
;Sa se construiasca o functie care intoarce adancimea unei liste.
(defun adancime(l)
	(cond
	( (null l) 0 )
	( (listp (car l)) (max(+ 1 (adancime(car l))) (adancime(cdr l))) )
	( t (adancime(cdr l)) )
	)
)

;------------------------------------------------------------
;Definiti  o  functie  care  sorteaza  fara  pastrarea
;dublurilor  o  lista liniara.
(defun minimum(l)
	(cond
	( (null l) nil )
	( (equal (length l) 1) (car l) )
	( (< (car l) (minimum (cdr l))) (car l) )
	( t (minimum (cdr l)) )
	)
)

(defun removeElem(l e)
	(cond
	( (null l) nil )
	( (equal (car l) e) (removeElem (cdr l) e) )
	( t (cons (car l) (removeElem (cdr l) e)) )
	)
)

(defun sortare(l)
	(cond
	( (null l) nil )
	( t (cons (minimum l) (sortare (removeElem l (minimum l) )  ) ))
	)
)

;--------------------------------------------------------------
;Sa se scrie o functie care intoarce intersectia a doua multimi.

(defun apartine (l e)
	(cond
	( (null l) nil)
	( (equal (car l) e) 1 )
	( t (apartine (cdr l) e) )
	)
)

(defun intersectie(m1 m2)
	(cond
	( (null m1) nil)
	( (apartine m2 (car m1)) (cons (car m1)(intersectie (cdr m1) m2) ))
	( t (intersectie (cdr m1) m2) )
	)
)

;----------------------------------------------------------------------
;Definiti  o  functie  care,  dintr-o  lista  de  atomi,
;produce  o  lista  de perechi (atom n), unde atom apare in lista
;initiala de n ori. De ex:(A B A B A C A) --> ((A 4) (B 2) (C 1)).
(defun nrAparitii(l e)
	(cond	
	( (null l) 0)
	( (equal (car l) e) (+ 1 (nrAparitii(cdr l) e) ) )
	( t (nrAparitii(cdr l) e) )
	)
)

(defun removeEl(l e)
	(cond 
	( (null l) nil)
	( (equal (car l) e) (removeEl(cdr l) e) )
	( t (cons (car l)(removeEl(cdr l) e)) )
	)
)

(defun nrAtomi(l)
	(cond
	( (null l) nil)
	( t (cons (list (car l)(nrAparitii l (car l)))(nrAtomi(removeEl l (car l)))) )
	)
)


;------------------------------------------------------------------
;Sa  se  scrie  o  functie  care  sa  testeze  daca  o  lista
;liniara  formata  din numere intregi are aspect de "vale"
;(o secvență se spune ca are aspect de "vale" daca elementele
;descresc pana la un moment dat, apoi cresc.
;De ex. 10 8 6 17 19 20).
(defun condVale(l directie)
	(cond
	( (and(equal (length l) 1) directie ) t )
	( (and(> (car l) (cadr l) ) directie ) nil)
	( (and(< (car l) (cadr l) ) (not directie) ) (condVale (cdr l) t ) )
	( t (condVale (cdr l) directie) )
	)
)

(defun vale(l)
	(cond
	( (< (length l) 3) nil)
	( t (condVale l nil) )
	)
)

;-------------------------------------------------------------------
;Se da un arbore de tipul (2).
;Sa se afiseze calea de la radacina pana la un nod x dat.
(defun cale(l e)
	(cond
	( (null l) nil )
	( (equal (car l) e) (list e) )
	( (equal (cale (cadr l) e) e) (cons (car l) (cale(cadr l) e) ) )
	( t (cons (car l) (cale (caddr l) e)) )
	)
)


;-----------------------------------------------------------------------------------
;Sa se tipareasca lista nodurilor de pe nivelul k dintr-un arbore de tipul (2).
(defun nivel(l k)
	(cond
	( (and (equal k 1) (not (equal (car l) nil))) (list (car l)) )
	( (equal (car l) nil) nil )
	( t (append (nivel (cadr l) (- k 1))(nivel (caddr l) (- k 1))) )
	)
)

;-----------------------------------------------------------------------------------
;Se da un arbore de tipul (2). Sa se precizeze numarul de niveluri din arbore.
(defun nrNiveluri(l)
	(cond
	( (null l) 0)
	( t (max (+ 1 (nrNiveluri (cadr l))) (+ 1 (nrNiveluri (caddr l))) ) )
	)
)

;---------------------------------------------------------------------------------------
;Sa se intoarca adancimea la care se afla un nod intr-un arbore de tipul (2).
(defun adancime(l e nivel)
	(cond
	( (null l) nil)
	( (equal (car l) e) nivel )
	( (not (equal (adancime (cadr l) e nivel) nil)) (adancime (cadr l) e (+ nivel 1)) )
	( (not (equal (adancime (caddr l) e nivel) nil)) (adancime (caddr l) e (+ nivel 1)) )
	)
)

(defun adancimeNod(l x)
	(adancime l x 0)
)

;-------------------------------------------------------------------------------------
;Sa se construiasca lista nodurilor unui arbore de tipul (2) parcurs in inordine.
(defun inordine(l)
	(cond
	( (null l) nil)
	( t (append (inordine(cadr l)) (list(car l)) (inordine(caddr l)) ) )
	)
)

;------------------------------------------------------------------------------------	 
;Se da un arbore de tipul (2). Sa se precizeze nivelul pe care apare un nod x
;in arbore. Nivelul radacii se considera a fi 0.
(defun Nivel(l x nivel)
	(cond
	( (null l) nil)
	( (equal (car l) x) nivel)
	( (not (equal (Nivel (cadr l) x nivel) nil)) (Nivel (cadr l) x (+ nivel 1) ) )
	( (not (equal (Nivel (caddr l) x nivel) nil)) (Nivel (caddr l) x (+ nivel 1) ) )
	)
)

(defun nivelNod(l x)
	(Nivel l x 0)
)


;-------------------------------------------------------------------------------
;Se da un arbore de tipul (2). Sa se afiseze nivelul (si lista corespunza-toarea
 ;nodurilor) avand numar maxim de noduri. Nivelul rad. se considera 0.
 ;nu merge!!!
(defun max-level (tree level max-level max-nodes)
  (cond ((null tree) max-level)
        ((= level max-level)
         (max-level (cadr tree) (1+ level) level (+ (length (car tree)) (length (cadr tree)) max-nodes)))
        ((> (+ (length (car tree)) (length (cadr tree))) max-nodes)
         (max-level (cadr tree) (1+ level) level (+ (length (car tree)) (length (cadr tree)) max-nodes)))
        (t (max-level (cadr tree) (1+ level) max-level max-nodes))
	)
)
(defun nodes-at-level (tree level target-level nodes)
  (cond ((null tree) nodes)
        ((= level target-level) (append (car tree) (cadr tree) nodes))
        (t (append (nodes-at-level (car tree) (1+ level) target-level nodes)
                   (nodes-at-level (cadr tree) (1+ level) target-level nodes)))
   )
)

;-------------------------------------------------------------------------------------
;Sa se construiasca lista nodurilor unui arbore de tipul (2) parcurs in preordine.
(defun preordine(l)
	(cond
	( (null l) nil)
	( t (append (list(car l)) (preordine(cadr l)) (preordine(caddr l)) ) )
	)
)

;---------------------------------------------------------------------------------
;Se da un arbore de tipul (2). Sa se afiseze calea de la radacina pana la un nod x dat.
(defun caleRadNod(l x)
	(cond
	( (null l) nil)
	( (equal (car l) x) (list (car l)) )
	( (not (equal (caleRadNod(cadr l) x) nil)) (cons (car l) (caleRadNod(cadr l) x)) )
	( (not (equal (caleRadNod(caddr l) x) nil)) (cons (car l) (caleRadNod(caddr l) x)) )
	)
)

;--------------------------------------------------------------------------------------
;Sa se construiasca lista nodurilor unui arbore de tipul (2) parcurs in postordine.
(defun postordine(l)
	(cond
	( (null l) nil)
	( (append (postordine(cadr l)) (postordine(caddr l)) (list (car l)) ) )
	)
)

;-----------------------------------------------------------------------------------
;Sa se decida daca un arbore de tipul (2) este echilibrat
;(diferenta dintre adancimile celor 2 subarbori nu este mai mare decat 1).
(defun adancimeArbore(l)
	(cond
	( (null (cdr l)) 0)
	(t (+ 1 (max(adancimeArbore(cadr l))(adancimeArbore(caddr l)))) )
	)
)  

(defun conditie(a b)
	(cond
	((equal (- a b) 1)) 
	((equal (- b a) 1))
	((equal a b))
	(t nil)
	)
)

(defun echilibrat(l)
	(cond
	( (null l) nil)
	( (equal(- (max (adancimeArbore(echilibrat (cadr l))) (adancimeArbore(echilibrat (caddr l))) ) (min (adancimeArbore(echilibrat (cadr l))) (adancimeArbore(echilibrat (caddr l))) ) )0) (list t))
	( (equal(- (max (adancimeArbore(echilibrat (cadr l))) (adancimeArbore(echilibrat (caddr l))) ) (min (adancimeArbore(echilibrat (cadr l))) (adancimeArbore(echilibrat (caddr l))) ) )1) (list t))
	( t nil)
	)
)
	
(defun inlocuire(l k nivel)
	(cond
		( (and (numberp l)(> l k)(oddp nivel)) (- l 1))
		( (atom l) l)
		( t (mapcar #'(lambda(x)
						( inlocuire x k (+ nivel 1) )
					  )l
			)
		)
	)
)

(defun mainInlocuire(l k)
	(inlocuire l k 0)
)