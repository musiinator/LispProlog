invers([],L,L).

invers([H|T],X,L):-
    invers(T,[H|X],L).

%invers(L:lista, R:Lista)
%L - lista initiala
%R - lista rezultata
%(i,o),(i,i)
inverseaza(L,R):-
    invers(L,[],R).


exista(E,[E|_]).
exista(E,[_|T]):-
    exista(E,T).

%trans(L:list, R:list)
% (i,i) (i,o)
% transforma o lista in multime

trans(L, Rez):-
    inverseaza(L,R),
    trans_aux(R,Rez).

trans_aux([],[]).
trans_aux([H|T],Rez):-
    exista(H,Rez),
    trans_aux(T,[Rez|H]).
trans_aux([_|T],Rez):-
    trans_aux(T,Rez).
