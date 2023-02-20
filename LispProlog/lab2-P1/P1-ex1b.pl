%adaugare(L:Lista, R:Lista)
%Modele de flux:(i,o),(i,i)
%L-lista initiala
%R-lista finala in care s-au adaugat elemente
adaugare([],[]).

adaugare([H|T],[H,1|Rez]):-
    H mod 2=:=0,
    adaugare(T,Rez),!.

adaugare([H|T],[H|Rez]):-
    adaugare(T,Rez).
