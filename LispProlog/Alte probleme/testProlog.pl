%exista(E:Element, L:lista)
%E - elementul pe care vrem sa verificam daca exista in lista
%L - lista in care cautam  elementul E
%Modele de flux: (i,i)
exista(E,[E|_]).
exista(E,[_|T]):-
    exista(E,T),!.

%elim(L:Lista, Rez:Lista)
%L - lista din care vrem sa eliminam dublurile
%P - lista rezultat (copia listei L, dar fara dubluri)
%Modele de flux: (i,i), (i,o)
elim([],[]).
elim([H|T],[H|Rez]):-
    elim(T,Rez),
    not(exista(H,Rez)),!.
elim([_|T],Rez):-
    elim(T,Rez).

%sortare(L:Lista, Rez:Lista)
%L - lista dorita a fi sortata
%Rez - lista rezultata cu elementele sortate
%Modele de flux: (i,i), (i,o)
sortare([],[]).


