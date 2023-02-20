% exista(E:intreg, L:lista)
% verifica daca un element exista in lista data
% (i,i)
% E - elementul cautat
% L - lista in care se cauta
exista(E,[E|_]).
exista(E,[_|T]):-
    exista(E,T).

%sterge(E:intreg, L:lista, Rez:lista)
% sterge un element din lista
% E - elementul de sters din lista
% L - lista din care se sterge
% Rez - lista rezultata

sterge(_,[],[]).

sterge(E,[E|T],Rez):-
    sterge(E,T,Rez),!.

sterge(E,[H|T],[H|Rez]):-
    sterge(E,T,Rez),!.

% intersectie(L:lista, J:Lista, Rez:Lista)
% L - prima lista
% J - lista cu care se face intersectia
% Rez - lista rezultata
% Sa se scrie un predicat care intoarce intersectia a doua multimi.
% modele de flux - (i,i,o) - (i,i,i)

intersectie([],_,[]).

intersectie(_,[],[]).

intersectie([H|T],J,[H|Rez]):-
    exista(H,J),
    sterge(H,J,Rez1),
    intersectie(T,Rez1,Rez),!.

intersectie([_|T],J,Rez):-
    intersectie(T,J,Rez),!.


