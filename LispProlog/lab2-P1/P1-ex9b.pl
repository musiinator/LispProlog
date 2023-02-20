%interval(S:intreg, D:intreg, Rez:lista)
%(i,i,o) - (i,i,i)
% S inceputul intervalului
% D finalul intervalului
% Rez - lista rezultata cu elementele [n,m]

interval(S,D,[]):-
    S > D,!.
interval(S,D,[S|Rez]):-
    E is S+1,
    interval(E,D,Rez),!.
