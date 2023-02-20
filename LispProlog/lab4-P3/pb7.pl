%aux(L: list, N:integer, R: list)
%L - lista de elemente pe care vrem sa facem aranjamentele
%N - Numarul de elemente ale submultimilor
%R - lista rezultata
aux(L, N, [H|T]):-
    N > 1,
    select(H, L, M),   % in M pune lista L fara prima aparitie a elementului H
    N1 is N-1,
    aux(M, N1, T).
aux(L, 1, [X]):-
    member(X, L).

arrangements(L, N, R):-
    findall(X, aux(L, N, X), R).
