%%%%%%%%%%%% 3 %%%%%%%%%%%%%%%%%%%
% lista(L) wtw, gdy L jest (prologową) listą
lista([]) .
lista([_|L]) :- lista(L) .

% pierwszy(E, L) wtw, gdy E jest pierwszym elementem L
pierwszy(E, [E|[]]) .
pierwszy(E, [X|_]) :- X = E .

% ostatni(E, L) wtw, gdy E jest ostatnim elementem L
ostatni(E, [E|[]]) .
ostatni(E, [_|L]) :- ostatni(E, L) .

% element(E, L) wtw, gdy E jest (dowolnym) elementem L
element(E, [X|L]) :- X = E ; element(E, L) .

% scal(L1, L2, L3) wtw, gdy L3 = konkatenacja listy L1 z L2
scal([], L2, L3) :- L2 = L3 .
scal(L1, [], L3) :- L1 = L3 .
scal([H1|T1], L2, [H3|T3]) :- scal(T1, L2, T3), H1 = H3 .

% intersect(Z1,Z2) wtw, gdy zbiory (listy) Z1 i Z2 mają niepuste przecięcie
intersect([H|T], Z2) :- element(H, Z2) ; intersect(T, Z2) .

% podziel(Lista, NieParz, Parz) == podział danej listy na dwie
% podlisty zawierające kolejne elementy (odpowiednio) z parzystych
% (nieparzystych) pozycji (np. podziel([1,3,5,7,9], [1,5,9], [3,7]) - sukces)
podziel([], [], []) .
podziel([X], [Y], []) :- X = Y .
podziel([X,Y|L], [H1|T1], [H2|T2]) :- X = H1, Y = H2, podziel(L, T1, T2) .

% podlista(P, L) wtw, gdy P jest spójną podlistą L
% TO DO

% podciag(P, L)  wtw, gdy P jest podciągiem L
% (czyli niekoniecznie spójną podlistą)
% (preferowane rozwiązanie: każdy podciąg wygenerowany jeden raz)
podciag([], _) .
podciag([H1|T1], [H2|T2]) :- H1 = H2, podciag(T1, T2) ; podciag([H1|T1], T2) .

% wypisz(L) == czytelne wypisanie elementów listy L, z zaznaczeniem
% jeśli lista pusta (np. elementy oddzielane przecinkami, po
% ostatnim elemencie kropka)
wypisz([]) :- write('Empty list.') .
wypisz([X|[]]) :- write(X), write('.'), nl .
wypisz([X|L]) :- write(X), write(', '), wypisz(L) .

% sortowanie przez wstawianie:
% insertionSort(Lista, Posortowana),
% insert(Lista, Elem, NowaLista)
% TO DO

% zadanie domowe:
% srodek(E, L) wtw, gdy E jest środkowym elementem L
% (lista nieparzystej długości; np. srodek(3,[1,2,3,4,5]))
% Uwagi:
% - w tym zadaniu nie używamy jeszcze arytmetyki (nie trzeba)
% - możliwe rozwiązania zarówno deklaratywne, jak i imperatywne (tylko jako
% ćwiczenie) o dużym koszcie
% - poszukiwane rozwiązanie o koszcie liniowym.
% TO DO