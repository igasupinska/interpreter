% flagaPolska(Lista, Flaga) wtw, gdy Flaga jest posortowaną
% listą Lista, złożoną ze stałych b,c

% rozwiazanie naiwne, bez akumulatora, wielokrotny append
% (koszt kwadratowy)

flagaPolska([], []).
flagaPolska([b|L], [b|F]) :- flagaPolska(L, F).
flagaPolska([c|L], F) :- flagaPolska(L, F2), append(F2, [c], F).

% dwa akumulatory (koszyki Dijkstry), jednokrotnie append

% początkowo puste:
fp2(L, F) :- fp2(L, [], [], F).
fp2([], B, C, F) :- append(B, C, F).
fp2([b|L], B, C, F) :- fp2(L, [b|B], C, F).
fp2([c|L], B, C, F) :- fp2(L, B, [c|C], F).

% początkowo nieukonkretnione:
fp3(L, F) :- fp3(L, B, C), append(B, C, F).
fp3([b|L], [b|B], C) :- fp3(L, B, C).
fp3([c|L], B, [c|C]) :- fp3(L, B, C).
fp3([], [], []).

% jeden akumulator (na stałe c), w ogóle bez append
fp4(L, F) :- fp4(L, [], F).
fp4([b|L], C, [b|F]) :- fp4(L, C, F).
fp4([c|L], C, F) :- fp4(L, [c|C], F).
fp4([], C, C).

% quickSort(L, S) wtw, gdy S jest wynikiem sortowania L
% (algorytm QuickSort)
quickSort([], []).
quickSort([E|L], S) :- partition(L, E, M, W), quickSort(M, MS), quickSort(W, WS), append(MS, [E|WS], S).

% partition(L, E, M, W) podzial L wzgledem E na dwie listy - Mniejszych i Większych od E
% wersja bez akumulatora (z append)

partition([], _, [], []).
partition([L|Ls], E, [L|M], W) :- L =< E, partition(Ls, E, M, W). 
partition([L|Ls], E, M, [L|W]) :- L > E, partition(Ls, E, M, W).

% wersja z akumulatorem (czyli bez append)
% to do
%% qsortA(L, S) :-  qsortA(L, [], S).
%% qSortA([], A, A).
%% qsortA([E|L], A, S) :- partition(L, E, M, W), qSortA(M, Ms), qSort([E|M], Ws).
%% % qsort(L, A, S) <=>  S = sort(L) ++ A  (konkatenacja)