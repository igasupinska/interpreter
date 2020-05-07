last(E, [E]).
last(E, [_|L]) :-  last(E, L).

addone([],[]).
addone([X|Xs],[Y|Ys]) :- Y is X+1, addone(Xs,Ys).

suma2([],A,A).
suma2([X|Xs],A,N) :- A1 is A+X, suma2(Xs,A1,N).
suma2(Xs,N) :- suma2(Xs,0,N).

% dlugosc(L, K) wtw, gdy K = liczba elementów listy L (length/2)
dlugosc([], A, A).
dlugosc([_|Ls], A, K) :- A1 is A+1, dlugosc(Ls, A1, K).
dlugosc(L, K) :- dlugosc(L, 0, K).

% min(L, M) wtw, gdy M jest minimalnym elementem L
% (L = lista np. liczb całkowitych)
min([L], L).
min([L|Ls], L) :- min(Ls, A), A >= L.
min([L|Ls], A) :- min(Ls, A), A < L.

min2([], E, E).
min2([L|Ls], A, M) :- A < L, min2(Ls, A, M).
min2([L|Ls], A, M) :- A >= L, min2(Ls, L, M).
min2([L|Ls], M) :- min2([L|Ls], L, M).

% odwroc(L, R) wtw, gdy R jest odwróconą listą L
% (np. odwroc([1,2,3,4], [4,3,2,1]) - sukces)
odwroc([], R, R).
odwroc([L|Ls], A, R) :- odwroc(Ls, [L|A], R).
odwroc(L, R) :- odwroc(L, [], R). 

% palindrom(Slowo) wtw, gdy (lista) Slowo jest palindromem
% (np. palindrom([k,a,j,a,k]), palindrom([1,b,2,b,1]) - sukcesy
palindrom(L) :- odwroc(L, L).

% slowo(Slowo) == Slowo= $a^n b^n$ (Uwaga: bez arytmetyki!)
% dwa warianty: (*) n > 0 (**) n >= 0 (np. slowo([a,a,b,b]) - sukces)
slowo([], []).
slowo([S|Ss], [R|Rs]) :- S == a, R == b, slowo(Ss, Rs).
slowo(S, R) :- slowoHelper(S, R).
slowo(S) :- odwroc(S, R), slowo(S, R).

slowoHelper([], []).
slowoHelper([S|Ss], [R|Rs]) :- S == b, R == a, slowoHelper(Ss, Rs).

% slowo2(Zdanie, Reszta) == Zdanie = Slowo * Reszta,
% Slowo - jw. (np. slowo2([a,a,b,b,c,d], [c,d]) - sukces)

% to do

%% slowo2([], A, A).
%% slowo2(Zdanie, A, A) :- slowo(Zdanie).
%% slowo2(Zdanie, A, Reszta) :- slowo2(S, [R|A], Reszta), odwroc(Zdanie, [R|Rev]), odwroc(Rev, S), .
%% slowo2(Zdanie, Reszta) :- slowo2(Zdanie, [], Reszta).

%% slowo2(Z, R) :- slowo(Z).
%% slowo2(Z, R) :- odwroc(Z, [X|Xs]), odwroc(Xs, S), slowo2(S, [X|R]).