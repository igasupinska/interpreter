%%%%%%%%%%%% 3 %%%%%%%%%%%%%%%%%%%
% Operacje na liczbach naturalnych
% reprezentacja liczb: stała 0, symbol funkcyjny s/1

%%%%%%%%%%%% 3a %%%%%%%%%%%%%%%%%%%
% nat(x) wtw, gdy x jest liczbą naturalną

nat(z) .
nat(s(X)) :- nat(X) .
     
% plus(x, y, z) wtw, gdy x + y = z
plus(z, X, X) .
plus(X, z, X) .
plus(s(X), Y, s(Z)) :- plus(X, Y, Z) .

% minus(x, y, z) wtw, gdy x - y = z
minus(X, z, X) .
minus(X, X, z) .
minus(X, Y, Z) :- plus(Y, Z, X) .  

% fib(k, n) wtw, gdy n = k-ta liczba Fibonacciego
fib(z, z) .
fib(s(z), s(z)) .
fib(s(s(K)), N) :- fib(s(K), X), fib(K, Y), plus(X, Y, N) .