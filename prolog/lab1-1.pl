% dziecko(Dziecko, Matka, Ojciec)
dziecko(jasio, ewa, jan).
dziecko(stasio, ewa, jan).
dziecko(basia, anna, piotr).
dziecko(jan, ela, jakub).
dziecko(iga, jola, janusz) .
dziecko(dominika, jola, janusz) .
dziecko(zuzia, jola, janusz) .
dziecko(jola, krysia, bogdan) .

%%%%%%%%%%%% 1a %%%%%%%%%%%%%%%%%%%
% czy Jasio jest dzieckiem Ewy i Jana
% ?- dziecko(jasio, ewa, jan) .

% dzieci pary rodziców
% ?- dziecko(X, ewa, jan) .

% dziecko jednego rodzica
% ?- dziecko(X, ewa, _) .

%%%%%%%%%%%% 1b %%%%%%%%%%%%%%%%%%%
% definiowanie predykatów

ojciec(O, D) :- dziecko(D, _, O) .
matka(M, D) :- dziecko(D, M, _) .
rodzic(R, D) :- ojciec(R, D) .
rodzic(R, D) :- matka(R, D) .
babcia(B, D) :- rodzic(X, D), matka(B, X) .
dziadek(Dz, D) :- rodzic(X, D), ojciec(Dz, X) .
wnuk(W, Dz) :- dziadek(Dz, W) .
wnuk(W, B) :- babcia(B, W) .
przodek(Przod, Pot) :- rodzic(Przod, Pot) .
przodek(Przod, Pot) :- wnuk(Pot, Przod) .