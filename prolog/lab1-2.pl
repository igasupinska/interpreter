%%%%%%%%%%%% 2 %%%%%%%%%%%%%%%%%%%
% rozszerzenie danych o dzieciach
% Płeć = ch (chłopiec) lub dz (dziewczynka).
% dziecko(Dziecko, Matka, Ojciec, płeć)
    
dziecko(iga, jola, janusz, dz) .
dziecko(dominika, jola, janusz, dz) .
dziecko(zuzia, jola, janusz, dz) .
dziecko(jola, krysia, bogdan, dz) .
dziecko(janusz, gabrysia, roman, ch) .

syn(D, M, O) :- dziecko(D, M, O, ch) .
corka(D, M, O) :- dziecko(D, M, O, dz) .
dziecko(D, M, O) :- syn(D, M, O) .
dziecko(D, M, O) :- corka(D, M, O) .
wnuczka(W, B) :- corka(W, X, _), dziecko(X, B, _) .
wnuczka(W, Dz) :- corka(W, X, _), dziecko(X, _, Dz) .
wnuczka(W, B) :- corka(W, _, X), dziecko(X, B, _) .
wnuczka(W, Dz) :- corka(W, _, X), dziecko(X, _, Dz) .