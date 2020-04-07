# Opis języka

## Struktura programu

Program w tym języku jest listą definicji funkcji. Wykonanie programu zaczyna się od obowiązkowej funkcji main typu int, nie przyjmującej argumentów.
Funkcje definiowane są jako typ zwracanej wartości, nazwa, lista argumentów i ciało funkcji. Funkcje o typie wyniku innym niż void, zwracają wartość za pomocą instrukcji return. Parametry do funkcji przekazywane są przez wartość lub przez zmienną.

entrypoints Program ;
Program.   Program ::= [TopDef] ;
FnDef.     TopDef ::= Type Ident "(" [ArgOrRef] ")" Block ;
Arg.       ArgOrRef ::= Type Ident ;
RefArg.    ArgOrRef ::= "ref" Type Ident ;

## Instrukcje
/* to do */
Instrukcje: pusta, złożona, if, while, return jak w C/Javie. Dodatkowymi instrukcjami są przypisanie.

Deklaracje zmiennych mogą występować w dowolnym miejscu bloku, jednak każda zmienna musi być zadeklarowana przed użyciem. Jeśli zmienna nie jest jawnie inicjalizowana w momencie deklaracji, jest inicjalizowana wartością domyślną (0 dla int, "" dla string, false dla bool).

Zmienne zadeklarowane w bloku nie są widoczne poza nim i przesłaniają zmienne o tej samej nazwie spoza bloku. W obrębie bloku zmienne muszą mieć unikalne nazwy.

Tablice deklarowane są `array<typ> nazwa_tablicy(rozmiar)`. W przypadku braku inicjalizacji, tablica wypełniana jest domyślnymi wartościami dla ustalonego typu. Tablica może zostać zainicjowana listą inicjalizacyjną, np. `array<int> days(5) {1,2,3,4,5}`.

    separator  Stmt ";" ;
    Block.     Block ::= "{" [Stmt] "}" ;
    BStmt.     Stmt ::= Block ;
    Decl.      Stmt ::= Type Item ;
    NoInit.    Item ::= Ident ; 
    Init.      Item ::= Ident "=" Expr ;
    ArrNoInit. Item ::= Ident "(" Expr ")" ;
    ArrInit.   Item ::= Ident "(" Expr ")" "{" [Expr] "}" ;
    separator nonempty Item "," ;
    Ass.       Stmt ::= Ident "=" Expr ;
    Ret.       Stmt ::= "return" Expr ;
    VRet.      Stmt ::= "return" ;
    Cond.      Stmt ::= "if" "(" Expr ")" Block;
    CondElse.  Stmt ::= "if" "(" Expr ")" Block "else" Block  ;
    While.     Stmt ::= "while" "(" Expr ")" Block ;
    For.       Stmt ::= "for" "(" Ident "from" Expr "to" Expr ")" Block ;
    Print.     Stmt ::= "print" Expr ;
    SExp.      Stmt ::= Expr ;
    Break.     Stmt ::= "break" ;
    Cont.      Stmt ::= "continue" ;

## Typy

Typy int, void jak w Javie; string odpowiada String, bool odpowiada boolean. Tablice statyczne. Nie ma rzutowania na inny typ.

    Int.       Type ::= "int" ;
    Str.       Type ::= "string" ;
    Bool.      Type ::= "bool" ;
    Void.      Type ::= "void" ;
    Arr.       Type ::= "array" "<" Type ">" ;

Tablice mogą być dowolnego typu, w tym także przechowywać inne tablice. Możliwy jest dostęp do elementu pod zadanym indeksem poprzez `nazwa_tablicy[indeks]`.
 
 W przypadku deklaracji zmiennych bez inicjalizacji, ustalone są domyślne wartości dla typów, tj.:
* `int` --> `0`
* `bool` --> `false`
* `string` --> `""`

## Arytmetyka
Dostępne są standardowe operatory jak w języku C, tj.:
* operatory arytmetyczne: `+`, `-`, `*`, `/`, `%`,
* operatory porównania: `<`, `<=`, `>`, `>=`, `==`, `!=`,
* operatory logiczne: `!`, `&&`, `||`.

## Wyrażenia
Wyrażenia stanowią podzbiór zbioru wyrażeń dostępnych w C/Javie:
    EVar.      Expr6 ::= Ident ;
    ELitInt.   Expr6 ::= Integer ;
    ELitTrue.  Expr6 ::= "true" ;
    ELitFalse. Expr6 ::= "false" ;
    EApp.      Expr6 ::= Ident "(" [ExprOrRef] ")" ;
    EString.   Expr6 ::= String ;
    ArrAcc.    Expr5 ::= Ident "[" Expr6 "]" ;
    Neg.       Expr5 ::= "-" Expr6 ;
    Not.       Expr5 ::= "!" Expr6 ;
    EMul.      Expr4 ::= Expr4 MulOp Expr5 ;
    EAdd.      Expr3 ::= Expr3 AddOp Expr4 ;
    ERel.      Expr2 ::= Expr2 RelOp Expr3 ;
    EAnd.      Expr1 ::= Expr2 "&&" Expr1 ;
    EOr.       Expr ::= Expr1 "||" Expr ;
    ERefArg.   ExprOrRef ::= "ref" Ident ;
    EExpArg.   ExprOrRef ::= Expr ;

Wyrażenia logiczne obliczane są leniwie. Podczas aplikacji funkcji, parametry przekazywane przez referencję poprzedzane są słówkiem kluczowym `ref`.

## Tabelka cech

  Na 15 punktów | planowane | I iteracja | II iteracja
------------ | ------------- |------------- |------------- 
  01 (trzy typy) | ++ 
  02 (literały, arytmetyka, porównania) | ++
  03 (zmienne, przypisanie) | ++
  04 (print) | ++
  05 (while, if) | ++
  06 (funkcje lub procedury, rekurencja) | ++
  07 (przez zmienną / przez wartość / in/out) | ++
  08 (zmienne read-only i pętla for) | ++
 
  Na 20 punktów | planowane | I iteracja | II iteracja
------------ | ------------- |------------- |------------- 
  09 (przesłanianie i statyczne wiązanie) | ++
  10 (obsługa błędów wykonania) | ++
  11 (funkcje zwracające wartość) | ++
  
  Na 30 punktów | planowane | I iteracja | II iteracja
------------ | ------------- |------------- |------------- 
  12 (4) (statyczne typowanie) | ++
  13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
  14 (1) (rekordy/tablice/listy) | ++
  15 (2) (krotki z przypisaniem)
  16 (1) (break, continue) | ++
  17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)
  18 (3) (generatory)
  
  W sumie: 26 punktów
