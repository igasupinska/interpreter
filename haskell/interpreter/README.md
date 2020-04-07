# Opis języka
/* to do */

## Struktura programu
/* to do */
Program w języku Latte jest listą definicji funkcji. Na definicję funkcji składa się typ zwracanej wartości, nazwa, lista argumentów oraz ciało. Funkcje muszą mieć unikalne nazwy. W programie musi wystąpić funkcja o nazwie main zwracająca int i nie przyjmująca argumentów (od niej zaczyna się wykonanie programu). Funkcje o typie wyniku innym niż void muszą zwracać wartość za pomocą instrukcji return

Funkcje mogą być wzajemnie rekurencyjne; co za tym idzie mogą być definiowane w dowolnej kolejności (użycie funkcji może występować przed jej definicją)

Program.   Program ::= [TopDef] ;
FnDef.	   TopDef ::= Type Ident "(" [Arg] ")" Block ;
separator nonempty TopDef "" ;
Arg. 	   Arg ::= Type Ident;
separator  Arg "," ;

## Instrukcje
/* to do */
Instrukcje: pusta,złożona,if,while,return jak w C/Javie. Dodatkowo instrukcjami są przypisanie, postinkrementacja, postdekrementacja (w wersji podstawowej języka l-wartościami są tylko zmienne).
Deklaracje zmiennych mogą występować w dowolnym miejscu bloku, jednak każda zmienna musi być zadeklarowana przed użyciem. Jeśli zmienna nie jest jawnie inicjalizowana w momencie deklaracji, jest inicjalizowana wartością domyślną (0 dla int, "" dla string, false dla bool).

Zmienne zadeklarowane w bloku nie są widoczne poza nim i przesłaniają zmienne o tej samej nazwie spoza bloku. W obrębie bloku zmienne muszą mieć unikalne nazwy.

Block.     Block ::= "{" [Stmt] "}" ;
separator  Stmt "" ;
Empty.     Stmt ::= ";" ;
BStmt.     Stmt ::= Block ;
Decl.      Stmt ::= Type [Item] ";" ;
NoInit.    Item ::= Ident ;
Init.      Item ::= Ident "=" Expr ;
separator nonempty Item "," ;
Ass.       Stmt ::= Ident "=" Expr  ";" ;
Incr.      Stmt ::= Ident "++"  ";" ;
Decr.      Stmt ::= Ident "--"  ";" ;
Ret.       Stmt ::= "return" Expr ";" ;
VRet.      Stmt ::= "return" ";" ;
Cond.      Stmt ::= "if" "(" Expr ")" Stmt  ;
CondElse.  Stmt ::= "if" "(" Expr ")" Stmt "else" Stmt  ;
While.     Stmt ::= "while" "(" Expr ")" Stmt ;
SExp.      Stmt ::= Expr  ";" ;

## Typy

Typy int, void jak w Javie; string odpowiada String, bool odpowiada boolean. Nie ma konwersji pomiedzy typami.

    Int.       Type ::= "int" ;
    Str.       Type ::= "string" ;
    Bool.      Type ::= "bool" ;
    Void.      Type ::= "void" ;
 
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
/*to do*/
Podzbiór zbioru wyrażeń dostępnych w Javie:
EVar.      Expr6 ::= Ident ;
ELitInt.   Expr6 ::= Integer ;
ELitTrue.  Expr6 ::= "true" ;
ELitFalse. Expr6 ::= "false" ;
EApp.      Expr6 ::= Ident "(" [Expr] ")" ;
EString.   Expr6 ::= String ;
Neg.       Expr5 ::= "-" Expr6 ;
Not.       Expr5 ::= "!" Expr6 ;
EMul.      Expr4 ::= Expr4 MulOp Expr5 ;
EAdd.      Expr3 ::= Expr3 AddOp Expr4 ;
ERel.      Expr2 ::= Expr2 RelOp Expr3 ;
EAnd.      Expr1 ::= Expr2 "&&" Expr1 ;
EOr.       Expr ::= Expr1 "||" Expr ;
Wyrażenie logiczne zwracają typ boolean i są obliczane leniwie (drugi argument nie jest wyliczany gdy pierwszy determinuje wartość wyrażenia).

## Napisy
/*to do*/
Napisy podobnie jak w Javie, czyli zmienne typu string zawierają referencję do napisu, zaalokowanego na stercie.
Napisy moga występować jako: literały, wartości zmiennych, argumentów i wyników funkcji

Napisy mogą być użyte jako argumenty wbudowanej funkcji printString

Napisy mogą być konkatenowane przy pomocy operatora +. Wynikiem tej operacji jest nowy napis będący konkatenacją argumentów

## 
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
