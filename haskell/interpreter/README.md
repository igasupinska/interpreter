# Opis języka

## Struktura programu

Program w tym języku (bazującym na Latte) jest listą definicji funkcji. Wykonanie programu zaczyna się od obowiązkowej funkcji main typu int, nie przyjmującej argumentów.
Funkcje definiowane są jako typ zwracanej wartości, nazwa, lista argumentów i ciało funkcji. Funkcje o typie wyniku innym niż void, zwracają wartość za pomocą instrukcji return. Parametry do funkcji przekazywane są przez wartość lub przez zmienną. Funkcje muszą być zdefiniowane przed ich użyciem.

    entrypoints Program ;
    Program.   Program ::= [TopDef] ;
    FnDef.     TopDef ::= Type Ident "(" [ArgOrRef] ")" Block ;
    Arg.       ArgOrRef ::= Type Ident ;
    RefArg.    ArgOrRef ::= "ref" Type Ident ;

## Instrukcje
Instrukcje języka takie jak instrukcja `pusta`, `if else`, `while` czy `return` przypominają instrukcje dostępne w C/Javie. Ciało pętli `while` czy instrukcji `if else` obowiązkowo musi być otoczone nawiasami klamrowymi.

Wymagana jest deklaracja zmiennej przed jej użyciem. Zmienne zadeklarowane w bloku, widoczne są jedynie w tym bloku i przesłaniają zmienne spoza bloku. Zmienne w bloku muszą mieć unikalne nazwy.

Tablice deklarowane są `array<typ> nazwa_tablicy(rozmiar)`. W przypadku braku inicjalizacji, tablica wypełniana jest domyślnymi wartościami dla ustalonego typu. Tablica może zostać zainicjowana listą inicjalizacyjną, np. `array<int> nums(5) {1,2,3,4,5}`. W przypadku listy inicjalizacyjnej krótszej niż wielkość tablicy, na niezainicjalizowane pola zostaną przypisane wartości domyślne dla danego typu.

Pętla `for i from pocz to kon` wykonuje się `kon - pocz + 1` razy, o ile `kon >= pocz`. W przeciwnym wypadku, pętla nie wykonuje żadnego obrotu. `pocz` i `kon` muszą być wyrażeniami typu int; nie ma możliwości zmiany kroku pętli.

Dostępne są dwie operacje sterujące pętlą `while`: `break` oraz `continue`.

    Block.     Block ::= "{" [Stmt] "}" ;
    BStmt.     Stmt ::= Block ;
    Decl.      Stmt ::= Type Item ;
    NoInit.    Item ::= Ident ; 
    Init.      Item ::= Ident "=" Expr ;
    ArrNoInit. Item ::= Ident "(" Expr ")" ;
    ArrInit.   Item ::= Ident "(" Expr ")" "{" [Expr] "}" ;
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

Język udostępnia wbudowaną funkcję `print`, przyjmującą argument dowolnego dostępnego typu.

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

## Przykładowe programy

### Hello world

    int main() {
        print("Hello world!");
        return 0;
    }
    
### Liczby Fibonacciego na dwa sposoby

    int fib(int num) {
        if (num == 0 || num == 1) {
            return num;
        }
        
        array<int> fibSeries(num+1);
        fibSeries[0] = 1;
        fibSeries[1] = 1;
        
        int i;
        for (i from 2 to num) {
            fibSeries[i] = fibSeries[i-1] + fibSeries[i-2];
        }
        
        return fibSeries[num];
    }
    
    int fibR(int num) {
        if (num == 0 || num == 1) {
            return num;
        } else {
            return (fibR(num-1) + fibR(num-2));
        }
    }

    int main() {
        
        print("Piąta liczba Fibonacciego to: ");
        print(fib(5));
        
        print("\n");
        
        print("Siódma liczba Fibonacciego to: ");
        print(fibR(7));
        
        return 0;
    }
    
### Przekazywanie przez referencję

    void increment(ref int num) {
        num = num + 1;
    }
    
    int main() {
        int num;
        increment(ref num);
        print(num); //outputs 1
        return 0;
    }

### Widoczność w blokach

    int main() {
        int x = 1;
        int y = 2;
        
        {
            x = 4;
            int y = 5; //nowe y
            
            print(x); //4
            print(y); //5
        }
        
        print(x); //4
        print(y); //2

        return 0;
    }

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
