%{
  open Syntax
%}

/* Lexemes */
%token <int> NUMERAL
%token <string> VARIABLE
%token PLUS
%token TIMES
%token LPAREN
%token RPAREN
%token EOF

/* Precedence and associativity */
%left PLUS
%left TIMES

/* Top level rule */
%start toplevel
%type <Syntax.t option> toplevel

%%

/* Grammar */

toplevel:
  | e = expression EOF { Some e }
  | EOF                { None }
;

expression:
  | x = VARIABLE                          { Var x }
  | n = NUMERAL                           { Const n }
  | e1 = expression TIMES e2 = expression { Mul (e1, e2) }
  | e1 = expression PLUS  e2 = expression { Add (e1, e2) }
  | LPAREN e = expression RPAREN          { e }
;
