%{
open Intro__syntax
%}

/* Lexemes */
%token <int> NUMERAL
%token <string> VARIABLE
%token PLUS
%token MINUS
%token TIMES
%token EXP
%token UMINUS
%token LPAREN
%token RPAREN
%token EOF

/* Precedence and associativity */
%left PLUS MINUS
%left TIMES
%right EXP
%nonassoc UMINUS

/* Top level rule */
%start toplevel
%type <t option> toplevel

%%

/* Grammar */

toplevel:
  | e = expression EOF { Some e }
  | EOF                { None }
;

expression:
  | x = VARIABLE                          { Var x }
  | n = NUMERAL                           { Const n }
  | e1 = expression EXP   e2 = expression { Exp (e1, e2) }
  | e1 = expression TIMES e2 = expression { Mul (e1, e2) }
  | e1 = expression PLUS  e2 = expression { Add (e1, e2) }
  | e1 = expression MINUS e2 = expression { Sub (e1, e2) }
  | MINUS e = expression %prec UMINUS     { Neg e }
  | LPAREN e = expression RPAREN          { e }
;
