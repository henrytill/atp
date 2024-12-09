%{
open Syntax
%}

/* Lexemes */
%token <int> NUMERAL
%token <string> METAVAR
%token <string> VAR
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
%type <Syntax.t option> toplevel
%type <Syntax.t> expression

%%

/* Grammar */

toplevel:
  | e = expression EOF { Some e }
  | EOF                { None }
;

expression:
  | v = METAVAR                           { Metavar v }
  | x = VAR                               { Var x }
  | n = NUMERAL                           { Const n }
  | e1 = expression EXP   e2 = expression { Exp (e1, e2) }
  | e1 = expression TIMES e2 = expression { Mul (e1, e2) }
  | e1 = expression PLUS  e2 = expression { Add (e1, e2) }
  | e1 = expression MINUS e2 = expression { Sub (e1, e2) }
  | MINUS e = expression %prec UMINUS     { Neg e }
  | LPAREN e = expression RPAREN          { e }
;
