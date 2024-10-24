%{
open Syntax
%}

/* Lexemes */
%token <string> ATOM
%token FALSE
%token TRUE
%token NOT
%token AND
%token OR
%token IMP
%token IFF
%token LPAREN
%token RPAREN
%token EOF

/* Precedence and associativity */
%right IMP IFF
%right OR
%right AND
%left NOT

/* Top level rule */
%start toplevel
%type <Syntax.t option> toplevel
%type <Syntax.t> formula

%%

/* Grammar */

toplevel:
  | f = formula EOF { Some f }
  | EOF             { None }
;

formula:
  | a = ATOM                      { Atom (Prop.inj a) }
  | FALSE                         { False }
  | TRUE                          { True }
  | f1 = formula AND f2 = formula { And (f1, f2) }
  | f1 = formula OR  f2 = formula { Or (f1, f2) }
  | f1 = formula IMP f2 = formula { Imp (f1, f2) }
  | f1 = formula IFF f2 = formula { Iff (f1, f2) }
  | NOT f = formula %prec NOT     { Not f }
  | LPAREN f = formula RPAREN     { f }
;
