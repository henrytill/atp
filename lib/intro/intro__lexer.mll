{
open Intro__parser
}

let int = (['0'-'9']+)
let id = (['a'-'z' 'A'-'Z']+ ['a'-'z' 'A'-'Z' '0'-'9']*)
let white = ([' ' '\t']+)
let newline = '\r' | '\n' | "\r\n"

rule token = parse
  | white   { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | int     { NUMERAL (int_of_string (Lexing.lexeme lexbuf)) }
  | id      { VARIABLE (Lexing.lexeme lexbuf) }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '^'     { EXP }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | eof     { EOF }
  | _ as c  { failwith (Printf.sprintf "unexpected character: %C" c) }
