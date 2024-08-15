{
open Prop_logic__parser
}

let id = (['a'-'z' 'A'-'Z']+ ['a'-'z' 'A'-'Z' '0'-'9']*)
let white = ([' ' '\t']+)
let newline = '\r' | '\n' | "\r\n"

rule token = parse
  | white   { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | id      { ATOM (Lexing.lexeme lexbuf) }
  | "false" { FALSE }
  | "true"  { TRUE }
  | '~'     { NOT }
  | "/\\"   { AND }
  | "\\/"   { OR }
  | "==>"   { IMP }
  | "<=>"   { IFF }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | eof     { EOF }
  | _ as c  { failwith (Printf.sprintf "unexpected character: %C" c) }
