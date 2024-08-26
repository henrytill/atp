{
open Parser
}

let id = (['a'-'z' 'A'-'Z']+ ['a'-'z' 'A'-'Z' '0'-'9']*)
let white = ([' ' '\t']+)
let newline = '\r' | '\n' | "\r\n"

rule token = parse
  | white   { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | "false" { FALSE }
  | "true"  { TRUE }
  | id      { ATOM (Lexing.lexeme lexbuf) }
  | '~'     { NOT }
  | "/\\"   { AND }
  | "\\/"   { OR }
  | "==>"   { IMP }
  | "<=>"   { IFF }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | eof     { EOF }
  | _ as c  { failwith (Printf.sprintf "unexpected character: %C" c) }
