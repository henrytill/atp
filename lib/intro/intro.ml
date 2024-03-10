module Syntax = Syntax

let parse (lexbuf : Lexing.lexbuf) : Syntax.t option =
  let maybe_toplevel = Parser.toplevel Lexer.token lexbuf in
  maybe_toplevel

let parse_string (s : string) : Syntax.t option =
  let lexbuf = Lexing.from_string s in
  let maybe_toplevel = Parser.toplevel Lexer.token lexbuf in
  maybe_toplevel

let simplify = Semantics.simplify
