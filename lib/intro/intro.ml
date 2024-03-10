module Syntax = Syntax

let parse_string (s : string) : Syntax.t option =
  let lexbuf = Lexing.from_string s in
  let maybe_toplevel = Parser.toplevel Lexer.token lexbuf in
  maybe_toplevel

let simplify = Semantics.simplify
