module Syntax = Syntax

let parse : Lexing.lexbuf -> Syntax.t option = Parser.toplevel Lexer.token
let parse_string (s : string) : Syntax.t option = Lexing.from_string s |> parse
let simplify = Semantics.simplify
let simplify_with_count = Semantics.simplify_with_count
