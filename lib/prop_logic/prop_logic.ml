module Syntax = Syntax
module Semantics = Semantics
module Semantics_internal = Semantics_internal

let parse : Lexing.lexbuf -> Syntax.t option = Parser.toplevel Lexer.token
let parse_string (s : string) : Syntax.t option = Lexing.from_string s |> parse
