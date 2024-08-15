module Syntax = Intro_syntax
module Semantics = Intro_semantics

let parse : Lexing.lexbuf -> Syntax.t option = Intro_parser.toplevel Intro_lexer.token
let parse_string (s : string) : Syntax.t option = Lexing.from_string s |> parse
