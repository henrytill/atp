module Syntax = Intro__syntax
module Semantics = Intro__semantics

let parse : Lexing.lexbuf -> Syntax.t option = Intro__parser.toplevel Intro__lexer.token
let parse_string (s : string) : Syntax.t option = Lexing.from_string s |> parse
