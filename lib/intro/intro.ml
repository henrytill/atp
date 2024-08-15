module Syntax = Intro__syntax
module Semantics = Intro__semantics

let parse = Intro__parser.toplevel Intro__lexer.token
let parse_string s = Lexing.from_string s |> parse
