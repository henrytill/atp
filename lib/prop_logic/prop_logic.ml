module Syntax = Prop_logic__syntax
module Semantics = Prop_logic__semantics

let parse : Lexing.lexbuf -> Syntax.t option = Prop_logic__parser.toplevel Prop_logic__lexer.token
let parse_string (s : string) : Syntax.t option = Lexing.from_string s |> parse
