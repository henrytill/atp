module Syntax = Prop_logic_syntax
module Semantics = Prop_logic_semantics

let parse : Lexing.lexbuf -> Syntax.t option = Prop_logic_parser.toplevel Prop_logic_lexer.token
let parse_string (s : string) : Syntax.t option = Lexing.from_string s |> parse
