module Syntax = Prop_logic__syntax
module Semantics = Prop_logic__semantics

val parse : Lexing.lexbuf -> Syntax.t option
val parse_string : string -> Syntax.t option
