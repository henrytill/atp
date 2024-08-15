module Syntax = Intro__syntax
module Semantics = Intro__semantics

val parse : Lexing.lexbuf -> Syntax.t option
val parse_string : string -> Syntax.t option
