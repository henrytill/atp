module Syntax = Syntax
module Semantics = Semantics
module Semantics_internal = Semantics_internal

val parse : Lexing.lexbuf -> Syntax.t option
val parse_string : string -> Syntax.t option
