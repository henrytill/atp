val parse : Lexing.lexbuf -> Syntax.t option
val parse_string : string -> Syntax.t option
val parse_string_exn : string -> Syntax.t
