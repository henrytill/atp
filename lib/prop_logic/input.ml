exception Parse_error

let parse = Parser.toplevel Lexer.token
let parse_string s = parse (Lexing.from_string s)

let parse_string_exn s =
  match parse_string s with
  | None -> raise Parse_error
  | Some syntax -> syntax
