let parse = Parser.toplevel Lexer.token
let parse_string s = Lexing.from_string s |> parse
