open Ppxlib

let rec syntax_to_expression ~(loc : location) : Intro.Syntax.t -> expression = function
  | Var x -> [%expr Intro.Syntax.Var [%e Ast_builder.Default.estring ~loc x]]
  | Neg a -> [%expr Intro.Syntax.Neg [%e syntax_to_expression ~loc a]]
  | Const m ->
      [%expr
        Intro.Syntax.Const
          [%e Ast_builder.Default.(pexp_constant ~loc (Pconst_integer (string_of_int m, None)))]]
  | Add (a, b) ->
      [%expr Intro.Syntax.Add ([%e syntax_to_expression ~loc a], [%e syntax_to_expression ~loc b])]
  | Sub (a, b) ->
      [%expr Intro.Syntax.Sub ([%e syntax_to_expression ~loc a], [%e syntax_to_expression ~loc b])]
  | Mul (a, b) ->
      [%expr Intro.Syntax.Mul ([%e syntax_to_expression ~loc a], [%e syntax_to_expression ~loc b])]
  | Exp (a, b) ->
      [%expr Intro.Syntax.Exp ([%e syntax_to_expression ~loc a], [%e syntax_to_expression ~loc b])]
  | Metavar s -> Ast_builder.Default.evar ~loc s

let expand ~loc ~path:_ s =
  match Intro.Input.parse_string s with
  | Some syntax -> syntax_to_expression ~loc syntax
  | None -> Location.raise_errorf ~loc "Failed to parse intro"

let extension =
  Extension.declare "intro" Extension.Context.expression
    Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ drop drop)))
    expand

let () =
  let rules = [ Context_free.Rule.extension extension ] in
  Driver.register_transformation ~rules "ppx_intro"
