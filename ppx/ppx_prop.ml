open Ppxlib

let prop_to_expression ~(loc : location) (p : Prop_logic.Syntax.Prop.t) : expression =
  [%expr
    Prop_logic.Syntax.Formula.Atom
      (Prop_logic.Syntax.Prop.inj
         [%e Ast_builder.Default.estring ~loc (Prop_logic.Syntax.Prop.prj p)])]

let rec syntax_to_expression ~(loc : location) : Prop_logic.Syntax.t -> expression = function
  | Atom p -> prop_to_expression ~loc p
  | False -> [%expr Prop_logic.Syntax.Formula.False]
  | True -> [%expr Prop_logic.Syntax.Formula.True]
  | Not p -> [%expr Prop_logic.Syntax.Formula.Not [%e syntax_to_expression ~loc p]]
  | And (p, q) ->
      [%expr
        Prop_logic.Syntax.Formula.And
          ([%e syntax_to_expression ~loc p], [%e syntax_to_expression ~loc q])]
  | Or (p, q) ->
      [%expr
        Prop_logic.Syntax.Formula.Or
          ([%e syntax_to_expression ~loc p], [%e syntax_to_expression ~loc q])]
  | Imp (p, q) ->
      [%expr
        Prop_logic.Syntax.Formula.Imp
          ([%e syntax_to_expression ~loc p], [%e syntax_to_expression ~loc q])]
  | Iff (p, q) ->
      [%expr
        Prop_logic.Syntax.Formula.Iff
          ([%e syntax_to_expression ~loc p], [%e syntax_to_expression ~loc q])]
  | Forall (x, p) ->
      [%expr
        Prop_logic.Syntax.Formula.Forall
          ([%e Ast_builder.Default.estring ~loc x], [%e syntax_to_expression ~loc p])]
  | Exists (x, p) ->
      [%expr
        Prop_logic.Syntax.Formula.Exists
          ([%e Ast_builder.Default.estring ~loc x], [%e syntax_to_expression ~loc p])]
  | Metavar s -> Ast_builder.Default.evar ~loc s

let expand ~loc ~path:_ s =
  match Prop_logic.Input.parse_string s with
  | Some syntax -> syntax_to_expression ~loc syntax
  | None -> Location.raise_errorf ~loc "Failed to parse prop"

let extension =
  Extension.declare "prop" Extension.Context.expression
    Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ drop drop)))
    expand

let () =
  let rules = [ Context_free.Rule.extension extension ] in
  Driver.register_transformation ~rules "ppx_prop"
