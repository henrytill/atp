open Ppxlib

let atom_to_expr ~(loc : location) (p : Prop_logic.Syntax.Prop.t) : expression =
  [%expr
    Atom
      (Prop_logic.Syntax.Prop.inj
         [%e Ast_builder.Default.estring ~loc (Prop_logic.Syntax.Prop.prj p)])]

let rec formula_to_expr ~(loc : location) : Prop_logic.Syntax.t -> expression =
  let open Prop_logic.Syntax.Formula in
  function
  | Atom p -> atom_to_expr ~loc p
  | False -> [%expr False]
  | True -> [%expr True]
  | Not p -> [%expr Not [%e formula_to_expr ~loc p]]
  | And (p, q) -> [%expr And ([%e formula_to_expr ~loc p], [%e formula_to_expr ~loc q])]
  | Or (p, q) -> [%expr Or ([%e formula_to_expr ~loc p], [%e formula_to_expr ~loc q])]
  | Imp (p, q) -> [%expr Imp ([%e formula_to_expr ~loc p], [%e formula_to_expr ~loc q])]
  | Iff (p, q) -> [%expr Iff ([%e formula_to_expr ~loc p], [%e formula_to_expr ~loc q])]
  | Forall (x, p) ->
      [%expr Forall ([%e Ast_builder.Default.estring ~loc x], [%e formula_to_expr ~loc p])]
  | Exists (x, p) ->
      [%expr Exists ([%e Ast_builder.Default.estring ~loc x], [%e formula_to_expr ~loc p])]
  | Metavar s -> Ast_builder.Default.evar ~loc s

let expand ~loc ~path:_ s =
  match Prop_logic.Input.parse_string s with
  | Some formula -> formula_to_expr ~loc formula
  | None -> Location.raise_errorf ~loc "Failed to parse propositional formula"

let extension =
  Extension.declare "prop" Extension.Context.expression
    Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ drop drop)))
    expand

let () =
  let rules = [ Context_free.Rule.extension extension ] in
  Driver.register_transformation ~rules "ppx_prop"
