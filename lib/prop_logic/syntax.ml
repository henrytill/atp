module Prop = struct
  type t = string

  let inj = Fun.id
  let prj = Fun.id
  let pp_ast = Fmt.(quote string)
  let pp = Fmt.string
  let equal = String.equal
  let compare = String.compare
  let hash = String.hash
  let to_string = Fun.id
end

module Formula = struct
  type 'a t =
    | Atom of 'a
    | False
    | True
    | Not of 'a t
    | And of 'a t * 'a t
    | Or of 'a t * 'a t
    | Imp of 'a t * 'a t
    | Iff of 'a t * 'a t
    | Forall of string * 'a t
    | Exists of string * 'a t
    | Metavar of string

  let pp_ast atom_pp_ast =
    let rec go fmt = function
      | Atom a -> Fmt.pf fmt "Atom %a" atom_pp_ast a
      | False -> Fmt.string fmt "False"
      | True -> Fmt.string fmt "True"
      | Not p -> Fmt.pf fmt "Not %a" pp_expr p
      | And (p, q) -> Fmt.pf fmt "And %a" Fmt.(parens (pair ~sep:comma go go)) (p, q)
      | Or (p, q) -> Fmt.pf fmt "Or %a" Fmt.(parens (pair ~sep:comma go go)) (p, q)
      | Imp (p, q) -> Fmt.pf fmt "Imp %a" Fmt.(parens (pair ~sep:comma go go)) (p, q)
      | Iff (p, q) -> Fmt.pf fmt "Iff %a" Fmt.(parens (pair ~sep:comma go go)) (p, q)
      | Forall (x, p) ->
          Fmt.pf fmt "Forall %a" Fmt.(parens (pair ~sep:comma (quote string) go)) (x, p)
      | Exists (x, p) ->
          Fmt.pf fmt "Exists %a" Fmt.(parens (pair ~sep:comma (quote string) go)) (x, p)
      | Metavar s -> Fmt.pf fmt "Metavar %a" Fmt.(quote string) s
    and pp_expr fmt = function
      | (False | True) as boolean -> go fmt boolean
      | expr -> Fmt.parens go fmt expr
    in
    pp_expr

  let rec pp atom_pp fmt = function
    | Atom a -> atom_pp fmt a
    | False -> Fmt.string fmt "false"
    | True -> Fmt.string fmt "true"
    | Not p -> Fmt.pf fmt "(~ %a)" (pp atom_pp) p
    | And (p, q) -> Fmt.pf fmt "(%a /\\ %a)" (pp atom_pp) p (pp atom_pp) q
    | Or (p, q) -> Fmt.pf fmt "(%a \\/ %a)" (pp atom_pp) p (pp atom_pp) q
    | Imp (p, q) -> Fmt.pf fmt "(%a ==> %a)" (pp atom_pp) p (pp atom_pp) q
    | Iff (p, q) -> Fmt.pf fmt "(%a <=> %a)" (pp atom_pp) p (pp atom_pp) q
    | Forall _ | Exists _ -> failwith "unimplemented"
    | Metavar s -> Fmt.pf fmt "$%s" s

  let rec equal atom_equal fm1 fm2 =
    match (fm1, fm2) with
    | Atom a, Atom b -> atom_equal a b
    | False, False | True, True -> true
    | Not p, Not q -> equal atom_equal p q
    | And (p1, q1), And (p2, q2)
    | Or (p1, q1), Or (p2, q2)
    | Imp (p1, q1), Imp (p2, q2)
    | Iff (p1, q1), Iff (p2, q2) ->
        equal atom_equal p1 p2 && equal atom_equal q1 q2
    | Forall (x1, p1), Forall (x2, p2) | Exists (x1, p1), Exists (x2, p2) ->
        String.equal x1 x2 && equal atom_equal p1 p2
    | _ -> false
end

type t = Prop.t Formula.t

let pp_ast = Formula.pp_ast Prop.pp_ast
let pp = Formula.pp Prop.pp
let equal = Formula.equal Prop.equal
