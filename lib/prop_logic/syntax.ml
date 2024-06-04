module Prop = struct
  type t = string

  let make s = s
  let pp_ast fmt = Format.fprintf fmt "@[%S@]"
  let pp fmt = Format.fprintf fmt "@[%s@]"
  let equal = String.equal
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

  let pp_ast atom_pp_ast fmt fm =
    let open Format in
    let rec go fmt = function
      | Atom a -> fprintf fmt "@[Atom %a@]" atom_pp_ast a
      | False -> fprintf fmt "@[False@]"
      | True -> fprintf fmt "@[True@]"
      | Not p -> fprintf fmt "@[Not (%a)@]" go p
      | And (p, q) -> fprintf fmt "@[And (%a, %a)@]" go p go q
      | Or (p, q) -> fprintf fmt "@[Or (%a, %a)@]" go p go q
      | Imp (p, q) -> fprintf fmt "@[Imp (%a, %a)@]" go p go q
      | Iff (p, q) -> fprintf fmt "@[Iff (%a, %a)@]" go p go q
      | Forall (x, p) -> fprintf fmt "@[Forall (%S, %a)@]" x go p
      | Exists (x, p) -> fprintf fmt "@[Exists (%S, %a)@]" x go p
    in
    fprintf fmt "@[(";
    go fmt fm;
    fprintf fmt ")@]"

  let pp atom_pp fmt fm =
    let open Format in
    let rec go fmt = function
      | Atom a -> atom_pp fmt a
      | False -> fprintf fmt "@[false@]"
      | True -> fprintf fmt "@[true@]"
      | Not p -> fprintf fmt "@[(~ %a)@]" go p
      | And (p, q) -> fprintf fmt "@[(%a /\\ %a)@]" go p go q
      | Or (p, q) -> fprintf fmt "@[(%a \\/ %a)@]" go p go q
      | Imp (p, q) -> fprintf fmt "@[(%a ==> %a)@]" go p go q
      | Iff (p, q) -> fprintf fmt "@[(%a <=> %a)@]" go p go q
      | Forall _ | Exists _ -> failwith "unimplemented"
    in
    go fmt fm

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
