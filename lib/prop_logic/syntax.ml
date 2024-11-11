module Prop = struct
  type t = string

  let inj = Fun.id
  let prj = Fun.id
  let pp_ast fmt = Format.fprintf fmt "%S"
  let pp = Format.pp_print_string
  let equal = String.equal
  let compare = String.compare
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
    let false_str = "False" in
    let true_str = "True" in
    let wrap flag fmt fm pp =
      if flag then
        fprintf fmt "@[<hv 1>(%a)@]" pp fm
      else
        pp fmt fm
    in
    let rec go flag fmt fm =
      wrap flag fmt fm @@ fun fmt -> function
      | Atom a -> fprintf fmt "Atom %a" atom_pp_ast a
      | False -> pp_print_string fmt false_str
      | True -> pp_print_string fmt true_str
      | Not (False as p) | Not (True as p) -> fprintf fmt "Not@ %a" unwrapped p
      | Not p -> fprintf fmt "Not@ %a" wrapped p
      | And (p, q) -> fprintf fmt "@[<h>And@ (%a,@ %a)@]" unwrapped p unwrapped q
      | Or (p, q) -> fprintf fmt "@[<h>Or@ (%a,@ %a)@]" unwrapped p unwrapped q
      | Imp (p, q) -> fprintf fmt "@[<hv 1>Imp@;<1 0>(%a,@, %a)@]" unwrapped p unwrapped q
      | Iff (p, q) -> fprintf fmt "@[<hv 1>Iff@;<1 0>(%a,@, %a)@]" unwrapped p unwrapped q
      | Forall (x, p) -> fprintf fmt "@[<hv 1>Forall@;<1 0>(%S,@, %a)@]" x unwrapped p
      | Exists (x, p) -> fprintf fmt "@[<hv 1>Exists@;<1 0>(%S,@, %a)@]" x unwrapped p
    and wrapped fmt fm = go true fmt fm
    and unwrapped fmt fm = go false fmt fm in
    match fm with
    | False -> pp_print_string fmt false_str
    | True -> pp_print_string fmt true_str
    | _ -> fprintf fmt "@[<hv 1>%a@]" wrapped fm

  let pp atom_pp fmt fm =
    let open Format in
    let rec go fmt = function
      | Atom a -> atom_pp fmt a
      | False -> fprintf fmt "false"
      | True -> fprintf fmt "true"
      | Not p -> fprintf fmt "@[<hv 1>(~ %a)@]" go p
      | And (p, q) -> fprintf fmt "@[<h>(%a@ /\\@ %a)@]" go p go q
      | Or (p, q) -> fprintf fmt "@[<h>(%a@ \\/@ %a)@]" go p go q
      | Imp (p, q) -> fprintf fmt "@[<hv 1>(%a ==>@;<1 0>%a)@]" go p go q
      | Iff (p, q) -> fprintf fmt "@[<hv 1>(%a <=>@;<1 0>%a)@]" go p go q
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
