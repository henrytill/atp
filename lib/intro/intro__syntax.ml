type t =
  | Var of string
  | Const of int
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Exp of t * t

let pp_ast fmt expr =
  let open Format in
  let rec go fmt = function
    | Var x -> fprintf fmt "Var %S" x
    | Const m -> fprintf fmt "Const %d" m
    | Neg a -> fprintf fmt "@[<hv 1>Neg@;<1 0>(%a)@]" go a
    | Add (a, b) -> fprintf fmt "@[<hv 1>Add@;<1 0>(%a,@, %a)@]" go a go b
    | Sub (a, b) -> fprintf fmt "@[<hv 1>Sub@;<1 0>(%a,@, %a)@]" go a go b
    | Mul (a, b) -> fprintf fmt "@[<hv 1>Mul@;<1 0>(%a,@, %a)@]" go a go b
    | Exp (a, b) -> fprintf fmt "@[<hv 1>Exp@;<1 0>(%a,@, %a)@]" go a go b
  in
  fprintf fmt "@[<hv 1>";
  go fmt expr;
  fprintf fmt "@]"

let rec pp fmt expr =
  let open Format in
  match expr with
  | Var x -> fprintf fmt "%s" x
  | Const m -> fprintf fmt "%d" m
  | Neg a -> fprintf fmt "@[<h>(-@ %a)@]" pp a
  | Add (a, b) -> fprintf fmt "@[<h>(%a@ +@ %a)@]" pp a pp b
  | Sub (a, b) -> fprintf fmt "@[<h>(%a@ -@ %a)@]" pp a pp b
  | Mul (a, b) -> fprintf fmt "@[<h>(%a@ *@ %a)@]" pp a pp b
  | Exp (a, b) -> fprintf fmt "@[<h>(%a@ ^@ %a)@]" pp a pp b

let rec equal e1 e2 =
  match (e1, e2) with
  | Var x, Var y -> String.equal x y
  | Const m, Const n -> Int.equal m n
  | Neg a, Neg b -> equal a b
  | Add (a1, b1), Add (a2, b2)
  | Sub (a1, b1), Sub (a2, b2)
  | Mul (a1, b1), Mul (a2, b2)
  | Exp (a1, b1), Exp (a2, b2) ->
      equal a1 a2 && equal b1 b2
  | _, _ -> false
