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
    | Var x -> fprintf fmt "@[Var %S@]" x
    | Const m -> fprintf fmt "@[Const %d@]" m
    | Neg a -> fprintf fmt "@[Neg (%a)@]" go a
    | Add (a, b) -> fprintf fmt "@[Add (%a, %a)@]" go a go b
    | Sub (a, b) -> fprintf fmt "@[Sub (%a, %a)@]" go a go b
    | Mul (a, b) -> fprintf fmt "@[Mul (%a, %a)@]" go a go b
    | Exp (a, b) -> fprintf fmt "@[Exp (%a, %a)@]" go a go b
  in
  fprintf fmt "@[(";
  go fmt expr;
  fprintf fmt ")@]"

let rec pp fmt expr =
  let open Format in
  match expr with
  | Var x -> fprintf fmt "@[%s@]" x
  | Const m -> fprintf fmt "@[%d@]" m
  | Neg a -> fprintf fmt "@[(- %a)@]" pp a
  | Add (a, b) -> fprintf fmt "@[(%a + %a)@]" pp a pp b
  | Sub (a, b) -> fprintf fmt "@[(%a - %a)@]" pp a pp b
  | Mul (a, b) -> fprintf fmt "@[(%a * %a)@]" pp a pp b
  | Exp (a, b) -> fprintf fmt "@[(%a ^ %a)@]" pp a pp b

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
