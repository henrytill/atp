type t =
  | Var of string
  | Const of int
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Exp of t * t

let pp_ast fmt x =
  let open Format in
  let rec go fmt = function
    | Var a -> fprintf fmt "@[Var %S@]" a
    | Const a -> fprintf fmt "@[Const %d@]" a
    | Neg a -> fprintf fmt "@[Neg (%a)@]" go a
    | Add (a, b) -> fprintf fmt "@[Add (%a, %a)@]" go a go b
    | Sub (a, b) -> fprintf fmt "@[Sub (%a, %a)@]" go a go b
    | Mul (a, b) -> fprintf fmt "@[Mul (%a, %a)@]" go a go b
    | Exp (a, b) -> fprintf fmt "@[Exp (%a, %a)@]" go a go b
  in
  fprintf fmt "@[(";
  go fmt x;
  fprintf fmt ")@]"

let rec pp fmt x =
  let open Format in
  match x with
  | Var a -> fprintf fmt "@[%s@]" a
  | Const a -> fprintf fmt "@[%d@]" a
  | Neg a -> fprintf fmt "@[(- %a)@]" pp a
  | Add (a, b) -> fprintf fmt "@[(%a + %a)@]" pp a pp b
  | Sub (a, b) -> fprintf fmt "@[(%a - %a)@]" pp a pp b
  | Mul (a, b) -> fprintf fmt "@[(%a * %a)@]" pp a pp b
  | Exp (a, b) -> fprintf fmt "@[(%a ^ %a)@]" pp a pp b

let rec equal x y =
  match (x, y) with
  | Var a, Var b -> String.equal a b
  | Const a, Const b -> Int.equal a b
  | Neg a, Neg b -> equal a b
  | Add (a1, a2), Add (b1, b2) -> equal a1 b1 && equal a2 b2
  | Sub (a1, a2), Sub (b1, b2) -> equal a1 b1 && equal a2 b2
  | Mul (a1, a2), Mul (b1, b2) -> equal a1 b1 && equal a2 b2
  | Exp (a1, a2), Exp (b1, b2) -> equal a1 b1 && equal a2 b2
  | _, _ -> false
