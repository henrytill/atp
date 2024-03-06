type t =
  | Var of string
  | Const of int
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Exp of t * t

let rec pp fmt x =
  let open Format in
  match x with
  | Var a -> fprintf fmt "@[Var %S@]" a
  | Const a -> fprintf fmt "@[Const %d@]" a
  | Add (a, b) -> fprintf fmt "@[Add (%a, %a)@]" pp a pp b
  | Sub (a, b) -> fprintf fmt "@[Sub (%a, %a)@]" pp a pp b
  | Mul (a, b) -> fprintf fmt "@[Mul (%a, %a)@]" pp a pp b
  | Exp (a, b) -> fprintf fmt "@[Exp (%a, %a)@]" pp a pp b

let rec equal x y =
  match (x, y) with
  | Var a, Var b -> String.equal a b
  | Const a, Const b -> Int.equal a b
  | Add (a1, a2), Add (b1, b2) -> equal a1 b1 && equal a2 b2
  | Sub (a1, a2), Sub (b1, b2) -> equal a1 b1 && equal a2 b2
  | Mul (a1, a2), Mul (b1, b2) -> equal a1 b1 && equal a2 b2
  | Exp (a1, a2), Exp (b1, b2) -> equal a1 b1 && equal a2 b2
  | _, _ -> false
