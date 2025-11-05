type t =
  | Var of string
  | Const of int
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Exp of t * t
  | Metavar of string

let pp_ast =
  let rec go fmt = function
    | Var x -> Fmt.pf fmt "Var %a" Fmt.(quote string) x
    | Const m -> Fmt.pf fmt "Const %d" m
    | Neg a -> Fmt.pf fmt "Neg %a" pp_expr a
    | Add (a, b) -> Fmt.pf fmt "Add %a" Fmt.(parens (pair ~sep:comma go go)) (a, b)
    | Sub (a, b) -> Fmt.pf fmt "Sub %a" Fmt.(parens (pair ~sep:comma go go)) (a, b)
    | Mul (a, b) -> Fmt.pf fmt "Mul %a" Fmt.(parens (pair ~sep:comma go go)) (a, b)
    | Exp (a, b) -> Fmt.pf fmt "Exp %a" Fmt.(parens (pair ~sep:comma go go)) (a, b)
    | Metavar s -> Fmt.pf fmt "Metavar %a" Fmt.(quote string) s
  and pp_expr fmt = function
    | Const _ as atom -> go fmt atom
    | expr -> Fmt.parens go fmt expr
  in
  pp_expr

let rec pp fmt = function
  | Var x -> Fmt.string fmt x
  | Const m -> Fmt.int fmt m
  | Neg a -> Fmt.pf fmt "(- %a)" pp a
  | Add (a, b) -> Fmt.pf fmt "(%a + %a)" pp a pp b
  | Sub (a, b) -> Fmt.pf fmt "(%a - %a)" pp a pp b
  | Mul (a, b) -> Fmt.pf fmt "(%a * %a)" pp a pp b
  | Exp (a, b) -> Fmt.pf fmt "(%a ^ %a)" pp a pp b
  | Metavar s -> Fmt.pf fmt "$%s" s

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
