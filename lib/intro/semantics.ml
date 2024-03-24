let err_raise_negative = "cannot raise to a negative power"

let rec pow (a : int) : int -> int = function
  | 0 -> 1
  | 1 -> a
  | n when n < 0 -> raise (Invalid_argument err_raise_negative)
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let simplify1 (count : int ref) (expr : Syntax.t) : Syntax.t =
  incr count;
  match expr with
  | Add (Const 0, x) -> x
  | Add (x, Const 0) -> x
  | Add (Const m, Const n) -> Const (m + n)
  | Sub (x, Const 0) -> x
  | Sub (x, y) when Syntax.equal x y -> Const 0
  | Sub (Const m, Const n) -> Const (m - n)
  | Mul (Const 0, _) -> Const 0
  | Mul (_, Const 0) -> Const 0
  | Mul (Const 1, x) -> x
  | Mul (x, Const 1) -> x
  | Mul (Const m, Const n) -> Const (m * n)
  | Exp (_, Const 0) -> Const 1
  | Exp (Const 0, _) -> Const 0
  | Exp (Const 1, _) -> Const 1
  | Exp (x, Const 1) -> x
  | Exp (Const m, Const n) -> Const (pow m n)
  | Exp (Const _, Neg (Const _)) -> raise (Invalid_argument err_raise_negative)
  | Neg (Neg m) -> m
  | Neg (Const m) -> Const (-m)
  | expr -> expr

let simplify_with_count (expr : Syntax.t) : Syntax.t * int =
  let count = ref 0 in
  let rec go (expr : Syntax.t) : Syntax.t =
    incr count;
    match expr with
    | Add (e1, e2) -> simplify1 count (Add (go e1, go e2))
    | Sub (e1, e2) -> simplify1 count (Sub (go e1, go e2))
    | Mul (e1, e2) -> simplify1 count (Mul (go e1, go e2))
    | Exp (e1, e2) -> simplify1 count (Exp (go e1, go e2))
    | expr -> simplify1 count expr
  in
  let result = go expr in
  (result, !count)

let simplify expr = fst (simplify_with_count expr)
