let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n when n < 0 -> raise (Invalid_argument "n must be > 0")
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let simplify1 : Syntax.t -> Syntax.t = function
  | Add (Const m, Const n) -> Const (m + n)
  | Mul (Const m, Const n) -> Const (m * n)
  | Exp (Const m, Const n) -> Const (pow m n)
  | Add (Const 0, x) -> x
  | Add (x, Const 0) -> x
  | Mul (Const 0, _) -> Const 0
  | Mul (_, Const 0) -> Const 0
  | Exp (Const 0, _) -> Const 0
  | Exp (_, Const 0) -> Const 1
  | Mul (Const 1, x) -> x
  | Mul (x, Const 1) -> x
  | Exp (Const 1, _) -> Const 1
  | Exp (x, Const 1) -> x
  | expr -> expr

let rec simplify : Syntax.t -> Syntax.t = function
  | Add (e1, e2) -> simplify1 (Add (simplify e1, simplify e2))
  | Mul (e1, e2) -> simplify1 (Mul (simplify e1, simplify e2))
  | Exp (e1, e2) -> simplify1 (Exp (simplify e1, simplify e2))
  | expr -> simplify1 expr
