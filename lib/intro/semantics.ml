let simplify1 : Syntax.t -> Syntax.t = function
  | Add (Const m, Const n) -> Const (m + n)
  | Mul (Const m, Const n) -> Const (m * n)
  | Add (Const 0, x) -> x
  | Add (x, Const 0) -> x
  | Mul (Const 0, _) -> Const 0
  | Mul (_, Const 0) -> Const 0
  | Mul (Const 1, x) -> x
  | Mul (x, Const 1) -> x
  | expr -> expr

let rec simplify : Syntax.t -> Syntax.t = function
  | Add (e1, e2) -> simplify1 (Add (simplify e1, simplify e2))
  | Mul (e1, e2) -> simplify1 (Mul (simplify e1, simplify e2))
  | expr -> simplify1 expr
