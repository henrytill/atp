open Syntax

let raise_negative () = invalid_arg "cannot raise to a negative power"

let rec pow (a : int) : int -> int = function
  | 0 -> 1
  | 1 -> a
  | n when n < 0 -> raise_negative ()
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let simplify1 count expr =
  incr count;
  match expr with
  | Add (Const 0, x) | Add (x, Const 0) -> x
  | Add (Const m, Const n) -> Const (m + n)
  | (Add (Sub (e, Const m), Const n) | Add (Const m, Sub (e, Const n))) when m = n -> e
  | Add (Add (e, Const m), Const n) | Add (Const m, Add (e, Const n)) -> Add (e, Const (m + n))
  | Sub (x, Const 0) -> x
  | Sub (x, y) when equal x y -> Const 0
  | Sub (Const m, Const n) -> Const (m - n)
  | Sub (Add (e, Const m), Const n) when m = n -> e
  | Sub (Const m, Add (e, Const n)) when m = n -> Neg e
  | Sub (Sub (e, Const m), Const n) -> Sub (e, Const (m + n))
  | Mul (Const 0, _) | Mul (_, Const 0) -> Const 0
  | Mul (Const 1, x) | Mul (x, Const 1) -> x
  | Mul (Const m, Const n) -> Const (m * n)
  | Exp (_, Const 0) -> Const 1
  | Exp (Const 0, _) -> Const 0
  | Exp (Const 1, _) -> Const 1
  | Exp (x, Const 1) -> x
  | Exp (_, Neg (Const _)) -> raise_negative ()
  | Exp (Const m, Const n) -> Const (pow m n)
  | Neg (Neg x) -> x
  | Neg (Const m) -> Const (-m)
  | Metavar _ -> failwith "metavariable"
  | _ -> expr

let simplify_with_count expr =
  let count = ref 0 in
  let rec simply x = simplify1 count (go x)
  and go expr =
    incr count;
    match expr with
    | Add (Const 0, x) | Add (x, Const 0) -> simply x
    | Add (x, y) -> simplify1 count (Add (go x, go y))
    | Sub (x, Const 0) -> simply x
    | Sub (x, y) when equal x y -> Const 0
    | Sub (x, y) -> simplify1 count (Sub (go x, go y))
    | Mul (Const 0, _) | Mul (_, Const 0) -> Const 0
    | Mul (Const 1, x) | Mul (x, Const 1) -> simply x
    | Mul (x, y) -> simplify1 count (Mul (go x, go y))
    | Exp (_, Const 0) -> Const 1
    | Exp (Const 0, _) -> Const 0
    | Exp (Const 1, _) -> Const 1
    | Exp (x, Const 1) -> simply x
    | Exp (_, Neg (Const _)) -> raise_negative ()
    | Exp (x, y) -> simplify1 count (Exp (go x, go y))
    | Neg (Neg x) -> simply x
    | Neg x -> simplify1 count (Neg (go x))
    | Const _ | Var _ -> expr
    | Metavar _ -> failwith "metavariable"
  in
  let ret = go expr in
  (ret, !count)

let simplify expr = fst (simplify_with_count expr)
