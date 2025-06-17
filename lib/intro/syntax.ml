type t =
  | Var of string
  | Const of int
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Exp of t * t
  | Metavar of string

let pp_ast fmt =
  let open Format in
  let wrap flag fmt x pp =
    if flag then
      fprintf fmt "@[<hv 1>(%a)@]" pp x
    else
      pp fmt x
  in
  let rec go flag fmt fm =
    wrap flag fmt fm @@ fun fmt -> function
    | Var x -> fprintf fmt "Var %S" x
    | Const m -> fprintf fmt "Const %d" m
    | Neg a -> fprintf fmt "@[<hv 1>Neg@;<1 0>%a@]" wrapped a
    | Add (a, b) -> fprintf fmt "@[<hv 1>Add@;<1 0>(%a,@, %a)@]" unwrapped a unwrapped b
    | Sub (a, b) -> fprintf fmt "@[<hv 1>Sub@;<1 0>(%a,@, %a)@]" unwrapped a unwrapped b
    | Mul (a, b) -> fprintf fmt "@[<hv 1>Mul@;<1 0>(%a,@, %a)@]" unwrapped a unwrapped b
    | Exp (a, b) -> fprintf fmt "@[<hv 1>Exp@;<1 0>(%a,@, %a)@]" unwrapped a unwrapped b
    | Metavar s -> fprintf fmt "@[<hv 1>Metavar@;<1 0>%S@]" s
  and wrapped fmt = go true fmt
  and unwrapped fmt = go false fmt in
  fprintf fmt "@[<hv 1>%a@]" wrapped

let rec pp fmt =
  let open Format in
  function
  | Var x -> fprintf fmt "%s" x
  | Const m -> fprintf fmt "%d" m
  | Neg a -> fprintf fmt "@[<h>(-@ %a)@]" pp a
  | Add (a, b) -> fprintf fmt "@[<h>(%a@ +@ %a)@]" pp a pp b
  | Sub (a, b) -> fprintf fmt "@[<h>(%a@ -@ %a)@]" pp a pp b
  | Mul (a, b) -> fprintf fmt "@[<h>(%a@ *@ %a)@]" pp a pp b
  | Exp (a, b) -> fprintf fmt "@[<h>(%a@ ^@ %a)@]" pp a pp b
  | Metavar s -> fprintf fmt "@[<h>$%s]" s

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
