module Prop = Syntax.Prop

let rec eval (fm : Syntax.t) (v : Prop.t -> bool) : bool =
  match fm with
  | False -> false
  | True -> true
  | Atom x -> v x
  | Not p -> not (eval p v)
  | And (p, q) -> eval p v && eval q v
  | Or (p, q) -> eval p v || eval q v
  | Imp (p, q) -> (not (eval p v)) || eval q v
  | Iff (p, q) -> eval p v = eval q v
  | Forall _ | Exists _ -> failwith "unimplemented"
