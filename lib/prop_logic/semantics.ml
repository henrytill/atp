module Prop = Syntax.Prop
module Formula = Syntax.Formula

let rec eval (fm : Syntax.t) (v : Prop.t -> bool) : bool =
  match fm with
  | False -> false
  | True -> true
  | Atom x -> v x
  | Not p -> not (eval p v)
  | And (p, q) -> eval p v && eval q v
  | Or (p, q) -> eval p v || eval q v
  | Imp (p, q) -> (not (eval p v)) || eval q v
  | Iff (p, q) -> Bool.equal (eval p v) (eval q v)
  | Forall _ | Exists _ -> failwith "unimplemented"

let rec onatoms (f : 'a -> 'b Formula.t) (fm : 'a Formula.t) : 'b Formula.t =
  match fm with
  | Atom a -> f a
  | False | True -> fm
  | Not p -> Not (onatoms f p)
  | And (p, q) -> And (onatoms f p, onatoms f q)
  | Or (p, q) -> Or (onatoms f p, onatoms f q)
  | Imp (p, q) -> Imp (onatoms f p, onatoms f q)
  | Iff (p, q) -> Iff (onatoms f p, onatoms f q)
  | Forall (x, p) -> Forall (x, onatoms f p)
  | Exists (x, p) -> Exists (x, onatoms f p)

let rec overatoms (f : 'a -> 'b -> 'b) (fm : 'a Formula.t) (b : 'b) : 'b =
  match fm with
  | Atom a -> f a b
  | False | True -> b
  | Not p -> overatoms f p b
  | And (p, q) | Or (p, q) | Imp (p, q) | Iff (p, q) -> overatoms f p (overatoms f q b)
  | Forall (_, p) | Exists (_, p) -> overatoms f p b

let setify xs = List.sort_uniq compare xs

let atom_union (f : 'a -> 'b list) (fm : 'a Formula.t) : 'b list =
  setify (overatoms (fun h t -> f h @ t) fm [])

let atoms (fm : 'a Formula.t) : 'a list = atom_union (fun a -> [ a ]) fm

(** Equivalent to [List.fold_right] *)
let itlist f l accu = List.fold_right f l accu

(** Returns [true] on all possible valuations of the atoms [ats], using an existing [v] for all other atoms. *)
let rec onallvaluations (subfn : ('a -> bool) -> bool) (v : 'a -> bool) (ats : 'a list) : bool =
  match ats with
  | [] -> subfn v
  | p :: ps ->
      let v' (t : bool) (q : 'a) : bool = if q = p then t else v q in
      onallvaluations subfn (v' false) ps && onallvaluations subfn (v' true) ps

let print_truthtable (fm : 'a Formula.t) : unit =
  let ats = atoms fm in
  let width = itlist (fun x -> max (String.length (Prop.prj x))) ats 5 + 1 in
  let fixw s = s ^ String.make (width - String.length s) ' ' in
  let truthstring p = fixw (if p then "true" else "false") in
  let mk_row v =
    let lis = List.map (fun x -> truthstring (v x)) ats and ans = truthstring (eval fm v) in
    print_string (itlist ( ^ ) lis ("| " ^ ans));
    print_newline ();
    true
  in
  let separator = String.make ((width * List.length ats) + 9) '-' in
  print_string (itlist (fun s t -> fixw (Prop.prj s) ^ t) ats "| formula");
  print_newline ();
  print_string separator;
  print_newline ();
  let _ = onallvaluations mk_row (Fun.const false) ats in
  print_string separator;
  print_newline ()
