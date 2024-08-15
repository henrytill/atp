module Syntax = Prop_logic__syntax

module Internal = struct
  module Prop = Prop_logic__syntax.Prop
  module Formula = Prop_logic__syntax.Formula

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

  (** Alias for [List.fold_right] *)
  let itlist = List.fold_right

  (** Returns [true] on all possible valuations of the atoms [ats], using an existing [v] for all other atoms. *)
  let rec onallvaluations (subfn : ('a -> bool) -> bool) (v : 'a -> bool) (ats : 'a list) : bool =
    match ats with
    | [] -> subfn v
    | p :: ps ->
        let v' (t : bool) (q : 'a) : bool = if q = p then t else v q in
        onallvaluations subfn (v' false) ps && onallvaluations subfn (v' true) ps

  let print_truthtable (fmt : Format.formatter) (fm : Prop.t Formula.t) : unit =
    let ats = atoms fm in
    let width = itlist (fun x -> max (String.length (Prop.prj x))) ats 5 + 1 in
    let fixw s = s ^ String.make (width - String.length s) ' ' in
    let truthstring p = fixw (string_of_bool p) in
    let mk_row (v : Prop.t -> bool) =
      let lis = List.map (fun x -> truthstring (v x)) ats in
      let ans = truthstring (eval fm v) in
      Format.pp_print_string fmt (itlist ( ^ ) lis ("| " ^ ans));
      Format.pp_print_newline fmt ();
      true
    in
    let separator = String.make ((width * List.length ats) + 9) '-' in
    Format.pp_print_string fmt (itlist (fun s t -> fixw (Prop.prj s) ^ t) ats "| formula");
    Format.pp_print_newline fmt ();
    Format.pp_print_string fmt separator;
    Format.pp_print_newline fmt ();
    let _ = onallvaluations mk_row (Fun.const false) ats in
    Format.pp_print_string fmt separator;
    Format.pp_print_newline fmt ()
end

let eval = Internal.eval
let atoms = Internal.atoms
let print_truthtable = Internal.print_truthtable
