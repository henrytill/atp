let rec eval fm v =
  let open Syntax.Formula in
  match fm with
  | Atom a -> v a
  | False -> false
  | True -> true
  | Not p -> not (eval p v)
  | And (p, q) -> eval p v && eval q v
  | Or (p, q) -> eval p v || eval q v
  | Imp (p, q) -> (not (eval p v)) || eval q v
  | Iff (p, q) -> Bool.equal (eval p v) (eval q v)
  | Forall _ | Exists _ -> failwith "unimplemented"
  | Metavar _ -> failwith "metavariable"

let rec onatoms f =
  let open Syntax.Formula in
  function
  | Atom a -> f a
  | False -> False
  | True -> True
  | Not p -> Not (onatoms f p)
  | And (p, q) -> And (onatoms f p, onatoms f q)
  | Or (p, q) -> Or (onatoms f p, onatoms f q)
  | Imp (p, q) -> Imp (onatoms f p, onatoms f q)
  | Iff (p, q) -> Iff (onatoms f p, onatoms f q)
  | Forall (x, p) -> Forall (x, onatoms f p)
  | Exists (x, p) -> Exists (x, onatoms f p)
  | Metavar _ -> failwith "metavariable"

let rec overatoms f fm b =
  let open Syntax.Formula in
  match fm with
  | Atom a -> f a b
  | False | True -> b
  | Not p -> overatoms f p b
  | And (p, q) | Or (p, q) | Imp (p, q) | Iff (p, q) -> overatoms f p (overatoms f q b)
  | Forall (_, p) | Exists (_, p) -> overatoms f p b
  | Metavar _ -> failwith "metavariable"

let rec dual =
  let open Syntax.Formula in
  function
  | Atom _ as fm -> fm
  | False -> True
  | True -> False
  | Not p -> Not (dual p)
  | And (p, q) -> Or (dual p, dual q)
  | Or (p, q) -> And (dual p, dual q)
  | _ -> failwith "Formula involves connectives ==> or <=>"

let simplify1 =
  let open Syntax.Formula in
  function
  | Not False -> True
  | Not True -> False
  | Not (Not p) -> p
  | And (_, False) | And (False, _) -> False
  | And (p, True) | And (True, p) -> p
  | Or (p, False) | Or (False, p) -> p
  | Or (_, True) | Or (True, _) -> True
  | Imp (False, _) | Imp (_, True) -> True
  | Imp (True, p) -> p
  | Imp (p, False) -> Not p
  | Iff (p, True) | Iff (True, p) -> p
  | Iff (p, False) | Iff (False, p) -> Not p
  | fm -> fm

let rec simplify =
  let open Syntax.Formula in
  function
  | Not p -> simplify1 (Not (simplify p))
  | And (p, q) -> simplify1 (And (simplify p, simplify q))
  | Or (p, q) -> simplify1 (Or (simplify p, simplify q))
  | Imp (p, q) -> simplify1 (Imp (simplify p, simplify q))
  | Iff (p, q) -> simplify1 (Iff (simplify p, simplify q))
  | fm -> fm

let negative =
  let open Syntax.Formula in
  function
  | Atom _ -> false
  | Not (Atom _) -> true
  | _ -> failwith "Not a literal: a literal is either an atomic formula or the negation of one"

let positive lit = not (negative lit)

let nnf fm =
  let rec go =
    let open Syntax.Formula in
    function
    | And (p, q) -> And (go p, go q)
    | Or (p, q) -> Or (go p, go q)
    | Imp (p, q) -> Or (go (Not p), go q)
    | Iff (p, q) -> Or (And (go p, go q), And (go (Not p), go (Not q)))
    | Not (Not p) -> go p
    | Not (And (p, q)) -> Or (go (Not p), go (Not q))
    | Not (Or (p, q)) -> And (go (Not p), go (Not q))
    | Not (Imp (p, q)) -> And (go p, go (Not q))
    | Not (Iff (p, q)) -> Or (And (go p, go (Not q)), And (go (Not p), go q))
    | fm -> fm
  in
  go (simplify fm)

module type ATOM_TYPE = sig
  type t

  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string
end

module Make (Atom : ATOM_TYPE) = struct
  let setify xs = List.sort_uniq Atom.compare xs
  let atom_union f fm = setify (overatoms (fun h t -> f h @ t) fm [])
  let atoms fm = atom_union (fun a -> [ a ]) fm

  let onallvaluations (subfn : (Atom.t -> bool) -> 'a) (ats : Atom.t list) : 'a Seq.t =
    let module Atom_map = Map.Make (Atom) in
    let ats_len = List.length ats in
    let _, offset_table =
      List.fold_left
        (fun (i, m) a -> (pred i, Atom_map.add a i m))
        (pred ats_len, Atom_map.empty)
        ats
    in
    let valuation_for row a = Z.testbit row (Atom_map.find a offset_table) in
    let num_valuations = Int.shift_left 1 ats_len in
    Seq.init num_valuations (fun row -> subfn (valuation_for (Z.of_int row)))

  let false_len = String.length (string_of_bool false)
  let formula_header = "| formula"

  let print_truthtable fmt fm =
    let open Format in
    let ats = atoms fm in
    let width =
      let f a = Int.max (String.length (Atom.to_string a)) in
      succ (List.fold_right f ats false_len)
    in
    let fixw s = s ^ String.make (width - String.length s) ' ' in
    let truthstring p = fixw (string_of_bool p) in
    let mk_row v =
      let lis = List.map (fun x -> truthstring (v x)) ats in
      let ans = truthstring (eval fm v) in
      pp_print_string fmt (List.fold_right ( ^ ) lis ("| " ^ ans));
      pp_print_newline fmt ()
    in
    let header = List.fold_right (fun a h -> fixw (Atom.to_string a) ^ h) ats formula_header in
    let separator = String.make ((width * List.length ats) + String.length formula_header) '-' in
    pp_print_string fmt header;
    pp_print_newline fmt ();
    pp_print_string fmt separator;
    pp_print_newline fmt ();
    Seq.iter Fun.id (onallvaluations mk_row ats);
    pp_print_string fmt separator;
    pp_print_newline fmt ()

  let tautology fm = Seq.for_all Fun.id (onallvaluations (eval fm) (atoms fm))
  let unsatisfiable fm = tautology (Not fm)
  let satisfiable fm = not (unsatisfiable fm)

  module Function = Function.Make (Atom)

  let psubst subfn = onatoms (fun p -> Function.tryapplyd subfn (Syntax.Formula.Atom p) p)
end
