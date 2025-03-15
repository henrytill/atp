let rec eval fm v =
  let open Syntax.Formula in
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
  | Metavar _ -> failwith "metavariable"

let rec onatoms f fm =
  let open Syntax.Formula in
  match fm with
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
      let f a = Atom.to_string a |> String.length |> Int.max in
      succ (List.fold_right f ats false_len)
    in
    let fixw s = s ^ String.make (width - String.length s) ' ' in
    let truthstring p = fixw (string_of_bool p) in
    let mk_row v =
      let lis = List.map (fun x -> truthstring (v x)) ats in
      let ans = truthstring (eval fm v) in
      pp_print_string fmt (List.fold_right ( ^ ) lis ("| " ^ ans));
      pp_print_newline fmt ();
      true
    in
    let header = List.fold_right (fun s t -> fixw (Atom.to_string s) ^ t) ats formula_header in
    let separator = String.make ((width * List.length ats) + String.length formula_header) '-' in
    pp_print_string fmt header;
    pp_print_newline fmt ();
    pp_print_string fmt separator;
    pp_print_newline fmt ();
    let _ = Seq.for_all Fun.id (onallvaluations mk_row ats) in
    pp_print_string fmt separator;
    pp_print_newline fmt ()

  let tautology fm = Seq.for_all Fun.id (onallvaluations (eval fm) (atoms fm))
  let unsatisfiable fm = tautology (Not fm)
  let satisfiable fm = not (unsatisfiable fm)

  module Function = Function.Make (Atom)

  let psubst subfn = onatoms (fun p -> Function.tryapplyd subfn p (Syntax.Formula.Atom p))
end
