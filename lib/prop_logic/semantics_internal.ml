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
    let offset_table =
      List.fold_left (fun (i, m) a -> (i - 1, Atom_map.add a i m)) (ats_len - 1, Atom_map.empty) ats
      |> snd
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
      List.fold_right (fun x -> Int.max (String.length (Atom.to_string x))) ats false_len + 1
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

  module Function = struct
    type 'a t =
      | Empty
      | Leaf of int * (Atom.t * 'a) list
      | Branch of int * int * 'a t * 'a t

    let undefined = Empty

    let is_undefined f =
      match f with
      | Empty -> true
      | _ -> false

    let assocd (l : (Atom.t * 'a) list) (default : Atom.t -> 'a) (x : Atom.t) : 'a =
      match List.assoc_opt x l with
      | Some a -> a
      | None -> default x

    let applyd (f : 'a t) (default : Atom.t -> 'a) (x : Atom.t) : 'a =
      let k = Atom.hash x in
      let rec look t =
        match t with
        | Leaf (h, l) when h = k -> assocd l default x
        | Branch (p, b, l, r) when k lxor p land (b - 1) = 0 -> look (if k land b = 0 then l else r)
        | _ -> default x
      in
      look f

    let tryapplyd (f : 'a t) (x : Atom.t) (default : 'a) : 'a = applyd f (fun _ -> default) x
    let apply (f : 'a t) : Atom.t -> 'a = applyd f (fun _ -> failwith "apply")

    let make_branch p1 t1 p2 t2 =
      (* Find differing bits between the two prefixes *)
      let zp = p1 lxor p2 in
      (* Find lowest differing bit aka the branching bit *)
      let b = zp land -zp in
      (* Get common prefix up to the branching bit *)
      let p = p1 land (b - 1) in
      if p1 land b = 0 then
        Branch (p, b, t1, t2)
      else
        Branch (p, b, t2, t1)

    let rec define_list ((x, _) as xy) l =
      match l with
      | [] -> [ xy ]
      | ((a, _) as ab) :: t ->
          let c = Atom.compare x a in
          if c = 0 then
            xy :: t
          else if c < 0 then
            xy :: l
          else
            ab :: define_list xy t

    let ( |-> ) x y =
      let k = Atom.hash x in
      let rec upd t =
        match t with
        | Empty -> Leaf (k, [ (x, y) ])
        | Leaf (h, l) when h = k -> Leaf (h, define_list (x, y) l)
        | Leaf (h, _) -> make_branch h t k (Leaf (k, [ (x, y) ]))
        | Branch (p, b, _, _) when k land (b - 1) <> p -> make_branch p t k (Leaf (k, [ (x, y) ]))
        | Branch (p, b, l, r) when k land b = 0 -> Branch (p, b, upd l, r)
        | Branch (p, b, l, r) -> Branch (p, b, l, upd r)
      in
      upd

    let ( |=> ) x y = (x |-> y) undefined
  end

  let psubst subfn = onatoms (fun p -> Function.tryapplyd subfn p (Syntax.Formula.Atom p))
end
