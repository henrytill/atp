include Function_intf

module Make (Key : KEY_TYPE) = struct
  type key = Key.t

  type 'a t =
    | Empty
    | Leaf of int * (key * 'a) list
    | Branch of int * int * 'a t * 'a t

  let undefined = Empty

  let is_undefined = function
    | Empty -> true
    | _ -> false

  let assocd (l : (key * 'a) list) (default : key -> 'a) (x : key) : 'a =
    match List.assoc_opt x l with
    | Some a -> a
    | None -> default x

  let applyd (f : 'a t) (default : key -> 'a) (x : key) : 'a =
    let k = Key.hash x in
    let rec look = function
      | Leaf (h, l) when h = k -> assocd l default x
      | Branch (p, b, l, r) when k lxor p land (b - 1) = 0 -> look (if k land b = 0 then l else r)
      | _ -> default x
    in
    look f

  let tryapplyd (f : 'a t) (x : key) (default : 'a) : 'a = applyd f (fun _ -> default) x
  let apply (f : 'a t) : key -> 'a = applyd f (fun _ -> failwith "apply")

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

  let[@tail_mod_cons] rec define_list ((x, _) as xy) l =
    match l with
    | [] -> [ xy ]
    | ((a, _) as ab) :: t ->
        let c = Key.compare x a in
        if c = 0 then
          xy :: t
        else if c < 0 then
          xy :: l
        else
          ab :: define_list xy t

  let ( |-> ) x y =
    let k = Key.hash x in
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
