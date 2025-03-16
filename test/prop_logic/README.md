# Prop_logic

```ocaml
# #require "atp.ppx_prop";;
# #require "atp.prop_logic";;
# open Prop_logic;;
```

## Parsing

```ocaml
# #install_printer Syntax.Formula.pp_ast;;
# #install_printer Syntax.Prop.pp_ast;;
```

$a$

```ocaml
# Input.parse_string {| a |};;
- : Syntax.t option = Some (Atom "a")
```

$\lnot \bot$

```ocaml
# Input.parse_string {| ~ false |};;
- : Syntax.t option = Some (Not False)
```

$p \implies q$

```ocaml
# Input.parse_string {| p ==> q |};;
- : Syntax.t option = Some (Imp (Atom "p", Atom "q"))
```

$p \lor q \implies r$

```ocaml
# Input.parse_string {| p \/ q ==> r |};;
- : Syntax.t option = Some (Imp (Or (Atom "p", Atom "q"), Atom "r"))
```

$p \implies q \land \neg r \lor s$

```ocaml
# Input.parse_string {| p ==> q /\ ~ r \/ s |};;
- : Syntax.t option =
Some (Imp (Atom "p", Or (And (Atom "q", Not (Atom "r")), Atom "s")))
```

$p \land q \land r$

```ocaml
# Input.parse_string {| p /\ q /\ r |};;
- : Syntax.t option = Some (And (Atom "p", And (Atom "q", Atom "r")))
```

$p \implies q \implies r$

```ocaml
# Input.parse_string {| p ==> q ==> r |};;
- : Syntax.t option = Some (Imp (Atom "p", Imp (Atom "q", Atom "r")))
```

## Pretty printing

```ocaml
# let roundtrip (s : string) : string option =
    let to_string fm =
      Syntax.pp Format.str_formatter fm;
      Format.flush_str_formatter ()
    in
    Option.map to_string (Input.parse_string s);;
val roundtrip : string -> string option = <fun>
```

$p \lor q \implies r$

```ocaml
# roundtrip {| p \/ q ==> r |};;
- : string option = Some "((p \\/ q) ==> r)"
```

$p \implies q \land \neg r \lor s$

```ocaml
# roundtrip {| p ==> q /\ ~ r \/ s |};;
- : string option = Some "(p ==> ((q /\\ (~ r)) \\/ s))"
```

$p \land q \land r$

```ocaml
# roundtrip {| p /\ q /\ r |};;
- : string option = Some "(p /\\ (q /\\ r))"
```

$p \implies q \implies r$

```ocaml
# roundtrip {| p ==> q ==> r |};;
- : string option = Some "(p ==> (q ==> r))"
```

## Semantics

### Evaluation

$v〚p〛= \text{true}$

$v〚q〛= \text{false}$

$v〚r〛= \text{true}$

```ocaml
# let v prop =
    match Syntax.Prop.prj prop with
    | "p" -> true
    | "q" -> false
    | "r" -> true
    | _ -> failwith "unknown prop";;
val v : Syntax.Prop.t -> bool = <fun>
```

$〚p \land q \implies q \land r〛_v = \text{true}$

```ocaml
# Semantics.eval {%prop| p /\ q ==> q /\ r |} v;;
- : bool = true
```

$v〚p〛= \text{true}$

$v〚q〛= \text{true}$

$v〚r〛= \text{false}$

```ocaml
# let v prop =
    match Syntax.Prop.prj prop with
    | "p" -> true
    | "q" -> true
    | "r" -> false
    | _ -> failwith "unknown prop";;
val v : Syntax.Prop.t -> bool = <fun>
```

$〚p \land q \implies q \land r〛_v = \text{false}$

```ocaml
# Semantics.eval {%prop| p /\ q ==> q /\ r |} v;;
- : bool = false
```

### Internals

```ocaml
# module Int_semantics = Semantics_internal.Make (Int);;
module Int_semantics :
  sig
    val setify : int list -> int list
    val atom_union : ('a -> int list) -> 'a Syntax.Formula.t -> int list
    val atoms : int Syntax.Formula.t -> int list
    val onallvaluations : ((int -> bool) -> 'a) -> int list -> 'a Seq.t
    val false_len : int
    val formula_header : string
    val print_truthtable : Format.formatter -> int Syntax.Formula.t -> unit
    val tautology : int Syntax.Formula.t -> bool
    val unsatisfiable : int Syntax.Formula.t -> bool
    val satisfiable : int Syntax.Formula.t -> bool
    module Function :
      sig
        type key = int
        type 'a t = 'a Prop_logic.Function.Make(Int).t
        val undefined : 'a t
        val is_undefined : 'a t -> bool
        val tryapplyd : 'a t -> key -> 'a -> 'a
        val apply : 'a t -> key -> 'a
        val ( |-> ) : key -> 'a -> 'a t -> 'a t
        val ( |=> ) : key -> 'a -> 'a t
      end
    val psubst :
      int Syntax.Formula.t Function.t ->
      int Syntax.Formula.t -> int Syntax.Formula.t
  end
```

```ocaml
# Int_semantics.setify [ 1; 2; 3; 1; 4; 3 ];;
- : int list = [1; 2; 3; 4]
```

```ocaml
# Int_semantics.setify [ 4; 3; 2; 1 ];;
- : int list = [1; 2; 3; 4]
```

```ocaml
# Semantics.atoms {%prop| p /\ q \/ s ==> ~p \/ (r <=> s) |};;
- : Syntax.Prop.t list = ["p"; "q"; "r"; "s"]
```

### Truth Tables

```ocaml
# let print_truthtable = Semantics.print_truthtable Format.std_formatter;;
val print_truthtable : Syntax.t -> unit = <fun>
```

$p \implies q$

```ocaml
# print_truthtable {%prop| p ==> q |};;
p     q     | formula
---------------------
false false | true
false true  | true
true  false | false
true  true  | true
---------------------
- : unit = ()
```

$p \land q \implies q$

```ocaml
# print_truthtable {%prop| p /\ q ==> q |};;
p     q     | formula
---------------------
false false | true
false true  | true
true  false | true
true  true  | true
---------------------
- : unit = ()
```

$p \land q \implies q \land r$

```ocaml
# print_truthtable {%prop| p /\ q ==> q /\ r |};;
p     q     r     | formula
---------------------------
false false false | true
false false true  | true
false true  false | true
false true  true  | true
true  false false | true
true  false true  | true
true  true  false | false
true  true  true  | true
---------------------------
- : unit = ()
```

#### Example, p. 36

$p \implies q \implies r$

```ocaml
# print_truthtable {%prop| p ==> q ==> r |};;
p     q     r     | formula
---------------------------
false false false | true
false false true  | true
false true  false | true
false true  true  | true
true  false false | true
true  false true  | true
true  true  false | false
true  true  true  | true
---------------------------
- : unit = ()
```

#### Peirce's Law, p. 39

$((p \implies q) \implies p) \implies p$

```ocaml
# print_truthtable {%prop| ((p ==> q) ==> p) ==> p |};;
p     q     | formula
---------------------
false false | true
false true  | true
true  false | true
true  true  | true
---------------------
- : unit = ()
```

#### A simple contradiction, p. 40

$p \land \lnot p$

```ocaml
# print_truthtable {%prop| p /\ ~p |};;
p     | formula
---------------
false | false
true  | false
---------------
- : unit = ()
```

#### Example, p. 56

$(p \lor q \land r) \land (\lnot p \lor \lnot r)$

```ocaml
# print_truthtable {%prop| (p \/ q /\ r) /\ (~p \/ ~r) |};;
p     q     r     | formula
---------------------------
false false false | false
false false true  | false
false true  false | false
false true  true  | true
true  false false | true
true  false true  | false
true  true  false | true
true  true  true  | false
---------------------------
- : unit = ()
```

### Tautology

$\forall v. 〚\top \iff \bot \implies \bot〛_v = \text{true}$

```ocaml
# Semantics.tautology {%prop| true <=> false ==> false |};;
- : bool = true
```

$\forall v. 〚\neg p \iff p \implies \bot〛_v = \text{true}$

```ocaml
# Semantics.tautology {%prop| ~p <=> p ==> false |};;
- : bool = true
```

$\forall v. 〚p \land q \iff (p \implies q \implies \bot) \implies \bot〛_v = \text{true}$

```ocaml
# Semantics.tautology {%prop| p /\ q <=> (p ==> q ==> false) ==> false |};;
- : bool = true
```

$\forall v. 〚p \lor q \iff (p \implies \bot) \implies q〛_v = \text{true}$

```ocaml
# Semantics.tautology {%prop| p \/ q <=> (p ==> false) ==> q |};;
- : bool = true
```

$\forall v. 〚(p \iff q) \iff ((p \implies q) \implies (q \implies p) \implies \bot) \implies \bot〛_v = \text{true}$

```ocaml
# Semantics.tautology {%prop| (p <=> q) <=> ((p ==> q) ==> (q ==> p) ==> false) ==> false |};;
- : bool = true
```

### Substitution

```ocaml
# let f =
    let p = Syntax.Prop.inj "p" in
    Semantics.Function.(p |=> {%prop| p /\ q |});;
val f : Syntax.Prop.t Syntax.Formula.t Semantics.Function.t = <abstr>
# Semantics.psubst f {%prop| p /\ q /\ p /\ q |};;
- : Syntax.t =
(And (And (Atom "p", Atom "q"), And (Atom "q", And (And (Atom "p", Atom "q"), Atom "q"))))
```
