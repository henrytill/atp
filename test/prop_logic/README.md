# Prop_logic

```ocaml
# #require "atp.ppx_prop";;
# #require "atp.prop_logic";;
# open Prop_logic;;
```

## 2.1 The syntax of propositional logic

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

### Pretty-printing

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

## 2.2 The semantics of propositional logic

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

### Truth-tables, mechanized

```ocaml
# Semantics.atoms {%prop| p /\ q \/ s ==> ~p \/ (r <=> s) |};;
- : Syntax.Prop.t list = ["p"; "q"; "r"; "s"]
```

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

**Example, p. 36**

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

## 2.3 Validity, satisﬁability and tautology

**Peirce's Law, p. 39**

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

**A simple contradiction, p. 40**

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

### Tautology and satisﬁability checking

**Examples, p. 41**

$\forall v. 〚p \lor \lnot p〛_v = \text{true}$

```ocaml
# Semantics.tautology {%prop| p \/ ~p |};;
- : bool = true
```

$\forall v. 〚p \lor q \implies p〛_v = \text{false}$

```ocaml
# Semantics.tautology {%prop| p \/ q ==> p |};;
- : bool = false
```

$\forall v. 〚p \lor q \implies q \lor (p \iff q)〛_v = \text{false}$

```ocaml
# Semantics.tautology {%prop| p \/ q ==> q \/ (p <=> q) |};;
- : bool = false
```

$\forall v. 〚(p \lor q) \land \neg (p \land q) \implies (\neg p \iff q)〛_v = \text{true}$

```ocaml
# Semantics.tautology {%prop| (p \/ q) /\ ~(p /\ q) ==> (~p <=> q) |};;
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

### Some important tautologies

$\neg \top \iff \bot$

```ocaml
# Semantics.tautology {%prop| ~true <=> false |};;
- : bool = true
```

$\neg \bot \iff \top$

```ocaml
# Semantics.tautology {%prop| ~false <=> true |};;
- : bool = true
```

$\neg \neg p \iff p$

```ocaml
# Semantics.tautology {%prop| ~~p <=> p |};;
- : bool = true
```

$p \land \bot \iff \bot$

```ocaml
# Semantics.tautology {%prop| p /\ false <=> false |};;
- : bool = true
```

$p \land \top \iff p$

```ocaml
# Semantics.tautology {%prop| p /\ true <=> p |};;
- : bool = true
```

$p \land p \iff p$

```ocaml
# Semantics.tautology {%prop| p /\ p <=> p |};;
- : bool = true
```

$p \land \neg p \iff \bot$

```ocaml
# Semantics.tautology {%prop| p /\ ~p <=> false |};;
- : bool = true
```

$p \land q \iff q \land p$

```ocaml
# Semantics.tautology {%prop| p /\ q <=> q /\ p |};;
- : bool = true
```

$p \land (q \land r) \iff (p \land q) \land r$

```ocaml
# Semantics.tautology {%prop| p /\ (q /\ r) <=> (p /\ q) /\ r |};;
- : bool = true
```

$p \lor \bot \iff p$

```ocaml
# Semantics.tautology {%prop| p \/ false <=> p |};;
- : bool = true
```

$p \lor \top \iff \top$

```ocaml
# Semantics.tautology {%prop| p \/ true <=> true |};;
- : bool = true
```

$p \lor p \iff p$

```ocaml
# Semantics.tautology {%prop| p \/ p <=> p |};;
- : bool = true
```

$p \lor \neg p \iff \top$

```ocaml
# Semantics.tautology {%prop| p \/ ~p <=> true |};;
- : bool = true
```

$p \lor q \iff q \lor p$

```ocaml
# Semantics.tautology {%prop| p \/ q <=> q \/ p |};;
- : bool = true
```

$p \lor (q \lor r) \iff (p \lor q) \lor r$

```ocaml
# Semantics.tautology {%prop| p \/ (q \/ r) <=> (p \/ q) \/ r |};;
- : bool = true
```

$p \land (q \lor r) \iff (p \land q) \lor  (p \land r)$

```ocaml
# Semantics.tautology {%prop| p /\ (q \/ r) <=> (p /\ q) \/ (p /\ r) |};;
- : bool = true
```

$p \lor (q \land r) \iff (p \lor q) \land  (p \lor r)$

```ocaml
# Semantics.tautology {%prop| p \/ (q /\ r) <=> (p \/ q) /\ (p \/ r) |};;
- : bool = true
```

$\bot \implies p \iff \top$

```ocaml
# Semantics.tautology {%prop| false ==> p <=> true |};;
- : bool = true
```

$p \implies \top \iff \top$

```ocaml
# Semantics.tautology {%prop| p ==> true <=> true |};;
- : bool = true
```

$p \implies \bot \iff \neg p$

```ocaml
# Semantics.tautology {%prop| p ==> false <=> ~p |};;
- : bool = true
```

$p \implies p \iff \top$

```ocaml
# Semantics.tautology {%prop| p ==> p <=> true |};;
- : bool = true
```

$p \implies q \iff \neg q \implies \neg p$

```ocaml
# Semantics.tautology {%prop| p ==> q <=> ~q ==> ~p |};;
- : bool = true
```

$p \implies q \iff (p \iff p \land q)$

```ocaml
# Semantics.tautology {%prop| p ==> q <=> (p <=> p /\ q) |};;
- : bool = true
```

$p \implies q \iff (q \iff q \lor p)$

```ocaml
# Semantics.tautology {%prop| p ==> q <=> (q <=> q \/ p) |};;
- : bool = true
```

$p \iff q \iff q \iff p$

```ocaml
# Semantics.tautology {%prop| p <=> q <=> q <=> p |};;
- : bool = true
```

$p \iff (q \iff r) \iff (p \iff q) \iff r$

```ocaml
# Semantics.tautology {%prop| p <=> (q <=> r) <=> (p <=> q) <=> r |};;
- : bool = true
```

## 2.4 The De Morgan laws, adequacy and duality

**Examples, p. 47**

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

**Example, p. 56**

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
