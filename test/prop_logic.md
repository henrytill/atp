# Prop_logic

```ocaml
# #require "atp.prop_logic";;
# open Prop_logic;;
```

## Parsing

```ocaml
# #install_printer Syntax.Prop.pp;;
# Input.parse_string {| p ==> q |};;
- : Syntax.t option =
Some
 (Prop_logic.Syntax.Formula.Imp (Prop_logic.Syntax.Formula.Atom p,
   Prop_logic.Syntax.Formula.Atom q))
```

## Truth Tables

```ocaml
# let print_truthtable s =
    Input.parse_string s
    |> Option.iter
    @@ Semantics.print_truthtable Format.std_formatter;;
val print_truthtable : string -> unit = <fun>

# print_truthtable {| p ==> q |};;
p     q     | formula
---------------------
false false | true
false true  | true
true  false | false
true  true  | true
---------------------
- : unit = ()

# print_truthtable {| p /\ q ==> q |};;
p     q     | formula
---------------------
false false | true
false true  | true
true  false | true
true  true  | true
---------------------
- : unit = ()
```
