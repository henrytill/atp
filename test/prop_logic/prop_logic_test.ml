open Prop_logic

let syntax = Alcotest.testable Syntax.pp_ast Syntax.equal
let prop = Alcotest.testable Syntax.Prop.pp_ast Syntax.Prop.equal

module Test_parse = struct
  let same_fm = "same formula"

  let atom () =
    let expected : Syntax.t = Atom (Syntax.Prop.inj "a") in
    let actual = {%prop| a |} in
    Alcotest.(check syntax) same_fm expected actual

  let example () =
    let expected : Syntax.t =
      Imp (Or (Atom (Syntax.Prop.inj "p"), Atom (Syntax.Prop.inj "q")), Atom (Syntax.Prop.inj "r"))
    in
    let actual = {%prop| p \/ q ==> r |} in
    Alcotest.(check syntax) same_fm expected actual

  let another_example () =
    let expected : Syntax.t =
      Imp
        ( Atom (Syntax.Prop.inj "p"),
          Or
            ( And (Atom (Syntax.Prop.inj "q"), Not (Atom (Syntax.Prop.inj "r"))),
              Atom (Syntax.Prop.inj "s") ) )
    in
    let actual = {%prop| p ==> q /\ ~ r \/ s |} in
    Alcotest.(check syntax) same_fm expected actual

  let right_associative_ands () =
    let expected : Syntax.t =
      And (Atom (Syntax.Prop.inj "p"), And (Atom (Syntax.Prop.inj "q"), Atom (Syntax.Prop.inj "r")))
    in
    let actual = {%prop| p /\ q /\ r |} in
    Alcotest.(check syntax) same_fm expected actual

  let right_associative_imps () =
    let expected : Syntax.t =
      Imp (Atom (Syntax.Prop.inj "p"), Imp (Atom (Syntax.Prop.inj "q"), Atom (Syntax.Prop.inj "r")))
    in
    let actual = {%prop| p ==> q ==> r |} in
    Alcotest.(check syntax) same_fm expected actual
end

module Test_pp = struct
  let same_string = "same string"
  let to_string = Format.asprintf "%a" Syntax.pp

  let example () =
    let expected = "((p \\/ q) ==> r)" in
    let actual = to_string {%prop| p \/ q ==> r |} in
    Alcotest.(check string) same_string expected actual

  let another_example () =
    let expected = "(p ==> ((q /\\ (~ r)) \\/ s))" in
    let actual = to_string {%prop| p ==> q /\ ~r \/ s |} in
    Alcotest.(check string) same_string expected actual

  let right_associative_ands () =
    let expected = "(p /\\ (q /\\ r))" in
    let actual = to_string {%prop| p /\ q /\ r |} in
    Alcotest.(check string) same_string expected actual

  let right_associative_imps () =
    let expected = "(p ==> (q ==> r))" in
    let actual = to_string {%prop| p ==> q ==> r |} in
    Alcotest.(check string) same_string expected actual
end

module Test_semantics = struct
  let same_bool = "same bool"
  let same_list = "same list"

  let example () =
    let v prop =
      match Syntax.Prop.prj prop with
      | "p" -> true
      | "q" -> false
      | "r" -> true
      | _ -> failwith "unknown prop"
    in
    let expected = true in
    let actual = Semantics.eval {%prop| p /\ q ==> q /\ r |} v in
    Alcotest.(check bool) same_bool expected actual

  let another_example () =
    let v prop =
      match Syntax.Prop.prj prop with
      | "p" -> true
      | "q" -> true
      | "r" -> false
      | _ -> failwith "unknown prop"
    in
    let expected = false in
    let actual = Semantics.eval {%prop| p /\ q ==> q /\ r |} v in
    Alcotest.(check bool) same_bool expected actual

  module Int_semantics = Semantics_internal.Make (Int)

  let setify_example () =
    let expected = [ 1; 2; 3; 4 ] in
    let actual = Int_semantics.setify [ 1; 2; 3; 1; 4; 3 ] in
    Alcotest.(check (list int)) same_list expected actual

  let setify_reverse () =
    let expected = [ 1; 2; 3; 4 ] in
    let actual = Int_semantics.setify [ 4; 3; 2; 1 ] in
    Alcotest.(check (list int)) same_list expected actual

  let atoms_example () =
    let expected = Syntax.[ Prop.inj "p"; Prop.inj "q"; Prop.inj "r"; Prop.inj "s" ] in
    let actual = Semantics.atoms {%prop| p /\ q \/ s ==> ~p \/ (r <=> s) |} in
    Alcotest.(check (list prop)) same_list expected actual

  let tautology_examples () =
    let fms =
      [
        {%prop| true <=> false ==> false |};
        {%prop| ~p <=> p ==> false |};
        {%prop| p /\ q <=> (p ==> q ==> false) ==> false |};
        {%prop| p \/ q <=> (p ==> false) ==> q |};
        {%prop| (p <=> q) <=> ((p ==> q) ==> (q ==> p) ==> false) ==> false |};
      ]
    in
    Alcotest.(check bool) same_bool true (List.for_all Semantics.tautology fms)

  let true_taut () =
    let fm = {%prop| true |} in
    Alcotest.(check bool) same_bool true (Semantics.tautology fm)

  let true_satis () =
    let fm = {%prop| true |} in
    Alcotest.(check bool) same_bool true (Semantics.satisfiable fm)

  let peirce_taut () =
    let fm = {%prop| ((p ==> q) ==> p) ==> p |} in
    Alcotest.(check bool) same_bool true (Semantics.tautology fm)

  let peirce_satis () =
    let fm = {%prop| ((p ==> q) ==> p) ==> p |} in
    Alcotest.(check bool) same_bool true (Semantics.satisfiable fm)

  let example_not_taut () =
    let fm = {%prop| p /\ q ==> q /\ r |} in
    Alcotest.(check bool) same_bool false (Semantics.tautology fm)

  let example_satis () =
    let fm = {%prop| p /\ q ==> q /\ r |} in
    Alcotest.(check bool) same_bool true (Semantics.satisfiable fm)

  let contradiction_not_taut () =
    let fm = {%prop| p /\ ~p |} in
    Alcotest.(check bool) same_bool false (Semantics.tautology fm)

  let contradiction_unsatis () =
    let fm = {%prop| p /\ ~p |} in
    Alcotest.(check bool) same_bool true (Semantics.unsatisfiable fm)
end

let prop_logic_tests =
  let open Alcotest in
  [
    ( "Test_parse",
      [
        test_case "Parse atom" `Quick Test_parse.atom;
        test_case "Parse example" `Quick Test_parse.example;
        test_case "Parse another example" `Quick Test_parse.another_example;
        test_case "Print right-associative ands" `Quick Test_parse.right_associative_ands;
        test_case "Print right-associative imps" `Quick Test_parse.right_associative_imps;
      ] );
    ( "Test_pp",
      [
        test_case "Parse and print example" `Quick Test_pp.example;
        test_case "Parse and print another example" `Quick Test_pp.another_example;
        test_case "Parse and print right-associative ands" `Quick Test_pp.right_associative_ands;
        test_case "Parse and print right-associative imps" `Quick Test_pp.right_associative_imps;
      ] );
    ( "Test_semantics",
      [
        test_case "Parse and eval example" `Quick Test_semantics.example;
        test_case "Parse and eval another example" `Quick Test_semantics.another_example;
        test_case "setify removes duplicates and sorts" `Quick Test_semantics.setify_example;
        test_case "setify reverses a list" `Quick Test_semantics.setify_reverse;
        test_case "atoms returns the set of propositions" `Quick Test_semantics.atoms_example;
        test_case "tautology is true for all examples" `Quick Test_semantics.tautology_examples;
        test_case "true is a tautology" `Quick Test_semantics.true_taut;
        test_case "true is satisfiable" `Quick Test_semantics.true_satis;
        test_case "Peirce's Law is a tautology" `Quick Test_semantics.peirce_taut;
        test_case "Peirce's Law is satisfiable" `Quick Test_semantics.peirce_satis;
        test_case "The example is not a tautology" `Quick Test_semantics.example_not_taut;
        test_case "The example is satisfiable" `Quick Test_semantics.example_satis;
        test_case "Contradiction is not a tautology" `Quick Test_semantics.contradiction_not_taut;
        test_case "Contradiction is unsatisfiable" `Quick Test_semantics.contradiction_unsatis;
      ] );
  ]

let () = Alcotest.run "Prop_logic" prop_logic_tests