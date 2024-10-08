open Prop_logic

let syntax = Alcotest.testable Syntax.pp_ast Syntax.equal
let prop = Alcotest.testable Syntax.Prop.pp_ast Syntax.Prop.equal

module Test_parse = struct
  let same_fm = "same formula"

  let atom () =
    let expected : Syntax.t option = Some (Atom (Syntax.Prop.inj "a")) in
    let actual = Input.parse_string {| a |} in
    Alcotest.(check (option syntax)) same_fm expected actual

  let example () =
    let expected : Syntax.t option =
      Some
        (Imp
           (Or (Atom (Syntax.Prop.inj "p"), Atom (Syntax.Prop.inj "q")), Atom (Syntax.Prop.inj "r")))
    in
    let actual = Input.parse_string {| p \/ q ==> r |} in
    Alcotest.(check (option syntax)) same_fm expected actual

  let another_example () =
    let expected : Syntax.t option =
      Some
        (Imp
           ( Atom (Syntax.Prop.inj "p"),
             Or
               ( And (Atom (Syntax.Prop.inj "q"), Not (Atom (Syntax.Prop.inj "r"))),
                 Atom (Syntax.Prop.inj "s") ) ))
    in
    let actual = Input.parse_string {| p ==> q /\ ~ r \/ s |} in
    Alcotest.(check (option syntax)) same_fm expected actual

  let right_associative_ands () =
    let expected : Syntax.t option =
      Some
        (And
           (Atom (Syntax.Prop.inj "p"), And (Atom (Syntax.Prop.inj "q"), Atom (Syntax.Prop.inj "r"))))
    in
    let actual = Input.parse_string {| p /\ q /\ r |} in
    Alcotest.(check (option syntax)) same_fm expected actual

  let right_associative_imps () =
    let expected : Syntax.t option =
      Some
        (Imp
           (Atom (Syntax.Prop.inj "p"), Imp (Atom (Syntax.Prop.inj "q"), Atom (Syntax.Prop.inj "r"))))
    in
    let actual = Input.parse_string {| p ==> q ==> r |} in
    Alcotest.(check (option syntax)) same_fm expected actual
end

module Test_pp = struct
  let same_string = "same string"

  let roundtrip (s : string) : string option =
    let to_string fm =
      Syntax.pp Format.str_formatter fm;
      Format.flush_str_formatter ()
    in
    Option.map to_string (Input.parse_string s)

  let example () =
    let expected = Some "((p \\/ q) ==> r)" in
    let actual = roundtrip {| p \/ q ==> r |} in
    Alcotest.(check (option string)) same_string expected actual

  let another_example () =
    let expected = Some "(p ==> ((q /\\ (~ r)) \\/ s))" in
    let actual = roundtrip {| p ==> q /\ ~r \/ s |} in
    Alcotest.(check (option string)) same_string expected actual

  let right_associative_ands () =
    let expected = Some "(p /\\ (q /\\ r))" in
    let actual = roundtrip {| p /\ q /\ r |} in
    Alcotest.(check (option string)) same_string expected actual

  let right_associative_imps () =
    let expected = Some "(p ==> (q ==> r))" in
    let actual = roundtrip {| p ==> q ==> r |} in
    Alcotest.(check (option string)) same_string expected actual
end

module Test_semantics = struct
  let same_bool = "same bool"
  let same_list = "same list"

  let read_eval (s : string) (v : Syntax.Prop.t -> bool) : bool option =
    Input.parse_string s |> Option.map (fun fm -> Semantics.eval fm v)

  let example () =
    let v prop =
      match Syntax.Prop.prj prop with
      | "p" -> true
      | "q" -> false
      | "r" -> true
      | _ -> failwith "unknown prop"
    in
    let expected = Some true in
    let actual = read_eval {| p /\ q ==> q /\ r |} v in
    Alcotest.(check (option bool)) same_bool expected actual

  let another_example () =
    let v prop =
      match Syntax.Prop.prj prop with
      | "p" -> true
      | "q" -> true
      | "r" -> false
      | _ -> failwith "unknown prop"
    in
    let expected = Some false in
    let actual = read_eval {| p /\ q ==> q /\ r |} v in
    Alcotest.(check (option bool)) same_bool expected actual

  let setify_example () =
    let expected = [ 1; 2; 3; 4 ] in
    let actual = Semantics.Internal.setify [ 1; 2; 3; 1; 4; 3 ] in
    Alcotest.(check (list int)) same_list expected actual

  let setify_reverse () =
    let expected = [ 1; 2; 3; 4 ] in
    let actual = Semantics.Internal.setify [ 4; 3; 2; 1 ] in
    Alcotest.(check (list int)) same_list expected actual

  let atoms_example () =
    let read_atoms s = Input.parse_string s |> Option.map Semantics.atoms in
    let expected = Some Syntax.[ Prop.inj "p"; Prop.inj "q"; Prop.inj "r"; Prop.inj "s" ] in
    let actual = read_atoms {| p /\ q \/ s ==> ~p \/ (r <=> s) |} in
    Alcotest.(check (option (list prop))) same_list expected actual

  let make_fm s = Input.parse_string s |> Option.get

  let tautology_examples () =
    let fms =
      [
        make_fm {| true <=> false ==> false |};
        make_fm {| ~p <=> p ==> false |};
        make_fm {| p /\ q <=> (p ==> q ==> false) ==> false |};
        make_fm {| p \/ q <=> (p ==> false) ==> q |};
        make_fm {| (p <=> q) <=> ((p ==> q) ==> (q ==> p) ==> false) ==> false |};
      ]
    in
    Alcotest.(check bool) same_bool true (List.for_all Semantics.tautology fms)

  let true_taut () =
    let fm = make_fm {| true |} in
    Alcotest.(check bool) same_bool true (Semantics.tautology fm)

  let true_satis () =
    let fm = make_fm {| true |} in
    Alcotest.(check bool) same_bool true (Semantics.satisfiable fm)

  let peirce_taut () =
    let fm = make_fm {| ((p ==> q) ==> p) ==> p |} in
    Alcotest.(check bool) same_bool true (Semantics.tautology fm)

  let peirce_satis () =
    let fm = make_fm {| ((p ==> q) ==> p) ==> p |} in
    Alcotest.(check bool) same_bool true (Semantics.satisfiable fm)

  let example_not_taut () =
    let fm = make_fm {| p /\ q ==> q /\ r |} in
    Alcotest.(check bool) same_bool false (Semantics.tautology fm)

  let example_satis () =
    let fm = make_fm {| p /\ q ==> q /\ r |} in
    Alcotest.(check bool) same_bool true (Semantics.satisfiable fm)

  let contradiction_not_taut () =
    let fm = make_fm {| p /\ ~p |} in
    Alcotest.(check bool) same_bool false (Semantics.tautology fm)

  let contradiction_unsatis () =
    let fm = make_fm {| p /\ ~p |} in
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
