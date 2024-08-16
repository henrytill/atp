module Syntax = Prop_logic.Syntax
module Prop = Prop_logic.Syntax.Prop

let syntax = Alcotest.testable Syntax.pp_ast Syntax.equal
let prop = Alcotest.testable Prop.pp_ast Prop.equal

module Test_parse = struct
  open Syntax.Formula

  let same_fm = "same formula"

  let atom () =
    let expected : Syntax.t option = Some (Atom (Prop.inj "a")) in
    let actual = Prop_logic.Input.parse_string {| a |} in
    Alcotest.(check (option syntax)) same_fm expected actual

  let example () =
    let expected =
      Some (Imp (Or (Atom (Prop.inj "p"), Atom (Prop.inj "q")), Atom (Prop.inj "r")))
    in
    let actual = Prop_logic.Input.parse_string {| p \/ q ==> r |} in
    Alcotest.(check (option syntax)) same_fm expected actual

  let another_example () =
    let expected =
      Some
        (Imp
           ( Atom (Prop.inj "p"),
             Or (And (Atom (Prop.inj "q"), Not (Atom (Prop.inj "r"))), Atom (Prop.inj "s")) ))
    in
    let actual = Prop_logic.Input.parse_string {| p ==> q /\ ~ r \/ s |} in
    Alcotest.(check (option syntax)) same_fm expected actual

  let right_associative_ands () =
    let expected =
      Some (And (Atom (Prop.inj "p"), And (Atom (Prop.inj "q"), Atom (Prop.inj "r"))))
    in
    let actual = Prop_logic.Input.parse_string {| p /\ q /\ r |} in
    Alcotest.(check (option syntax)) same_fm expected actual

  let right_associative_imps () =
    let expected =
      Some (Imp (Atom (Prop.inj "p"), Imp (Atom (Prop.inj "q"), Atom (Prop.inj "r"))))
    in
    let actual = Prop_logic.Input.parse_string {| p ==> q ==> r |} in
    Alcotest.(check (option syntax)) same_fm expected actual
end

module Test_pp = struct
  let same_string = "same string"

  let roundtrip (s : string) : string option =
    let to_string fm =
      Syntax.pp Format.str_formatter fm;
      Format.flush_str_formatter ()
    in
    Option.map to_string (Prop_logic.Input.parse_string s)

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

  let read_eval (s : string) (v : Prop.t -> bool) : bool option =
    Prop_logic.(Input.parse_string s |> Option.map (fun fm -> Semantics.eval fm v))

  let example () =
    let v prop =
      match Prop.prj prop with
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
      match Prop.prj prop with
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
    let actual = Prop_logic.Semantics.Internal.setify [ 1; 2; 3; 1; 4; 3 ] in
    Alcotest.(check (list int)) same_list expected actual

  let setify_reverse () =
    let expected = [ 1; 2; 3; 4 ] in
    let actual = Prop_logic.Semantics.Internal.setify [ 4; 3; 2; 1 ] in
    Alcotest.(check (list int)) same_list expected actual

  let atoms_example () =
    let read_atoms s = Prop_logic.(Input.parse_string s |> Option.map Semantics.atoms) in
    let expected = Some [ Prop.inj "p"; Prop.inj "q"; Prop.inj "r"; Prop.inj "s" ] in
    let actual = read_atoms {| p /\ q \/ s ==> ~p \/ (r <=> s) |} in
    Alcotest.(check (option (list prop))) same_list expected actual
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
        test_case "Setify removes duplicates and sorts" `Quick Test_semantics.setify_example;
        test_case "Setify reverses a list" `Quick Test_semantics.setify_reverse;
        test_case "Check atoms against example" `Quick Test_semantics.atoms_example;
      ] );
  ]

let () = Alcotest.run "Prop_logic" prop_logic_tests
