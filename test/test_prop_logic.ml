module Syntax = Prop_logic.Syntax

let syntax = Alcotest.testable Syntax.pp_ast Syntax.equal
let same_expr = "same expression"

module Test_parse = struct
  module Prop = Syntax.Prop
  open Syntax.Formula

  let atom () =
    let expected : Syntax.t option = Some (Atom (Prop.make "a")) in
    let actual = Prop_logic.parse_string {| a |} in
    Alcotest.(check (option syntax)) same_expr expected actual

  let example () =
    let expected =
      Some (Imp (Or (Atom (Prop.make "p"), Atom (Prop.make "q")), Atom (Prop.make "r")))
    in
    let actual = Prop_logic.parse_string {| p \/ q ==> r |} in
    Alcotest.(check (option syntax)) same_expr expected actual

  let another_example () =
    let expected =
      Some
        (Imp
           ( Atom (Prop.make "p"),
             Or (And (Atom (Prop.make "q"), Not (Atom (Prop.make "r"))), Atom (Prop.make "s")) ))
    in
    let actual = Prop_logic.parse_string {| p ==> q /\ ~ r \/ s |} in
    Alcotest.(check (option syntax)) same_expr expected actual

  let right_associative_ands () =
    let expected =
      Some (And (Atom (Prop.make "p"), And (Atom (Prop.make "q"), Atom (Prop.make "r"))))
    in
    let actual = Prop_logic.parse_string {| p /\ q /\ r |} in
    Alcotest.(check (option syntax)) same_expr expected actual

  let right_associative_imps () =
    let expected =
      Some (Imp (Atom (Prop.make "p"), Imp (Atom (Prop.make "q"), Atom (Prop.make "r"))))
    in
    let actual = Prop_logic.parse_string {| p ==> q ==> r |} in
    Alcotest.(check (option syntax)) same_expr expected actual
end

module Test_pp = struct
  let roundtrip (s : string) : string option =
    let to_str fm =
      Syntax.pp Format.str_formatter fm;
      Format.flush_str_formatter ()
    in
    Option.map to_str (Prop_logic.parse_string s)

  let example () =
    let actual = Some "((p \\/ q) ==> r)" in
    let expected = roundtrip {| p \/ q ==> r |} in
    Alcotest.(check (option string)) same_expr expected actual

  let another_example () =
    let actual = Some "(p ==> ((q /\\ (~ r)) \\/ s))" in
    let expected = roundtrip {| p ==> q /\ ~r \/ s |} in
    Alcotest.(check (option string)) same_expr expected actual

  let right_associative_ands () =
    let actual = Some "(p /\\ (q /\\ r))" in
    let expected = roundtrip {| p /\ q /\ r |} in
    Alcotest.(check (option string)) same_expr expected actual

  let right_associative_imps () =
    let actual = Some "(p ==> (q ==> r))" in
    let expected = roundtrip {| p ==> q ==> r |} in
    Alcotest.(check (option string)) same_expr expected actual
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
  ]

let () = Alcotest.run "Prop_logic" prop_logic_tests
