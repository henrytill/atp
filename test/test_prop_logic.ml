module Syntax = Prop_logic.Syntax

let syntax = Alcotest.testable Syntax.pp_ast Syntax.equal

module Test_parse = struct
  module Prop = Syntax.Prop
  open Syntax.Formula

  let same_fm = "same formula"

  let atom () =
    let expected : Syntax.t option = Some (Atom (Prop.inj "a")) in
    let actual = Prop_logic.parse_string {| a |} in
    Alcotest.(check (option syntax)) same_fm expected actual

  let example () =
    let expected =
      Some (Imp (Or (Atom (Prop.inj "p"), Atom (Prop.inj "q")), Atom (Prop.inj "r")))
    in
    let actual = Prop_logic.parse_string {| p \/ q ==> r |} in
    Alcotest.(check (option syntax)) same_fm expected actual

  let another_example () =
    let expected =
      Some
        (Imp
           ( Atom (Prop.inj "p"),
             Or (And (Atom (Prop.inj "q"), Not (Atom (Prop.inj "r"))), Atom (Prop.inj "s")) ))
    in
    let actual = Prop_logic.parse_string {| p ==> q /\ ~ r \/ s |} in
    Alcotest.(check (option syntax)) same_fm expected actual

  let right_associative_ands () =
    let expected =
      Some (And (Atom (Prop.inj "p"), And (Atom (Prop.inj "q"), Atom (Prop.inj "r"))))
    in
    let actual = Prop_logic.parse_string {| p /\ q /\ r |} in
    Alcotest.(check (option syntax)) same_fm expected actual

  let right_associative_imps () =
    let expected =
      Some (Imp (Atom (Prop.inj "p"), Imp (Atom (Prop.inj "q"), Atom (Prop.inj "r"))))
    in
    let actual = Prop_logic.parse_string {| p ==> q ==> r |} in
    Alcotest.(check (option syntax)) same_fm expected actual
end

module Test_pp = struct
  let same_string = "same string"

  let roundtrip (s : string) : string option =
    let to_string fm =
      Syntax.pp Format.str_formatter fm;
      Format.flush_str_formatter ()
    in
    Option.map to_string (Prop_logic.parse_string s)

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

module Test_eval = struct
  module Prop = Syntax.Prop

  let same_bool = "same bool"

  let read_eval (s : string) (v : Prop.t -> bool) : bool option =
    Option.map (fun fm -> Prop_logic.eval fm v) (Prop_logic.parse_string s)

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
    ( "Test_eval",
      [
        test_case "Parse and eval example" `Quick Test_eval.example;
        test_case "Parse and eval another example" `Quick Test_eval.another_example;
      ] );
  ]

let () = Alcotest.run "Prop_logic" prop_logic_tests
