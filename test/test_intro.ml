module Syntax = Intro.Syntax

let intro_testable = Alcotest.testable Syntax.pp Syntax.equal

let test_parse_const () =
  let open Syntax in
  let expected = Some (Const 42) in
  let actual = Intro.parse {| 42 |} in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let test_parse_add () =
  let open Syntax in
  let expected = Some (Add (Const 42, Const 42)) in
  let actual = Intro.parse {| 42 + 42 |} in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let intro_tests =
  [
    ( "test_parse",
      [
        Alcotest.test_case "Parse constant expression" `Quick test_parse_const;
        Alcotest.test_case "Parse add expression" `Quick test_parse_add;
      ] );
  ]

let () = Alcotest.run "intro" intro_tests
