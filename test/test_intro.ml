module Syntax = Intro.Syntax

let intro_testable = Alcotest.testable Syntax.pp Syntax.equal

let test_parse_var () =
  let open Syntax in
  let expected = Some (Var "a") in
  let actual = Intro.parse {| a |} in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

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

let test_parse_mul () =
  let open Syntax in
  let expected = Some (Mul (Const 42, Const 42)) in
  let actual = Intro.parse {| 42 * 42 |} in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let test_parse_compound1 () =
  let open Syntax in
  let expected = Some (Add (Mul (Const 2, Var "x"), Var "y")) in
  let actual = Intro.parse {| 2 * x + y |} in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let test_parse_compound2 () =
  let open Syntax in
  let expected = Some (Add (Mul (Add (Mul (Const 0, Var "x"), Const 1), Const 3), Const 12)) in
  let actual = Intro.parse {| (0 * x + 1) * 3 + 12 |} in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let test_precedence_right () =
  let open Syntax in
  let expected = Some (Add (Const 1, Mul (Const 2, Const 3))) in
  let actual = Intro.parse {| 1 + 2 * 3 |} in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let test_precedence_left () =
  let open Syntax in
  let expected = Some (Add (Mul (Const 1, Const 2), Const 3)) in
  let actual = Intro.parse {| 1 * 2 + 3 |} in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let test_parens_left () =
  let open Syntax in
  let expected = Some (Mul (Add (Const 1, Const 2), Const 3)) in
  let actual = Intro.parse {| (1 + 2) * 3 |} in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let test_parens_right () =
  let open Syntax in
  let expected = Some (Mul (Const 1, Add (Const 2, Const 3))) in
  let actual = Intro.parse {| 1 * (2 + 3) |} in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let intro_tests =
  [
    ( "test_parse",
      [
        Alcotest.test_case "Parse variable expression" `Quick test_parse_var;
        Alcotest.test_case "Parse constant expression" `Quick test_parse_const;
        Alcotest.test_case "Parse add expression" `Quick test_parse_add;
        Alcotest.test_case "Parse mul expression" `Quick test_parse_mul;
        Alcotest.test_case "Parse compound expression (1)" `Quick test_parse_compound1;
        Alcotest.test_case "Parse compound expression (2)" `Quick test_parse_compound2;
        Alcotest.test_case "Parse with correct precedence (r)" `Quick test_precedence_right;
        Alcotest.test_case "Parse with correct precedence (l)" `Quick test_precedence_left;
        Alcotest.test_case "Parse parens (l)" `Quick test_parens_left;
        Alcotest.test_case "Parse parens (r)" `Quick test_parens_right;
      ] );
  ]

let () = Alcotest.run "intro" intro_tests
