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

let test_parse_exp () =
  let open Syntax in
  let expected = Some (Exp (Const 2, Const 3)) in
  let acutal = Intro.parse {| 2 ^ 3 |} in
  Alcotest.(check (option intro_testable)) "same expression" expected acutal

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

let test_precedence_add () =
  let open Syntax in
  let expected = Some (Add (Add (Var "x", Var "y"), Var "z")) in
  let actual = Intro.parse {| x + y + z |} in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let test_precedence_sub () =
  let open Syntax in
  let expected = Some (Sub (Sub (Var "x", Var "y"), Var "z")) in
  let actual = Intro.parse {| x - y - z |} in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let test_precedence_mul () =
  let open Syntax in
  let expected = Some (Mul (Mul (Var "x", Var "y"), Var "z")) in
  let actual = Intro.parse {| x * y * z |} in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let test_precedence_exp () =
  let open Syntax in
  let expected = Some (Exp (Var "x", Exp (Var "y", Var "z"))) in
  let acutal = Intro.parse {| x ^ y ^ z |} in
  Alcotest.(check (option intro_testable)) "same expression" expected acutal

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

let test_simplify_sub () =
  let open Syntax in
  let expected = Some (Const 1) in
  let actual = Option.map Intro.simplify (Intro.parse {| 3 - 2 |}) in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let test_simplify_sub_zero () =
  let open Syntax in
  let expected = Some (Var "x") in
  let actual = Option.map Intro.simplify (Intro.parse {| x - 0 |}) in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let test_simplify_sub_same () =
  let open Syntax in
  let expected = Some (Const 0) in
  let actual = Option.map Intro.simplify (Intro.parse {| x - x |}) in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let test_simplify_mul () =
  let open Syntax in
  let expected = Some (Const 12) in
  let actual = Option.map Intro.simplify (Intro.parse {| 3 * 4 |}) in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let test_simplify_exp () =
  let open Syntax in
  let expected = Some (Const 8) in
  let actual = Option.map Intro.simplify (Intro.parse {| 2 ^ 3 |}) in
  Alcotest.(check (option intro_testable)) "same expression" expected actual

let test_simplify_example () =
  let open Syntax in
  let expected = Const 15 in
  let actual =
    Intro.simplify (Add (Mul (Add (Mul (Const 0, Var "x"), Const 1), Const 3), Const 12))
  in
  Alcotest.(check intro_testable) "same expression" expected actual

let intro_tests =
  [
    ( "test_parse",
      [
        Alcotest.test_case "Parse variable" `Quick test_parse_var;
        Alcotest.test_case "Parse constant" `Quick test_parse_const;
        Alcotest.test_case "Parse add" `Quick test_parse_add;
        Alcotest.test_case "Parse mul" `Quick test_parse_mul;
        Alcotest.test_case "Parse exp" `Quick test_parse_exp;
        Alcotest.test_case "Parse compound (1)" `Quick test_parse_compound1;
        Alcotest.test_case "Parse compound (2)" `Quick test_parse_compound2;
        Alcotest.test_case "Parse with correct precedence (r)" `Quick test_precedence_right;
        Alcotest.test_case "Parse with correct precedence (l)" `Quick test_precedence_left;
        Alcotest.test_case "Parse with correct add precedence" `Quick test_precedence_add;
        Alcotest.test_case "Parse with correct sub precedence" `Quick test_precedence_sub;
        Alcotest.test_case "Parse with correct mul precedence" `Quick test_precedence_mul;
        Alcotest.test_case "Parse with correct exp precedence" `Quick test_precedence_exp;
        Alcotest.test_case "Parse parens (l)" `Quick test_parens_left;
        Alcotest.test_case "Parse parens (r)" `Quick test_parens_right;
      ] );
    ( "test_simplify",
      [
        Alcotest.test_case "Simplify sub" `Quick test_simplify_sub;
        Alcotest.test_case "Simplify sub with zero" `Quick test_simplify_sub_zero;
        Alcotest.test_case "Simplify sub with same operands" `Quick test_simplify_sub_same;
        Alcotest.test_case "Simplify mul" `Quick test_simplify_mul;
        Alcotest.test_case "Simplify exp" `Quick test_simplify_exp;
        Alcotest.test_case "Simplify example" `Quick test_simplify_example;
      ] );
  ]

let () = Alcotest.run "intro" intro_tests
