module Syntax = Intro.Syntax

let intro_testable = Alcotest.testable Syntax.pp Syntax.equal
let same_expr = "same expression"
let eval (s : string) : Syntax.t option = Intro.(parse_string s |> Option.map simplify)

let test_parse_var () =
  let open Syntax in
  let expected = Some (Var "a") in
  let actual = Intro.parse_string {| a |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_parse_const () =
  let open Syntax in
  let expected = Some (Const 42) in
  let actual = Intro.parse_string {| 42 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_parse_add () =
  let open Syntax in
  let expected = Some (Add (Const 42, Const 42)) in
  let actual = Intro.parse_string {| 42 + 42 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_parse_mul () =
  let open Syntax in
  let expected = Some (Mul (Const 42, Const 42)) in
  let actual = Intro.parse_string {| 42 * 42 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_parse_exp () =
  let open Syntax in
  let expected = Some (Exp (Const 2, Const 3)) in
  let actual = Intro.parse_string {| 2 ^ 3 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_parse_neg () =
  let open Syntax in
  let expected = Some (Sub (Var "x", Neg (Neg (Var "x")))) in
  let actual = Intro.parse_string {| x - - - x|} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_parse_compound1 () =
  let open Syntax in
  let expected = Some (Add (Mul (Const 2, Var "x"), Var "y")) in
  let actual = Intro.parse_string {| 2 * x + y |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_parse_compound2 () =
  let open Syntax in
  let expected = Some (Add (Mul (Add (Mul (Const 0, Var "x"), Const 1), Const 3), Const 12)) in
  let actual = Intro.parse_string {| (0 * x + 1) * 3 + 12 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_precedence_right () =
  let open Syntax in
  let expected = Some (Add (Const 1, Mul (Const 2, Const 3))) in
  let actual = Intro.parse_string {| 1 + 2 * 3 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_precedence_left () =
  let open Syntax in
  let expected = Some (Add (Mul (Const 1, Const 2), Const 3)) in
  let actual = Intro.parse_string {| 1 * 2 + 3 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_precedence_add () =
  let open Syntax in
  let expected = Some (Add (Add (Var "x", Var "y"), Var "z")) in
  let actual = Intro.parse_string {| x + y + z |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_precedence_sub () =
  let open Syntax in
  let expected = Some (Sub (Sub (Var "x", Var "y"), Var "z")) in
  let actual = Intro.parse_string {| x - y - z |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_precedence_mul () =
  let open Syntax in
  let expected = Some (Mul (Mul (Var "x", Var "y"), Var "z")) in
  let actual = Intro.parse_string {| x * y * z |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_precedence_exp () =
  let open Syntax in
  let expected = Some (Exp (Var "x", Exp (Var "y", Var "z"))) in
  let acutal = Intro.parse_string {| x ^ y ^ z |} in
  Alcotest.(check (option intro_testable)) same_expr expected acutal

let test_parens_left () =
  let open Syntax in
  let expected = Some (Mul (Add (Const 1, Const 2), Const 3)) in
  let actual = Intro.parse_string {| (1 + 2) * 3 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_parens_right () =
  let open Syntax in
  let expected = Some (Mul (Const 1, Add (Const 2, Const 3))) in
  let actual = Intro.parse_string {| 1 * (2 + 3) |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_add_0x () =
  let open Syntax in
  let expected = Some (Var "x") in
  let actual = eval {| 0 + x |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_add_x0 () =
  let open Syntax in
  let expected = Some (Var "x") in
  let actual = eval {| x + 0 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_sub () =
  let open Syntax in
  let expected = Some (Const 1) in
  let actual = eval {| 3 - 2 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_sub_x0 () =
  let open Syntax in
  let expected = Some (Var "x") in
  let actual = eval {| x - 0 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_sub_xx () =
  let open Syntax in
  let expected = Some (Const 0) in
  let actual = eval {| x - x |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_mul () =
  let open Syntax in
  let expected = Some (Const 12) in
  let actual = eval {| 3 * 4 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_mul_0x () =
  let open Syntax in
  let expected = Some (Const 0) in
  let actual = eval {| 0 * x |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_mul_x0 () =
  let open Syntax in
  let expected = Some (Const 0) in
  let actual = eval {| x * 0 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_mul_1x () =
  let open Syntax in
  let expected = Some (Var "x") in
  let actual = eval {| 1 * x |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_mul_x1 () =
  let open Syntax in
  let expected = Some (Var "x") in
  let actual = eval {| x * 1 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_exp () =
  let open Syntax in
  let expected = Some (Const 8) in
  let actual = eval {| 2 ^ 3 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_exp_0x () =
  let open Syntax in
  let expected = Some (Const 0) in
  let actual = eval {| 0 ^ x |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_exp_x0 () =
  let open Syntax in
  let expected = Some (Const 1) in
  let actual = eval {| x ^ 0 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_exp_1x () =
  let open Syntax in
  let expected = Some (Const 1) in
  let actual = eval {| 1 ^ x |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_exp_x1 () =
  let open Syntax in
  let expected = Some (Var "x") in
  let actual = eval {| x ^ 1 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_neg () =
  let open Syntax in
  let expected = Some (Const 0) in
  let actual = eval {| x - - - x|} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let test_simplify_example () =
  let open Syntax in
  let expected = Some (Const 15) in
  let actual = eval {| (0 * x + 1) * 3 + 12 |} in
  Alcotest.(check (option intro_testable)) same_expr expected actual

let intro_tests =
  let open Alcotest in
  [
    ( "test_parse",
      [
        test_case "Parse variable" `Quick test_parse_var;
        test_case "Parse constant" `Quick test_parse_const;
        test_case "Parse add" `Quick test_parse_add;
        test_case "Parse mul" `Quick test_parse_mul;
        test_case "Parse exp" `Quick test_parse_exp;
        test_case "Parse neg" `Quick test_parse_neg;
        test_case "Parse compound (1)" `Quick test_parse_compound1;
        test_case "Parse compound (2)" `Quick test_parse_compound2;
      ] );
    ( "test_precedence",
      [
        test_case "Parse with correct precedence (r)" `Quick test_precedence_right;
        test_case "Parse with correct precedence (l)" `Quick test_precedence_left;
        test_case "Parse with correct add precedence" `Quick test_precedence_add;
        test_case "Parse with correct sub precedence" `Quick test_precedence_sub;
        test_case "Parse with correct mul precedence" `Quick test_precedence_mul;
        test_case "Parse with correct exp precedence" `Quick test_precedence_exp;
        test_case "Parse parens (l)" `Quick test_parens_left;
        test_case "Parse parens (r)" `Quick test_parens_right;
      ] );
    ( "test_simplify",
      [
        test_case "Simplify 0 + x" `Quick test_simplify_add_0x;
        test_case "Simplify x + 0" `Quick test_simplify_add_x0;
        test_case "Simplify sub" `Quick test_simplify_sub;
        test_case "Simplify x - 0" `Quick test_simplify_sub_x0;
        test_case "Simplify x - x" `Quick test_simplify_sub_xx;
        test_case "Simplify mul" `Quick test_simplify_mul;
        test_case "Simplify 0 * x" `Quick test_simplify_mul_0x;
        test_case "Simplify x * 0" `Quick test_simplify_mul_x0;
        test_case "Simplify 1 * x" `Quick test_simplify_mul_1x;
        test_case "Simplify x * 1" `Quick test_simplify_mul_x1;
        test_case "Simplify exp" `Quick test_simplify_exp;
        test_case "Simplify 0 ^ x" `Quick test_simplify_exp_0x;
        test_case "Simplify x ^ 0" `Quick test_simplify_exp_x0;
        test_case "Simplify 1 ^ x" `Quick test_simplify_exp_1x;
        test_case "Simplify x ^ 1" `Quick test_simplify_exp_x1;
        test_case "Simplify neg" `Quick test_simplify_neg;
        test_case "Simplify example" `Quick test_simplify_example;
      ] );
  ]

let () = Alcotest.run "intro" intro_tests
