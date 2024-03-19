module Syntax = Intro.Syntax

let intro_testable = Alcotest.testable Syntax.pp Syntax.equal
let same_expr = "same expression"

module Test_parse = struct
  open Syntax

  let var () =
    let expected = Some (Var "a") in
    let actual = Intro.parse_string {| a |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let const () =
    let expected = Some (Const 42) in
    let actual = Intro.parse_string {| 42 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let add () =
    let expected = Some (Add (Const 42, Const 42)) in
    let actual = Intro.parse_string {| 42 + 42 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let mul () =
    let expected = Some (Mul (Const 42, Const 42)) in
    let actual = Intro.parse_string {| 42 * 42 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let exp () =
    let expected = Some (Exp (Const 2, Const 3)) in
    let actual = Intro.parse_string {| 2 ^ 3 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let neg () =
    let expected = Some (Sub (Var "x", Neg (Neg (Var "x")))) in
    let actual = Intro.parse_string {| x - - - x|} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let compound1 () =
    let expected = Some (Add (Mul (Const 2, Var "x"), Var "y")) in
    let actual = Intro.parse_string {| 2 * x + y |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let compound2 () =
    let expected = Some (Add (Mul (Add (Mul (Const 0, Var "x"), Const 1), Const 3), Const 12)) in
    let actual = Intro.parse_string {| (0 * x + 1) * 3 + 12 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual
end

module Test_precedence = struct
  open Syntax

  let right () =
    let expected = Some (Add (Const 1, Mul (Const 2, Const 3))) in
    let actual = Intro.parse_string {| 1 + 2 * 3 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let left () =
    let expected = Some (Add (Mul (Const 1, Const 2), Const 3)) in
    let actual = Intro.parse_string {| 1 * 2 + 3 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let add () =
    let expected = Some (Add (Add (Var "x", Var "y"), Var "z")) in
    let actual = Intro.parse_string {| x + y + z |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let sub () =
    let expected = Some (Sub (Sub (Var "x", Var "y"), Var "z")) in
    let actual = Intro.parse_string {| x - y - z |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let mul () =
    let expected = Some (Mul (Mul (Var "x", Var "y"), Var "z")) in
    let actual = Intro.parse_string {| x * y * z |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let exp () =
    let expected = Some (Exp (Var "x", Exp (Var "y", Var "z"))) in
    let acutal = Intro.parse_string {| x ^ y ^ z |} in
    Alcotest.(check (option intro_testable)) same_expr expected acutal

  let parens_left () =
    let expected = Some (Mul (Add (Const 1, Const 2), Const 3)) in
    let actual = Intro.parse_string {| (1 + 2) * 3 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let parens_right () =
    let expected = Some (Mul (Const 1, Add (Const 2, Const 3))) in
    let actual = Intro.parse_string {| 1 * (2 + 3) |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual
end

module Test_simplify = struct
  open Syntax

  let eval (s : string) : Syntax.t option = Intro.(parse_string s |> Option.map simplify)

  let add_0x () =
    let expected = Some (Var "x") in
    let actual = eval {| 0 + x |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let add_x0 () =
    let expected = Some (Var "x") in
    let actual = eval {| x + 0 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let sub () =
    let expected = Some (Const 1) in
    let actual = eval {| 3 - 2 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let sub_x0 () =
    let expected = Some (Var "x") in
    let actual = eval {| x - 0 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let sub_xx () =
    let expected = Some (Const 0) in
    let actual = eval {| x - x |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let mul () =
    let expected = Some (Const 12) in
    let actual = eval {| 3 * 4 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let mul_0x () =
    let expected = Some (Const 0) in
    let actual = eval {| 0 * x |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let mul_x0 () =
    let expected = Some (Const 0) in
    let actual = eval {| x * 0 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let mul_1x () =
    let expected = Some (Var "x") in
    let actual = eval {| 1 * x |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let mul_x1 () =
    let expected = Some (Var "x") in
    let actual = eval {| x * 1 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let exp () =
    let expected = Some (Const 8) in
    let actual = eval {| 2 ^ 3 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let exp_0x () =
    let expected = Some (Const 0) in
    let actual = eval {| 0 ^ x |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let exp_x0 () =
    let expected = Some (Const 1) in
    let actual = eval {| x ^ 0 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let exp_1x () =
    let open Syntax in
    let expected = Some (Const 1) in
    let actual = eval {| 1 ^ x |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let exp_x1 () =
    let expected = Some (Var "x") in
    let actual = eval {| x ^ 1 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let neg () =
    let expected = Some (Const 0) in
    let actual = eval {| x - - - x|} in
    Alcotest.(check (option intro_testable)) same_expr expected actual

  let example () =
    let expected = Some (Const 15) in
    let actual = eval {| (0 * x + 1) * 3 + 12 |} in
    Alcotest.(check (option intro_testable)) same_expr expected actual
end

let intro_tests =
  let open Alcotest in
  [
    ( "Test_parse",
      [
        test_case "Parse variable" `Quick Test_parse.var;
        test_case "Parse constant" `Quick Test_parse.const;
        test_case "Parse add" `Quick Test_parse.add;
        test_case "Parse mul" `Quick Test_parse.mul;
        test_case "Parse exp" `Quick Test_parse.exp;
        test_case "Parse neg" `Quick Test_parse.neg;
        test_case "Parse compound (1)" `Quick Test_parse.compound1;
        test_case "Parse compound (2)" `Quick Test_parse.compound2;
      ] );
    ( "Test_precedence",
      [
        test_case "Parse with correct precedence (r)" `Quick Test_precedence.right;
        test_case "Parse with correct precedence (l)" `Quick Test_precedence.left;
        test_case "Parse with correct add precedence" `Quick Test_precedence.add;
        test_case "Parse with correct sub precedence" `Quick Test_precedence.sub;
        test_case "Parse with correct mul precedence" `Quick Test_precedence.mul;
        test_case "Parse with correct exp precedence" `Quick Test_precedence.exp;
        test_case "Parse parens (l)" `Quick Test_precedence.parens_left;
        test_case "Parse parens (r)" `Quick Test_precedence.parens_right;
      ] );
    ( "Test_simplify",
      [
        test_case "Simplify 0 + x" `Quick Test_simplify.add_0x;
        test_case "Simplify x + 0" `Quick Test_simplify.add_x0;
        test_case "Simplify sub" `Quick Test_simplify.sub;
        test_case "Simplify x - 0" `Quick Test_simplify.sub_x0;
        test_case "Simplify x - x" `Quick Test_simplify.sub_xx;
        test_case "Simplify mul" `Quick Test_simplify.mul;
        test_case "Simplify 0 * x" `Quick Test_simplify.mul_0x;
        test_case "Simplify x * 0" `Quick Test_simplify.mul_x0;
        test_case "Simplify 1 * x" `Quick Test_simplify.mul_1x;
        test_case "Simplify x * 1" `Quick Test_simplify.mul_x1;
        test_case "Simplify exp" `Quick Test_simplify.exp;
        test_case "Simplify 0 ^ x" `Quick Test_simplify.exp_0x;
        test_case "Simplify x ^ 0" `Quick Test_simplify.exp_x0;
        test_case "Simplify 1 ^ x" `Quick Test_simplify.exp_1x;
        test_case "Simplify x ^ 1" `Quick Test_simplify.exp_x1;
        test_case "Simplify neg" `Quick Test_simplify.neg;
        test_case "Simplify example" `Quick Test_simplify.example;
      ] );
  ]

let () = Alcotest.run "Intro" intro_tests
