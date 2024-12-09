open Intro

let syntax = Alcotest.testable Syntax.pp_ast Syntax.equal
let same_expr = "same expression"

module Test_parse = struct
  let var () =
    let expected : Syntax.t = Var "a" in
    let actual = {%intro| a |} in
    Alcotest.(check syntax) same_expr expected actual

  let const () =
    let expected : Syntax.t = Const 42 in
    let actual = {%intro| 42 |} in
    Alcotest.(check syntax) same_expr expected actual

  let add () =
    let expected : Syntax.t = Add (Const 42, Const 42) in
    let actual = {%intro| 42 + 42 |} in
    Alcotest.(check syntax) same_expr expected actual

  let mul () =
    let expected : Syntax.t = Mul (Const 42, Const 42) in
    let actual = {%intro| 42 * 42 |} in
    Alcotest.(check syntax) same_expr expected actual

  let exp () =
    let expected : Syntax.t = Exp (Const 2, Const 3) in
    let actual = {%intro| 2 ^ 3 |} in
    Alcotest.(check syntax) same_expr expected actual

  let neg () =
    let expected : Syntax.t = Sub (Var "x", Neg (Neg (Var "x"))) in
    let actual = {%intro| x - - - x|} in
    Alcotest.(check syntax) same_expr expected actual

  let compound1 () =
    let expected : Syntax.t = Add (Mul (Const 2, Var "x"), Var "y") in
    let actual = {%intro| 2 * x + y |} in
    Alcotest.(check syntax) same_expr expected actual

  let compound2 () =
    let expected : Syntax.t =
      Add (Mul (Add (Mul (Const 0, Var "x"), Const 1), Const 3), Const 12)
    in
    let actual = {%intro| (0 * x + 1) * 3 + 12 |} in
    Alcotest.(check syntax) same_expr expected actual
end

module Test_precedence = struct
  let right () =
    let expected : Syntax.t = Add (Const 1, Mul (Const 2, Const 3)) in
    let actual = {%intro| 1 + 2 * 3 |} in
    Alcotest.(check syntax) same_expr expected actual

  let left () =
    let expected : Syntax.t = Add (Mul (Const 1, Const 2), Const 3) in
    let actual = {%intro| 1 * 2 + 3 |} in
    Alcotest.(check syntax) same_expr expected actual

  let add () =
    let expected : Syntax.t = Add (Add (Var "x", Var "y"), Var "z") in
    let actual = {%intro| x + y + z |} in
    Alcotest.(check syntax) same_expr expected actual

  let sub () =
    let expected : Syntax.t = Sub (Sub (Var "x", Var "y"), Var "z") in
    let actual = {%intro| x - y - z |} in
    Alcotest.(check syntax) same_expr expected actual

  let mul () =
    let expected : Syntax.t = Mul (Mul (Var "x", Var "y"), Var "z") in
    let actual = {%intro| x * y * z |} in
    Alcotest.(check syntax) same_expr expected actual

  let exp () =
    let expected : Syntax.t = Exp (Var "x", Exp (Var "y", Var "z")) in
    let acutal = {%intro| x ^ y ^ z |} in
    Alcotest.(check syntax) same_expr expected acutal

  let parens_left () =
    let expected : Syntax.t = Mul (Add (Const 1, Const 2), Const 3) in
    let actual = {%intro| (1 + 2) * 3 |} in
    Alcotest.(check syntax) same_expr expected actual

  let parens_right () =
    let expected : Syntax.t = Mul (Const 1, Add (Const 2, Const 3)) in
    let actual = {%intro| 1 * (2 + 3) |} in
    Alcotest.(check syntax) same_expr expected actual
end

module Test_simplify = struct
  let add_0x () =
    let expected : Syntax.t = Var "x" in
    let actual = Semantics.simplify {%intro| 0 + x |} in
    Alcotest.(check syntax) same_expr expected actual

  let add_x0 () =
    let expected : Syntax.t = Var "x" in
    let actual = Semantics.simplify {%intro| x + 0 |} in
    Alcotest.(check syntax) same_expr expected actual

  let sub () =
    let expected : Syntax.t = Const 1 in
    let actual = Semantics.simplify {%intro| 3 - 2 |} in
    Alcotest.(check syntax) same_expr expected actual

  let sub_x0 () =
    let expected : Syntax.t = Var "x" in
    let actual = Semantics.simplify {%intro| x - 0 |} in
    Alcotest.(check syntax) same_expr expected actual

  let sub_xx () =
    let expected : Syntax.t = Const 0 in
    let actual = Semantics.simplify {%intro| x - x |} in
    Alcotest.(check syntax) same_expr expected actual

  let mul () =
    let expected : Syntax.t = Const 12 in
    let actual = Semantics.simplify {%intro| 3 * 4 |} in
    Alcotest.(check syntax) same_expr expected actual

  let mul_0x () =
    let expected : Syntax.t = Const 0 in
    let actual = Semantics.simplify {%intro| 0 * x |} in
    Alcotest.(check syntax) same_expr expected actual

  let mul_x0 () =
    let expected : Syntax.t = Const 0 in
    let actual = Semantics.simplify {%intro| x * 0 |} in
    Alcotest.(check syntax) same_expr expected actual

  let mul_1x () =
    let expected : Syntax.t = Var "x" in
    let actual = Semantics.simplify {%intro| 1 * x |} in
    Alcotest.(check syntax) same_expr expected actual

  let mul_x1 () =
    let expected : Syntax.t = Var "x" in
    let actual = Semantics.simplify {%intro| x * 1 |} in
    Alcotest.(check syntax) same_expr expected actual

  let exp () =
    let expected : Syntax.t = Const 8 in
    let actual = Semantics.simplify {%intro| 2 ^ 3 |} in
    Alcotest.(check syntax) same_expr expected actual

  let exp_0x () =
    let expected : Syntax.t = Const 0 in
    let actual = Semantics.simplify {%intro| 0 ^ x |} in
    Alcotest.(check syntax) same_expr expected actual

  let exp_x0 () =
    let expected : Syntax.t = Const 1 in
    let actual = Semantics.simplify {%intro| x ^ 0 |} in
    Alcotest.(check syntax) same_expr expected actual

  let exp_1x () =
    let expected : Syntax.t = Const 1 in
    let actual = Semantics.simplify {%intro| 1 ^ x |} in
    Alcotest.(check syntax) same_expr expected actual

  let exp_x1 () =
    let expected : Syntax.t = Var "x" in
    let actual = Semantics.simplify {%intro| x ^ 1 |} in
    Alcotest.(check syntax) same_expr expected actual

  let neg () =
    let expected : Syntax.t = Const 0 in
    let actual = Semantics.simplify {%intro| x - - - x|} in
    Alcotest.(check syntax) same_expr expected actual

  let example () =
    let expected : Syntax.t = Const 15 in
    let actual = Semantics.simplify {%intro| (0 * x + 1) * 3 + 12 |} in
    Alcotest.(check syntax) same_expr expected actual
end

module Test_count = struct
  let var () =
    let expected : Syntax.t * int = (Var "a", 1) in
    let actual = Semantics.simplify_with_count {%intro| a |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let add_1_1 () =
    let expected : Syntax.t * int = (Const 2, 4) in
    let actual = Semantics.simplify_with_count {%intro| 1 + 1 |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let mul_compound () =
    let expected : Syntax.t * int = (Const 21, 10) in
    let actual = Semantics.simplify_with_count {%intro| (1 + 2) * (3 + 4) |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let add_0_x () =
    let expected : Syntax.t * int = (Var "x", 3) in
    let actual = Semantics.simplify_with_count {%intro| 0 + x |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let add_x_0 () =
    let expected : Syntax.t * int = (Var "x", 3) in
    let actual = Semantics.simplify_with_count {%intro| x + 0 |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let sub_x_0 () =
    let expected : Syntax.t * int = (Var "x", 3) in
    let actual = Semantics.simplify_with_count {%intro| x - 0 |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let sub_x_x () =
    let expected : Syntax.t * int = (Const 0, 1) in
    let actual = Semantics.simplify_with_count {%intro| x - x |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let mul_0_x () =
    let expected : Syntax.t * int = (Const 0, 1) in
    let actual = Semantics.simplify_with_count {%intro| 0 * x |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let mul_x_0 () =
    let expected : Syntax.t * int = (Const 0, 1) in
    let actual = Semantics.simplify_with_count {%intro| x * 0 |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let mul_1_x () =
    let expected : Syntax.t * int = (Var "x", 3) in
    let actual = Semantics.simplify_with_count {%intro| 1 * x |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let mul_x_1 () =
    let expected : Syntax.t * int = (Var "x", 3) in
    let actual = Semantics.simplify_with_count {%intro| x * 1 |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let exp_0_x () =
    let expected : Syntax.t * int = (Const 0, 1) in
    let actual = Semantics.simplify_with_count {%intro| 0 ^ x |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let exp_x_0 () =
    let expected : Syntax.t * int = (Const 1, 1) in
    let actual = Semantics.simplify_with_count {%intro| x ^ 0 |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let exp_1_x () =
    let expected : Syntax.t * int = (Const 1, 1) in
    let actual = Semantics.simplify_with_count {%intro| 1 ^ x |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let exp_x_1 () =
    let expected : Syntax.t * int = (Var "x", 3) in
    let actual = Semantics.simplify_with_count {%intro| x ^ 1 |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let add_compound () =
    let expected : Syntax.t * int = (Const 0, 5) in
    let actual = Semantics.simplify_with_count {%intro| 0 + (0 + (1 - 1)) |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual

  let neg_compound () =
    let expected : Syntax.t * int = (Const 0, 3) in
    let actual = Semantics.simplify_with_count {%intro| (- - (1 - 1)) |} in
    Alcotest.(check (pair syntax int)) same_expr expected actual
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
    ( "Test_count",
      [
        test_case "Simplify a" `Quick Test_count.var;
        test_case "Simplify 1 + 1" `Quick Test_count.add_1_1;
        test_case "Simplify (1 + 2) * (3 + 4)" `Quick Test_count.mul_compound;
        test_case "Simplify 0 + x" `Quick Test_count.add_0_x;
        test_case "Simplify x + 0" `Quick Test_count.add_x_0;
        test_case "Simplify x - 0" `Quick Test_count.sub_x_0;
        test_case "Simplify x - x" `Quick Test_count.sub_x_x;
        test_case "Simplify 0 * x" `Quick Test_count.mul_0_x;
        test_case "Simplify x * 0" `Quick Test_count.mul_x_0;
        test_case "Simplify 1 * x" `Quick Test_count.mul_1_x;
        test_case "Simplify x * 1" `Quick Test_count.mul_x_1;
        test_case "Simplify 0 ^ x" `Quick Test_count.exp_0_x;
        test_case "Simplify x ^ 0" `Quick Test_count.exp_x_0;
        test_case "Simplify 1 ^ x" `Quick Test_count.exp_1_x;
        test_case "Simplify x ^ 1" `Quick Test_count.exp_x_1;
        test_case "Simplify 0 + (0 + (1 - 1))" `Quick Test_count.add_compound;
        test_case "Simplify (- - (1 - 1))" `Quick Test_count.neg_compound;
      ] );
  ]

let () = Alcotest.run "Intro" intro_tests
