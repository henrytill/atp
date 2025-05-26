open Intro

let syntax = Alcotest.testable Syntax.pp_ast Syntax.equal
let same_expr = "same expression"

module Test_parse = struct
  let inputs : (Syntax.t * string) list =
    [
      (Var "a", {| a |});
      (Const 42, {| 42 |});
      (Add (Const 42, Const 42), {| 42 + 42 |});
      (Mul (Const 42, Const 42), {| 42 * 42 |});
      (Exp (Const 2, Const 3), {| 2 ^ 3 |});
      (Sub (Var "x", Neg (Neg (Var "x"))), {| x - - - x |});
      (Add (Mul (Const 2, Var "x"), Var "y"), {| 2 * x + y |});
      ( Add (Mul (Add (Mul (Const 0, Var "x"), Const 1), Const 3), Const 12),
        {| (0 * x + 1) * 3 + 12 |} );
      (Add (Const 1, Mul (Const 2, Const 3)), {| 1 + 2 * 3 |});
      (Add (Mul (Const 1, Const 2), Const 3), {| 1 * 2 + 3 |});
      (Add (Add (Var "x", Var "y"), Var "z"), {| x + y + z |});
      (Sub (Sub (Var "x", Var "y"), Var "z"), {| x - y - z |});
      (Mul (Mul (Var "x", Var "y"), Var "z"), {| x * y * z |});
      (Exp (Var "x", Exp (Var "y", Var "z")), {| x ^ y ^ z |});
      (Mul (Add (Const 1, Const 2), Const 3), {| (1 + 2) * 3 |});
      (Mul (Const 1, Add (Const 2, Const 3)), {| 1 * (2 + 3) |});
    ]

  let generate (inputs : (Syntax.t * string) list) =
    let f (output, input) =
      let expected = Some output in
      let actual = Input.parse_string input in
      let test () = Alcotest.(check (option syntax)) same_expr expected actual in
      Alcotest.(test_case ("Parse " ^ String.trim input) `Quick test)
    in
    List.map f inputs
end

module Test_simplify = struct
  let inputs : (Syntax.t * string) list =
    [
      (Var "x", {| 0 + x |});
      (Var "x", {| x + 0 |});
      (Const 1, {| 3 - 2 |});
      (Var "x", {| x - 0 |});
      (Const 0, {| x - x |});
      (Const 12, {| 3 * 4 |});
      (Const 0, {| 0 * x |});
      (Const 0, {| x * 0 |});
      (Var "x", {| 1 * x |});
      (Var "x", {| x * 1 |});
      (Const 8, {| 2 ^ 3 |});
      (Const 0, {| 0 ^ x |});
      (Const 1, {| x ^ 0 |});
      (Const 1, {| 1 ^ x |});
      (Var "x", {| x ^ 1 |});
      (Const 0, {| x - - - x |});
      (Const 15, {| (0 * x + 1) * 3 + 12 |});
    ]

  let generate (inputs : (Syntax.t * string) list) =
    let f (output, input) =
      let expected = Some output in
      let actual = Option.map Semantics.simplify (Input.parse_string input) in
      let test () = Alcotest.(check (option syntax)) same_expr expected actual in
      Alcotest.(test_case ("Simplify " ^ String.trim input) `Quick test)
    in
    List.map f inputs
end

module Test_count = struct
  let inputs : (Syntax.t * int * string) list =
    [
      (Var "a", 1, {| a |});
      (Const 2, 4, {| 1 + 1 |});
      (Const 21, 10, {| (1 + 2) * (3 + 4) |});
      (Var "x", 3, {| 0 + x |});
      (Var "x", 3, {| x + 0 |});
      (Var "x", 3, {| x - 0 |});
      (Const 0, 1, {| x - x |});
      (Const 0, 1, {| 0 * x |});
      (Const 0, 1, {| x * 0 |});
      (Var "x", 3, {| 1 * x |});
      (Var "x", 3, {| x * 1 |});
      (Const 0, 1, {| 0 ^ x |});
      (Const 1, 1, {| x ^ 0 |});
      (Const 1, 1, {| 1 ^ x |});
      (Var "x", 3, {| x ^ 1 |});
      (Const 0, 5, {| 0 + (0 + (1 - 1)) |});
      (Const 0, 3, {| (- - (1 - 1)) |});
    ]

  let generate (inputs : (Syntax.t * int * string) list) =
    let f (output, count, input) =
      let expected = Some (output, count) in
      let actual = Option.map Semantics.simplify_with_count (Input.parse_string input) in
      let test () = Alcotest.(check (option (pair syntax int))) same_expr expected actual in
      Alcotest.(test_case ("Simplify (with count) " ^ String.trim input) `Quick test)
    in
    List.map f inputs
end

module Test_more = struct
  let inputs =
    [
      ("7", "1 + 2 * 3");
      ("21", "(1 + 2) * (3 + 4)");
      ("15", "(0 * x + 1) * 3 + 12");
      ("0", "0 + (0 + (1 - 1))");
      ("x + 15", "x + 15 - 12 * 0");
      ("-x", "-(-(-(x)))");
      ("x  + y", "0 + (x + (0 + y))");
      ("x * y", "1 * (x * (1 * y))");
      ("0", "z * (0 * (x * y))");
      ("x - (y - (y - x))", "x - (y - (y - x))");
      ("8", "2 ^ (1 + 2)");
      ("x + 1", "(x + 0) * (1 + (y - y)) + (z ^ 0)");
      ("x + z", "(x + 0) * (1 + (y - y)) + (z ^ 1)");
      ("x + 3", "((((x + 1) - 1) + 2) - 2) + 3");
      (* Tests for c1 + (x - c2) -> x when c1 == c2 *)
      ("x", "5 + (x - 5)");
      ("y + 3", "7 + ((y + 3) - 7)");
      (* Tests for c1 - (x + c2) -> -x when c1 == c2 *)
      ("-z", "4 - (z + 4)");
      ("-(a * b)", "10 - ((a * b) + 10)");
      (* More complex nested cases *)
      ("x", "3 + ((x - 1) - 2)");
      ("-y", "5 - ((3 + (y + 2)))");
      ("x * (y + z)", "x * (y + (z * (2 - 1))) + (0 * w)");
      ("x * y", "(x * (y + 0)) + (0 * z)");
      ("x * y", "x * (y ^ ((0 + 2) - 1))");
      ("x", "((x * 1) + 0) - ((y - y) * z)");
      ("1", "1 + ((x - x) * (y + z))");
    ]

  let generate (inputs : (string * string) list) =
    let f (expected_str, actual_str) =
      let expected = Input.parse_string expected_str in
      let actual = Option.map Semantics.simplify (Input.parse_string actual_str) in
      let test () = Alcotest.(check (option syntax)) same_expr expected actual in
      Alcotest.(test_case ("Simplify " ^ actual_str) `Quick test)
    in
    List.map f inputs
end

let intro_tests =
  [
    ("Test_parse", Test_parse.(generate inputs));
    ("Test_simplify", Test_simplify.(generate inputs));
    ("Test_count", Test_count.(generate inputs));
    ("Test_more", Test_more.(generate inputs));
  ]

let () = Alcotest.run "Intro" intro_tests
