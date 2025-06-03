open Intro

let syntax = Alcotest.testable Syntax.pp_ast Syntax.equal
let same_expr = "same expression"

module Test_parse_string = struct
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

  let generate ?(is = inputs) () =
    let f (output, input) =
      let expected = Some output in
      let actual = Input.parse_string input in
      let test () = Alcotest.(check (option syntax)) same_expr expected actual in
      Alcotest.(test_case ("Parse " ^ String.trim input) `Quick test)
    in
    List.map f is
end

module Test_simplify = struct
  let generate ?(is = Intro_test_data.simplify) () =
    let f (output, input) =
      let expected = Some output in
      let actual = Option.map Semantics.simplify (Input.parse_string input) in
      let test () = Alcotest.(check (option syntax)) same_expr expected actual in
      Alcotest.(test_case ("Simplify " ^ String.trim input) `Quick test)
    in
    List.map f is
end

module Test_simplify_with_count = struct
  let generate ?(is = Intro_test_data.simplify_with_count) () =
    let f (output, count, input) =
      let expected = Some (output, count) in
      let actual = Option.map Semantics.simplify_with_count (Input.parse_string input) in
      let test () = Alcotest.(check (option (pair syntax int))) same_expr expected actual in
      Alcotest.(test_case ("Simplify (with count) " ^ String.trim input) `Quick test)
    in
    List.map f is
end

module Test_simplify_partial = struct
  let generate ?(is = Intro_test_data.simplify_partial) () =
    let f (output, input) =
      let expected = Input.parse_string output in
      let actual = Option.map Semantics.simplify (Input.parse_string input) in
      let test () = Alcotest.(check (option syntax)) same_expr expected actual in
      Alcotest.(test_case ("Simplify " ^ String.trim input) `Quick test)
    in
    List.map f is
end

let intro_tests =
  [
    ("Test_parse_string", Test_parse_string.generate ());
    ("Test_simplify", Test_simplify.generate ());
    ("Test_simplify_with_count", Test_simplify_with_count.generate ());
    ("Test_simplify_partial", Test_simplify_partial.generate ());
  ]

let () = Alcotest.run "Intro" intro_tests
