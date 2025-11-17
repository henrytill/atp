let is_equiv () =
  let obtained = {%intro| x + (y * 2) |} in
  let expected = Intro.Input.parse_string_exn {| x + (y * 2) |} in
  Intro.Syntax.equal expected obtained

let const_works () =
  let obtained = {%intro| 42 |} in
  let expected = Intro.Input.parse_string_exn {| 42 |} in
  Intro.Syntax.equal expected obtained

let neg_works () =
  let obtained = {%intro| - x |} in
  let expected = Intro.Input.parse_string_exn {| - x |} in
  Intro.Syntax.equal expected obtained

let complex_expr () =
  let obtained = {%intro| (x + y) * (z - 3) |} in
  let expected = Intro.Input.parse_string_exn {| (x + y) * (z - 3) |} in
  Intro.Syntax.equal expected obtained

let exp_works () =
  let obtained = {%intro| x ^ 2 |} in
  let expected = Intro.Input.parse_string_exn {| x ^ 2 |} in
  Intro.Syntax.equal expected obtained

let meta_works () =
  let x = {%intro| a + b |} in
  let obtained = {%intro| 2 * $x |} in
  let expected = Intro.Input.parse_string_exn {| 2 * (a + b) |} in
  Intro.Syntax.equal expected obtained

let meta_fn_works () =
  let make x = {%intro| $x ^ 2 |} in
  let input = {%intro| x + 1 |} in
  let obtained = make input in
  let expected = Intro.Input.parse_string_exn {| (x + 1) ^ 2 |} in
  Intro.Syntax.equal expected obtained

let () =
  let tests =
    [|
      ("is_equiv", is_equiv);
      ("const_works", const_works);
      ("neg_works", neg_works);
      ("complex_expr", complex_expr);
      ("exp_works", exp_works);
      ("meta_works", meta_works);
      ("meta_fn_works", meta_fn_works);
    |]
  in
  let ret = ref 0 in
  let run (name, f) =
    print_string ("Testing " ^ name ^ ": ");
    let passed = f () in
    if passed then
      print_endline "PASSED"
    else begin
      print_endline "FAILED";
      ret := 1
    end
  in
  Array.iter run tests;
  exit !ret
