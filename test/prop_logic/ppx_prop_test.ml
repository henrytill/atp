let is_equiv () =
  let actual = {%prop| (p ==> q) /\ (q ==> r) ==> (p ==> r) |} in
  let expected = Prop_logic.Input.parse_string_exn {|(p ==> q) /\ (q ==> r) ==> (p ==> r)|} in
  Prop_logic.Syntax.equal expected actual

let meta_works () =
  let x = {%prop| p \/ q |} in
  let actual = {%prop| p /\ $x |} in
  let expected = Prop_logic.Input.parse_string_exn {|p /\ (p \/ q)|} in
  Prop_logic.Syntax.equal expected actual

let meta_fn_works () =
  let make x = {%prop| p /\ $x |} in
  let input = {%prop| p \/ q |} in
  let expected = Prop_logic.Input.parse_string_exn {|p /\ (p \/ q)|} in
  Prop_logic.Syntax.equal expected (make input)

let () =
  let tests =
    [| ("is_equiv", is_equiv); ("meta_works", meta_works); ("meta_fn_works", meta_fn_works) |]
  in
  let ret = ref 0 in
  let run (name, f) =
    print_string ("Testing " ^ name ^ ": ");
    let passed = f () in
    if passed then
      print_endline "PASSED"
    else (
      print_endline "FAILED";
      ret := 1)
  in
  Array.iter run tests;
  exit !ret
