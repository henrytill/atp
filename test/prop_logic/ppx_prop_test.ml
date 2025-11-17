let is_equiv () =
  let obtained = {%prop| (p ==> q) /\ (q ==> r) ==> (p ==> r) |} in
  let expected = Prop_logic.Input.parse_string_exn {| (p ==> q) /\ (q ==> r) ==> (p ==> r) |} in
  Prop_logic.Syntax.equal expected obtained

let meta_works () =
  let x = {%prop| p \/ q |} in
  let obtained = {%prop| p /\ $x |} in
  let expected = Prop_logic.Input.parse_string_exn {| p /\ (p \/ q) |} in
  Prop_logic.Syntax.equal expected obtained

let meta_fn_works () =
  let make x = {%prop| p /\ $x |} in
  let input = {%prop| p \/ q |} in
  let obtained = make input in
  let expected = Prop_logic.Input.parse_string_exn {| p /\ (p \/ q) |} in
  Prop_logic.Syntax.equal expected obtained

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
    else begin
      print_endline "FAILED";
      ret := 1
    end
  in
  Array.iter run tests;
  exit !ret
