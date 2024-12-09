let is_equiv () =
  let open Prop_logic in
  let actual : Syntax.t = {%prop| (p ==> q) /\ (q ==> r) ==> (p ==> r) |} in
  let expected = Input.parse_string {|(p ==> q) /\ (q ==> r) ==> (p ==> r)|} |> Option.get in
  Syntax.equal expected actual

let meta_works () =
  let open Prop_logic in
  let x : Syntax.t = {%prop| p \/ q |} in
  let actual : Syntax.t = {%prop| p /\ $x |} in
  let expected = Input.parse_string {|p /\ (p \/ q)|} |> Option.get in
  Syntax.equal expected actual

let () =
  let tests = [| ("is_equiv", is_equiv); ("meta_works", meta_works) |] in
  for i = 0 to Array.length tests - 1 do
    let name, f = tests.(i) in
    print_string ("Testing " ^ name ^ ": ");
    let result = f () in
    if result then
      print_endline "PASSED"
    else (
      print_endline "FAILED";
      exit 1)
  done
