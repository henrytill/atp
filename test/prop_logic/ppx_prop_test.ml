let is_equiv () =
  let open Prop_logic in
  let actual : Syntax.t = {%prop| (p ==> q) /\ (q ==> r) ==> (p ==> r) |} in
  let expected = Input.parse_string {|(p ==> q) /\ (q ==> r) ==> (p ==> r)|} |> Option.get in
  Syntax.equal expected actual

let () =
  if is_equiv () then
    exit 0
  else
    exit 1
