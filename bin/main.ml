let args = ref []
let main = ref Fun.id
let parse = ref ignore

module type COMMAND = sig
  val args : (string * Arg.spec * string) list
  val main : unit -> unit
end

module Intro_command = struct
  let args = []
  let main () = print_endline "Hello, world!"
end

let select arg =
  let switch (module Command : COMMAND) =
    args := Arg.align (Command.args @ !args);
    main := Command.main
  in
  match arg with
  | "intro" -> switch (module Intro_command)
  | _ -> raise (Arg.Bad ("Unknown command: " ^ arg))

let dispatch arg = !parse arg

let usage_msg = {|USAGE

  atp <command>
|}

let () =
  parse := select;
  Arg.parse_dynamic args dispatch usage_msg;
  !main ()
