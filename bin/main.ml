let args = ref []
let main = ref Fun.id
let parse = ref ignore

module type COMMAND = sig
  val args : (string * Arg.spec * string) list
  val main : unit -> unit
end

module Intro_command = struct
  let dump_ast = ref false
  let args = [ ("-dump-ast", Arg.Set dump_ast, "Dump AST") ]

  let print_ast formatter expr =
    let open Intro in
    Syntax.pp_ast formatter expr;
    Format.pp_print_newline formatter ()

  let print_expr formatter expr =
    let open Intro in
    Syntax.pp formatter expr;
    Format.pp_print_newline formatter ()

  let print_exn formatter exn =
    let err_msg = Printexc.to_string exn in
    Format.pp_print_string formatter err_msg;
    Format.pp_print_newline formatter ()

  let read_eval_print lexbuf formatter =
    let open Intro in
    match parse lexbuf with
    | Some expr when !dump_ast ->
        print_ast formatter expr;
        true
    | Some expr ->
        print_expr formatter expr;
        print_expr formatter (simplify expr);
        true
    | None -> false

  let main () =
    let continue = ref true in
    let in_channel = Stdlib.stdin in
    let out_channel = Stdlib.stdout in
    let err_channel = Stdlib.stderr in
    let out_formatter = Format.formatter_of_out_channel out_channel in
    let err_formatter = Format.formatter_of_out_channel err_channel in
    let lexbuf = Lexing.from_channel in_channel in
    try
      while !continue do
        continue := read_eval_print lexbuf out_formatter;
        Format.pp_print_flush out_formatter ()
      done;
      exit 0
    with exn ->
      print_exn err_formatter exn;
      Format.pp_print_flush err_formatter ();
      exit 1
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
