module type COMMAND = sig
  val speclist : (string * Arg.spec * string) list
  val anon_fun : string -> unit
  val main : unit -> unit
end

module Intro_command = struct
  let dump_ast = ref false
  let show_count = ref false

  let speclist =
    [
      ("-dump-ast", Arg.Set dump_ast, "Dump AST");
      ("-count", Arg.Set show_count, "Show number of simplification steps");
    ]

  let anon_args = ref []

  let print_ast formatter expr =
    Intro.Syntax.pp_ast formatter expr;
    Format.pp_print_newline formatter ()

  let print_expr formatter expr =
    Intro.Syntax.pp formatter expr;
    Format.pp_print_newline formatter ()

  let maybe_print_count formatter count =
    if !show_count then begin
      Format.fprintf formatter "Steps: %a" Format.pp_print_int count;
      Format.pp_print_newline formatter ()
    end

  let print_exn formatter exn =
    Format.pp_print_string formatter (Printexc.to_string exn);
    Format.pp_print_newline formatter ()

  let read_eval_print lexbuf formatter =
    match Intro.parse lexbuf with
    | Some expr when !dump_ast ->
        print_ast formatter expr;
        true
    | Some expr ->
        let simplified, count = Intro.simplify_with_count expr in
        print_expr formatter expr;
        print_expr formatter simplified;
        maybe_print_count formatter count;
        true
    | None -> false

  let anon_fun arg = anon_args := arg :: !anon_args

  let run_args () =
    let out_formatter = Format.formatter_of_out_channel Stdlib.stdout in
    let err_formatter = Format.formatter_of_out_channel Stdlib.stderr in
    try
      List.iter
        (fun a ->
          let lexbuf = Lexing.from_string a in
          ignore (read_eval_print lexbuf out_formatter);
          Format.pp_print_flush out_formatter ())
        !anon_args;
      exit 0
    with exn ->
      print_exn err_formatter exn;
      Format.pp_print_flush err_formatter ();
      exit 1

  let run_stdin () =
    let out_formatter = Format.formatter_of_out_channel Stdlib.stdout in
    let err_formatter = Format.formatter_of_out_channel Stdlib.stderr in
    let continue = ref true in
    let lexbuf = Lexing.from_channel Stdlib.stdin in
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

  let main () =
    let has_anon_args = List.length !anon_args > 0 in
    let f = if has_anon_args then run_args else run_stdin in
    f ()
end

let speclist = ref []
let main = ref Fun.id
let anon_fun = ref ignore

let select arg =
  let switch (module Command : COMMAND) =
    speclist := Arg.align (Command.speclist @ !speclist);
    anon_fun := Command.anon_fun;
    main := Command.main
  in
  match arg with
  | "intro" -> switch (module Intro_command)
  | _ -> raise (Arg.Bad ("Unknown command: " ^ arg))

let dispatch arg = !anon_fun arg
let usage_msg = String.empty

let () =
  anon_fun := select;
  begin
    try Arg.parse_argv_dynamic Sys.argv speclist dispatch usage_msg with
    | Arg.Bad msg ->
        Printf.eprintf "%s" msg;
        exit 2
    | Arg.Help msg ->
        Printf.printf "%s" msg;
        exit 0
  end;
  !main ()
