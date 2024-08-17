open Format

let print_exn formatter exn =
  pp_print_string formatter (Printexc.to_string exn);
  pp_print_newline formatter ()

let run_with_args read_eval_print anon_args out_formatter err_formatter =
  try
    List.iter
      (fun a ->
        let lexbuf = Lexing.from_string a in
        ignore (read_eval_print lexbuf out_formatter);
        pp_print_flush out_formatter ())
      anon_args;
    exit 0
  with exn ->
    print_exn err_formatter exn;
    pp_print_flush err_formatter ();
    exit 1

let run_with_stdin read_eval_print out_formatter err_formatter =
  let continue = ref true in
  let lexbuf = Lexing.from_channel Stdlib.stdin in
  try
    while !continue do
      continue := read_eval_print lexbuf out_formatter;
      pp_print_flush out_formatter ()
    done;
    exit 0
  with exn ->
    print_exn err_formatter exn;
    pp_print_flush err_formatter ();
    exit 1

let run read_eval_print anon_args out_formatter err_formatter =
  let has_anon_args = List.length anon_args > 0 in
  if has_anon_args then
    run_with_args read_eval_print anon_args out_formatter err_formatter
  else
    run_with_stdin read_eval_print out_formatter err_formatter

module type COMMAND = sig
  val speclist : (string * Arg.spec * string) list
  val anon_fun : string -> unit
  val main : unit -> unit
end

module Command_intro : COMMAND = struct
  let dump_ast = ref false
  let show_count = ref false

  let speclist =
    [
      ("-dump-ast", Arg.Set dump_ast, "Dump AST");
      ("-count", Arg.Set show_count, "Show number of simplification steps");
    ]

  let print_ast formatter expr =
    Intro.Syntax.pp_ast formatter expr;
    pp_print_newline formatter ()

  let print_expr formatter expr =
    Intro.Syntax.pp formatter expr;
    pp_print_newline formatter ()

  let maybe_print_count formatter count =
    if !show_count then begin
      fprintf formatter "Steps: %a" pp_print_int count;
      pp_print_newline formatter ()
    end

  let read_eval_print lexbuf formatter =
    match Intro.Input.parse lexbuf with
    | Some expr when !dump_ast ->
        print_ast formatter expr;
        true
    | Some expr ->
        let simplified, count = Intro.Semantics.simplify_with_count expr in
        print_expr formatter expr;
        print_expr formatter simplified;
        maybe_print_count formatter count;
        true
    | None -> false

  let anon_args = ref []
  let anon_fun arg = anon_args := arg :: !anon_args
  let main () = run read_eval_print !anon_args std_formatter err_formatter
end

module Command_prop_logic : COMMAND = struct
  let dump_ast = ref false
  let dump_truthtable = ref false

  let speclist =
    [
      ("-dump-ast", Arg.Set dump_ast, "Dump AST");
      ("-dump-truthtable", Arg.Set dump_truthtable, "Dump truth table");
    ]

  let print_ast formatter fm =
    Prop_logic.Syntax.pp_ast formatter fm;
    pp_print_newline formatter ()

  let print_formula formatter fm =
    Prop_logic.Syntax.pp formatter fm;
    pp_print_newline formatter ()

  let read_eval_print lexbuf formatter =
    match Prop_logic.Input.parse lexbuf with
    | Some fm when !dump_ast && !dump_truthtable ->
        print_ast formatter fm;
        Prop_logic.Semantics.print_truthtable formatter fm;
        true
    | Some fm when !dump_ast ->
        print_ast formatter fm;
        true
    | Some fm when !dump_truthtable ->
        Prop_logic.Semantics.print_truthtable formatter fm;
        true
    | Some fm ->
        print_formula formatter fm;
        true
    | None -> false

  let anon_args = ref []
  let anon_fun arg = anon_args := arg :: !anon_args
  let main () = run read_eval_print !anon_args std_formatter err_formatter
end

let speclist = ref []
let anon_fun = ref ignore
let main = ref Fun.id

let select (arg : string) =
  let switch (module Command : COMMAND) =
    speclist := Arg.align (Command.speclist @ !speclist);
    anon_fun := Command.anon_fun;
    main := Command.main
  in
  match arg with
  | "intro" -> switch (module Command_intro)
  | "prop-logic" -> switch (module Command_prop_logic)
  | _ -> raise (Arg.Bad ("Unknown command: " ^ arg))

let dispatch arg = !anon_fun arg
let usage_msg = "Usage: command [options] <arguments>"

let () =
  anon_fun := select;
  begin
    try Arg.parse_argv_dynamic Sys.argv speclist dispatch usage_msg with
    | Arg.Bad msg ->
        eprintf "%s" msg;
        exit 2
    | Arg.Help msg ->
        printf "%s" msg;
        exit 0
  end;
  !main ()
