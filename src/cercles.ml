(* Florian Thibord  --  Projet CERCLES *)

open Lexing
open Lexer
open Parser
open Ast_repr_b

let usage = "usage: "^Sys.argv.(0)^" [options] file.scade main"

let handle_error (start,finish) =
  let line = start.pos_lnum in
  let first_char = start.pos_cnum - start.pos_bol + 1 in
  let last_char = finish.pos_cnum - start.pos_bol + 1 in
  Printf.eprintf "line %d, characters %d-%d:\n" line first_char last_char

let parse_only = ref false
let norm_only = ref false
let verbose = ref false

let spec =
  ["-parse-only", Arg.Set parse_only, "stops after parsing";
   "-norm-only", Arg.Set norm_only, "stops after normalization";
   "-v", Arg.Set verbose, "print intermediate transformations";
  ]

let file, main_node =
  let f = ref None in
  let main = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".scade") then
      raise (Arg.Bad "no .scade extension");
    f := Some s
  in
  let set_main s =
    main := Some s
  in
  let cpt = ref 0 in
  let set s =
    incr cpt;
    match !cpt with
    | 1 -> set_file s
    | 2 -> set_main s
    | _ -> raise (Arg.Bad "Too many arguments")
  in
  Arg.parse spec set usage;
  (match !f with Some n -> n | None -> Arg.usage spec usage; exit 1),
  (match !main with Some n -> n | None -> Arg.usage spec usage; exit 1)


let () =
  let channel = open_in file in
  let lexbuf = Lexing.from_channel channel in
  try
    let ast = Parser.prog Lexer.token lexbuf in
    close_in channel;
    if !verbose then Ast_printer.print_prog ast;
    if !parse_only then exit 0;
    let ast_n = Normalizer.normalize ast main_node in
    if !verbose then Ast_printer_norm.print_prog ast_n;
    if !norm_only then exit 0 ;
    let ast_b = Trad.translate ast_n in
    let babst_file = open_out (Filename.concat (Filename.dirname file) (String.capitalize(main_node^".mch"))) in
    Babst_generator.print_prog ast_b.machine_abstraite babst_file;
    let bimpl_file = open_out (Filename.concat (Filename.dirname file) (String.capitalize(main_node^"_i.imp"))) in
    Bimpl_generator.print_prog ast_b.implementation bimpl_file;
    close_out babst_file;
    close_out bimpl_file;
    ()
  with
  | Lexer.Lexical_error s ->
      Format.eprintf "lexical error: %s\n@." s;
      handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
      exit 1
  | Parsing.Parse_error ->
      Format.eprintf "syntax error\n@.";
      handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
      exit 1
  | Normalizer.Assert_id_error e ->
      Format.eprintf "assert error: %s \n@." e
  | Trad.Register_cond_error e ->
      Format.eprintf "Register condition error: %s isn't related to an input/output \n@." e
  | e ->
      Format.eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
