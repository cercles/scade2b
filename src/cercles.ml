(* Florian Thibord  --  Projet CERCLES *)

open Lexing
open Lexer
open Parser
open Ast_repr_b

let usage = "usage: "^Sys.argv.(0)^" [options] file.lus main"

let handle_error (start,finish) =
  let line = start.pos_lnum in
  let first_char = start.pos_cnum - start.pos_bol + 1 in
  let last_char = finish.pos_cnum - start.pos_bol + 1 in
  Printf.eprintf "line %d, characters %d-%d:\n" line first_char last_char

let parse_only = ref false
let norm_only = ref false
let verbose = ref false

let spec =
  ["-parse-only", Arg.Set parse_only, "  stops after parsing";
   "-norm-only", Arg.Set norm_only, "  stops after normalization";
   "-v", Arg.Set verbose, "print intermediate transformations";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".lus") then
      raise (Arg.Bad "no .lus extension");
    file := Some s
  in
  let cpt = ref 0 in
  let set s =
    incr cpt;
    match !cpt with
    | 1 -> set_file s
    | _ -> raise (Arg.Bad "Too many arguments")
  in
  Arg.parse spec set usage;
  (match !file with Some f -> f | None -> Arg.usage spec usage; exit 1)

let () =
  let channel = open_in file in
  let lexbuf = Lexing.from_channel channel in
  try
    let ast = Parser.prog Lexer.token lexbuf in
    close_in channel;
    if !verbose then Ast_printer.print_prog ast;
    if !parse_only then exit 0;
    let ast_n = Normalizer.normalize ast in
    if !verbose then Ast_printer_norm.print_prog ast_n;
    if !norm_only then exit 0 ;
    let ast_b = Trad.translate ast_n in
    let bsig_file = open_out ((Filename.chop_extension file)^".mch") in
    Bsig_generator.print_prog ast_b.signature bsig_file;
    let bimpl_file = open_out ((Filename.chop_extension file)^"_i.imp") in
    Bimpl_generator.print_prog ast_b.implementation bimpl_file;
    close_out bsig_file;
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
  | e ->
    Format.eprintf "Anomaly: %s\n@." (Printexc.to_string e);
    exit 2
