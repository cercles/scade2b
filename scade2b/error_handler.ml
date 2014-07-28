(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == error_handler.ml                                                      == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

open Lexing
open Ast_xml
open Ast_scade
open Ast_prog
open Parser_xml
open Parser_scade_error

(*** FICHIER LOG ERREURS                           *)

let error_log = ref stdout

let set_error_log_output dir_output =
  error_log := open_out (Filename.concat (Unix.getcwd ()) ("error.log"))

let close_error_log_output () =
  close_out !error_log

let handle_error (start, finish, lex) =
  let line = start.pos_lnum in
  let first_char = start.pos_cnum - start.pos_bol + 1 in
  let last_char = finish.pos_cnum - start.pos_bol + 1 in
  let msg = Printf.sprintf " line %d, characters %d-%d : lexeme %s\n" line first_char last_char lex in
  msg

let print_exc_and_exit e code =
  Printf.printf "Fatal error: exception %s\n" (Printexc.to_string e);
  exit code

(*** INITIALISATION                                *)

(* CRITICAL ERROR : ARRET DE LA TRADUCTION, LE FICHIER PRINCIPAL NE PASSE PAS DANS LE PARSEUR *)
let initialisation_xml_parsing e lexbuf =
  match e with
  | Lexer_xml.Lexical_error s ->
    let msg = Printf.sprintf "\nCRITICAL ERROR (lexer_xml): Lexical Error in XML: %s," s in
    let msg2 = handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf, lexeme lexbuf) in
    output_string !error_log (msg^msg2);
    output_string stderr (msg^msg2);
    exit 1
  | Parsing.Parse_error ->
    let msg = Printf.sprintf "\nCRITICAL ERROR (parser_xml): Syntax error in XML," in
    let msg2 = handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf, lexeme lexbuf) in
    output_string !error_log (msg^msg2);
    output_string stderr (msg^msg2);
    exit 1
  | e -> print_exc_and_exit e 1

(* CRITICAL ERROR : ARRET DE LA TRADUCTION, LE FICHIER PRINCIPAL NE PASSE PAS DANS LE PARSEUR *)
let initialisation_kcg_parsing e lexbuf =
  match e with
  | Lexer_kcg.Lexical_error s ->
    let msg = Printf.sprintf "\nCRITICAL ERROR (lexer_kcg): Lexical Error in SCADE: %s," s in
    let msg2 = handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf, lexeme lexbuf) in
    output_string !error_log (msg^msg2);
    output_string stderr (msg^msg2);
    exit 2
  | Parsing.Parse_error ->
    let msg = Printf.sprintf "\nCRITICAL ERROR (lexer_kcg): Syntax Error in SCADE," in
    let msg2 = handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf, lexeme lexbuf) in
    output_string !error_log (msg^msg2);
    output_string stderr (msg^msg2);
    exit 2
  | e -> print_exc_and_exit e 2


(*** PROG_BUILDER                                  *)

let node_parsing e lexbuf node_xml dir_output node_string consts enums arraytypes =
  let node_name = node_xml.xml_node_name in
  begin 
    match e with
    | Lexer_scade.Lexical_error s ->
      let msg = Printf.sprintf "\nERROR (lexer_scade): Lexical Error in node %s: %s," node_name s in
      let msg2 = handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf, lexeme lexbuf) in
      output_string !error_log (msg^msg2);
      output_string stderr (msg^msg2)
    | Parsing.Parse_error ->
      let msg = Printf.sprintf "\nERROR (parser_scade): Syntax Error in node %s," node_name in
      let msg2 = handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf, lexeme lexbuf) in
      output_string !error_log (msg^msg2);
      output_string stderr (msg^msg2)
    | e -> print_exc_and_exit e 3 (* TODO: vérifier les cas, asert false si ok *)
  end;
  let lexbuf = Lexing.from_string node_string in 
  let conditions =
    try
      Parser_scade_error.node Lexer_scade_error.token lexbuf
    with
    | Lexer_kcg.Lexical_error s ->
      let msg = Printf.sprintf
  	"\nWARNING (lexer_scade_error): failed to retrieve conditions in node %s: %s," node_name s in
      let msg2 = handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf, lexeme lexbuf) in
      output_string !error_log (msg^msg2);
      output_string stderr (msg^msg2);
      [], []
    | Parsing.Parse_error ->
      let msg = Printf.sprintf
  	"\nWARNING (parser_scade_error): failed to retrieve conditions in node %s" node_name in
      let msg2 = handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf, lexeme lexbuf) in
      output_string !error_log (msg^msg2);
      output_string stderr (msg^msg2);
      [], []
  in
  begin
  try
    let conditions = Conds_retriever.compute_conditions_error_m conditions consts enums node_xml arraytypes in
    Babsterror_generator.generate node_xml dir_output conditions
  with e ->
    Printf.printf "\nERROR UNKNOWN (node_parsing)";
    Babsterror_generator.generate_without_cond node_xml dir_output
  end;
  exit 3 (* an error machine has been produced *)
    

    
(*** SCADE2B                                       *)
    
(* WARNING : noeud non trouvé! ne devrait pas arriver *)
let scade2b_node_not_found nodename =
  let msg = Printf.sprintf 
    "\nWARNING (scade2b.translate_nodes): %s not found in prog." nodename
  in
  output_string !error_log msg;
  output_string stderr msg


(* TRANSLATION ERROR : génération d'une machine error *)
let scade2b_trad_node e node_xml dir_output ast prog =
  let node_name = node_xml.xml_node_name in
  begin
    match e with
    | Normalizer.Assert_id_error e ->
      let msg = Printf.sprintf
	"\nERROR (normalizer): node(%s) -> assume/guarantee id error on %s." node_name e
      in
      output_string !error_log msg;
      output_string stderr msg;
    | Normalizer.Register_error ->
      let msg = Printf.sprintf
	"\nERROR (normalizer): node(%s) -> register could not be normalised." node_name
      in
      output_string !error_log msg;
      output_string stderr msg;
    | Scheduler.Eqs_scheduler ->
      let msg = Printf.sprintf
	"\nERROR (eq_scheduler): node(%s) -> equations could not be scheduled." node_name
      in
      output_string !error_log msg;
      output_string stderr msg;
    | Conds_retriever.Failed -> 
      let msg = Printf.sprintf
	"\nERROR : %s Cond_retriever failed!" node_name 
      in
      output_string !error_log msg;
      output_string stderr msg;
    | _ as e ->
      let msg = Printf.sprintf
    	"\nERROR anomaly: %s in node(%s)." (Printexc.to_string e) node_name
      in
      output_string !error_log msg;
      output_string stderr msg;
  end;
  let conditions = ast.p_assumes, ast.p_guarantees in
  try 
    let conditions = 
      Conds_retriever.compute_conditions_error_m 
	conditions 
	prog.consts
	prog.enum_types
	node_xml
	prog.arraytypes
    in
    Babsterror_generator.generate node_xml dir_output conditions
  with _ -> Babsterror_generator.generate_without_cond node_xml dir_output
