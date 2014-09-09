(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == initialisation.ml                                                     == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

open Ast_prog


(*** INITIALISATION PART 1 :
       PARSING ENTREES DE SCADE2B
       -> PARAMETRISATION DE SCADE2B               *)

let usage = "usage: "^Sys.argv.(0)^" [options] dir/project/"

let scade_print = ref false
let norm_print = ref false
let schedeqs_print = ref false
let debug_xml = ref false

let spec =
  ["-ps", Arg.Set scade_print, "print scade ast after parsing on stdout";
   "-pn", Arg.Set norm_print, "print ast after normalisation on stdout";
   "-pe", Arg.Set schedeqs_print, "print ast after scheduling of equations on stdout";
   "-debug_xml", Arg.Set debug_xml, "print ast_xml in ast_xml.txt";
  ]

let parse_scade2b_inputs () =
  let scade_file, xml_file, dir_output =
    let dir = ref None in
    let cpt = ref 0 in
    let set s =
      incr cpt;
      match !cpt with
       | 1 -> dir := Some s
       | _ -> raise (Arg.Bad "Too many arguments")
    in
    Arg.parse spec set usage;
    (match !dir with 
      | Some n -> 
        n^"/KCG/kcg_xml_filter_out.scade", n^"/KCG/kcg_trace.xml", n^"/Machines_B/"
      | None -> Arg.usage spec usage; exit 1
    )
  in
  Utils.create_dir_output dir_output;
  scade_file, xml_file, dir_output
  
    

(*** INITIALISATION PART 2 :
       PARSING XML + SCADE
       -> RECUPERATION DE L'AST PROG               *)

let parse_xml_file xml_file dir_output =
  let channel = open_in xml_file in
  let lexbuf = Lexing.from_channel channel in
  let xml_prog =
    try
      Parser_xml.model Lexer_xml.token lexbuf
    with
    | e -> Error_handler.initialisation_xml_parsing e lexbuf
  in
  close_in channel;
  if !debug_xml then(
    let ast_xml_out = open_out (Filename.concat dir_output "ast_xml.txt") in  
    Printer_xml_ast.print_ast_xml xml_prog ast_xml_out);
  let xml_prog = Xml_utils.e_t_r_prog xml_prog in
  xml_prog

let parse_scade_file scade_file =
  let channel = open_in scade_file in
  let lexbuf = Lexing.from_channel channel in
  let scade_prog =
    try
      Parser_kcg.prog Lexer_kcg.token lexbuf
    with
    | e -> Error_handler.initialisation_kcg_parsing e lexbuf
  in
  close_in channel;
  scade_prog



(*** INITIALISATION SCADE2B :                      *)

let initialisation_scade2b () =
  let scade_file, xml_file, dir_output = parse_scade2b_inputs () in
  Error_handler.set_error_log_output dir_output;
  let xml_prog = parse_xml_file xml_file dir_output in
  let scade_prog = parse_scade_file scade_file in
  let scade2b_parameters =
    { ast_scade_print = !scade_print;
      ast_norm_print = !norm_print;
      ast_schedeqs_print = !schedeqs_print;
      dir_output = dir_output;
      m_consts_used = false;
      m_enum_used = false;
    } in
  let prog = Prog_builder.build_prog xml_prog scade_prog scade2b_parameters in
  prog
