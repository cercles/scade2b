(* Florian Thibord  --  Projet CERCLES *)

open Lexing
open Ast_repr_b
open Ast_kcg
open Ast_prog
open Ast_xml
open Xml_utils


let usage = "usage: "^Sys.argv.(0)^" [options] dir/project/"

let parse_only = ref false
let norm_only = ref false
let verbose = ref false

let handle_error (start, finish, lex) =
  let line = start.pos_lnum in
  let first_char = start.pos_cnum - start.pos_bol + 1 in
  let last_char = finish.pos_cnum - start.pos_bol + 1 in
  Printf.eprintf "line %d, characters %d-%d : lexeme %s\n" line first_char last_char lex

let spec =
  ["-parse-only", Arg.Set parse_only, "stops after parsing";
   "-norm-only", Arg.Set norm_only, "stops after normalization";
   "-v", Arg.Set verbose, "print intermediate transformations";
  ]

let scade_file, xml_file, main_dir =
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
       Some n -> n^"/KCG/kcg_xml_filter_out.scade", n^"/KCG/kcg_trace.xml", n^"/Machines_B/"
     | None -> Arg.usage spec usage; exit 1)


let parse_files xml_target kcg_scade =
  (* 1 - Parsing XML -> Information related to kcg_scade *)
  let channel = open_in xml_file in
  let lexbuf = Lexing.from_channel channel in
  let xml_prog =
    try
      Parser_xml.model Lexer_xml.token lexbuf
    with
      | Lexer_xml.Lexical_error s ->
	  Format.eprintf "\nLexical Error in XML: %s @." s;
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf, lexeme lexbuf);
	  exit 1
      | Parsing.Parse_error ->
	  Format.eprintf "\nSyntax Error in XML: @.";
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf, lexeme lexbuf);
	  exit 1
  in
  close_in channel;
  let xml_prog = Xml_utils.e_t_r_prog xml_prog in
  (* DEBUG PURPOSE *)
  (* let ast_xml_out = open_out (Filename.concat main_dir "ast_xml.txt") in  *)
  (* Printer_xml_ast.print_ast_xml xml_prog ast_xml_out;  *)

  (* 2 - Parsing kcg_scade -> Node map + Const map + Enum map *)
  let channel = open_in scade_file in
  let lexbuf = Lexing.from_channel channel in
  let kcg_prog =
    try
      Parser_kcg.prog Lexer_kcg.token lexbuf
    with
      | Lexer_kcg.Lexical_error s ->
	  Format.eprintf "\nLexical Error in kcg file: %s@." s;
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf, lexeme lexbuf);
	  exit 2
      | Parsing.Parse_error ->
	  Format.eprintf "\nSyntax Error in kcg file, @.";
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf, lexeme lexbuf);
	  exit 2
  in
  close_in channel;
  let prog = Prog_builder.build_prog xml_prog kcg_prog main_dir in (* todo : change main_dir in utils *)
  prog


let () =

  (* Création du répertoire contenant les fichiers de sorties *)
  if not(Sys.file_exists main_dir) then Unix.mkdir main_dir 0o764;
  (* Parsing files *)
  let prog = parse_files xml_file scade_file in

  (* Création d'une machine pour les constantes. *)
  let bconst_file =
    open_out (Filename.concat main_dir ("M_Consts.mch")) in
  Bconst_generator.print_m_const prog.consts bconst_file prog.env_prog;
  (* Création d'une machine pour les types enumeres. *)
  let benum_file =
    open_out (Filename.concat main_dir ("M_Enum.mch")) in
  Benum_generator.print_m_enum prog.enum_types benum_file prog.env_prog;

  (* Traduction de chaque noeud du programme *)
  let node_translator node imports_map =
    let node_name = node.node_name in
    let scade_node = node.ast_scade in
    let node_xml = node.node_xml in
    let id_consts = List.map (fun cst -> cst.c_id) prog.consts in
    let id_enums = List.fold_left (fun acc enum -> enum.p_enum_list @ acc) [] prog.enum_types in
    match scade_node with
      | None -> Babsterror_generator.generate node_xml main_dir; 
	imports_map
      | Some ast -> begin
	  let import_list =
	    try
  	      Imports_map.find node_name imports_map
	    with Not_found -> []
	  in
	  try
	    let ast_n = Normalizer.normalize_node ast prog.consts id_enums in
	    let ast_b = Trad.translate ast_n import_list id_consts in
	    (* Impression des machines *)
	    let babst_file =
	      open_out (Filename.concat main_dir ("M_" ^ node_name ^ ".mch")) in
	    Babst_generator.print_prog ast_b.machine_abstraite babst_file node_xml.is_root;
	    close_out babst_file;
	    let bimpl_file =
	      open_out (Filename.concat main_dir ("M_" ^ node_name ^ "_i.imp")) in
	    Bimpl_generator.print_prog 
	      ast_b.implementation bimpl_file node_xml.is_root prog.env_instances prog.env_prog;
	    close_out bimpl_file;
	    Xml_utils.update_imports_map imports_map ast_n
	  with
	    | Normalizer.Assert_id_error e ->
  		Format.eprintf "\nNormalizer Error: Assert %s@." e;
  		Babsterror_generator.generate node_xml main_dir;
  		imports_map
	    | Normalizer.Ident_Call_Error e ->
  		Format.eprintf "\nNormalizer Error: The node name %s is reserved in B @." e;
  		Babsterror_generator.generate node_xml main_dir;
  		imports_map
	    | Trad.Register_cond_error e ->
  		Format.eprintf "\nTrad Error: Register condition %s isn't related to an input/output @." e;
  		Babsterror_generator.generate node_xml main_dir;
  		imports_map
	    | e ->
  		Format.eprintf "\nAnomaly: %s @." (Printexc.to_string e);
  		Babsterror_generator.generate node_xml main_dir;
  		imports_map		  
	end
  in
  
  (* Creation de la liste de noeuds ordonnée selon l'ordre des IMPORTS *)
  let xml_nodes = List.map (fun node -> node.node_xml) prog.nodes in
  let scheduled_node_list = Xml_utils.schedule_node_list xml_nodes in
  let imports_map = build_imports_map xml_nodes in

  (* let print_map map = *)
  (*   Imports_map.iter (fun name imp_list -> *)
  (* 		Printf.printf "\n\n %s\n" name; *)
  (* 		List.iter (fun imp -> Printf.printf "name : %s  id : %s\n" imp.import_name imp.instance_id) imp_list ) map *)
  (* in *)
  (* print_map imports_map; *)

  ignore(
    List.fold_left (fun map node_ident ->
  		      try node_translator 
			(List.find (fun node -> node_ident = node.node_name) prog.nodes) map
  		      with Not_found ->
  			(Printf.printf "Scade2b error, node %s not found in node list\n" node_ident; map)
  		   ) imports_map scheduled_node_list)
