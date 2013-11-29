(* Florian Thibord  --  Projet CERCLES *)

open Lexing
open Ast_repr_b 
open Ast_kcg
open Xml_utils  


let usage = "usage: "^Sys.argv.(0)^" [options] dir/project/"

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
    Some n -> n^"KCG/kcg_xml_filter_out.scade", n^"KCG/kcg_trace.xml", n^"Machines_B/"
  | None -> Arg.usage spec usage; exit 1)


let () =

  (* Création du répertoire contenant les fichiers de sorties *)
  if not(Sys.file_exists main_dir) then Unix.mkdir main_dir 0o764;
  
  (* Récupération des noms de noeuds, et des noeuds importés pour chaque noeud, à partir du xml *) 
  let channel = open_in xml_file in
  let lexbuf = Lexing.from_channel channel in
  let xml_prog =
    try
      Parser_xml.model Lexer_xml.token lexbuf
    with
    | Lexer_xml.Lexical_error s ->
      Format.eprintf "\nLexical Error in XML: %s @." s;
      handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
      exit 1
    | Parsing.Parse_error ->
      Format.eprintf "\nSyntax Error in XML: @.";
      handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
      exit 1
  in
  close_in channel;

  let ast_xml_out = open_out (Filename.concat main_dir "ast_xml.txt") in 
  Printer_xml_ast.print_ast_xml xml_prog ast_xml_out;
  
  (* Récupération d'une map de noeuds qui sont indexés par leur nom, ainsi qu'une map de constantes également indexées par leur nom. *)
  let channel = open_in scade_file in
  let lexbuf = Lexing.from_channel channel in
  let prog =
    try
      Parser_prog.prog Lexer_prog.token lexbuf
    with
    | Lexer_prog.Lexical_error s ->
      Format.eprintf "\nLexical Error in kcg file: %s@." s;
      handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
      exit 2
    | Parsing.Parse_error ->
      Format.eprintf "\nSyntax Error in kcg file, @.";
      handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
      exit 2
  in
  close_in channel;

  let id_consts = List.map (fun cst -> cst.c_id) prog.const_list in

  
  (* Traduction de chaque noeud du programme *)
  let node_translator node_name node xml_map =
    let import_list =
      try
  	XML_prog.find node_name xml_map
      with Not_found -> []
    in
    (* ne pas oublier const_list dans normalizer *)
    let lexbuf = Lexing.from_string node in
    try
      let ast = Parser.prog Lexer.token lexbuf in
      let ast_n = Normalizer.normalize_node ast prog.const_list in
      let ast_b = Trad.translate ast_n import_list id_consts in
      let is_root = (get_root_id xml_prog) = node_name in
      let babst_file =
  	open_out (Filename.concat main_dir ("M_" ^ node_name ^ ".mch")) in
      Babst_generator.print_prog ast_b.machine_abstraite babst_file is_root;
      let bimpl_file =
  	open_out (Filename.concat main_dir ("M_" ^ node_name ^ "_i.imp")) in
      Bimpl_generator.print_prog ast_b.implementation bimpl_file is_root;
      close_out babst_file;
      close_out bimpl_file;
      Xml_utils.update_xml_map xml_map ast_n
    with
    | Lexer.Lexical_error s ->
      Format.eprintf "\nLexical Error: %s@." s;
      handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
      let babst_err =
  	open_out (Filename.concat main_dir ("M_" ^ node_name ^ ".mch")) in
      Babsterror_generator.print_m (Xml_utils.get_node xml_prog node_name) babst_err;
      close_out babst_err;
      xml_map
    | Parsing.Parse_error ->
      Format.eprintf "\nSyntax Error in %s@." node_name;
      handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
      let babst_err =
  	open_out (Filename.concat main_dir ("M_" ^ node_name ^ ".mch")) in
      Babsterror_generator.print_m (Xml_utils.get_node xml_prog node_name) babst_err;
      close_out babst_err;
      xml_map
    | Normalizer.Assert_id_error e ->
      Format.eprintf "\nNormalizer Error: Assert %s@." e;
      let babst_err =
  	open_out (Filename.concat main_dir ("M_" ^ node_name ^ ".mch")) in
      Babsterror_generator.print_m (Xml_utils.get_node xml_prog node_name) babst_err;
      close_out babst_err;
      xml_map
    | Normalizer.Ident_Call_Error e ->
      Format.eprintf "\nNormalizer Error: The node name %s is reserved in B @." e;
      let babst_err =
  	open_out (Filename.concat main_dir ("M_" ^ node_name ^ ".mch")) in
      Babsterror_generator.print_m (Xml_utils.get_node xml_prog node_name) babst_err;
      close_out babst_err;
      xml_map
    | Trad.Register_cond_error e ->
      Format.eprintf "\nTrad Error: Register condition %s isn't related to an input/output @." e;
      let babst_err =
  	open_out (Filename.concat main_dir ("M_" ^ node_name ^ ".mch")) in
      Babsterror_generator.print_m (Xml_utils.get_node xml_prog node_name) babst_err;
      close_out babst_err;
      xml_map
    | e ->
      Format.eprintf "\nAnomaly: %s @." (Printexc.to_string e);
      let babst_err =
  	open_out (Filename.concat main_dir ("M_" ^ node_name ^ ".mch")) in
      Babsterror_generator.print_m (Xml_utils.get_node xml_prog node_name) babst_err;
      close_out babst_err;
      xml_map
  in

  
  (* Création d'une machine pour les constantes. *)
  let bconst_file =
    open_out (Filename.concat main_dir ("M_Consts.mch")) in
  Bconst_generator.print_m_const prog.const_list bconst_file;

  let benum_file =
    open_out (Filename.concat main_dir ("M_Enum.mch")) in
  Benum_generator.print_m_enum prog.enum_list benum_file;


  (* Creation de la liste de noeuds ordonnée selon l'ordre des IMPORTS *)

  let instances_map = build_instance_map xml_prog in
  let xml_map = 
    XML_prog.fold
      (fun elt l res -> XML_prog.add elt (
	 List.map (fun inst -> {node_name = inst.i_node_name; params_m = inst.i_params_m}) l) res)
      instances_map XML_prog.empty in

  let node_list =
    let xml_imports_list = XML_prog.bindings xml_map in
    let remove_param_from_imports l =
      List.map (fun (elt, l2) -> elt, (List.map (fun import -> import.node_name)) l2) l
    in
    let to_schedule = remove_param_from_imports xml_imports_list in
    let rec scheduler not_ordered ordered =
      match not_ordered with
      | [] -> ordered
      | (node, imports) :: l ->
  	if List.for_all (fun ident -> (ident = "$+$" or ident = "$*$") or (List.mem ident ordered)) imports
  	then scheduler l (ordered @ [node])
  	else scheduler (l @ [(node, imports)]) ordered
    in
    scheduler to_schedule []
  in
  
  ignore(
    List.fold_left (fun map node_ident ->
      try node_translator node_ident (T_Node.find node_ident prog.node_map) map
      with Not_found -> (Printf.printf "Scade2b error, node %s not found in node_map\n" node_ident; map)
    ) xml_map node_list)
    
