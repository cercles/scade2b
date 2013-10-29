(* Florian Thibord  --  Projet CERCLES *)

open Lexing
open Ast_repr_b
open Ast_prog
open Utils 

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

(* MAIN *)
let () =
  
  (* Récupération des noms de noeuds, et des noeuds importés pour chaque noeud, à partir du xml *) 
  let channel = open_in xml_file in
  let lexbuf = Lexing.from_channel channel in
  let xml_map =
    try
      Parser_xml.model Lexer_xml.token lexbuf
    with
      | Lexer_xml.Lexical_error s ->
	  Format.eprintf "Lexical Error XML: %s\n@." s;
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	  exit 1
      | Parsing.Parse_error ->
	  Format.eprintf "Syntax Error XML\n@.";
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	  exit 1
  in
  close_in channel;
  
  (* Récupération d'une map de noeuds qui sont indexés par leur nom, ainsi qu'une map de constantes également indexées par leur nom. *)
  let channel = open_in scade_file in
  let lexbuf = Lexing.from_channel channel in
  let prog = 
    try
      Parser_prog.prog Lexer_prog.token lexbuf
    with
      | Lexer_prog.Lexical_error s ->
	  Format.eprintf "Lexical Error kcg file: %s\n@." s;
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	exit 1
      | Parsing.Parse_error ->
	  Format.eprintf "Syntax Error kcg file \n@.";
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	  exit 1
  in
  close_in channel;  

  let id_consts = List.map (fun cst -> cst.c_id) prog.const_list in

  (* Traduction de chaque noeud du programme *)
  let node_translator node_name node =
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
      let ast_b = Trad.translate ast_n (List.map (fun {node_name=ident; params_m=param} -> ident) import_list) id_consts in   
      let babst_file = 
	open_out (Filename.concat (Filename.dirname main_dir) ("M_" ^ node_name ^ ".mch")) in
      Babst_generator.print_prog ast_b.machine_abstraite babst_file;
      let bimpl_file = 
	open_out (Filename.concat (Filename.dirname main_dir) ("M_" ^ node_name ^ "_i.imp")) in
      Bimpl_generator.print_prog ast_b.implementation bimpl_file;
      close_out babst_file;
      close_out bimpl_file
    with
      | Lexer.Lexical_error s ->
	  Format.eprintf "Lexical Error: %s\n@." s;
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	  exit 1
      | Parsing.Parse_error ->
	  Format.eprintf "Syntax Error \n@.";
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	  exit 1
      | Normalizer.Assert_id_error e ->
	  Format.eprintf "Error: Assert  %s.\n@." e
      | Normalizer.Ident_Call_Error e ->
	  Format.eprintf "Error: The node name %s is reserved in B.\n@." e
      | Trad.Register_cond_error e ->
	  Format.eprintf "Register condition error: %s isn't related to an input/output \n@." e
      | e ->
	  Format.eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	  exit 2
  in


  (* Création du répertoire contenant les fichiers de sorties *)

  if not(Sys.file_exists main_dir) then Unix.mkdir main_dir 0o764; 

  (* Création d'une machine pour les constantes. *)
  let bconst_file = 
    open_out (Filename.concat (Filename.dirname main_dir) ("M_Consts.mch")) in
  Bconst_generator.print_m_const prog.const_list bconst_file;


  (* Creation de la liste de noeuds ordonnée selon l'ordre des IMPORTS *)

  let node_list =
    let xml_imports_list = XML_prog.bindings xml_map in
    let remove_param_from_imports l =
      List.map (fun (elt, l2) -> List.map (fun {node_name=ident; params_m=param} -> ident) l2) l 
    in
    let to_schedule = remove_param_from_imports xml_imports_list in
    let rec scheduler not_ordered ordered =
      match not_ordered with
      | [] -> ordered
      | (node, imports) :: l ->  if List.for_all (fun ident -> List.mem ident ordered) imports 
	then scheduler not_ordered (ordered @ [node])
	else scheduler (l @ [(node, imports)]) ordered
    in
    scheduler to_schedule []
  in
      
      
  T_Node.iter (fun name node -> if XML_prog.mem name xml_map then node_translator name node) prog.node_map
