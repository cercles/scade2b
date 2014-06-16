(* Florian Thibord  --  Projet CERCLES *)

open Ast_repr_b
open Ast_kcg
open Ast_prog
open Ast_xml
open Utils
open Call_graph
open Lexing


  
(*** FONCTION DE TRADUCTION D'UN NOEUD SCADE 
       VERS UN COUPLE DE MACHINES B                *)

let node_translator node call_map s2b_params prog =
  let dir_output = s2b_params.dir_output in
  let node_name = node.node_name in
  let node_xml = node.node_xml in

  (* Récupération de la liste de noeud appelés par le noeud traduit *)
  let node_call_list =
    try
      Call_map.find node_name call_map
    with Not_found -> []
  in
  match node.ast_scade with
  | None -> 
      (* Babsterror_generator.generate node_xml dir_output; *)(*DEJA FAIT DANS PROG_BUILDER*)
    call_map, s2b_params
  | Some ast_scade -> 
    try
      if s2b_params.ast_scade_print then Ast_printer_scade.print_node ast_scade;

      (* Normalisation *)
      let ast_n = Normalizer.normalize_node ast_scade prog in

      if s2b_params.ast_norm_print then Ast_printer_norm.print_node ast_n;

      (* Ordonnancement des equations *)
      let ast_n = Scheduler.schedule_eqs ast_n prog in

      (* Initialisation d'un registre par une entrée - côté appelé: 
	 déplacement des entrées en lambdas (paramètres de machine B) *)
      let ast_n = Reg_ini_input.search_input_in_reg ast_n in
      
      (* Traduction *)
      let ast_b = Trad.translate ast_n node_call_list node.sees_cond in

      (* Initialisation d'un registre par une entrée - côté appelant: 
	 déplacement des paramètres d'appels en imports *)
      let ast_b = Reg_ini_input.check_imports_params ast_b node_call_list in
            
      if s2b_params.ast_schedeqs_print then Ast_printer_norm.print_node ast_n;

      (* Impression des machines *)
      let babst_file =
	open_out (Filename.concat dir_output ("M_" ^ node_name ^ ".mch")) in
      Babst_generator.print_prog 
	ast_b.machine_abstraite babst_file node_xml.is_root;
      close_out babst_file;
      let bimpl_file =
	open_out (Filename.concat dir_output ("M_" ^ node_name ^ "_i.imp")) in
      Bimpl_generator.print_prog 
	ast_b.implementation bimpl_file node_xml.is_root prog.env_instances;
      close_out bimpl_file;
      
      (* Actualisation de la call_map *)
      let call_map_updated = Call_graph.update_call_map call_map ast_n in
      (* Actualisation des paramètres de scade2b 
	 si le noeud utilise une constante ou un enum *)
      (* let s2b_params_updated = Utils.find_const_and_enum_in_node ast_scade prog s2b_params in *)

      call_map_updated, s2b_params
      
    with e -> Error_handler.scade2b_trad_node e node_xml dir_output ast_scade prog; call_map, s2b_params
    



  

(*** TRADUCTION DE CHAQUE NOEUD SCADE 
       AVEC MAJ DE LA MAP D'APPEL DE NOEUDS        *)

let translate_prog prog  = 

  let s2b_params = prog.s2b_params in
  let call_map = prog.call_map in
  (* Traduction de la liste ordonnée de noeuds *)
  let rec translate_node_list node_list call_map s2b_params =
    match node_list with
    | [] -> s2b_params
    | node :: list ->
      let updated_call_map, updated_s2b_params =
	node_translator
	  node
	  call_map
	  s2b_params
	  prog
      in
      translate_node_list list updated_call_map updated_s2b_params
  in
  let s2b_params_updated = 
    translate_node_list prog.nodes call_map s2b_params in

  (* Impression des machines M_Consts et M_Enum *)	
  if s2b_params_updated.m_consts_used then begin
    let bconst_file =
      open_out (Filename.concat s2b_params.dir_output ("M_Consts.mch"))
    in    
    Bconst_generator.print_m_const prog.consts bconst_file prog.env_prog;
  end;
  if s2b_params_updated.m_enum_used then begin
    let benum_file =
      open_out (Filename.concat s2b_params.dir_output ("M_Enum.mch")) 
    in
    Benum_generator.print_m_enum prog.enum_types benum_file prog.env_prog
  end





(*** MAIN SCADE2B                                  *)

let () =

  (*** INITIALISATION DE SCADE2B :
        prog <- Ast_prog.prog                      *)
  let prog = Initialisation.initialisation_scade2b () in

  (*** TRADUCTION DU PROGRAMME SCADE :             *)
  translate_prog prog; 

  (*** FERMETURE DU CANAL DE SORTIE D'ERREURS      *)
  Error_handler.close_error_log_output ()
