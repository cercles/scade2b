(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == prog_builder.ml                                                       == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

open Ast_base
open Ast_prog
open Ast_kcg
open Ast_xml
open Ast_scade_norm
open Lexing
open Error_handler


(*** CREATION DE L'ENVIRONNEMENT DES INSTANCES :
      CHAQUE INSTANCE DE NOEUD POSSEDE 
      UN IDENT UNIQUE                              *)

let get_insts_node node =
  let node_name = node.node_name in
  let node_xml = node.node_xml in
  let instances = node_xml.instances in
  List.map (fun inst -> node_name, inst.inst_name, inst.inst_id) instances

let get_insts_nodes nodes =
  List.fold_left (fun insts node -> (get_insts_node node) @ insts) [] nodes

let build_env_instances nodes =
  let insts = get_insts_nodes nodes in
  let env_instances = List.fold_left (fun env inst ->
					let b_id = Env_builder.make_instance_bid inst env in
					Env_instances.add inst b_id env
				     ) Env_instances.empty insts in
  env_instances



(*** CREATION DE L'ENVIRONNEMENT GLOBAL 
     DU PROGRAMME SCADE CONTENANT LES IDENTS DE
     NODES, CONSTANTES ET ENUMS                  *)

let get_node_ids nodes =
  List.map (fun node -> node.node_name) nodes

let get_enum_elts enums =
  List.fold_left (fun acc enum -> enum.p_enum_list @ acc) [] enums
    
let get_const_ids consts =
  List.map (fun const -> const.c_id) consts

let build_env nodes enum_types consts =
  let node_ids = get_node_ids nodes in
  let env = List.fold_left (fun env node_id -> 
			      let b_id = node_id in
			      Env.add node_id b_id env
			   ) Env.empty node_ids in
  let enum_elts = get_enum_elts enum_types in
  let env = List.fold_left (fun env elt_id ->
			      let b_id = Env_builder.make_b_ident_global elt_id env in
			      Env.add elt_id b_id env
			   ) env enum_elts in
  let const_ids = get_const_ids consts in
  let env = List.fold_left (fun env const_id ->
			      let b_id = Env_builder.make_b_ident_global const_id env in
			      Env.add const_id b_id env
			   ) env const_ids in
  env


(*** PARSER DE NOEUDS SCADE
      -> retourne un ast_scade                     *)

let node_parser node_name node_xml dir_output node consts enums arraytypes =
  let lexbuf = Lexing.from_string node in 
  let ast = 
    try 
      Some (Parser_scade.prog Lexer_scade.token lexbuf)
    with
    | e -> Error_handler.node_parsing e lexbuf node_xml dir_output node consts enums arraytypes; None
  in
  ast


(*** CONSTRUIT LA LISTE DE NOEUDS
      -> retourne une liste de ast_prog.nodes      *)

let build_node_list xml_nodes kcg_prog scheduled_nodename_list dir_output arraytypes =
  let kcg_nodes = kcg_prog.node_map in
  List.map 
    (fun node_name -> 
      let node_xml = List.find (fun n_xml -> n_xml.xml_node_name = node_name) xml_nodes in
      let node_scade = 
	try 
	  node_parser 
	    node_name 
	    node_xml
	    dir_output 
	    (T_Node.find node_name kcg_nodes) 
	    kcg_prog.const_list
	    kcg_prog.enum_list
	    arraytypes
	with 
	| Not_found -> None
      in
      { node_name = node_name;
	node_xml = node_xml;
	ast_scade = node_scade;
	sees_cond = Utils.find_const_enum_in_node node_scade kcg_prog.const_list kcg_prog.enum_list;
      }
    ) scheduled_nodename_list


(*** PROG_BUILDER
      -> retourne un ast_prog                      *)
    
let build_prog xml_prog kcg_prog s2b_params =

  (*** CALCUL DU GRAPHE D'APPELS DE NOEUDS SCADE : 
        call_map <- Call_graph.nodename_calls_map  
        scheduled_nodename_list <- ident list      *)
  let call_map, scheduled_nodename_list = 
    Call_graph.compute_call_map xml_prog.xml_nodes in

  (*** RECUPERATION DE LA LISTE DE NOEUDS (ORDONNEE) : 
       nodes <- prog.nodes                        *)
  let nodes = 
    build_node_list 
      xml_prog.xml_nodes 
      kcg_prog
      scheduled_nodename_list 
      s2b_params.dir_output 
      xml_prog.xml_arraytypes
  in
  
  let enum_types = kcg_prog.enum_list in
  let consts = kcg_prog.const_list in
  let arraytypes = xml_prog.xml_arraytypes in
  let env = build_env nodes enum_types consts in
  let env_instances = build_env_instances nodes in
  let const_used, enum_used = Utils.const_enums_used_in_prog nodes in

  { nodes = nodes;
    call_map = call_map;
    enum_types = enum_types;
    consts = consts;
    arraytypes = arraytypes;
    env_prog = env;
    env_instances = env_instances;
    s2b_params = 
      {s2b_params with m_consts_used = const_used; m_enum_used = enum_used;}
  }
