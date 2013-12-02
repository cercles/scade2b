(* Florian Thibord  --  Projet CERCLES *)


open Ast_base
open Ast_prog
open Ast_kcg
open Ast_xml
open Ast_scade_norm
open Lexing



let get_insts_node node =
  let node_name = node.node_name in
  let node_xml = node.node_xml in
  let instances = node_xml.instances in
  List.map (fun inst -> node_name, inst.inst_name, inst.inst_id) instances

let get_insts_nodes nodes =
  List.fold_left (fun insts node -> (get_insts_node node) @ insts) [] nodes

let build_env_instances nodes =
  let insts = get_insts_nodes nodes in
  List.fold_left (fun env inst ->
		    let b_id = Utils.make_instance_bid inst env in
		    Env_instances.add inst b_id env
		 ) Env_instances.empty insts

(* Env permet d'associer l'ident scade à un ident B en vérifiant qu'il n'y a pas de collisions *)

let build_b_machine_id node_id =
  "M_"^node_id

let get_node_ids nodes =
  List.map (fun node -> node.node_name) nodes

let get_enum_elts enums =
  List.fold_left (fun acc enum -> enum.p_enum_list @ acc) [] enums
    
let get_const_ids consts =
  List.map (fun const -> const.c_id) consts


let build_env nodes enum_types consts =
  let node_ids = get_node_ids nodes in
  let env = List.fold_left (fun env node_id -> 
			      let b_id = build_b_machine_id node_id in
			      Env.add node_id b_id env
			   ) Env.empty node_ids in
  let enum_elts = get_enum_elts enum_types in
  let env = List.fold_left (fun env elt_id ->
			      let b_id = Utils.make_b_ident_global elt_id env in
			      Env.add elt_id b_id env
			   ) env enum_elts in
  let const_ids = get_const_ids consts in
  let env = List.fold_left (fun env const_id ->
			      let b_id = Utils.make_b_ident_global const_id env in
			      Env.add const_id b_id env
			   ) env const_ids in
  env

let node_parser node =
   let lexbuf = Lexing.from_string node in 
   let ast = Parser.prog Lexer.token lexbuf in
   ast


let build_node_list xml_nodes kcg_nodes =
  List.map (fun node -> 
	      let node_name = node.xml_node_name in
	      let node_xml = node in
	      let ast_scade = 
		try 
		  Some (node_parser (T_Node.find node_name kcg_nodes))
		with 
		  | Not_found -> None
		  | Lexer.Lexical_error s ->
		      Format.eprintf "\nLexical Error: %s@." s; None
		  | Parsing.Parse_error ->
		      Format.eprintf "\nSyntax Error in %s@." node_name; None
	      in
	      { node_name = node_name; 
		node_xml = node_xml;
		ast_scade = ast_scade;
	      }
	   ) xml_nodes
    
let build_prog xml_prog kcg_prog =
  let nodes = build_node_list xml_prog.xml_nodes kcg_prog.node_map in
  let enum_types = kcg_prog.enum_list in
  let consts = kcg_prog.const_list in
  let arraytypes = xml_prog.xml_arraytypes in
  let env = build_env nodes enum_types consts in
  let env_instances = build_env_instances nodes in
  { nodes = nodes;
    enum_types = enum_types;
    consts = consts;
    arraytypes = arraytypes;
    env_prog = env;
    env_instances = env_instances;
  }
