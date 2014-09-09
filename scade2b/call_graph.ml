(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == call_graph.ml                                                         == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

open Ast_xml
open Ast_scade_norm
open Format
open Ast_base


(******** Creation de la liste de noeuds ordonnée selon les appels de noeuds ********)

(* In : xml_nodes
   Out : (id * id list) list , liste de couples (node_name, instances associees) *)
let retrieve_instances_list xml_nodes =
  let get_instance_ids instances =
    List.map (fun instance -> instance.inst_name) instances
  in
  List.map (fun node -> node.name, (get_instance_ids node.instances)) xml_nodes

let schedule_node_list xml_nodes = 
  let to_schedule = retrieve_instances_list xml_nodes in 
  let rec scheduler not_ordered ordered =
    match not_ordered with
      | [] -> ordered
      | (node_name, imports) :: l ->
  	  if List.for_all (fun ident -> (ident = "$+$" || ident = "$*$") || (List.mem ident ordered)) imports
  	  then scheduler l (ordered @ [node_name])
  	  else scheduler (l @ [(node_name, imports)]) ordered
  in
  scheduler to_schedule []


(******************** Creation d'une map : node -> liste calls ********************)

type calls = { call_name : ident; params_index : int list option; instance_id : ident } 

module Call_map = Map.Make(
  struct
    type t = ident
    let compare = compare
  end
)

type nodename_calls_map = calls list Call_map.t

let get_insts_node node =
  let instances = node.instances in
  List.map (fun inst -> 
	      { call_name = inst.inst_name; 
		params_index = None;
		instance_id = inst.inst_id;
	      }) instances

let build_imports_map xml_nodes =
  List.fold_left 
    (fun map node -> Call_map.add node.name (get_insts_node node) map)
    Call_map.empty xml_nodes


(********************  MAJ de la nodename_imports_map ********************)

let update_call_map xml_map ast =
  let node_ident = ast.n_id in
  let params_index = List.map (fun l -> l.n_l_index) ast.n_lambdas in
  Call_map.map (fun import_list ->
		     List.map (fun import ->
				 if import.call_name = node_ident then 
				   {import with params_index = Some params_index}
				 else 
				   import) import_list
		  ) xml_map



(**** Creation de la liste de noeuds ordonnée selon l'ordre des IMPORTS ****)

let compute_call_map xml_node_list =
  let scheduled_nodename_list = schedule_node_list xml_node_list in
  let call_map = build_imports_map xml_node_list in
  call_map, scheduled_nodename_list
