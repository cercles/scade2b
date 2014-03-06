(* Florian Thibord  --  Projet CERCLES *)

open Ast_xml
open Ast_scade_norm
open Format
open Ast_base

(******************** Parser_xml functions ********************)

(* remove_package_name package::node_name retourne node_name *)
let remove_package_name s =
  try
    let index = String.index s ':' in
    if s.[index+1] = ':' then
      String.sub s (index+2) ((String.length s) - (index+2))
    else s
  with
    | Not_found -> s
    | Invalid_argument e -> Printf.printf "\n!Warning : invalid argument in node package name"; s


(******************** ast_xml Enum_to_Records functions ********************)

let rec get_scadename options =
  match options with
    | [] -> assert false
    | (ScadeName name) :: l' -> name
    | _ :: l' -> get_scadename l'

let rec get_instname options =
  match options with
    | [] -> assert false
    | (InstName name) :: l' -> name
    | _ :: l' -> get_instname l'

let rec get_targetname options =
  match options with
    | [] -> assert false
    | (TargetName name) :: l' -> name
    | _ :: l' -> get_targetname l'

let rec get_targettype options =
  match options with
    | [] -> assert false
    | (TargetType typ) :: l' -> typ
    | _ :: l' -> get_targettype l'

let rec get_celltype options =
  match options with
    | [] -> assert false
    | (CellType typ) :: l' -> typ
    | _ :: l' -> get_celltype l'

let rec get_size options =
  match options with
    | [] -> assert false
    | (Size s) :: l' -> s
    | _ :: l' -> get_size l'

let get_ins nbs =
  let fold_fun element =
    match element with
      | Input options -> 
	  let var_id = get_scadename options in
	  let var_type = get_targettype options in
	  Some { var_id = var_id;
		 var_type = var_type;
	       }
      | _ -> None
  in
  List.fold_left (fun acc nb -> match fold_fun nb with 
		    | Some input -> input :: acc 
		    | None -> acc 
		 ) [] nbs
    
let get_outs nbs =
  let fold_fun element =
    match element with
      | Output options -> 
	  let var_id = get_scadename options in
	  let var_type = get_targettype options in
	  Some { var_id = var_id;
		 var_type = var_type;
	       }
      | _ -> None
  in
  List.fold_left (fun acc nb -> match fold_fun nb with 
		    | Some output -> output :: acc 
		    | None -> acc 
		 ) [] nbs
    
let get_locals nbs =
  let fold_fun element =
    match element with
      | Local options -> 
	  let local_id = get_scadename options in
	  let local_target = get_targetname options in
	  Some { local_id = local_id;
		 local_target = local_target;
	       }
      | _ -> None
  in
  List.fold_left (fun acc nb -> match fold_fun nb with 
		    | Some local -> local :: acc 
		    | None -> acc 
		 ) [] nbs

let get_instances nbs =
  let fold_fun element =
    match element with
      | NodeInstance options -> 
	  let inst_name = get_scadename options in
	  let inst_id = get_instname options in
	  Some { inst_name = inst_name;
		 inst_id = inst_id;
	       }
      | _ -> None
  in
  List.fold_left (fun acc nb -> match fold_fun nb with 
		    | Some instance -> instance :: acc 
		    | None -> acc 
		 ) [] nbs

let etr_get_nodes list =
  let fold_fun element =
    match element with
      | Node (options, nbs) -> 
	  let node_name = get_scadename options in
	  let is_root = false in
	  let ins = get_ins nbs in
	  let outs = get_outs nbs in
	  let locals = get_locals nbs in
	  let instances = get_instances nbs in
	  Some { xml_node_name = node_name;
		 is_root = is_root;
		 ins = ins;
		 outs = outs;
		 locals = locals;
		 instances = instances;
	       }
      | Root (options, nbs) -> 
	  let node_name = get_scadename options in
	  let is_root = true in
	  let ins = get_ins nbs in
	  let outs = get_outs nbs in
	  let locals = get_locals nbs in
	  let instances = get_instances nbs in
	  Some { xml_node_name = node_name;
		 is_root = is_root;
		 ins = ins;
		 outs = outs;
		 locals = locals;
		 instances = instances;
	       } 
      | ArrayType _ -> None
  in
  List.fold_left (fun acc node -> 
		    match fold_fun node with 
		      | Some node -> node :: acc 
		      | None -> acc 
		 ) [] list

let etr_get_arraytypes list =
  let fold_fun element =
    match element with
      | ArrayType options -> 
	  let name = get_targetname options in
	  let celltype = get_celltype options in
	  let size = int_of_string (get_size options) in (* !!!!  TODO : PROTECTION NEEDED !!!! *)
	  Some { name = name;
		 celltype = celltype; 
		 size = size;
	       }
      | Root _ | Node _ -> None
  in
  List.fold_left (fun acc node -> 
		    match fold_fun node with 
		      | Some node -> node :: acc 
		      | None -> acc 
		 ) [] list
    
let e_t_r_prog prog_list =
  let node_list = etr_get_nodes prog_list in
  let arraytypes = etr_get_arraytypes prog_list in
  { xml_nodes = node_list;
    xml_arraytypes = arraytypes; 
  }



(******************** Creation de la liste d'imports ordonnée ********************)

(* In : xml_nodes
   Out : (id * id list) list , liste de couples (node, instances associees) *)
let retrieve_instances_list xml_nodes =
  let get_instance_ids instances =
    List.map (fun instance -> instance.inst_name) instances
  in
  List.map (fun node -> node.xml_node_name, (get_instance_ids node.instances)) xml_nodes

let schedule_node_list xml_nodes = 
  let to_schedule = retrieve_instances_list xml_nodes in 
  let rec scheduler not_ordered ordered =
    match not_ordered with
      | [] -> ordered
      | (node, imports) :: l ->
  	  if List.for_all (fun ident -> (ident = "$+$" || ident = "$*$") || (List.mem ident ordered)) imports
  	  then scheduler l (ordered @ [node])
  	  else scheduler (l @ [(node, imports)]) ordered
  in
  scheduler to_schedule []


(******************** Creation d'une map : node -> liste imports ********************)

type imports = { import_name : ident; params_index : int list option; instance_id : ident } 

module Imports_map = Map.Make(
  struct
    type t = ident
    let compare = compare
  end
)

type nodename_imports_map = imports list Imports_map.t

let get_insts_node node =
  let instances = node.instances in
  List.map (fun inst -> 
	      { import_name = inst.inst_name; 
		params_index = None;
		instance_id = inst.inst_id;
	      }) instances

let build_imports_map xml_nodes =
  List.fold_left 
    (fun map node -> Imports_map.add node.xml_node_name (get_insts_node node) map)
    Imports_map.empty xml_nodes


(********************  MAJ de la nodename_imports_map ********************)

let update_imports_map xml_map ast =
  let node_ident = ast.n_id in
  let params_index = List.map (fun l -> l.n_l_index) ast.n_lambdas in
  Imports_map.map (fun import_list ->
		     List.map (fun import ->
				 if import.import_name = node_ident then 
				   {import with params_index = Some params_index}
				 else 
				   import) import_list
		  ) xml_map


(******************** MISC / OLD ********************)

