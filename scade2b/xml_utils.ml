(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == xml_utils.ml                                                          == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

open Ast_xml

(*******************        parser_xml function        *********************)

(* remove_package_name package::node_name retourne node_name *)
  let remove_package_name s =
    try
      let index = String.index s ':' in
      if s.[index+1] = ':' then
	String.sub s (index+2) ((String.length s) - (index+2))
      else s
    with
    | Not_found -> s
    | Invalid_argument e ->
      let msg = Printf.sprintf 
	"\nWARNING (xml_utils): invalid node package name: %s." s
      in
      output_string stderr msg; s

(******************** ast_xml Enum_to_Records functions ********************)

let get_scadename options = List.assoc ScadeName options
let get_instname options = List.assoc InstName options
let get_targetname options = List.assoc TargetName options
let get_targettype options = List.assoc TargetType options
let get_celltype options = List.assoc CellType options
let get_size options = List.assoc Size options

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
	  let size = int_of_string (get_size options) in
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
