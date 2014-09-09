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
open Ast_scade
open Ast_base

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
	  let name = get_scadename options in
	  let in_params = get_ins nbs in
	  let out_params = get_outs nbs in
	  let instances = get_instances nbs in
	  Some { name = name;
		 in_params = in_params;
		 out_params = out_params;
		 instances = instances;
	       }
  in
  List.fold_left (fun acc node -> 
		    match fold_fun node with 
		      | Some node -> node :: acc 
		      | None -> acc 
		 ) [] list


let e_t_r_prog prog_list =
  let node_list = etr_get_nodes prog_list in
  { xml_nodes = node_list;
  }

(******************** ast_xml to ast_scade functions ********************)


let rec xml_to_scade_prog xml_prog = 
  let rec xml_to_scade_node xml_prog =
    let p_id = xml_prog.name in
    let p_param_in = List.map xml_to_scade_param xml_prog.in_params in 
    let p_param_out = List.map xml_to_scade_param xml_prog.out_params in 
    { p_id = p_id;
      p_param_in = p_param_in;
      p_param_out = p_param_out;
      p_assumes = [];
      p_guarantees = [];
      p_vars = [];
      p_eqs = []; 
    }
  in
  match xml_prog with
  | [] -> []
  | xml_node :: xml_prog_bis -> 
      xml_to_scade_node xml_node :: (xml_to_scade_prog xml_prog_bis)

and xml_to_scade_param xml_param =
  let id = xml_param.var_id in
  let typ = xml_to_scade_type xml_param.var_type in
  (id, typ)

and xml_to_scade_type = function
  | "int" -> PT_Base (T_Int)
  | "real" -> PT_Base (T_Float)
  | "bool" -> PT_Base (T_Bool)
  | _ as id -> PT_Base (T_Enum (id))

