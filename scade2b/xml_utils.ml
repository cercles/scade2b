(* Florian Thibord  --  Projet CERCLES *)

open Ast_xml
open Ast_repr_norm
open Format
open Ast_base

let remove_package_name s =
  try
    let index = String.index s ':' in
    if s.[index+1] = ':' then
      String.sub s (index+2) ((String.length s) - (index+2))
    else s
  with
  | Not_found -> s
  | Invalid_argument e -> Printf.printf "\n!Warning : invalid argument in node package name"; s



type import_t = { node_name : ident; params_m : int list option } 

type instances = { i_node_name : ident; i_params_m : int list option; instance : ident } 

module XML_prog = Map.Make(
  struct
    type t = ident
    let compare = compare
  end
)

type xml_prog_instances = instances list XML_prog.t

type xml_prog = import_t list XML_prog.t



let get_instances node map =
  let inst_list = XML_prog.find node map in
  let dummy_list = ref [] in
  let list_with_instances, list_without =
    List.partition (fun a -> let res = List.mem a.i_node_name !dummy_list in
			     dummy_list := (a.i_node_name) :: !dummy_list; res) inst_list in
  list_with_instances, list_without
    
(* let get_imports node map = *)
(*   let list_ok *)


  

let retrieve_opts_node_instance option_list =
  List.fold_left (fun (scadename, instname) o -> match o with 
		    | ScadeName id -> (id, instname)
		    | InstName id -> (scadename, id)
		    | _ -> assert false
		 ) ("", "") option_list

let retrieve_inputs_outputs nbs =
  List.fold_left (fun (inputs, outputs) nb -> match nb with 
		    | Input opts -> (opts :: inputs, outputs)
		    | Output opts -> (inputs, opts :: outputs )
		    | _ -> (inputs, outputs)
		 ) ([], []) nbs

let retrieve_inputs_outputs_options options =
  List.fold_left (fun (scadename, targettype) o -> match o with 
		    | ScadeName id -> (id, targettype)
		    | TargetType id -> (scadename, id)
		    | _ -> (scadename, targettype)
		 ) ("", "") options

let retrieve_node_name options =
  List.fold_left (fun name o -> match o with 
		    | ScadeName id -> id
		    | _ -> assert false ) "" options

let build_instance_map xml_ast =
  List.fold_left 
    (fun inst_map node_or_arraytype -> match node_or_arraytype with
       | Node (options, nbs) | Root (options, nbs) -> 
	   let node_name = retrieve_node_name options in
	   let inst_list = List.fold_left (fun l nb -> match nb with 
					     | NodeInstance ni_options -> 
						 let scadename, instname = retrieve_opts_node_instance ni_options in
						 { i_node_name = scadename; 
						   i_params_m = None; 
						   instance = instname } :: l
					     | _ -> l
					  ) [] nbs in
	   XML_prog.add node_name inst_list inst_map
       | ArrayType options -> inst_map
    ) XML_prog.empty xml_ast
  

let get_root_id xml_ast =
  List.fold_left
    (fun res node_or_arraytype -> match node_or_arraytype with
       | Root (options, nbs) -> 
	   let node_name = retrieve_node_name options in
	   res ^ node_name
       | _ -> res
    ) "" xml_ast


type xml_node_decl =
    { xmlu_node_name : ident;
      xmlu_ins : (ident * ident) list;
      xmlu_outs : (ident * ident) list;
    }

let get_node xml_ast name =
  let node = List.find
    (fun node_or_arraytype -> match node_or_arraytype with
       | Node (options, nbs) | Root (options, nbs) -> 
	   let node_name = retrieve_node_name options in
	   node_name = name
       | _ -> false
    ) xml_ast in
  let node = match node with Node (_, nbs) | Root (_, nbs) -> nbs | _ -> assert false in
  let inputs, outputs = retrieve_inputs_outputs node in
  let ins = List.map retrieve_inputs_outputs_options inputs in
  let outs = List.map retrieve_inputs_outputs_options outputs in
  { xmlu_node_name = name; xmlu_ins = ins; xmlu_outs = outs; }

let update_xml_map xml_map ast = 
  let node_ident = ast.n_id in
  let params_index = List.map (fun l -> l.n_l_index) ast.n_lambdas in
  XML_prog.map (fun import_list ->
    List.map (fun import ->
      if import.i_node_name = node_ident then {import with i_params_m = Some params_index}
      else import) import_list) xml_map
