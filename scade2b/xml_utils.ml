(* Florian Thibord  --  Projet CERCLES *)

open Ast_xml

let remove_package_name s =
  try
    let index = String.index s ':' in
    if s.[index+1] = ':' then
      String.sub s (index+2) ((String.length s) - (index+2))
    else s
  with
  | Not_found -> s
  | Invalid_argument e -> Printf.printf "\n!Warning : invalid argument in node package name"; s
  

let update_xml_map xml_map ast =
  let node_ident = ast.n_id in
  let params_index = List.map (fun l -> l.n_l_index) ast.n_lambdas in
  XML_prog.map (fun import_list ->
    List.map (fun import ->
      if import.node_name = node_ident then {import with params_m = Some params_index}
      else import) import_list) xml_map
