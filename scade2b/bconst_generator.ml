(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_prog


let print_bid ppt id =
  fprintf ppt "%s" id


let rec print_properties_list ppt = function 
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_condition c
  | c::l -> fprintf ppt "%a & @,%a" print_condition c print_invariant_list l 

let print_properties ppt const_list = 
  if (List.length const_list) = 0 then () 
  else 
    fprintf ppt "PROPERTIES @\n@[<v 3>   %a@]" print_properties_list const_list 


let rec print_idlist_comma ppt = function
  | [] -> ()
  | [id] -> fprintf ppt "%a" print_bid id
  | id::l -> fprintf ppt "%a, %a" print_bid id print_idlist_comma l

let print_concrete_constants ppt const_id_list =
  if (List.length reg_list) = 0 then () 
  else 
    fprintf ppt "CONCRETE_CONSTANTS %a" print_idlist_comma const_id_list 


let print_machine ppt const_list =
  fprintf ppt
    "MACHINE M_Consts@\n%a@\n%a @\nEND"
    print_concrete_constants (List.map (fun cst -> cst.id) const_list)
    print_properties const_list 

let print_macihne const_list file =
  fprintf (formatter_of_out_channel file) "%a@." print_machine b_abst
