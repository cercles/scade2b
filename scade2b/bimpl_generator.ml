(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_repr_b
open Ast_base
open Ast_prog
open Ast_scade_norm
open Printer

let env_instances = ref Env_instances.empty
let node_name = ref ""

let print_instname imp_name ppt inst_id =
  
  let bid = 
    try 
      Env_instances.find (!node_name, imp_name, inst_id) !env_instances
    with
	Not_found -> Printf.printf "\n %s  %s  %s" !node_name imp_name inst_id; "" (* DEBUG *)
  in
  if bid = "" then () else fprintf ppt "%s." bid


let print_lp ppt = function
  | BLP_Ident id -> print_bid ppt id
  | BLP_Tuple id_list -> print_idlist_comma ppt id_list


let print_alternative ppt a =
  fprintf ppt "IF %a = TRUE THEN %a := %a ELSE %a := %a END" 
    print_expr a.alt_cond
    print_lp a.alt_lp
    print_expr a.alt_then     
    print_lp a.alt_lp
    print_expr a.alt_else

let print_call ppt f =
  fprintf ppt "%a <-- %a%s(%a)"
    print_lp f.call_lp
    (print_instname f.call_id) f.call_instance
    f.call_id
    print_expr_list f.call_params
    
let print_op ppt o =
  fprintf ppt "%a := %a"
    print_lp o.op_lp
    print_expr o.op_expr
    
let print_eq ppt = function
  | Alternative a -> fprintf ppt "%a" print_alternative a
  | Call f -> fprintf ppt "%a" print_call f
  | Op_Base o -> fprintf ppt "%a" print_op o

let print_eq_list ppt = print_list_semicolon print_eq ppt
      
let print_registre ppt r =
  fprintf ppt "%a := %a"
    print_bid r.reg_lpid
    print_expr r.reg_val
    
let print_reg_list ppt = print_list_semicolon print_registre ppt

let print_vars ppt var_list =
  if var_list <> [] then
    fprintf ppt "VAR %a IN" print_idlist_comma var_list

let print_op_decl ppt op_decl =
  fprintf ppt "%a <-- %s(%a)"
    print_idlist_comma op_decl.param_out
    op_decl.id
    print_idlist_comma op_decl.param_in
    
let print_operation ppt operations =
  let sep = if operations.op_2 <> [] then ";" else "" in
  let print_end = if operations.vars <> [] then "END" else "" in
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n %a@\n@[<v 3>   %a%s@,%a@]@\n %s"
    print_op_decl operations.op_decl
    print_vars operations.vars
    print_eq_list operations.op_1
    sep
    print_reg_list operations.op_2
    print_end

let print_initialisation ppt ini_list = 
  let print_init_clause ppt (id, e) =
    fprintf ppt "%a := %a" print_bid id print_expr e
  in
  let print_initialisation_list ppt l =
    print_list ~sep:" ;" ~break:true print_init_clause ppt l
  in
  if ini_list <> [] then
    fprintf ppt "INITIALISATION @\n@[<v 3>   %a@]@\n" print_initialisation_list ini_list

let print_condition ppt = function
  | Base_expr (id, t, expr, _) -> 
      fprintf ppt "%a : %a & %a"
	print_bid id
	print_basetype t
	print_expr_in_pred expr 
  | Base_no_expr (id, t, _) ->
      fprintf ppt "%a : %a"
	print_bid id
	print_basetype t
  | Fun_expr (id, t, e_list, expr, _) ->
      fprintf ppt "%a : %a & %a "
	print_bid id
	(print_array_type t) e_list
	print_expr_in_pred expr
  | Fun_no_expr (id, t, e_list, _) ->
      fprintf ppt "%a : %a"
	print_bid id
	(print_array_type t) e_list

let print_invariant ppt inv_list = 
  let print_invariant_list ppt l =
    print_list ~sep:" &" ~break:true print_condition ppt l
  in
  if inv_list <> [] then
    fprintf ppt "INVARIANT @\n@[<v 3>   %a@]@\n" print_invariant_list inv_list 

let print_concrete_var ppt reg_list =
  if reg_list <> [] then
    fprintf ppt "CONCRETE_VARIABLES %a@\n" print_idlist_comma reg_list

let string_of_formatter print x =
  let buf = Buffer.create 0 in
  let ppt = formatter_of_buffer buf in
  print ppt x;
  Buffer.contents buf

let print_imports_root sees ppt imports =
  let print_import ppt import =
    match import.b_params_expr with
	None -> fprintf ppt "%aM_%a" 
	  (print_instname import.b_import_name) import.b_instance_id 
	  print_bid import.b_import_name
      | Some p ->
	  fprintf ppt "%aM_%a(%a)" 
	    (print_instname import.b_import_name) import.b_instance_id 
	    print_bid import.b_import_name
	    print_expr_list p
  in
  let import_strs = List.map (string_of_formatter print_import) imports in
  let all_strs = import_strs @ sees in
  if all_strs != [] then
    fprintf ppt "IMPORTS %a@\n" print_idlist_comma all_strs

let print_imports ppt imports =
  let print_import ppt import =
    match import.b_params_expr with
	None -> fprintf ppt "%aM_%a" 
	  (print_instname import.b_import_name) import.b_instance_id 
	  print_bid import.b_import_name
      | Some p -> 
	  fprintf ppt "%aM_%a(%a)" 
	    (print_instname import.b_import_name) import.b_instance_id 
	    print_bid import.b_import_name
	    print_expr_list p
  in
  if imports != [] then
    fprintf ppt "IMPORTS %a@\n" (print_list print_import) imports

let print_refines ppt id =
  fprintf ppt "REFINES %s" id

let print_implementation ppt impl_name =
  fprintf ppt "%s" impl_name

let print_root_machine ppt b_impl =
  fprintf ppt
    "IMPLEMENTATION %a%a@\n%a@\n%a%a%a%a%a@\nEND"
    print_implementation b_impl.name
    print_params_machine b_impl.params
    print_refines b_impl.refines
    (print_imports_root b_impl.sees) b_impl.imports
    print_concrete_var b_impl.concrete_variables
    print_invariant b_impl.invariant
    print_initialisation b_impl.initialisation
    print_operation b_impl.operation


let print_machine ppt b_impl =
  fprintf ppt
    "IMPLEMENTATION %a%a@\n%a@\n%a@\n%a%a%a%a%a@\nEND"
    print_implementation b_impl.name
    print_params_machine b_impl.params
    print_refines b_impl.refines
    print_sees b_impl.sees
    print_imports b_impl.imports
    print_concrete_var b_impl.concrete_variables
    print_invariant b_impl.invariant
    print_initialisation b_impl.initialisation
    print_operation b_impl.operation


let print_prog b_impl file is_root env_inst env =
  node_name := String.sub b_impl.name 2 ((String.length b_impl.name)-4);
  env_instances := env_inst;
  if is_root then
    fprintf (formatter_of_out_channel file) "%a@." print_machine b_impl
  else 
    fprintf (formatter_of_out_channel file) "%a@." print_machine b_impl
