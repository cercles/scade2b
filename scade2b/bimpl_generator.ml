(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == bimpl_generator.ml                                                    == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

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
	Not_found -> ""
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
  if (List.length f.call_params = 0) then
  fprintf ppt "%a <-- %a%s"
    print_lp f.call_lp
    (print_instname f.call_id) f.call_instance
    f.call_id
  else
  fprintf ppt "%a <-- %a%s(%a)"
    print_lp f.call_lp
    (print_instname f.call_id) f.call_instance
    f.call_id
    print_expr_list f.call_params
    
let print_simpl ppt s =
  fprintf ppt "%a := %a"
    print_lp s.simpl_lp
    print_expr s.simpl_expr
    
let print_subs ppt = function
  | Alt a -> fprintf ppt "%a" print_alternative a
  | Call f -> fprintf ppt "%a" print_call f
  | Simpl s -> fprintf ppt "%a" print_simpl s

let print_subs_list ppt = print_list ~sep:";" ~break:true ~forcebreak:true print_subs ppt

let print_vars ppt var_list =
  if var_list <> [] then
    fprintf ppt "VAR %a IN" print_idlist_comma var_list
  else
    fprintf ppt "BEGIN"
    
let print_op_decl ppt b_impl =
  if (List.length b_impl.op_out_params = 0) && (List.length b_impl.op_in_params = 0) then
    fprintf ppt "%s" b_impl.name
  else if (List.length b_impl.op_out_params = 0) then
    fprintf ppt "%s(%a)" b_impl.name print_idlist_comma b_impl.op_in_params
  else if (List.length b_impl.op_in_params = 0) then
    fprintf ppt "%a <-- %s" print_idlist_comma b_impl.op_out_params b_impl.name
  else
    fprintf ppt "%a <-- %s(%a)"
      print_idlist_comma b_impl.op_out_params
      b_impl.name
      print_idlist_comma b_impl.op_in_params


let print_operation ppt b_impl =
  let print_end = "END" in
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n %a@\n@[<v 3>   %a@]@\n %s"
    print_op_decl b_impl
    print_vars b_impl.m_vars
    print_subs_list b_impl.imp_substitutions
    print_end

let print_initialisation ppt ini_list = 
  let print_init_clause ppt (id, e) =
    fprintf ppt "%a := %a" print_bid id print_expr e
  in
  let print_initialisation_list ppt l =
    print_list ~sep:" ; " ~break:true print_init_clause ppt l
  in
  if ini_list <> [] then
    fprintf ppt "INITIALISATION @\n@[<v 3>   %a@]@\n" print_initialisation_list ini_list

let print_condition ppt (id, set, cond) = 
  match set with
  | typ, None -> (
    match cond with
    | None ->
      fprintf ppt "%a : %a"
	print_bid id
	print_basetype typ
    | Some expr -> 
      fprintf ppt "%a : %a & %a"
	print_bid id
	print_basetype typ
	print_expr_in_pred expr 
  )
  | typ, Some dims -> (
    match cond with
    | None ->
      fprintf ppt "%a : %a"
	print_bid id
	(print_array_type typ id) dims
    | Some expr ->
      fprintf ppt "%a : %a & !%s.(%s : dom(%a) => %a)"
	print_bid id
	(print_array_type typ id) dims
	"jj_index" "jj_index"
	print_bid id
	print_expr_in_pred expr
  )

let print_invariant ppt inv_list = 
  let print_invariant_list ppt l =
    print_list ~sep:" & " ~break:true print_condition ppt l
  in
  if inv_list <> [] then
    fprintf ppt "INVARIANT @\n@[<v 3>   %a@]@\n" print_invariant_list inv_list 

let print_concrete_var ppt reg_list =
  if reg_list <> [] then
    fprintf ppt "CONCRETE_VARIABLES %a@\n" print_idlist_comma reg_list

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
  fprintf ppt "REFINES M_%s" id

let print_implementation ppt impl_name =
  fprintf ppt "M_%s_i" impl_name

let print_impl_machine ppt b_impl =
  fprintf ppt
    "IMPLEMENTATION %a%a@\n%a@\n%a@\n%a%a%a%a%a@\nEND"
    print_implementation b_impl.name
    print_params_machine b_impl.m_params
    print_refines b_impl.name
    print_sees b_impl
    print_imports b_impl.m_imports
    print_concrete_var b_impl.m_concrete_vars
    print_invariant b_impl.m_invariant
    print_initialisation b_impl.m_initialisation
    print_operation b_impl


let print_prog b_elt file env_inst =
  env_instances := env_inst;
  fprintf (formatter_of_out_channel file) "%a@." print_impl_machine b_elt
