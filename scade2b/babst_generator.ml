(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == babst_generator.ml                                                    == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

open Format
open Ast_repr_b
open Ast_base
open Printer

let print_then_condition ppt (id, set, cond) = 
  match set with
  | typ, None -> (
    match cond with
    | None ->
      fprintf ppt "%a : ( %a : %a )"
	print_bid id
	print_bid id
	print_basetype typ
    | Some expr ->
      fprintf ppt "%a : ( %a : %a & %a )"
	print_bid id
	print_bid id
	print_basetype typ
	print_expr_in_pred expr
  )
  | typ, Some dims -> (
    match cond with
    | None ->
      fprintf ppt "%a : ( %a : %a )"
	print_bid id
	print_bid id
	(print_array_type typ id) dims
    | Some expr ->
      fprintf ppt "%a : ( %a : %a & !%s.(%s : dom(%a) => %a) )"
	print_bid id
	print_bid id
	(print_array_type typ id) dims 
	"jj_index" "jj_index"
	print_bid id
	print_expr_in_pred expr
  )

let print_post_list ppt = function
  | [] -> fprintf ppt "skip" 
  | l -> print_list ~sep:"||" ~break:true print_then_condition ppt l


let print_pre_condition ppt (id, set, cond) = 
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

let print_pre_list ppt = function
  | [] -> fprintf ppt "TRUE = TRUE" 
  | l -> print_list ~sep:" &" ~break:true print_pre_condition ppt l

let print_op_decl ppt b_abst =
  if (List.length b_abst.op_out_params = 0) && (List.length b_abst.op_in_params = 0) then
    fprintf ppt "%s" b_abst.name
  else if (List.length b_abst.op_out_params = 0) then
    fprintf ppt "%s(%a)" b_abst.name print_idlist_comma b_abst.op_in_params
  else if (List.length b_abst.op_in_params = 0) then
    fprintf ppt "%a <-- %s" print_idlist_comma b_abst.op_out_params b_abst.name
  else
    fprintf ppt "%a <-- %s(%a)"
      print_idlist_comma b_abst.op_out_params
      b_abst.name
      print_idlist_comma b_abst.op_in_params
 
let print_operation ppt b_abst =
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n@[<v 3> PRE@,@[<v>%a@]@]@\n@[<v 3> THEN@,@[<v>%a@]@]@\n END"
    print_op_decl b_abst
    print_pre_list b_abst.abs_pre_condition
    print_post_list b_abst.abs_post_condition

let print_constraints ppt constraints =
  if constraints <> [] then
    fprintf ppt "CONSTRAINTS@\n  %a@\n" print_pre_list constraints

let print_id_machine ppt id_machine =
  fprintf ppt "M_%s" id_machine

let print_abst_machine ppt b_abst =
  fprintf ppt
    "MACHINE %a%a@\n%a@\n%a%a @\nEND"
    print_id_machine b_abst.name
    print_params_machine b_abst.m_params
    print_sees b_abst
    print_constraints b_abst.m_constraints
    print_operation b_abst

let print_prog b_elt file =
  fprintf (formatter_of_out_channel file) "%a@." print_abst_machine b_elt
