(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_repr_b
open Ast_base
open Printer

let print_then_condition ppt = function
  | Base_expr (id, t, expr, ens_id) -> 
    fprintf ppt "%a :: { %a | %a : %a & %a }"
      print_bid id
      print_bid ens_id
      print_bid ens_id
      print_basetype t
      print_expr_in_pred expr
  | Base_no_expr (id, t, ens_id) -> 
    fprintf ppt "%a :: { %a | %a : %a }"
      print_bid id
      print_bid ens_id
      print_bid ens_id
      print_basetype t
  | Fun_expr (id, t, e_list, expr, ens_id) ->
    fprintf ppt "%a :: { %a | %a : %a & %a } "
      print_bid id
      print_bid ens_id 
      print_bid ens_id
      (print_array_type t) e_list 
      print_expr_in_pred expr;
  | Fun_no_expr (id, t, e_list, ens_id) ->
    fprintf ppt "%a :: { %a | %a : %a }"
      print_bid id
      print_bid ens_id
      print_bid ens_id
      (print_array_type t) e_list

let rec print_thenlist ppt = function
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_then_condition c
  | c::l -> fprintf ppt "%a||@,%a" print_then_condition c print_thenlist l 


let print_pre_condition ppt = function
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

let rec print_prelist ppt = function 
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_pre_condition c
  | c::l -> fprintf ppt "%a &@,%a" print_pre_condition c print_prelist l 


let print_op_decl ppt op_decl =
  fprintf ppt "%a <-- %s(%a)"
    print_idlist_comma op_decl.param_out
    op_decl.id
    print_idlist_comma op_decl.param_in

let print_operation ppt abstop =
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n@[<v 3> PRE@,@[<v>%a@]@]@\n@[<v 3> THEN@,@[<v>%a@]@]@\n END"
    print_op_decl abstop.abstop_decl
    print_prelist abstop.abstop_pre
    print_thenlist abstop.abstop_post

let print_constraints ppt constraints =
  if (List.length constraints) = 0 then () 
  else 
    fprintf ppt "CONSTRAINTS@\n  %a@\n" print_prelist constraints

let print_id_machine ppt id_machine =
  fprintf ppt "%s" id_machine

let print_machine ppt b_abst =
  fprintf ppt
    "MACHINE %a%a@\n%a@\n%a%a @\nEND"
    print_id_machine b_abst.machine
    print_params_machine b_abst.abst_params
    print_sees b_abst.abst_sees
    print_constraints b_abst.abst_constraints
    print_operation b_abst.abst_operation

let print_prog b_abst file is_root =
  fprintf (formatter_of_out_channel file) "%a@." print_machine b_abst
