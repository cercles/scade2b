(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_repr_b
open Ast_base
open Printer

let rec print_e_list ppt = function
  | [] -> ()
  | [v] -> fprintf ppt "%a" print_expr v
  | v::l -> fprintf ppt "%a, %a" print_expr v print_e_list l

and print_expr ppt = function
  | BE_Ident id -> print_bid ppt id
  | BE_Value v -> print_value ppt v
  | BE_Array ar -> print_array ppt ar
  | BE_Op_Arith1 (op, e) -> (
      match op with 
	| Op_minus ->  
	    fprintf ppt "(%a%a)" print_op_arith1 op print_expr e
	| Op_cast_real | Op_cast_int ->  
	    fprintf ppt "(%a (%a))" print_op_arith1 op print_expr e
    )
  | BE_Op_Arith2 (op, e1, e2) -> (
      match op with
	| Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge ->
            fprintf ppt "bool(%a %a %a)" print_expr e1 print_op_arith2 op print_expr e2
        | _ -> fprintf ppt "%a %a %a" print_expr e1 print_op_arith2 op print_expr e2
    )
  | BE_Op_Sharp e_list ->
      fprintf ppt "sharp(%a)" print_e_list e_list
  | BE_Op_Logic (Op_xor, e1, e2) ->
      fprintf ppt "%a /= %a" print_expr e1 print_expr e2
  | BE_Op_Not e ->
      fprintf ppt "not (%a = TRUE)" print_expr e
  | BE_Op_Logic (op, e1, e2) ->
      fprintf ppt "%a = TRUE %a %a = TRUE" print_expr e1 print_op_logic op print_expr e2

and print_array ppt = function 
  | BA_Def e_list -> fprintf ppt "{%a}" print_def_list e_list
  | BA_Index (id, e_list) -> fprintf ppt "%a(%a)" print_bid id print_index_list e_list
  | BA_Caret (e1, e2) -> fprintf ppt "caret(%a, %a)" print_expr e1 print_expr e2
  | BA_Concat (e1, e2) -> fprintf ppt "concat(%a, %a)" print_expr e1 print_expr e2
  | BA_Slice (id, e_list) -> fprintf ppt "slice(%a, %a)" print_bid id print_slice_list e_list

and print_def_list ppt e_list = 
  let rec fun_rec n ppt = function 
    | [] -> ()
    | [v] -> fprintf ppt "%d |-> %a" n print_expr v
    | v::l -> fprintf ppt "%d |-> %a, %a" n print_expr v (fun_rec (n+1)) l
  in
  fun_rec 0 ppt e_list

and print_slice_list ppt = function
  | [] -> ()
  | [(e1, e2)] -> fprintf ppt "(%a, %a)" print_expr e1 print_expr e2
  | (e1, e2)::l -> fprintf ppt "(%a, %a), %a" print_expr e1 print_expr e2 print_slice_list l

and print_index_list ppt = function
  | [] -> ()
  | [(e)] -> fprintf ppt "%a" print_expr e
  | (e)::l -> fprintf ppt "%a, %a" print_expr e print_index_list l

and print_op_arith1 ppt = function
  | Op_minus -> fprintf ppt "-"
  | Op_cast_real -> fprintf ppt "REAL"
  | Op_cast_int -> fprintf ppt "INT"

and print_op_arith2 ppt = function
  | Op_eq -> fprintf ppt "="
  | Op_neq -> fprintf ppt "/="
  | Op_lt -> fprintf ppt "<"
  | Op_le -> fprintf ppt "<="
  | Op_gt -> fprintf ppt ">"
  | Op_ge -> fprintf ppt ">="
  | Op_add -> fprintf ppt "+"
  | Op_sub -> fprintf ppt "-"
  | Op_mul -> fprintf ppt "*"
  | Op_div -> fprintf ppt "/"
  | Op_mod -> fprintf ppt "mod"
  | Op_div_f -> fprintf ppt "/"

and print_op_logic ppt = function
  | Op_and -> fprintf ppt "&"
  | Op_or -> fprintf ppt "or"
  | Op_xor -> assert false


let print_basetype ppt = function
  | T_Bool -> fprintf ppt "%s" "BOOL"
  | T_Int -> fprintf ppt "%s" "INT"
  | T_Float -> fprintf ppt "%s" "REAL"

let rec print_dim_list ppt = function
  | [] -> ()
  | [BE_Value (Int i)] -> fprintf ppt "0 .. %a" print_value (Int (i-1))
  | BE_Value (Int i) :: l -> fprintf ppt "0 .. %a, %a " print_value (Int (i-1)) print_dim_list l
  | [d] -> fprintf ppt "0 .. (%a-1)" print_expr d
  | d :: l -> fprintf ppt "0 .. (%a-1), %a " print_expr d print_dim_list l

let print_array_type t ppt e_list =
  fprintf ppt "(%a) --> %a" print_dim_list e_list print_basetype t


let print_then_condition ppt = function
  | Base_expr (id, t, expr, ens_id) -> 
    fprintf ppt "%a :: { %a | %a : %a & %a }"
      print_bid id
      print_bid ens_id
      print_bid ens_id
      print_basetype t
      print_expr expr
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
      print_expr expr;
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
      print_expr expr 
  | Base_no_expr (id, t, _) ->
      fprintf ppt "%a : %a"
	print_bid id
	print_basetype t
  | Fun_expr (id, t, e_list, expr, _) ->
    fprintf ppt "%a : %a & %a "
      print_bid id
      (print_array_type t) e_list
      print_expr expr
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

let print_sees ppt mach_list =
  if (List.length mach_list) = 0 then () 
  else 
    fprintf ppt "SEES %a" print_idlist_comma mach_list

let print_constraints ppt constraints =
  if (List.length constraints) = 0 then () 
  else 
    fprintf ppt "CONSTRAINTS@\n  %a" print_prelist constraints

let print_params_machine ppt params_machine =
  if (List.length params_machine) = 0 then () 
  else 
    fprintf ppt "(%a)" print_idlist_comma params_machine

let print_id_machine ppt id_machine =
  fprintf ppt "%s" id_machine


let print_machine_root ppt b_abst =
  fprintf ppt
    "MACHINE %a%a@\n%a@\n%a @\nEND"
    print_id_machine b_abst.machine
    print_params_machine b_abst.abst_params
    print_constraints b_abst.abst_constraints
    print_operation b_abst.abst_operation

let print_machine ppt b_abst =
  fprintf ppt
    "MACHINE %a%a@\n%a@\n%a@\n%a @\nEND"
    print_id_machine b_abst.machine
    print_params_machine b_abst.abst_params
    print_sees b_abst.abst_sees
    print_constraints b_abst.abst_constraints
    print_operation b_abst.abst_operation


let print_prog b_abst file is_root =
  if is_root then
    fprintf (formatter_of_out_channel file) "%a@." print_machine_root b_abst
  else 
    fprintf (formatter_of_out_channel file) "%a@." print_machine b_abst
