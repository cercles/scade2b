(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_repr_b
open Ast_base

let print_bid ppt id =
  fprintf ppt "%s" id

let rec print_idlist_comma ppt = function
  | [] -> ()
  | [id] -> fprintf ppt "%a" print_bid id
  | id::l -> fprintf ppt "%a, %a" print_bid id print_idlist_comma l

let print_value ppt = function
  | Bool b -> fprintf ppt "%b" b
  | Int i -> fprintf ppt "%d" i
  | Float f -> fprintf ppt "%f" f

let rec print_e_list ppt = function 
  | [] -> ()
  | [v] -> fprintf ppt "%a" print_expr v
  | v::l -> fprintf ppt "%a, %a" print_expr v print_e_list l

and print_expr ppt = function
  | BE_Ident id -> print_bid ppt id
  | BE_Tuple e_list -> fprintf ppt "(@[%a@])" print_e_list e_list
  | BE_Value v -> print_value ppt v
  | BE_Array ar -> print_array ppt ar
  | BE_Bop (bop, e1, e2) -> fprintf ppt "%a %a %a" print_expr e1 print_bop bop print_expr e2
  | BE_Unop (unop, e) -> fprintf ppt "%a%a" print_unop unop print_expr e
  | BE_Sharp e_list -> fprintf ppt "#@[(%a)@]" print_e_list e_list (* TROUVER TRADUCTION *)

(* A FAIRE! *)
and print_array ppt = function 
  | BA_Def e_list -> fprintf ppt "[%a]" print_e_list e_list
  | BA_Caret (e1, e2) -> fprintf ppt "%a ^ %a" print_expr e1 print_expr e2
  | BA_Concat (e1, e2) -> fprintf ppt "%a | %a" print_expr e1 print_expr e2
  | BA_Slice (id, e_list) -> fprintf ppt "%a[%a]" print_bid id print_slice_list e_list
  | BA_Index (id, e_list) -> fprintf ppt "%a[%a]" print_bid id print_index_list e_list

and print_slice_list ppt = function
  | [] -> ()
  | [(e1, e2)] when e1 = e2 -> fprintf ppt "[%a]" print_expr e1
  | (e1, e2)::l when e1 = e2 -> fprintf ppt "[%a]%a" print_expr e1 print_slice_list l
  | [(e1, e2)] -> fprintf ppt "[%a .. %a]" print_expr e1 print_expr e2
  | (e1, e2)::l -> fprintf ppt "[%a .. %a]%a" print_expr e1 print_expr e2 print_slice_list l

and print_index_list ppt = function
  | [] -> ()
  | [(e)] -> fprintf ppt "[%a]" print_expr e
  | (e)::l -> fprintf ppt "[%a]%a" print_expr e print_index_list l

and print_bop ppt = function
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
  | Op_add_f -> fprintf ppt "+"
  | Op_sub_f -> fprintf ppt "-"
  | Op_mul_f -> fprintf ppt "*"
  | Op_div_f -> fprintf ppt "/"
  | Op_and -> fprintf ppt "&"
  | Op_or -> fprintf ppt "or"
  | Op_xor -> fprintf ppt "xor" (* XOR en B?? *)

and print_unop ppt = function 
  | Op_not -> fprintf ppt "not "
  | Op_minus -> fprintf ppt "-"

let print_basetype ppt = function
  | T_Bool -> fprintf ppt "%s" "BOOL"
  | T_Int -> fprintf ppt "%s" "INT"
  | T_Float -> fprintf ppt "%s" "REAL"

let print_type ppt = function
  | BT_Base t -> print_basetype ppt t
  | BT_Array (t, expr) -> assert false (* SEQUENCES A FAIRE *)

let print_then_condition ppt (id, t, expr) =
  fprintf ppt "%a :: { %a | %a : %a & %a }"
    print_bid id
    print_bid id
    print_bid id
    print_type t
    print_expr expr

let rec print_thenlist ppt = function
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_then_condition c
  | c::l -> fprintf ppt "%a||@,%a" print_then_condition c print_thenlist l 

let print_pre_condition ppt (id, t, expr) =
  fprintf ppt "%a : %a & %a"
    print_bid id
    print_type t
    print_expr expr 

let rec print_prelist ppt = function 
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_pre_condition c
  | c::l -> fprintf ppt "%a &@,%a" print_pre_condition c print_prelist l 

let print_op_decl ppt op_decl =
  fprintf ppt "%a <-- %s(%a)"
    print_idlist_comma op_decl.param_out
    op_decl.id
    print_idlist_comma op_decl.param_in

let print_operation ppt sigop =
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n@[<v 3> PRE@,@[<v> %a@]@]@\n@[<v 3> THEN@,@[<v> %a@]@]@\n END"
    print_op_decl sigop.sigop_decl
    print_prelist sigop.sigop_pre
    print_thenlist sigop.sigop_post

(* The file list can be configured in utils.ml *)
let print_sees ppt mach_list =
  if (List.length mach_list) = 0 then () 
  else 
    fprintf ppt "SEES %a" print_idlist_comma mach_list

let print_id_machine ppt id_machine =
  fprintf ppt "MACHINE %s" id_machine

let print_machine ppt b_sig =
  fprintf ppt
    "%a@\n%a@\n%a"
    print_id_machine b_sig.machine
    print_sees b_sig.sig_sees
    print_operation b_sig.sig_operation

let print_prog b_sig file =
  fprintf (formatter_of_out_channel file) "%a@." print_machine b_sig
