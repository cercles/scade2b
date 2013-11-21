(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_prog
open Ast_base
open Ast_repr_b

let print_bid ppt id =
  fprintf ppt "%s" id

let rec print_idlist_comma ppt = function
  | [] -> ()
  | [id] -> fprintf ppt "%a" print_bid id
  | id::l -> fprintf ppt "%a, %a" print_bid id print_idlist_comma l

let print_value ppt = function
  | Bool b -> fprintf ppt "%s" (if b then "TRUE" else "FALSE")
  | Int i -> fprintf ppt "%d" i
  | Float f -> fprintf ppt "%f" f

let rec print_opa_list op ppt = function 
  | [] -> ()
  | [v] -> fprintf ppt "%a %a" print_op_arith op print_expr v
  | v::l -> fprintf ppt "%a %a" print_expr v print_e_list l

and print_opl_list op ppt = function 
  | [] -> ()
  | [v] -> fprintf ppt "%a %a = TRUE" print_op_logic op print_expr v
  | v::l -> fprintf ppt "%a %a = TRUE %a" print_op_logic op print_expr v print_e_list l

and print_e_list ppt = function 
  | [] -> ()
  | [v] -> fprintf ppt "%a" print_expr v
  | v::l -> fprintf ppt "%a, %a" print_expr v print_e_list l

and print_expr ppt = function
  | BE_Ident id -> print_bid ppt id
  | BE_Value v -> print_value ppt v
  | BE_Array ar -> print_array ppt ar
  | BE_Op_Arith (op, e_list) -> (
      match op with 
	| Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge ->
	    fprintf ppt "bool(%a %a)" print_expr (List.hd e_list) (print_opa_list op) (List.tl e_list)
	| Op_minus ->  
	    fprintf ppt "(%a%a)" print_op_arith op print_expr (List.hd e_list)
	| Op_to_real ->  
	    fprintf ppt "(%a %a)" print_op_arith op print_expr (List.hd e_list)
	| _ -> fprintf ppt "%a %a" print_expr (List.hd e_list) (print_opa_list op) (List.tl e_list)
    )
  | BE_Op_Logic (op, e_list) when op = Op_sharp -> 
      fprintf ppt "sharp(%a)" print_e_list e_list
  | BE_Op_Logic (op, e_list) when op = Op_xor -> 
      fprintf ppt "xor(%a)" print_e_list e_list
  | BE_Op_Logic (op, e_list) when op = Op_not -> 
      fprintf ppt "bool(%a(%a = TRUE))" print_op_logic op print_expr (List.hd e_list)
  | BE_Op_Logic (op, e_list) -> 
      fprintf ppt "bool(%a = TRUE %a)" print_expr (List.hd e_list) (print_opl_list op) (List.tl e_list)
  
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

and print_op_arith ppt = function
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
  | Op_minus -> fprintf ppt "-"
  | Op_to_real -> fprintf ppt "REAL"

and print_op_logic ppt = function
  | Op_and -> fprintf ppt "&"
  | Op_or -> fprintf ppt "or"
  | Op_not -> fprintf ppt "not "
  | Op_xor -> assert false
  | Op_sharp -> assert false

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





let print_property ppt = function
  | Const_Base (id, t, expr) -> 
    fprintf ppt "%a : %a & %a = %a "
      print_bid id
      print_basetype t
      print_bid id
      print_expr expr 
  | Const_Fun (id, t, e_list, expr) ->
    fprintf ppt "%a : %a & %a = %a "
      print_bid id
      (print_array_type t) e_list
      print_bid id
      print_expr expr

let rec print_properties_list ppt = function 
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_property c
  | c::l -> fprintf ppt "%a & @,%a" print_property c print_properties_list l 

let print_properties ppt const_list = 
  if (List.length const_list) = 0 then () 
  else 
    fprintf ppt "PROPERTIES @\n@[<v 3>   %a@]" 
      print_properties_list (List.map Utils.p_const_to_b_const const_list) 


let print_concrete_constants ppt const_id_list =
  if (List.length const_id_list) = 0 then () 
  else 
    fprintf ppt "CONCRETE_CONSTANTS %a" print_idlist_comma const_id_list 


let print_machine ppt const_list =
  fprintf ppt
    "MACHINE M_Consts@\n@\n%a@\n@\n%a@\n @\nEND"
    print_concrete_constants (List.map (fun cst -> cst.c_id) const_list)
    print_properties const_list 

let print_m_const const_list file =
  fprintf (formatter_of_out_channel file) "%a@." print_machine const_list
