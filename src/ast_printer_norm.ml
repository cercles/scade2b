(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_repr_norm
open Ast_base

let print_id ppt id = fprintf ppt "%s" id

let print_value ppt = function
  | Bool b -> fprintf ppt "%b" b
  | Int i -> fprintf ppt "%d" i
  | Float f -> fprintf ppt "%f" f

let rec print_expr ppt = function
  | NE_Ident id -> print_id ppt id
  | NE_Tuple e_list -> fprintf ppt "(@[%a@])" print_e_list e_list
  | NE_Value v -> print_value ppt v
  | NE_Array ar -> print_array ppt ar
  | NE_Bop (bop, e1, e2) -> fprintf ppt "%a@[(%a, %a)@]" print_bop bop print_expr e1 print_expr e2
  | NE_Unop (unop, e) -> fprintf ppt "%a@[(%a)@]" print_unop unop print_expr e
  | NE_Sharp e_list -> fprintf ppt "#@[(%a)@]" print_e_list e_list

and print_array ppt = function
  | NA_Def e_list -> fprintf ppt "[%a]" print_e_list e_list
  | NA_Caret (e1, e2) -> fprintf ppt "%a ^ %a" print_expr e1 print_expr e2
  | NA_Concat (e1, e2) -> fprintf ppt "%a | %a" print_expr e1 print_expr e2
  | NA_Slice (id, e_list) -> fprintf ppt "%a[%a]" print_id id print_slice_list e_list
  | NA_Index (id, e_list) -> fprintf ppt "%a[%a]" print_id id print_index_list e_list

and print_e_list ppt = function 
  | [] -> ()
  | [v] -> fprintf ppt "%a" print_expr v
  | v::l -> fprintf ppt "%a, %a" print_expr v print_e_list l

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
  | Op_eq -> fprintf ppt "eq"
  | Op_neq -> fprintf ppt "neq"
  | Op_lt -> fprintf ppt "lt"
  | Op_le -> fprintf ppt "le"
  | Op_gt -> fprintf ppt "gt"
  | Op_ge -> fprintf ppt "ge"
  | Op_add -> fprintf ppt "add"
  | Op_sub -> fprintf ppt "sub"
  | Op_mul -> fprintf ppt "mul"
  | Op_div -> fprintf ppt "div"
  | Op_mod -> fprintf ppt "mod"
  | Op_add_f -> fprintf ppt "add_f"
  | Op_sub_f -> fprintf ppt "sub_f"
  | Op_mul_f -> fprintf ppt "mul_f"
  | Op_div_f -> fprintf ppt "div_f"
  | Op_and -> fprintf ppt "and"
  | Op_or -> fprintf ppt "or"
  | Op_xor -> fprintf ppt "xor"

and print_unop ppt = function
  | Op_not -> fprintf ppt "~"
  | Op_minus -> fprintf ppt "-"


let rec print_type ppt = function
  | NT_Base b -> print_base_type ppt b
  | NT_Array (t, e) -> 
    fprintf ppt 
      "%a ^ %a" 
      print_type t
      print_expr e 

and print_base_type ppt = function
  | T_Bool -> fprintf ppt "bool"
  | T_Int -> fprintf ppt "int"
  | T_Float -> fprintf ppt "real"


let rec print_eq_list ppt = function
  | [] -> ()
  | [eq] -> fprintf ppt "@[%a@];" print_eq eq
  | eq::l -> fprintf ppt "@[%a@];@\n%a" print_eq eq print_eq_list l 

and print_eq ppt = function
  | N_Alternative a -> 
      fprintf ppt "%a =@[<v 2>@,IF%a@]@,@[<v 4>THEN@,%a@]@,@[<v 4>ELSE@,%a@]" 
	print_leftpart a.n_alt_lp 
	print_expr a.n_alt_cond
	print_expr a.n_alt_then
	print_expr a.n_alt_else
  | N_Fonction f -> 
      fprintf ppt "%a = @[%a(%a)@]" 
	print_leftpart f.n_fun_lp 
	print_id f.n_fun_id
	print_e_list f.n_fun_params
  | N_Operation o ->
      fprintf ppt "%a = @[%a@]" 
	print_leftpart o.n_op_lp
	print_expr o.n_op_expr
  | N_Registre r ->
      fprintf ppt "%a = @[REG(%a,%a)@] : %a" 
	print_leftpart r.n_reg_lp
	print_expr r.n_reg_ini
	print_expr r.n_reg_val
	print_type r.n_reg_type

and print_leftpart ppt = function
  | NLP_Ident id -> print_id ppt id
  | NLP_Tuple id_list -> print_id_list ppt id_list

and print_id_list ppt = function
  | [] -> ()
  | [id] -> fprintf ppt "%a" print_id id
  | id::l -> fprintf ppt "%a, %a" print_id id print_id_list l
 

let rec print_decl_list ppt =  function
  | [] -> ()
  | [d] -> fprintf ppt "%a" print_decl d
  | d::l -> fprintf ppt "%a; %a" print_decl d print_decl_list l

and print_decl ppt = function
  | (name, ty) -> fprintf ppt "%a : %a" print_id name print_type ty

let rec print_cond_list ppt = function
  | [] -> ()
  | [r] -> fprintf ppt "%a" print_condition r
  | r::l -> fprintf ppt "%a;@\n%a" print_condition r print_cond_list l

and print_condition ppt = function
  |(id, ty, e) -> fprintf ppt "%a : %a & @[%a@]" print_id id print_type ty print_expr e

let print_node ppt node =
  fprintf ppt
    "@[NODE %a (@[%a@]) RETURNS (@[%a@]) @\nVAR @[%a;@] @\nPRE : @[%a@]@\n@[<v 2>LET @\n @[%a@] @]@\nTEL @\nPOST : @[%a@] @]"
    print_id node.n_id
    print_decl_list node.n_param_in
    print_decl_list node.n_param_out
    print_decl_list node.n_vars
    print_cond_list node.n_pre
    print_eq_list node.n_eqs
    print_cond_list node.n_post

let print_prog node =
  Format.printf "Program normalized : @\n@\n%a@\n@." print_node node
