(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_scade_norm
open Ast_base
open Printer

let print_id ppt id = fprintf ppt "%s" id

let print_value ppt = function
  | Bool b -> fprintf ppt "%b" b
  | Int i -> fprintf ppt "%d" i
  | Float f -> fprintf ppt "%f" f

let rec print_expr ppt = function
  | NE_Ident id -> print_id ppt id
  | NE_Value v -> print_value ppt v
  | NE_Array ar -> print_array ppt ar
  | NE_Op_Arith1 (op, e) -> fprintf ppt "%a@[(%a)@]" print_op_a1 op print_expr e
  | NE_Op_Arith2 (op, e1, e2) -> fprintf ppt "%a@[(%a, %a)@]" print_op_a2 op print_expr e1 print_expr e2
  | NE_Op_Relat (op, e1, e2) -> fprintf ppt "%a@[(%a, %a)@]" print_op_r op print_expr e1 print_expr e2
  | NE_Op_Sharp e_list -> fprintf ppt "#@[(%a)@]" print_e_list e_list
  | NE_Op_Not e -> fprintf ppt "@[!(%a)@]" print_expr e
  | NE_Op_Logic (op, e1, e2) -> fprintf ppt "%a@[(%a, %a)@]" print_op_l op print_expr e1 print_expr e2

and print_array ppt = function
  | NA_Def e_list -> fprintf ppt "def[ %a ]" print_e_list e_list
  | NA_Caret (e1, e2) -> fprintf ppt "(%a ^ %a)" print_expr e1 print_expr e2
  | NA_Concat (e1, e2) -> fprintf ppt "%a | %a" print_expr e1 print_expr e2
  | NA_Slice (id, e_list) -> fprintf ppt "%a[%a]" print_id id print_slice_list e_list
  | NA_Index (id, e_list) -> fprintf ppt "%a[%a]" print_id id print_index_list e_list
  | NA_Reverse (id) -> fprintf ppt "rev(%a)" print_id id

and print_e_list ppt l =
  print_list print_expr ppt l

and print_slice ppt (e1, e2) =
  if e1 = e2 then
    fprintf ppt "[%a]" print_expr e1
  else
    fprintf ppt "[%a .. %a]" print_expr e1 print_expr e2

and print_slice_list ppt l =
  print_list print_slice ~sep:"" ~break:false ppt l

and print_index_list ppt l =
  let print_index ppt e =
    fprintf ppt "[%a]" print_expr e
  in
  print_list print_index ~sep:"" ~break:false ppt l

and print_op_a1 ppt = function
  | Op_minus -> fprintf ppt "-"
  | Op_cast_real -> fprintf ppt "real"
  | Op_cast_int -> fprintf ppt "int"

and print_op_a2 ppt = function
  | Op_eq -> fprintf ppt "eq"
  | Op_neq -> fprintf ppt "neq"
  | Op_add -> fprintf ppt "add"
  | Op_sub -> fprintf ppt "sub"
  | Op_mul -> fprintf ppt "mul"
  | Op_div -> fprintf ppt "div"
  | Op_mod -> fprintf ppt "mod"
  | Op_div_f -> fprintf ppt "div_f"

and print_op_r ppt = function
  | Op_lt -> fprintf ppt "lt"
  | Op_le -> fprintf ppt "le"
  | Op_gt -> fprintf ppt "gt"
  | Op_ge -> fprintf ppt "ge"

and print_op_l ppt = function
  | Op_and -> fprintf ppt "and"
  | Op_or -> fprintf ppt "or"
  | Op_xor -> fprintf ppt "xor"

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
  | T_Poly -> fprintf ppt "'T"
  | T_Enum id -> fprintf ppt "%s" id

let rec print_eq_list ppt = function
  | [] -> ()
  | [eq] -> fprintf ppt "@[%a@];" print_eq eq
  | eq::l -> fprintf ppt "@[%a@];@\n%a" print_eq eq print_eq_list l 

and print_eq ppt = function
  | N_Alternative a -> 
      fprintf ppt "%a = IF %a @,@[<v 4>THEN %a @]@,@[<v 4>ELSE %a @]" 
	print_leftpart a.n_alt_lp 
	print_expr a.n_alt_cond
	print_expr a.n_alt_then
	print_expr a.n_alt_else
  | N_Call f -> 
      fprintf ppt "%a = @[%a_%a(%a)@]" 
	print_leftpart f.n_fun_lp 
	print_id f.n_fun_id
	print_id f.n_fun_pragma
	print_e_list f.n_fun_params
  | N_Operation o ->
      fprintf ppt "%a = @[%a@]" 
	print_leftpart o.n_op_lp
	print_expr o.n_op_expr
  | N_Registre r ->
      fprintf ppt "%a = @[REG(%a,%a)@] : %a" 
	print_id r.n_reg_lpid
	print_expr r.n_reg_ini
	print_expr r.n_reg_val
	print_type r.n_reg_type

and print_leftpart ppt = function
  | NLP_Ident id -> print_id ppt id
  | NLP_Tuple id_list -> print_id_list ppt id_list

and print_id_list ppt l =
  print_list print_id ppt l

let print_decl_list ppt l =
  let print_decl ppt (name, ty) =
    fprintf ppt "%a : %a" print_id name print_type ty
  in
  print_list print_decl ~sep:"; " ppt l

let print_condition ppt = function
  |(id, ty, Some e) -> fprintf ppt "%a : %a & @[%a@]" print_id id print_type ty print_expr e
  |(id, ty, None) -> fprintf ppt "%a : %a " print_id id print_type ty

let print_cond_list ppt l =
  print_list print_condition ~sep:";" ~break:true ~forcebreak:true ppt l

let print_lambda_list ppt l =
  let print_lambda ppt l =
    fprintf ppt "%a(%d) @[%a@] " print_id l.n_l_ident l.n_l_index print_condition l.n_l_cond
  in
  print_list print_lambda ~sep:"" ~break:true ppt l

let print_my_node ppt node =
  fprintf ppt
    "@[NODE %a (@[%a@]) RETURNS (@[%a@]) @\nVAR @[%a;@] @\nPRE : @[%a@]@\n@[<v 2>LET @\n @[%a@] @]@\nTEL @\nPOST : @[%a@] @\nLAMBDAS : @[%a@] @]"
    print_id node.n_id
    print_decl_list node.n_param_in
    print_decl_list node.n_param_out
    print_decl_list node.n_vars
    print_cond_list node.n_pre
    print_eq_list node.n_eqs
    print_cond_list node.n_post
    print_lambda_list node.n_lambdas

let print_node node =
  Format.printf "Node normalized : @\n%a@\n@." print_my_node node
