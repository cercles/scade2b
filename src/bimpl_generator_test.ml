(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_repr_norm
open Ast_base

(* A reecrire? *)
let print_bid env ppt id =
  let (_,bid,_) = 
    try
      Env.choose (Env.filter (fun (i,bid,_) -> i=id) env) 
    with Not_found -> assert false
  in
  fprintf ppt "%s" bid

let print_bid_list

let print_value env ppt = function
  | Bool b -> fprintf ppt "%b" b
  | Int i -> fprintf ppt "%d" i
  | Float f -> fprintf ppt "%f" f

let rec print_expr env ppt = function
  | NE_Ident id -> print_bid env ppt id
  | NE_Tuple e_list -> fprintf ppt "(@[%a@])" (print_e_list env) e_list
  | NE_Value v -> print_value env ppt v
  | NE_Array ar -> print_array env ppt ar
  | NE_Bop (bop, e1, e2) -> fprintf ppt "%a %a %a" (print_expr env) e1 print_bop bop (print_expr env) e2
  | NE_Unop (unop, e) -> fprintf ppt "%a%a" print_unop unop (print_expr env) e
  | NE_Sharp e_list -> fprintf ppt "#@[(%a)@]" (print_e_list env) e_list (* TROUVER TRADUCTION *)

(* A FAIRE! *)
and print_array env ppt = function 
  | NA_Def e_list -> fprintf ppt "[%a]" (print_e_list env) e_list
  | NA_Caret (e1, e2) -> fprintf ppt "%a ^ %a" (print_expr env) e1 (print_expr env) e2
  | NA_Concat (e1, e2) -> fprintf ppt "%a | %a" (print_expr env) e1 (print_expr env) e2
  | NA_Slice (id, e_list) -> fprintf ppt "%a[%a]" (print_bid env) id (print_slice_list env) e_list
  | NA_Index (id, e_list) -> fprintf ppt "%a[%a]" (print_bid env) id (print_index_list env) e_list

and print_e_list env ppt = function 
  | [] -> ()
  | [v] -> fprintf ppt "%a" (print_expr env) v
  | v::l -> fprintf ppt "%a, %a" (print_expr env) v (print_e_list env) l

and print_slice_list env ppt = function
  | [] -> ()
  | [(e1, e2)] when e1 = e2 -> fprintf ppt "[%a]" (print_expr env) e1
  | (e1, e2)::l when e1 = e2 -> fprintf ppt "[%a]%a" (print_expr env) e1 (print_slice_list env) l
  | [(e1, e2)] -> fprintf ppt "[%a .. %a]" (print_expr env) e1 (print_expr env) e2
  | (e1, e2)::l -> fprintf ppt "[%a .. %a]%a" (print_expr env) e1 (print_expr env) e2 (print_slice_list env) l

and print_index_list env ppt = function
  | [] -> ()
  | [(e)] -> fprintf ppt "[%a]" (print_expr env) e
  | (e)::l -> fprintf ppt "[%a]%a" (print_expr env) e (print_index_list env) l

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
  | Op_minus -> fprintf ppt " -"















let rec print_eqs env ppt = function
  | [] -> ()
  | [(lp, expr)] -> ()
  | _ -> ()




and print_eq ppt = function
  | N_Alternative a -> 
      fprintf ppt "%a =@[<v 2>@,IF%a@]@,@[<v 4>THEN@,%a@]@,@[<v 4>ELSE@,%a@]" 
	print_leftpart a.alt_lp 
	print_expr a.alt_cond
	print_expr a.alt_then
	print_expr a.alt_else
  | N_Fonction f -> 
      fprintf ppt "%a = @[%a(%a)@]" 
	print_leftpart f.fun_lp 
	print_id f.fun_id
	print_e_list f.fun_params
  | N_Operation o ->
      fprintf ppt "%a = @[%a@]" 
	print_leftpart o.op_lp
	print_expr o.op_expr
  | N_Registre r ->
      fprintf ppt "%a = @[REG(%a,%a)@] : %a" 
	print_leftpart r.reg_lp
	print_expr r.reg_ini
	print_expr r.reg_val
	print_type r.reg_type



let print_vars ppt var_list =
  if (List.length var_list) = 0 then () else
    let (id_var_list, _) = List.split var_list in
    fprintf ppt "VAR %s IN" (Utils.string_of_list id_var_list)

let rec print_declist env ppt = function
  | [] -> ()
  | [(id, _)] -> fprintf ppt "%a" (print_bid env) id
  | (id, _)::l -> fprintf ppt "%a, %a" (print_bid env) id (print_declist env) l 

let print_op_decl env ppt node =
  fprintf ppt "%a <-- %s(%a)"
    (print_declist env) node.n_param_out
    node.n_id
    (print_declist env) node.n_param_in
 
let print_operation ppt node =
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n%a@[<v 3>@,@[<v>%a@]@]@\n@\nEND"
    (print_op_decl node.env) node
    (print_vars node.env) node.n_vars
    (print_eqs node.env) node.n_eqs

let print_basetype ppt = function
  | T_Bool -> fprintf ppt "%s" "BOOL"
  | T_Int -> fprintf ppt "%s" "INT"
  | T_Float -> fprintf ppt "%s" "REAL"

let print_type ppt = function
  | NT_Base t -> print_basetype ppt t
  | NT_Array (t, expr) -> assert false (* SEQUENCES A FAIRE *)


let print_initialisation env ppt reg_list = ()

let print_invariant env ppt reg_list = ()

let print_concrete_var env ppt reg_list =
  if (List.length reg_list) = 0 then () 
  else 
    fprintf ppt "CONCRETE_VARIABLES %a" (print_bid reg_list)


(* A REFAIRE PAR RAPPORT AUX INCLUDES! *)
(* The file list can be configured in utils.ml *)
let print_imports ppt node =
  if (List.length Utils.imports_list) = 0 then () 
  else 
    fprintf ppt "IMPORTS %s" (Utils.string_of_list Utils.imports_list)

let print_sees ppt node =
  if (List.length Utils.sees_list) = 0 then () 
  else 
    fprintf ppt "SEES %s" (Utils.string_of_list Utils.sees_list)

let print_refines ppt id =
  fprintf ppt "REFINES %s" (String.capitalize id)

let print_implementation ppt id =
  fprintf ppt "IMPLEMENTATION %s" ((String.capitalize id)^"_i")

let print_machine ppt node =
  fprintf ppt
    "%a@\n%a@\n%a@\n%a@\n@\n%a@\n%a@\n%a@\n@\n%a"
    print_implementation node.n_id
    print_refines node.n_id
    print_sees node
    print_imports node
    (print_concrete_var node.n_env) node.n_reg
    (print_invariant node.n_env) node.n_reg
    (print_initialisation node.n_env) node.n_reg
    print_operation node

let print_prog node =
  printf "@\n@\nB Implementation : @\n@\n%a@\n@." print_machine node

