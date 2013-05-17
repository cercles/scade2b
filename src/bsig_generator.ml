(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_repr_norm
open Ast_base

(* A CHANGER, CREER UN ENVIRONNEMENT DANS UTILS POUR FAIRE LA CORRESPONDANCE ENTRE L'ID SCADE ET L'ID B. *)
let print_bid env ppt id =
  let bid = if (String.length id) > 1 then id else id^id in
  fprintf ppt "%s" bid

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

let rec print_declist env ppt = function
  | [] -> ()
  | [(id, _)] -> fprintf ppt "%a" (print_bid env) id
  | (id, _)::l -> fprintf ppt "%a, %a" (print_bid env) id (print_declist env) l 

let print_op_decl env ppt node =
  fprintf ppt "%a <-- %s(%a)"
    (print_declist env) node.n_param_out
    node.n_id
    (print_declist env) node.n_param_in

let print_basetype ppt = function
  | T_Bool -> fprintf ppt "%s" "BOOL"
  | T_Int -> fprintf ppt "%s" "INT"
  | T_Float -> fprintf ppt "%s" "REAL"

let print_type env ppt = function
  | NT_Base t -> print_basetype ppt t
  | NT_Array (t, expr) -> assert false (* SEQUENCES A FAIRE *)

let print_pre_condition env ppt (id, t, expr) =
  fprintf ppt "%a : %a & %a"
    (print_bid env) id
    (print_type env) t
    (print_expr env) expr 

let print_then_condition env ppt (id, t, expr) =
  fprintf ppt "%a :: { %a | %a : %a & %a }"
    (print_bid env) id
    (print_bid env) id
    (print_bid env) id
    (print_type env) t
    (print_expr env) expr

let rec print_prelist env ppt = function 
  | [] -> ()
  | [c] -> fprintf ppt "%a" (print_pre_condition env) c
  | c::l -> fprintf ppt "%a &@,%a" (print_pre_condition env) c (print_prelist env) l 

let rec print_thenlist env ppt = function
  | [] -> ()
  | [c] -> fprintf ppt "%a" (print_then_condition env) c
  | c::l -> fprintf ppt "%a@,||%a" (print_then_condition env) c (print_prelist env) l 

let print_operation env ppt node =
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n@[<v 3> PRE@,@[<v> %a@]@]@\n@[<v 3> THEN@,@[<v> %a@]@]@\n END"
    (print_op_decl env) node
    (print_prelist env) node.n_pre
    (print_thenlist env) node.n_post

(* The file list can be configured in utils.ml *)
let print_sees ppt node =
  if (List.length Utils.sees_list) = 0 then () 
  else 
    fprintf ppt "SEES %s" (Utils.string_of_list Utils.sees_list)

let print_id_machine ppt id =
  fprintf ppt "MACHINE %s" (String.capitalize id)

let print_machine ppt node =
  fprintf ppt
    "%a@\n%a@\n%a"
    print_id_machine node.n_id
    print_sees node
    (print_operation node.n_env) node

let print_prog node =
  printf "@\n@\nB Signature : @\n@\n%a@\n@." print_machine node
