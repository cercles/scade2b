(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_norm_repr
open Ast_base

(* a faire et a  mettre dans utils *)
let new_id = "ii"

let print_bid ppt id =
  let bid = if (String.length id) > 1 then id else id^id in
  fprintf ppt "%s" bid

let print_value ppt = function
  | Bool b -> fprintf ppt "%b" b
  | Int i -> fprintf ppt "%d" i
  | Float f -> fprintf ppt "%f" f

let rec print_expr ppt = function
  | NE_Ident id -> print_bid ppt id
  | NE_Tuple e_list -> fprintf ppt "(@[%a@])" print_e_list e_list
  | NE_Value v -> print_value ppt v
  | NE_Array ar -> print_array ppt ar
  | NE_App (id, e_list) -> fprintf ppt "%a@[(%a)@]" print_bid id print_e_list e_list
  | NE_Bop (bop, e1, e2) -> fprintf ppt "%a %a %a" print_expr e1 print_bop bop print_expr e2
  | NE_Unop (unop, e) -> fprintf ppt "%a%a" print_unop unop print_expr e
  | NE_If (cond, e1, e2) -> fprintf ppt "IF @[%a@] THEN @\n@[<v 2>%a@] @\nELSE @\n@[<v 2>%a@]" print_expr cond print_expr e1 print_expr e2
  | NE_Sharp e_list -> fprintf ppt "#@[(%a)@]" print_e_list e_list (* TROUVER TRADUCTION *)

(* A FAIRE! *)
and print_array ppt = function 
  | NA_Def e_list -> fprintf ppt "[%a]" print_e_list e_list
  | NA_Caret (e1, e2) -> fprintf ppt "%a ^ %a" print_expr e1 print_expr e2
  | NA_Concat (e1, e2) -> fprintf ppt "%a | %a" print_expr e1 print_expr e2
  | NA_Slice (id, e_list) -> fprintf ppt "%a[%a]" print_bid id print_slice_list e_list
  | NA_Index (id, e_list) -> fprintf ppt "%a[%a]" print_bid id print_index_list e_list

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
  | Op_or -> fprintf ppt "|"
  | Op_xor -> fprintf ppt "xor" (* XOR en B?? *)

and print_unop ppt = function 
  | Op_not -> fprintf ppt "not "
  | Op_minus -> fprintf ppt "-"

let rec print_declist ppt = function
  | [] -> ()
  | [(id, _)] -> fprintf ppt "%a" print_bid id
  | (id, _)::l -> fprintf ppt "%a, %a" print_bid id print_declist l 

let print_op_decl ppt node =
  fprintf ppt "%a <-- %s(%a)"
    print_declist node.n_param_out
    node.n_id
    print_declist node.n_param_in

let print_basetype ppt = function
  | T_Bool -> fprintf ppt "%s" "BOOL"
  | T_Int -> fprintf ppt "%s" "INT"
  | T_Float -> fprintf ppt "%s" "REAL"

let print_type ppt = function
  | NT_Base t -> print_basetype ppt t
  | _ -> assert false (* SEQUENCES A FAIRE *)

let print_pre_condition ppt (id, t, expr) =
  fprintf ppt "%a : %a & %a"
    print_bid id
    print_type t
    print_expr expr 

let print_then_condition ppt (id, t, expr) =
  fprintf ppt "%a :: { %a | %a : %a & %a }"
    print_bid id
    print_bid (new_id)
    print_bid (new_id) 
    print_type t
    print_expr expr (* PROBLEME, PAS LE BON ID DANS L'EXPR (devrait etre new_id) *)

(* factoriser les 2 *)

let rec print_prelist ppt = function 
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_pre_condition c
  | c::l -> fprintf ppt "%a, %a" print_pre_condition c print_prelist l 

let rec print_thenlist ppt = function
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_then_condition c
  | c::l -> fprintf ppt "%a, %a" print_then_condition c print_prelist l 

let print_operation ppt node =
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n@[<v 3> PRE@,@[<v> %a@]@]@\n@[<v 3> THEN@,@[<v> %a@]@]@\n END"
    print_op_decl node
    print_prelist node.n_pre
    print_thenlist node.n_post

let print_id_machine ppt id =
  fprintf ppt "%s" (String.capitalize id)

let print_machine ppt node =
  fprintf ppt
    "MACHINE %a @\n@\n%a"
    print_id_machine node.n_id
    print_operation node

let print_prog node =
  printf "@\n@\nB Signature : @\n@\n%a@\n@." print_machine node
