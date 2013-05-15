(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_norm_repr
open Ast_base

(* A CHANGER, CREER UN ENVIRONNEMENT DANS UTILS POUR FAIRE LA CORRESPONDANCE ENTRE L'ID SCADE ET L'ID B. *)
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
  | Op_or -> fprintf ppt "or"
  | Op_xor -> fprintf ppt "xor" (* XOR en B?? *)

and print_unop ppt = function 
  | Op_not -> fprintf ppt "not "
  | Op_minus -> fprintf ppt " -"



(*                *              *STAND_BY REFACTORING AST*                  *                   *)

let rec print_eqs ppt = function
  | [] -> ()
  | [(lp, expr)] -> ()
  | _ -> ()



let print_vars ppt var_list =
  if (List.length var_list) = 0 then () else
    let (id_var_list, _) = List.split var_list in
    fprintf ppt "VAR %s IN" (Utils.string_of_list id_var_list)

let rec print_declist ppt = function
  | [] -> ()
  | [(id, _)] -> fprintf ppt "%a" print_bid id
  | (id, _)::l -> fprintf ppt "%a, %a" print_bid id print_declist l 

let print_op_decl ppt node =
  fprintf ppt "%a <-- %s(%a)"
    print_declist node.n_param_out
    node.n_id
    print_declist node.n_param_in
 
let print_operation ppt node =
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n%a@[<v 3>@,@[<v>%a@]@]@\n@\nEND"
    print_op_decl node
    print_vars node.n_vars
    print_eqs node.n_eqs




let print_basetype ppt = function
  | T_Bool -> fprintf ppt "%s" "BOOL"
  | T_Int -> fprintf ppt "%s" "INT"
  | T_Float -> fprintf ppt "%s" "REAL"

let print_type ppt = function
  | NT_Base t -> print_basetype ppt t
  | NT_Array (t, expr) -> assert false (* SEQUENCES A FAIRE *)

let print_initialisation ppt reg_list = ()

let print_invariant ppt reg_list = ()

let print_concrete_var ppt reg_list =
  if (List.length reg_list) = 0 then () 
  else 
    fprintf ppt "CONCRETE_VARIABLES %s" (Utils.string_of_reglist reg_list)


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
    print_concrete_var node.n_reg
    print_invariant node.n_reg
    print_initialisation node.n_reg
    print_operation node

let print_prog node =
  printf "@\n@\nB Implementation : @\n@\n%a@\n@." print_machine node
