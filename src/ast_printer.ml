
open Format
open Ast_repr
open Ast_base


let print_id ppt id = fprintf ppt "%s" id

let print_value ppt = function
  | Bool b -> fprintf ppt "%b" b
  | Int i -> fprintf ppt "%d" i
  | Float f -> fprintf ppt "%f" f

let rec print_expr ppt = function
  | PE_Ident id -> print_id ppt id
  | PE_Tuple e_list -> fprintf ppt "(@[%a@])" print_e_list e_list
  | PE_Value v -> print_value ppt v
  | PE_Array ar -> print_array ppt ar
  | PE_App (id, e_list) -> fprintf ppt "%a@[(%a)@]" print_id id print_e_list e_list
  | PE_Bop (bop, e1, e2) -> fprintf ppt "%a@[(%a, %a)@]" print_bop bop print_expr e1 print_expr e2
  | PE_Unop (unop, e) -> fprintf ppt "%a@[(%a)@]" print_unop unop print_expr e
  | PE_Fby (e1, e2) -> fprintf ppt "@[(%a)@] -> @[(%a)@]" print_expr e1 print_expr e2
  | PE_Pre e -> fprintf ppt "pre@[(%a)@]" print_expr e
  | PE_If (cond, e1, e2) -> fprintf ppt "if @[%a@] then @\n@[<v 4>%a@] @\nelse @\n@[<v 4>%a@]" print_expr cond print_expr e1 print_expr e2
  | PE_Sharp e_list -> fprintf ppt "#@[(%a)@]" print_e_list e_list

and print_array ppt = function
  | PA_Def e_list -> fprintf ppt "[%a]" print_e_list e_list
  | PA_Caret (e1, e2) -> fprintf ppt "%a ^ %a" print_expr e1 print_expr e2
  | PA_Concat (e1, e2) -> fprintf ppt "%a | %a" print_expr e1 print_expr e2
  | PA_Slice (id, e_list) -> fprintf ppt "%a%a" print_id id print_slice_list e_list
  | PA_Index (id, e_list) -> fprintf ppt "%a%a" print_id id print_index_list e_list

and print_e_list ppt = function 
  | [] -> ()
  | [v] -> fprintf ppt "%a" print_expr v
  | v::l -> fprintf ppt "%a, %a" print_expr v print_e_list l

and print_slice_list ppt = function
  | [] -> ()
  | [(e1, e2)] -> fprintf ppt "[%a .. %a]" print_expr e1 print_expr e2
  | (e1, e2)::l -> fprintf ppt "[%a .. %a]%a" print_expr e1 print_expr e2 print_slice_list l

and print_index_list ppt = function
  | [] -> ()
  | [e] -> fprintf ppt "[%a]" print_expr e
  | e::l -> fprintf ppt "[%a]%a" print_expr e print_index_list l

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


let rec print_eq_list ppt = function
  | [] -> ()
  | [eq] -> fprintf ppt "@[%a@];" print_eq eq
  | eq::l -> fprintf ppt "@[%a@];@\n%a" print_eq eq print_eq_list l 

and print_eq ppt = function
  | P_Eq (lp, e) -> fprintf ppt "%a = @[%a@]" print_leftpart lp print_expr e
  | P_Assert e -> fprintf ppt "ASSERT : %a " print_expr e

and print_leftpart ppt = function
  | PLP_Ident id -> print_id ppt id
  | PLP_Tuple id_list -> print_id_list ppt id_list

and print_id_list ppt = function
  | [] -> ()
  | [id] -> fprintf ppt "%a" print_id id
  | id::l -> fprintf ppt "%a, %a" print_id id print_id_list l
 

let rec print_type ppt = function
  | PT_Base b -> print_base_type ppt b
  | PT_Array (t, e) -> 
    fprintf ppt 
      "%a ^ %a" 
      print_type t
      print_expr e 

and print_base_type ppt = function
  | T_Bool -> fprintf ppt "bool"
  | T_Int -> fprintf ppt "int"
  | T_Float -> fprintf ppt "real"


let rec print_decl_list ppt =  function
  | [] -> ()
  | [d] -> fprintf ppt "%a" print_decl d
  | d::l -> fprintf ppt "%a; %a" print_decl d print_decl_list l

and print_decl ppt = function
  | (name, ty) -> fprintf ppt "%a : %a" print_id name print_type ty

let print_node ppt node =
  fprintf ppt
    "@[NODE %a (@[%a@]) RETURNS (@[%a@]) @\nVAR @[%a;@] @\n@[<v 2>LET @ @[%a @] @]@\nTEL @]"
    print_id node.p_id
    print_decl_list node.p_param_in
    print_decl_list node.p_param_out
    print_decl_list node.p_vars
    print_eq_list node.p_eqs

let print_prog prog =
  Format.printf "@\nProgram Parsed :@\n%a@\n@." print_node prog.p_node
