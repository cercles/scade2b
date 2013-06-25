(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_repr_b
open Ast_base



(* TODO: Generateur de nom de variables absentes de l'environnement. --> dans trad.ml *)

(* too naive, change that. *)
let fun_cond = ref false 
let var_cond = ref []

let print_bid ppt id =
  if !fun_cond = true then
    let indice = 
      try
	"(" ^ (List.assoc id !var_cond) ^ ")"
      with Not_found -> ""
    in
    fprintf ppt "%s%s" id indice
  else fprintf ppt "%s" id

let rec print_idlist_comma ppt = function
  | [] -> ()
  | [id] -> fprintf ppt "%a" print_bid id
  | id::l -> fprintf ppt "%a, %a" print_bid id print_idlist_comma l

let print_value ppt = function
  | Bool b -> fprintf ppt "%s" (if b then "TRUE" else "FALSE")
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
  | BE_Bop (bop, e1, e2) when bop = Op_xor -> fprintf ppt "xor(%a, %a)" print_expr e1 print_expr e2
  | BE_Bop (bop, e1, e2) -> fprintf ppt "%a %a %a" print_expr e1 print_bop bop print_expr e2
  | BE_Unop (unop, e) -> fprintf ppt "%a%a" print_unop unop print_expr e
  | BE_Sharp e_list -> fprintf ppt "sharp(%a)" print_e_list e_list

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
  fun_rec 1 ppt e_list

and print_slice_list ppt = function
  | [] -> ()
  | [(e1, e2)] -> fprintf ppt "(%a, %a)" print_expr e1 print_expr e2
  | (e1, e2)::l -> fprintf ppt "(%a, %a), %a" print_expr e1 print_expr e2 print_slice_list l

and print_index_list ppt = function
  | [] -> ()
  | [(e)] -> fprintf ppt "%a" print_expr e
  | (e)::l -> fprintf ppt "%a, %a" print_expr e print_index_list l

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
  | Op_xor -> assert false

and print_unop ppt = function 
  | Op_not -> fprintf ppt "not "
  | Op_minus -> fprintf ppt "-"

let print_lp ppt = function
  | BLP_Ident id -> print_bid ppt id
  | BLP_Tuple id_list -> print_idlist_comma ppt id_list


let print_alternative ppt a =
  fprintf ppt "IF %a THEN %a := %a ELSE %a := %a" 
    print_expr a.alt_cond
    print_lp a.alt_lp
    print_expr a.alt_then     
    print_lp a.alt_lp
    print_expr a.alt_else

let print_function ppt f =
  fprintf ppt "%a <-- %s(%a)"
    print_lp f.fun_lp
    f.fun_id
    print_e_list f.fun_params
    
let print_op ppt o =
  fprintf ppt "%a := %a"
    print_lp o.op_lp
    print_expr o.op_expr
  
let print_eq ppt = function
  | Alternative a -> fprintf ppt "%a" print_alternative a
  | Fonction f -> fprintf ppt "%a" print_function f
  | Operation o -> fprintf ppt "%a" print_op o

let rec print_eq_list ppt = function
  | [] -> ()
  | [eq] -> fprintf ppt "%a" print_eq eq
  | eq::l -> fprintf ppt "%a; @,%a" print_eq eq print_eq_list l 

  
let print_registre ppt r =
  fprintf ppt "%a := %a"
    print_bid r.reg_lpid
    print_expr r.reg_val
  
let rec print_reg_list ppt = function
  | [] -> ()
  | [r] -> fprintf ppt "%a" print_registre r
  | r::l -> fprintf ppt "%a; @,%a" print_registre r print_reg_list l 


let print_vars ppt var_list =
  if (List.length var_list) = 0 then () 
  else
    fprintf ppt "VAR %a IN" print_idlist_comma var_list

let print_op_decl ppt op_decl =
  fprintf ppt "%a <-- %s(%a)"
    print_idlist_comma op_decl.param_out
    op_decl.id
    print_idlist_comma op_decl.param_in
 
let print_operation ppt operations =
  let sep = if (List.length operations.op_2) > 0 then ";" else "" in
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n %a@\n@[<v 3>   %a%s@,%a@]@\n END"
    print_op_decl operations.op_decl
    print_vars operations.vars
    print_eq_list operations.op_1
    sep
    print_reg_list operations.op_2

let print_basetype ppt = function
  | T_Bool -> fprintf ppt "%s" "BOOL"
  | T_Int -> fprintf ppt "%s" "INT"
  | T_Float -> fprintf ppt "%s" "REAL"

let rec print_dim_list ppt = function
  | [] -> ()
  | [d] -> fprintf ppt "1 .. %a" print_expr d
  | d :: l -> fprintf ppt "1 .. %a, %a " print_expr d print_dim_list l

let print_array_type t ppt e_list =
  fprintf ppt "%a --> %a" print_dim_list e_list print_basetype t

let rec print_initialisation_list ppt = function
  | [] -> ()
  | [(id, e)] -> fprintf ppt "%a := %a" print_bid id print_expr e
  | (id, e)::l -> fprintf ppt "%a := %a ; @,%a" print_bid id print_expr e print_initialisation_list l 

let print_initialisation ppt ini_list = 
  if (List.length ini_list) = 0 then () 
  else 
    fprintf ppt "INITIALISATION @\n@[<v 3>   %a@]" print_initialisation_list ini_list 


let print_condition ppt = function
  | Base_expr (id, t, expr) -> 
    fprintf ppt "%a : %a & %a"
      print_bid id
      print_basetype t
      print_expr expr 
  | Base_no_expr (id, t) ->
      fprintf ppt "%a : %a"
	print_bid id
	print_basetype t
  | Fun_expr (id, t, e_list, expr) ->
    var_cond := (id, "iii") :: !var_cond;
    fprintf ppt "%a : %a & !%s. (%s : (%a) => "
      print_bid id
      (print_array_type t) e_list
      "iii"
      "iii"
      print_dim_list e_list;
    fun_cond := true;    
    fprintf ppt "%a)" print_expr expr;
    var_cond := List.tl !var_cond;
    fun_cond := false
  | Fun_no_expr (id, t, e_list) ->
    fprintf ppt "%a : %a"
      print_bid id
      (print_array_type t) e_list


let rec print_invariant_list ppt = function 
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_condition c
  | c::l -> fprintf ppt "%a & @,%a" print_condition c print_invariant_list l 

let print_invariant ppt inv_list = 
  if (List.length inv_list) = 0 then () 
  else 
    fprintf ppt "INVARIANT @\n@[<v 3>   %a@]" print_invariant_list inv_list 


let print_concrete_var ppt reg_list =
  if (List.length reg_list) = 0 then () 
  else 
    fprintf ppt "CONCRETE_VARIABLES %a" print_idlist_comma reg_list 


let print_imports ppt imports_l =
  if (List.length imports_l) = 0 then () 
  else 
    fprintf ppt "IMPORTS %a" print_idlist_comma imports_l


let print_sees ppt sees_l =
  if (List.length sees_l) = 0 then () 
  else 
    fprintf ppt "SEES %a" print_idlist_comma sees_l


let print_refines ppt id =
  fprintf ppt "REFINES %s" id


let print_implementation ppt impl_name =
  fprintf ppt "%s" impl_name


let print_machine ppt b_impl =
  fprintf ppt
    "IMPLEMENTATION %a@\n%a@\n%a@\n%a@\n@\n%a@\n%a@\n%a@\n@\n%a @\nEND"
    print_implementation b_impl.name
    print_refines b_impl.refines
    print_sees b_impl.sees
    print_imports b_impl.imports
    print_concrete_var b_impl.concrete_variables
    print_invariant b_impl.invariant
    print_initialisation b_impl.initialisation
    print_operation b_impl.operations

let print_prog b_impl file =
  fprintf (formatter_of_out_channel file) "%a@." print_machine b_impl
