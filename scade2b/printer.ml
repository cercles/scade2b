open Ast_base
open Ast_repr_b
open Ast_scade_norm
open Format

let env = ref None

let print_bid ppt id =
    let bid = match !env with
      | None -> id
      | Some e -> Env.find id e
    in
    fprintf ppt "%s" bid

let with_env e f =
    (match !env with
        | Some _ -> failwith "with_env: env should be None"
        | None -> ());
    env := Some e;
    let r = f () in
    env := None;
    r

(**
 * Print a list to a Format.formatter.
 *
 * The elements are printed using print_elem.
 * They are separated by sep and a space.
 * If break is true, a good break hint (@,) is added.
 *)
let print_list ?(sep=",") ?(break=false) print_elem ppt l =
  let rec go ppt = function
  | [] -> ()
  | [x] -> fprintf ppt "%a" print_elem x
  | x::xs ->
      let spc : ((_, _, _, _, _, _) format6) =
        if break then " @," else " "
      in
      fprintf ppt ("%a%s" ^^ spc ^^ "%a")
        print_elem x sep go xs
  in
  go ppt l

let print_list_semicolon print_elem ppt =
  print_list ~sep:";" ~break:true print_elem ppt

let print_idlist_comma = print_list print_bid

let print_value ppt = function
  | Bool b -> fprintf ppt "%s" (if b then "TRUE" else "FALSE")
  | Int i -> fprintf ppt "%d" i
  | Float f -> fprintf ppt "%f" f

let print_op_arith1 ppt = function
  | Op_minus -> fprintf ppt "-"
  | Op_cast_real -> fprintf ppt "REAL"
  | Op_cast_int -> fprintf ppt "INT"

let print_op_arith2 ppt = function
  | Op_eq -> fprintf ppt "="
  | Op_neq -> fprintf ppt "/="
  | Op_add -> fprintf ppt "+"
  | Op_sub -> fprintf ppt "-"
  | Op_mul -> fprintf ppt "*"
  | Op_div -> fprintf ppt "/"
  | Op_mod -> fprintf ppt "mod"
  | Op_div_f -> fprintf ppt "/"

let print_op_relat ppt = function
  | Op_lt -> fprintf ppt "<"
  | Op_le -> fprintf ppt "<="
  | Op_gt -> fprintf ppt ">"
  | Op_ge -> fprintf ppt ">="

let rec print_expr ppt = function
  | BE_Ident id -> print_bid ppt id
  | BE_Value v -> print_value ppt v
  | BE_Array ar -> print_array ppt ar
  | BE_Op_Arith1 (op, e) -> (
      match op with
	| Op_minus ->
	    fprintf ppt "(%a%a)" print_op_arith1 op print_expr e
	| Op_cast_real | Op_cast_int ->
	    fprintf ppt "(%a (%a))" print_op_arith1 op print_expr e
    )
  | BE_Op_Arith2 (op, e1, e2) -> (
      match op with
	| Op_eq | Op_neq ->
            fprintf ppt "bool(%a %a %a)" print_expr e1 print_op_arith2 op print_expr e2
        | _ -> fprintf ppt "%a %a %a" print_expr e1 print_op_arith2 op print_expr e2
    )
  | BE_Op_Sharp e_list ->
      fprintf ppt "sharp(%a)" print_expr_list e_list
  | BE_Pred p ->
      fprintf ppt "bool%a" print_pred p

(* TODO: find a way to merge *)
and print_expr_in_pred ppt = function
  | BE_Ident id -> print_bid ppt id
  | BE_Value v -> print_value ppt v
  | BE_Array ar -> print_array ppt ar
  | BE_Op_Arith1 (op, e) -> (
      match op with
	| Op_minus ->
	    fprintf ppt "(%a%a)" print_op_arith1 op print_expr e
	| Op_cast_real | Op_cast_int ->
	    fprintf ppt "(%a (%a))" print_op_arith1 op print_expr e
    )
  | BE_Op_Arith2 (op, e1, e2) -> (
      match op with
	| Op_eq | Op_neq ->
            fprintf ppt "bool(%a %a %a)" print_expr e1 print_op_arith2 op print_expr e2
        | _ -> fprintf ppt "%a %a %a" print_expr e1 print_op_arith2 op print_expr e2
    )
  | BE_Op_Sharp e_list ->
      fprintf ppt "sharp(%a)" print_expr_list e_list
  | BE_Pred p ->
      fprintf ppt "%a" print_pred p


and print_pred ppt = function
  | BP_Op_Logic (op, p1, p2) ->
    fprintf ppt "(%a %a %a)" print_pred p1 print_op op print_pred p2
  | BP_Op_Relat (op, e1, e2) ->
    fprintf ppt "(%a %a %a)" print_expr e1 print_op_relat op print_expr e2
  | BP_Not p ->
    fprintf ppt "(not %a)" print_pred p
  | BP_Expr e ->
    fprintf ppt "(%a = TRUE)" print_expr e

and print_op ppt = function
  | Op_and -> fprintf ppt "&"
  | Op_or  -> fprintf ppt "or"
  | Op_xor -> fprintf ppt "/="

and print_expr_list ppt = print_list print_expr ppt

and print_array ppt = function
  | BA_Def e_list -> fprintf ppt "{%a}" print_def_list e_list
  | BA_Index (id, e_list) -> fprintf ppt "%a(%a)" print_bid id print_expr_list e_list
  | BA_Caret (e1, e2) -> fprintf ppt "caret(%a, %a)" print_expr e1 print_expr e2
  | BA_Concat (e1, e2) -> fprintf ppt "%a ^ %a" print_expr e1 print_expr e2
  | BA_Slice (id, e_list) -> fprintf ppt "(%a)" (print_slice id) (List.rev e_list)
  | BA_Reverse (id) -> fprintf ppt "rev(%a)" print_bid id

and print_def_list ppt e_list =
  let rec fun_rec n ppt = function
    | [] -> ()
    | [v] -> fprintf ppt "%d |-> %a" n print_expr v
    | v::l -> fprintf ppt "%d |-> %a, %a" n print_expr v (fun_rec (n+1)) l
  in
  fun_rec 0 ppt e_list

and print_slice id ppt e_list_rev =
  match e_list_rev with
  | [] -> assert false
  | [(e1, e2)] -> fprintf ppt "(%a /|\\ (%a + 1)) \\|/ %a"
       print_bid id print_expr e2 print_expr e1
  | (e1, e2) :: list -> fprintf ppt "((%a) /|\\ (%a + 1)) \\|/ %a"
       (print_slice id) list print_expr e2 print_expr e1

let print_basetype ppt = function
  | T_Bool -> fprintf ppt "%s" "BOOL"
  | T_Int -> fprintf ppt "%s" "INT"
  | T_Float -> fprintf ppt "%s" "REAL"
  | T_Poly -> fprintf ppt "%s" "BIG"
  | T_Enum id -> fprintf ppt "%s" id 

let print_sees ppt = function
    | [] -> ()
    | sees_l -> fprintf ppt "SEES %a" print_idlist_comma sees_l

let print_params_machine ppt = function
    | [] -> ()
    | params_machine -> fprintf ppt "(%a)" print_idlist_comma params_machine

let rec print_array_type t ppt e_list =
  match e_list with
  | [] -> fprintf ppt "%a" print_basetype t
  | _ :: l -> fprintf ppt "seq(%a)" (print_array_type t) l
