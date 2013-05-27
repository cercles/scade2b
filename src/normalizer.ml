(* Florian Thibord  --  Projet CERCLES *)

open Ast_base
open Ast_repr
open Ast_repr_norm
open Utils

exception Assert_id_error of string
exception Register_error
exception Non_Atomic of string

(* ast_repr to ast_repr_norm functions *)
let rec p_expr_to_n_expr = function
  | PE_Ident iden -> NE_Ident iden
  | PE_Tuple elist -> NE_Tuple (List.map p_expr_to_n_expr elist)
  | PE_Value v -> NE_Value v
  | PE_Array array -> NE_Array (p_array_to_n_array array)
  | PE_Bop (bop, e1, e2) -> NE_Bop (bop, p_expr_to_n_expr e1, p_expr_to_n_expr e2)
  | PE_Unop (unop, e) -> NE_Unop (unop, p_expr_to_n_expr e)
  | PE_Sharp elist -> NE_Sharp (List.map p_expr_to_n_expr elist)
  | PE_Fby (e1, e2) -> raise (Non_Atomic "->")
  | PE_Pre e -> raise (Non_Atomic "pre")
  | PE_If (e1, e2, e3) -> raise (Non_Atomic "if")
  | PE_App (id, elist) -> raise (Non_Atomic "function")

and p_array_to_n_array = function
  | PA_Def elist -> NA_Def (List.map p_expr_to_n_expr elist)
  | PA_Caret (e1, e2) -> NA_Caret (p_expr_to_n_expr e1, p_expr_to_n_expr e2)
  | PA_Concat (e1, e2) -> NA_Concat (p_expr_to_n_expr e1, p_expr_to_n_expr e2)
  | PA_Slice (id, l) -> NA_Slice (id, (List.map (fun (e1, e2) -> p_expr_to_n_expr e1, p_expr_to_n_expr e2) l))
  | PA_Index (id, l) -> NA_Index (id, List.map p_expr_to_n_expr l)

let plp_to_nlp = function
  | PLP_Ident id -> NLP_Ident id
  | PLP_Tuple ids -> NLP_Tuple ids

let rec p_type_to_n_type = function
  | PT_Base b -> NT_Base b
  | PT_Array (t, e) -> NT_Array (p_type_to_n_type t, p_expr_to_n_expr e)

let p_decl_to_n_decl declist =
  List.map (fun (id, p_type) -> (id, p_type_to_n_type p_type)) declist



(* Type used for pre/post conditions *)
type pre_post = Pre of n_condition | Post of n_condition

(* Find the type related to a declaration *)
let rec find_type id declist =
  match declist with
  | [] -> None
  | (ident, typ)::l -> if ident = id then Some typ else find_type id l

(* Transform an assert into a pre/post condition *)
let handle_assert node asser =
  let id =
    try
      Utils.find_ident_in_pexpr asser
    with Two_ident (id1, id2) -> raise (Assert_id_error id1) (* A CHANGER *)
  in
  match find_type id node.p_param_in with
  | Some typ -> Pre (id, p_type_to_n_type typ, p_expr_to_n_expr asser)
  | None -> match find_type id node.p_param_out with
    | Some typ -> Post (id, p_type_to_n_type typ, p_expr_to_n_expr asser)
    | None -> raise (Assert_id_error id)


(* MODIFIER PAR UN SCAN_FOR_TUPLE_IN_EXPR
   faire le scan après passage du parseur, pour vérifier que les équations sont toutes atomiques.
   Seuls les tuples à gauche sont autorisés pour les retours de fonctions.
   
 *)

let rec get_atomic_reg lp_list ini_list expr_list node =
  match lp_list, ini_list, expr_list with
  | [],[],[] -> []
  | lp_id::l1, ini::l2, e::l3 ->
    let id = match e with
	PE_Ident id -> id
      | _ -> raise Register_error
    in
    let typ = match find_type id (node.p_param_in@node.p_param_out@node.p_vars) with
      | Some typ -> typ
      | None -> raise Register_error
    in
    N_Registre { n_reg_lpid = lp_id;
		 n_reg_ini = p_expr_to_n_expr ini;
		 n_reg_type = p_type_to_n_type typ;
		 n_reg_val = (NE_Ident id);
	       }::(get_atomic_reg l1 l2 l3 node)
  | _ -> raise Register_error

let handle_reg node = function
  | P_Eq (PLP_Ident lp_id, PE_Fby (ini, PE_Pre pre_expr)) ->
    let id = (match pre_expr with
	PE_Ident id -> id
      | _ -> raise Register_error
    ) in
    let typ = match find_type id (node.p_param_in@node.p_param_out@node.p_vars) with
      | Some typ -> typ
      | None -> raise Register_error
    in
    [ N_Registre { n_reg_lpid = lp_id;
		   n_reg_ini = p_expr_to_n_expr ini;
		   n_reg_type = p_type_to_n_type typ;
		   n_reg_val = (NE_Ident id);
		 } ]
  | P_Eq (PLP_Tuple id_list, PE_Fby (PE_Tuple ini_list, PE_Pre (PE_Tuple pre_expr_list))) ->
    get_atomic_reg id_list ini_list pre_expr_list node
  | _ -> raise Register_error

(* DEPRECATED *)
(* let handle_reg node = function *)
(*   | P_Eq (lp, PE_Fby (ini, PE_Pre (PE_Ident id))) -> *)
(*     let typ = match find_type id (node.p_param_in@node.p_param_out@node.p_vars) with *)
(*       | Some typ -> typ *)
(*       | None -> raise Register_error *)
(*     in *)
(*     N_Registre { n_reg_lp = plp_to_nlp lp; *)
(* 		 n_reg_ini = p_expr_to_n_expr ini; *)
(* 		 n_reg_type = p_type_to_n_type typ; *)
(* 		 n_reg_val = (NE_Ident id); *)
(* 	       }  *)
(*   | _ -> raise Register_error *)

let handle_alt = function
  | P_Eq (lp, PE_If (c, e1, e2)) ->
    N_Alternative { n_alt_lp = plp_to_nlp lp;
		    n_alt_cond = p_expr_to_n_expr c;
		    n_alt_then = p_expr_to_n_expr e1;
		    n_alt_else = p_expr_to_n_expr e2;
		  }
  | _ -> assert false

let handle_app = function
  | P_Eq (lp, PE_App (id_app, elist)) ->
    N_Fonction { n_fun_lp = plp_to_nlp lp;
		 n_fun_id = id_app;
		 n_fun_params = List.map p_expr_to_n_expr elist;
	       }
  | _ -> assert false

let handle_op = function
  | P_Eq (lp, expr) ->
    N_Operation { n_op_lp = plp_to_nlp lp;
		  n_op_expr =  p_expr_to_n_expr expr;
		}
  | _ -> assert false

let get_env vars pre post =
  let vars_cond = List.map (fun (id, t) -> (id, t, None)) vars in
  let inputs_cond = List.map (fun (id, t, cond) -> (id, t, Some cond)) pre in
  let outputs_cond = List.map (fun (id, t, cond) -> (id, t, Some cond)) post in  
  Utils.make_n_env (inputs_cond@outputs_cond@vars_cond)

(* VERIFIER QU'IL Y A AUTANT DE ASSUME QUE DE INPUTS (pareil pour outputs-guarantee) *)

let normalize_node node =
  let pre = ref [] in
  let post = ref [] in
  let normalize_eq res = function
    | P_Eq (lp, expr) as eq -> (
      match expr with
      | PE_Fby _ -> (handle_reg node eq)@res
      | PE_If _ -> (handle_alt eq)::res
      | PE_App _ -> (handle_app eq)::res
      | _ -> (handle_op eq)::res
    )
    | P_Assert expr ->
      match (handle_assert node expr) with
      | Post (id, typ, e) -> post := (id, typ, e):: !post; res
      | Pre (id, typ, e) -> pre := (id, typ, e):: !pre; res
  in
  let eqs = List.fold_left normalize_eq [] node.p_eqs in
  let inputs = p_decl_to_n_decl node.p_param_in in
  let outputs = p_decl_to_n_decl node.p_param_out in
  let vars = p_decl_to_n_decl node.p_vars in
  let scheduled_eqs =
    let (id_inputs, _) = List.split inputs in
    Scheduler.scheduler eqs (id_inputs)
  in
  let scheduled_eqs = scheduled_eqs in
  let env = get_env vars !pre !post in
  { n_id = node.p_id;
    n_env = env;
    n_param_in = inputs;
    n_param_out = outputs;
    n_vars = vars;
    n_pre = !pre;
    n_post = !post;
    n_eqs = scheduled_eqs; }

let normalize prog =
  { n_node = normalize_node prog.p_node;
    n_includes = prog.p_includes;
  }
