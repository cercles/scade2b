(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == trad.ml                                                               == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

open Ast_base
open Ast_repr_b
open Ast_scade_norm
open Utils


(******************** ast_repr_norm to ast_repr_b functions ********************)

let id_to_bid env id =
  try
    let bid, _, _ = Env.find id env in bid
  with Not_found -> 
    let msg = Printf.sprintf 
      "\nWARNING (trad.id_to_bid): Identifier not found: '%s'" id;
    in
    output_string stderr msg;
    id 

let rec n_expr_to_b_expr env = function
  | NE_Ident id ->  BE_Ident (id_to_bid env id)
  | NE_Value v -> BE_Value v
  | NE_Array ar -> BE_Array (n_array_to_b_array env ar)
  | NE_Op_Arith1 (op, e) -> BE_Op_Arith1 (op, n_expr_to_b_expr env e)
  | NE_Op_Arith2 (op, e1, e2) -> BE_Op_Arith2 (op, n_expr_to_b_expr env e1, n_expr_to_b_expr env e2)
  | ( NE_Op_Logic _
    | NE_Op_Not _
    | NE_Op_Relat _
    ) as e ->
        BE_Pred (n_expr_to_b_pred env e)

and n_expr_to_b_pred env = function
  | NE_Op_Not e -> BP_Not (n_expr_to_b_pred env e)
  | NE_Op_Logic (op, e1, e2) -> BP_Op_Logic (op, n_expr_to_b_pred env e1, n_expr_to_b_pred env e2)
  | NE_Op_Relat (op, e1, e2) -> BP_Op_Relat (op, n_expr_to_b_expr env e1, n_expr_to_b_expr env e2)
  | ( NE_Ident _
    | NE_Value _
    | NE_Array _
    | NE_Op_Arith1 _
    | NE_Op_Arith2 _
    ) as e ->
        BP_Expr (n_expr_to_b_expr env e)

and n_array_to_b_array env = function
  | NA_Def e_list -> BA_Def (List.map (n_expr_to_b_expr env) e_list)
  | NA_Caret (e1, NE_Value (Int i)) -> n_array_to_b_array env (Utils.caret_to_def e1 i)
  | NA_Caret (e1, e2) -> BA_Caret (n_expr_to_b_expr env e1, n_expr_to_b_expr env e2)
  | NA_Concat (e1, e2) -> BA_Concat (n_expr_to_b_expr env e1, n_expr_to_b_expr env e2)
  | NA_Slice (id, e_list) -> 
      BA_Slice (id_to_bid env id, 
		(List.map (fun (e1, e2) -> 
			     (n_expr_to_b_expr env e1, n_expr_to_b_expr env e2)) e_list))
  | NA_Index (id, e_list) -> 
      BA_Index (id_to_bid env id, (List.map (n_expr_to_b_expr env) e_list))
  | NA_Reverse id -> BA_Reverse (id_to_bid env id)

let nlp_to_blp env = function
  | NLP_Ident id -> BLP_Ident (id_to_bid env id)
  | NLP_Tuple id_list -> BLP_Tuple (List.map (id_to_bid env) id_list)

let n_decl_to_decl env (id, _) =
  id_to_bid env id


(***************** Traduction des conditions *****************)

(* flatten a NT_Array into a n_expr list (a list of dimensions) *)
let flatten_array env a =
  let rec fun_rec_expr = function
    | NT_Base t -> []
    | NT_Array (t, expr) -> (n_expr_to_b_expr env expr) :: (fun_rec_expr t)
  in
  let rec fun_rec_type = function
    | NT_Base t -> t
    | NT_Array (t, expr) -> fun_rec_type t
  in
  (fun_rec_type a, Some (fun_rec_expr a))

let n_condition_to_condition env (id, t, e) =
  match t with 
    | NT_Base typ -> (
	match e with 
	  | None -> (id_to_bid env id, (typ, None), None)
	  | Some expr -> (id_to_bid env id, (typ, None), Some (n_expr_to_b_expr env expr))
    )
    | NT_Array (_, _) -> (
	let typ, dims = flatten_array env t in
	let index_tab_ident = Env_builder.make_b_ident "jj_index" env in
	let subst_id = Utils.make_subst_id index_tab_ident dims id in
	match e with 
	  | None -> (id_to_bid env id, (typ, dims), None)
	  | Some expr -> 
	    let env2 = Env.add id (subst_id, t, None) env in
	    (id_to_bid env id, (typ, dims), Some (n_expr_to_b_expr env2 expr))
    )


(******************** Traduction des registres ********************)

let n_type_to_set env t =
 match t with 
    | NT_Base typ -> (typ, None)
    | NT_Array _ -> flatten_array env t

let get_concrete_vars env reg = 
  id_to_bid env reg.n_reg_lpid

let get_invariant env node reg =
  (id_to_bid env reg.n_reg_lpid, n_type_to_set env reg.n_reg_type, None)
  
let get_initialisation env reg =
  (id_to_bid env reg.n_reg_lpid, n_expr_to_b_expr env reg.n_reg_ini)


(******************** DIVERS ********************)

let rec trad_list env to_call = function
  | [] -> []
  | elt::l -> (to_call env elt)::(trad_list env to_call l)


(********** FONCTION DE TRANSFORMATION **********)

let translate node sees_cond =
  let env = node.n_env in
  let name = node.n_id in
  let m_params, params_cond =
    List.split (List.map (fun lambda -> id_to_bid env lambda.n_l_ident, lambda.n_l_cond) node.n_lambdas) in
  let m_see_const, m_see_enum = sees_cond in
  let m_constraints = trad_list env n_condition_to_condition params_cond in
  let op_in_params = trad_list env n_decl_to_decl node.n_param_in in
  let op_out_params = trad_list env n_decl_to_decl node.n_param_out in
  let abs_pre_condition = trad_list env n_condition_to_condition node.n_pre in
  let abs_post_condition = trad_list env n_condition_to_condition node.n_post in
  let concrete_vars = ref [] in
  let invariant = ref [] in
  let initialisation = ref [] in
  let translate_eqs env eqs =
    let translator = function
      | N_Alternative a ->
	Alt { alt_lp = nlp_to_blp env a.n_alt_lp;
	      alt_cond = n_expr_to_b_expr env a.n_alt_cond;
	      alt_then = n_expr_to_b_expr env a.n_alt_then;
	      alt_else = n_expr_to_b_expr env a.n_alt_else;
    	    }
      | N_Call f ->
	Call { call_lp = nlp_to_blp env f.n_fun_lp;
	       call_id = f.n_fun_id;
	       call_params = List.map (n_expr_to_b_expr env) f.n_fun_params;
	       call_instance = f.n_fun_pragma;
	     }
      | N_Operation o ->
	Simpl { simpl_lp = nlp_to_blp env o.n_op_lp;
		simpl_expr = n_expr_to_b_expr env o.n_op_expr;
	      }
      | _ -> assert false
    in
    List.map translator eqs
  in
  let translate_regs env regs =
    let translator = function
      | N_Registre r ->
	concrete_vars := (get_concrete_vars env r):: !concrete_vars;
	invariant := (get_invariant env node r):: !invariant;
	initialisation := (get_initialisation env r):: !initialisation;
	Simpl { simpl_lp = nlp_to_blp env (NLP_Ident r.n_reg_lpid);
		simpl_expr = n_expr_to_b_expr env r.n_reg_val;
	      }
      | _ -> assert false
    in
    List.map translator regs
  in
  let (eqs, regs) = List.partition
    (fun eq -> match eq with N_Registre _ -> false | _ -> true) node.n_eqs in
  let op_1 = translate_eqs env eqs in
  let op_2 = translate_regs env regs in
  let vars = trad_list env n_decl_to_decl node.n_vars in
  let vars_without_regs =
    List.filter (fun id -> not(List.mem id !concrete_vars)) vars in
  let imp_substitutions = op_1 @ op_2 in

  { name = name;
    m_params = m_params;
    m_see_const = m_see_const;  
    m_see_enum = m_see_enum;
    m_imports = [];
    m_constraints = m_constraints;
    m_concrete_vars = !concrete_vars;
    m_invariant = !invariant;
    m_initialisation = !initialisation;
    op_in_params = op_in_params;
    op_out_params = op_out_params;
    m_vars = vars_without_regs;
    abs_pre_condition = abs_pre_condition;
    abs_post_condition = abs_post_condition;
    imp_substitutions = imp_substitutions;
  }

