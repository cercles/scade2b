(* Florian Thibord  --  Projet CERCLES *)


open Ast_base
open Ast_scade
open Ast_kcg
open Ast_prog
open Ast_scade_norm
open Utils


exception Assert_id_error of string
exception Register_error

(******************** ast_repr to ast_repr_norm functions ********************)

let rec p_expr_to_n_expr = function
  | PE_Ident iden -> NE_Ident iden
  | PE_Value v -> NE_Value v
  | PE_Array array -> NE_Array (p_array_to_n_array array)
  | PE_Op_Arith1 (op, e) -> NE_Op_Arith1 (op, p_expr_to_n_expr e)
  | PE_Op_Arith2 (op, e1, e2) -> NE_Op_Arith2 (op, p_expr_to_n_expr e1, p_expr_to_n_expr e2)
  | PE_Op_Relat (op, e1, e2) -> NE_Op_Relat (op, p_expr_to_n_expr e1, p_expr_to_n_expr e2)
  | PE_Op_Logic (op, e1, e2) -> NE_Op_Logic (op, p_expr_to_n_expr e1, p_expr_to_n_expr e2)
  | PE_Op_Sharp e_list -> NE_Op_Sharp (List.map p_expr_to_n_expr e_list)
  | PE_Op_Not e -> NE_Op_Not (p_expr_to_n_expr e)
  | _ -> assert false

and p_array_to_n_array = function
  | PA_Def e_list -> NA_Def (List.map p_expr_to_n_expr e_list)
  | PA_Caret (e1, e2) -> NA_Caret (p_expr_to_n_expr e1, p_expr_to_n_expr e2)
  | PA_Concat (e1, e2) -> NA_Concat (p_expr_to_n_expr e1, p_expr_to_n_expr e2)
  | PA_Slice (id, l) -> 
     NA_Slice (id, (List.map (fun (e1, e2) -> p_expr_to_n_expr e1, p_expr_to_n_expr e2) l))
  | PA_Index (id, l) -> NA_Index (id, List.map p_expr_to_n_expr l)
  | PA_Reverse id -> NA_Reverse id

let plp_to_nlp = function
  | PLP_Ident id -> NLP_Ident id
  | PLP_Tuple ids -> NLP_Tuple ids

let rec p_type_to_n_type = function
  | PT_Base b -> NT_Base b
  | PT_Array (t, e) -> NT_Array (p_type_to_n_type t, p_expr_to_n_expr e)

let p_decl_to_n_decl declist =
  List.map (fun (id, p_type) -> (id, p_type_to_n_type p_type)) declist




(* Cherche l'entrée liée à la condition expr, trouve son type au passage 
   et retourne la condition normalisée *)
let handle_assume params_in consts expr =
  let id =
    try
      Utils.find_ident_in_pexpr expr consts
    with Two_ident_used_in_condition (id1, id2) -> raise (Assert_id_error id1)
  in
  match Utils.find_type id params_in with
  | Some typ -> (id, p_type_to_n_type typ, Some (p_expr_to_n_expr expr))
  | None -> raise (Assert_id_error id)

(* Idem pour les sorties *)
let handle_guarantee params_out consts expr =
  let id =
    try
      Utils.find_ident_in_pexpr expr consts
    with Two_ident_used_in_condition (id1, id2) -> raise (Assert_id_error id1) 
  in
  match Utils.find_type id params_out with
  | Some typ -> (id, p_type_to_n_type typ, Some (p_expr_to_n_expr expr))
  | None ->  raise (Assert_id_error id)

(* Retourne un registre normalisé, trouve son type au passage *) 
let handle_reg node = function
  | PLP_Ident lp_id, PE_Fby (PE_Ident id, delai, ini) ->
    let typ = match Utils.find_type lp_id (node.p_param_in@node.p_param_out@node.p_vars) with
      | Some typ -> typ
      | None -> raise Register_error
    in
    N_Registre { n_reg_lpid = lp_id;
		 n_reg_ini = p_expr_to_n_expr ini;
		 n_reg_delai = p_expr_to_n_expr delai;
		 n_reg_val = (NE_Ident id);
		 n_reg_type = p_type_to_n_type typ;
	       } 
  | _ -> raise Register_error

(* Retourne une alternative normalisée *)
let handle_alt = function
  | lp, PE_If (c, e1, e2) ->
    N_Alternative { n_alt_lp = plp_to_nlp lp;
		    n_alt_cond = p_expr_to_n_expr c;
		    n_alt_then = p_expr_to_n_expr e1;
		    n_alt_else = p_expr_to_n_expr e2;
		  }
  | _ -> assert false

(* Retourne un appel normalisé *)
let handle_call env = function
  | lp, PE_Call (pragma, id_call, elist) ->
    N_Call { n_fun_lp = plp_to_nlp lp;
	     n_fun_id = id_call;
	     n_fun_params = List.map p_expr_to_n_expr elist;
	     n_fun_pragma = pragma;
	   }
  | _ -> assert false

(* Retourne une opération de base normalisée *)
let handle_op = function
  | lp, expr ->
    N_Operation { n_op_lp = plp_to_nlp lp;
		  n_op_expr =  p_expr_to_n_expr expr;
		}

(* Supprime les équations contenant un terminator en lhs *)
let remove_terminator eq_list =
  let remover acc = function
    | lp, _ as eq -> (
      match lp with
      | PLP_Ident id when id = "_"-> acc
      | _ -> eq::acc
    )
  in
  List.fold_left remover [] eq_list



(* Fonction principale de normalisation *)
let normalize_node node prog =
  (* Normalisation des déclarations *)
  let inputs = p_decl_to_n_decl node.p_param_in in
  let outputs = p_decl_to_n_decl node.p_param_out in
  let vars = p_decl_to_n_decl node.p_vars in
  (* Normalisation des conditions *)
  let consts = List.map (fun c -> c.c_id) prog.consts in
  let enum_elts = List.concat (List.map (fun e -> e.p_enum_list) prog.enum_types) in
  let assumes = List.map (handle_assume node.p_param_in (consts@enum_elts)) node.p_assumes in
  let guarantees = List.map (handle_guarantee node.p_param_out (consts@enum_elts)) node.p_guarantees in
  (* Construction de l'environnement *)
  (* Si une entrée/sortie n'a pas de condition, on ajoute une condition vide *)
  let add_non_existing_cond cond_list decl_list =
    List.fold_left (fun cond_l (id, t) ->
      if (List.exists (fun (id_bis, _, _) -> id = id_bis) cond_l)
      then cond_l else (id, t, None) :: cond_l) cond_list decl_list
  in
  let assumes = add_non_existing_cond assumes inputs in
  let guarantees = add_non_existing_cond guarantees outputs in
  let env = Env_builder.make_env vars assumes guarantees ([node.p_id]@consts@enum_elts) in
  (* Normalisation des équations *)
  let eq_list = remove_terminator node.p_eqs in
  let normalize_eq res = function
    | lp, expr as eq ->
      begin
	match expr with
	| PE_Fby _ -> (handle_reg node eq) :: res
	| PE_If _ -> (handle_alt eq) :: res
	| PE_Call _ -> (handle_call env eq) :: res
	| _ -> (handle_op eq) :: res
      end
  in
  let eqs = List.fold_left normalize_eq [] eq_list in
  (* Noeud normalisé *)
  { n_id = node.p_id; 
    n_lambdas = [];
    n_env = env;
    n_param_in = inputs;
    n_param_out = outputs;
    n_vars = vars;
    n_pre = assumes; 
    n_post = guarantees;
    n_eqs = eqs; }
