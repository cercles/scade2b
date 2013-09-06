(* Florian Thibord  --  Projet CERCLES *)

open Ast_base
open Ast_repr
open Ast_repr_norm
open Utils

exception Assert_id_error of string
exception Register_error
exception Non_Atomic of string
exception Normalisation_Error of string

(* ast_repr to ast_repr_norm functions *)
let rec p_expr_to_n_expr = function
  | PE_Ident iden -> NE_Ident iden
  | PE_Value v -> NE_Value v
  | PE_Array array -> NE_Array (p_array_to_n_array array)
  | PE_Bop (bop, e1, e2) -> NE_Bop (bop, p_expr_to_n_expr e1, p_expr_to_n_expr e2)
  | PE_Unop (unop, e) -> NE_Unop (unop, p_expr_to_n_expr e)
  | PE_Sharp elist -> NE_Sharp (List.map p_expr_to_n_expr elist)
  | _ -> raise (Normalisation_Error "Une equation n'est pas atomique") 
               (* ne devrait jamais arriver d'après la grammaire Scade *)

and p_array_to_n_array = function
  | PA_Def elist -> NA_Def (List.map p_expr_to_n_expr elist)
  | PA_Caret (e1, e2) -> NA_Caret (p_expr_to_n_expr e1, p_expr_to_n_expr e2)
  | PA_Concat (e1, e2) -> NA_Concat (p_expr_to_n_expr e1, p_expr_to_n_expr e2)
  | PA_Slice (id, l) -> 
     NA_Slice (id, (List.map (fun (e1, e2) -> p_expr_to_n_expr e1, p_expr_to_n_expr e2) l))
  | PA_Index (id, l) -> NA_Index (id, List.map p_expr_to_n_expr l)

let plp_to_nlp = function
  | PLP_Ident id -> NLP_Ident id
  | PLP_Tuple ids -> NLP_Tuple ids

let rec p_type_to_n_type = function
  | PT_Base b -> NT_Base b
  | PT_Array (t, e) -> NT_Array (p_type_to_n_type t, p_expr_to_n_expr e)

let p_decl_to_n_decl declist =
  List.map (fun (id, p_type) -> (id, p_type_to_n_type p_type)) declist


(* Find the type related to a variable *)
let rec find_type id declist =
  match declist with
  | [] -> None
  | (ident, typ)::l -> if ident = id then Some typ else find_type id l

(* Cherche l'entrée liée à la condition expr, et retourne la condition normalisée *)
let handle_assume node expr =
  let id =
    try
      Utils.find_ident_in_pexpr expr
    with Two_ident (id1, id2) -> raise (Assert_id_error id1)
  in
  match find_type id node.p_param_in with
  | Some typ -> (id, p_type_to_n_type typ, Some (p_expr_to_n_expr expr))
  | None ->  raise (Assert_id_error id)

(* Idem pour les sorties *)
let handle_guarantee node expr =
  let id =
    try
      Utils.find_ident_in_pexpr expr
    with Two_ident (id1, id2) -> raise (Assert_id_error id1) 
  in
  match find_type id node.p_param_out with
  | Some typ -> (id, p_type_to_n_type typ, Some (p_expr_to_n_expr expr))
  | None ->  raise (Assert_id_error id)


(* Retourne un registre normalisé *) 
let handle_reg node = function
  | PLP_Ident lp_id, PE_Fby (PE_Ident id, delai, ini) ->
    let typ = match find_type lp_id (node.p_param_in@node.p_param_out@node.p_vars) with
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
let handle_app = function
  | lp, PE_App (id_app, elist) ->
    N_Fonction { n_fun_lp = plp_to_nlp lp;
		 n_fun_id = id_app;
		 n_fun_params = List.map p_expr_to_n_expr elist;
	       }
  | _ -> assert false

(* Retourne une opération de base normalisée *)
let handle_op = function
  | lp, expr ->
    N_Operation { n_op_lp = plp_to_nlp lp;
		  n_op_expr =  p_expr_to_n_expr expr;
		}


(* Fonction principale de normalisation *)
let normalize_node node =
  let normalize_eq res = function
    | lp, expr as eq -> 
      begin
	match expr with
	| PE_Fby _ -> (handle_reg node eq) :: res
	| PE_If _ -> (handle_alt eq) :: res
	| PE_App _ -> (handle_app eq) :: res
	| _ -> (handle_op eq) :: res
      end
  in 
  (* Normalisation des équations *)
  let eqs = List.fold_left normalize_eq [] node.p_eqs in
  (* Normalisation des déclarations *)
  let inputs = p_decl_to_n_decl node.p_param_in in
  let outputs = p_decl_to_n_decl node.p_param_out in
  let vars = p_decl_to_n_decl node.p_vars in
  (* Ordonnancement des équations *)
  let scheduled_eqs =
    let (id_inputs, _) = List.split inputs in
    Scheduler.scheduler eqs (id_inputs)
  in
  (* Normalisation des conditions *)
  let assumes = List.map (handle_assume node) node.p_assumes in
  let guarantees = List.map (handle_guarantee node) node.p_guarantees in
  (* Si une entrée/sortie n'a pas de condition, on ajoute une condition vide *)
  let add_non_existing_cond cond_list decl_list =
    List.fold_left (fun cond_l (id, t) -> 
      if (List.exists (fun (id_bis, _, _) -> id = id_bis) cond_l)
      then cond_l else (id, t, None) :: cond_l) cond_list decl_list
  in
  let vars_cond = List.map (fun (id, t) -> (id, t, None)) vars in
  let assumes = add_non_existing_cond assumes inputs in
  let guarantees = add_non_existing_cond guarantees outputs in  
  (* Construction de l'environnement *)
  let env = Utils.make_n_env (assumes@guarantees@vars_cond) in
  (* Noeud normalisé *)
  { n_id = String.lowercase node.p_id;
    n_env = env;
    n_param_in = inputs;
    n_param_out = outputs;
    n_vars = vars;
    n_pre = assumes; 
    n_post = guarantees;
    n_eqs = scheduled_eqs; }


(* Retourne le noeud principal normalisé *)
let normalize prog main =
  let main_node =
    try 
      List.find (fun node -> node.p_id = main) prog
    with Not_found -> assert false
  in
  normalize_node main_node

