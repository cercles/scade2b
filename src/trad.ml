(* Florian Thibord  --  Projet CERCLES *)

open Ast_base
open Ast_repr_b
open Ast_repr_norm
open Utils


(* LES CONST SONT A AJOUTER A L'ENV !!! *)

let id_to_bid env id =
try
  let bid, _ = Env.find id env in bid
with Not_found -> id

let rec n_expr_to_b_expr env = function
  | NE_Ident id ->  BE_Ident (id_to_bid env id)
  | NE_Tuple e_list -> BE_Tuple (List.map (n_expr_to_b_expr env) e_list)
  | NE_Value v -> BE_Value v
  | NE_Array ar -> BE_Array (n_array_to_b_array env ar)
  | NE_Bop (bop, e1, e2) -> BE_Bop (bop, n_expr_to_b_expr env e1, n_expr_to_b_expr env e2)
  | NE_Unop (unop, e) -> BE_Unop (unop, n_expr_to_b_expr env e)
  | NE_Sharp e_list -> BE_Sharp (List.map (n_expr_to_b_expr env) e_list)

and n_array_to_b_array env = function
  | NA_Def e_list -> BA_Def (List.map (n_expr_to_b_expr env) e_list)
  | NA_Caret (e1, e2) -> BA_Caret (n_expr_to_b_expr env e1, n_expr_to_b_expr env e2)
  | NA_Concat (e1, e2) -> BA_Concat (n_expr_to_b_expr env e1, n_expr_to_b_expr env e2)
  | NA_Slice (id, e_list) -> 
    BA_Slice (id_to_bid env id, 
	      (List.map (fun (e1, e2) -> 
		(n_expr_to_b_expr env e1, n_expr_to_b_expr env e2)) e_list))
  | NA_Index (id, e_list) -> 
    BA_Index (id_to_bid env id, (List.map (n_expr_to_b_expr env) e_list))

(* flatten a NT_Array into a n_expr list (a list of dimensions) *)
let flatten_array env a =
  let base_t = ref T_Int in (* default ref *)
  let rec fun_rec = function
  | NT_Base t -> base_t := t; []
  | NT_Array (t, expr) -> (n_expr_to_b_expr env expr) :: (fun_rec t)
  in
  (!base_t, fun_rec a)

let nlp_to_blp env = function
  | NLP_Ident id -> BLP_Ident (id_to_bid env id)
  | NLP_Tuple id_list -> BLP_Tuple (List.map (id_to_bid env) id_list)

let n_decl_to_decl env (id, _) =
  id_to_bid env id

let n_condition_to_condition env (id, t, e) =
  match t with 
  | NT_Base typ ->  (
      match e with 
	| None -> Base_no_expr (id_to_bid env id, typ)
	| Some expr -> Base_expr (id_to_bid env id, typ, n_expr_to_b_expr env expr))
  | NT_Array (_, _) -> (
    let typ, dims = flatten_array env t in
    match e with 
      | None -> Fun_no_expr (id_to_bid env id, typ, dims)
      | Some expr -> Fun_expr (id_to_bid env id, typ, dims, n_expr_to_b_expr env expr))


let rec trad_list env to_call = function
  | [] -> []
  | elt::l -> (to_call env elt)::(trad_list env to_call l)

let get_concrete_vars env reg =
  id_to_bid env reg.n_reg_lpid


let rec rename_id_expr old ident = function
  | NE_Ident i -> if i = old then NE_Ident ident else NE_Ident i
  | NE_Tuple e_list -> NE_Tuple (List.map (rename_id_expr old ident) e_list)
  | NE_Value v -> NE_Value v
  | NE_Array ar -> NE_Array (rename_id_array old ident ar)
  | NE_Bop (bop, e1, e2) -> NE_Bop (bop, rename_id_expr old ident e1, rename_id_expr old ident e2)
  | NE_Unop (unop, e) -> NE_Unop (unop, rename_id_expr old ident e)
  | NE_Sharp e_list -> NE_Sharp (List.map (rename_id_expr old ident) e_list)

and rename_id_array old ident = function
  | NA_Def e_list -> NA_Def (List.map (rename_id_expr old ident) e_list)
  | NA_Caret (e1, e2) -> NA_Caret (rename_id_expr old ident e1, rename_id_expr old ident e2)
  | NA_Concat (e1, e2) -> NA_Concat (rename_id_expr old ident e1, rename_id_expr old ident e2)
  | NA_Slice (i, e_list) -> 
    NA_Slice ((if i = old then ident else i), 
	      (List.map (fun (e1, e2) ->
		(rename_id_expr old ident e1, rename_id_expr old ident e2)) e_list))
  | NA_Index (i, e_list) -> 
    NA_Index ((if i = old then ident else i), 
	      (List.map (rename_id_expr old ident) e_list))

let retrieve_cond_expr env reg =
  let id_val = match reg.n_reg_val with
    | NE_Ident i -> i
    | _ -> assert false
  in
  let cond_expr = 
    match Env.find id_val env with
      | _, Some c -> Some (rename_id_expr id_val reg.n_reg_lpid c)
      | _, None -> failwith "Register not related to input/output"
  in
  cond_expr 

let get_invariant env reg =
  n_condition_to_condition env (reg.n_reg_lpid, reg.n_reg_type, retrieve_cond_expr env reg) 
  
let get_initialisation env reg =
  (id_to_bid env reg.n_reg_lpid, n_expr_to_b_expr env reg.n_reg_ini)


let bimpl_translator env node includes =
  let implem_name = String.capitalize (node.n_id ^ "_i") in
  let refines = String.capitalize (node.n_id) in
  let sees = Utils.sees_list in
  let imports = List.map String.capitalize includes in
  let concrete_vars = ref [] in
  let invariant = ref [] in
  let initialisation = ref [] in
  let translate_eqs env eqs =
    let translator = function
      | N_Alternative a ->
	Alternative { alt_lp = nlp_to_blp env a.n_alt_lp;
		      alt_cond = n_expr_to_b_expr env a.n_alt_cond;
		      alt_then = n_expr_to_b_expr env a.n_alt_then;
		      alt_else = n_expr_to_b_expr env a.n_alt_else;
    		    }
      | N_Fonction f ->
	Fonction { fun_lp = nlp_to_blp env f.n_fun_lp;
		   fun_id = f.n_fun_id;
		   fun_params = List.map (n_expr_to_b_expr env) f.n_fun_params;
		 }
      | N_Operation o ->
	Operation { op_lp = nlp_to_blp env o.n_op_lp;
		    op_expr = n_expr_to_b_expr env o.n_op_expr;
		  }
      | _ -> assert false
    in
    List.map translator eqs
  in
  let translate_regs env regs =
    let translator = function
      | N_Registre r ->
	concrete_vars := (get_concrete_vars env r):: !concrete_vars;
	invariant := (get_invariant env r):: !invariant;
	initialisation := (get_initialisation env r):: !initialisation;
	{ reg_lpid = id_to_bid env r.n_reg_lpid;
	  reg_val =  n_expr_to_b_expr env r.n_reg_val;
	}
      | _ -> assert false
    in
    List.map translator regs
  in
  let op_decl = { id = node.n_id;
		  param_in = trad_list env n_decl_to_decl node.n_param_in;
		  param_out = trad_list env n_decl_to_decl node.n_param_out;
		} in
  let (eqs, regs) = List.partition
    (fun eq -> match eq with N_Registre _ -> false | _ -> true) node.n_eqs in
  let op_1 = translate_eqs env eqs in
  let op_2 = translate_regs env regs in
  let reg_ids = !concrete_vars in
  let vars = trad_list env n_decl_to_decl node.n_vars in
  let vars_without_regs =
    List.filter (fun id -> not(List.mem id reg_ids)) vars in
  let operations = { op_decl = op_decl;
		     vars = vars_without_regs;
		     op_1 = op_1;
		     op_2 = op_2;
		   } in
  { name = implem_name;
    refines = refines;
    sees = sees;
    imports = imports;
    concrete_variables = !concrete_vars;
    invariant = !invariant;
    initialisation = !initialisation;
    operations = operations;
  }

let bsig_translator env node =
  let machine = String.capitalize node.n_id in
  let sees = Utils.sees_list in
  let sigop_decl = { id = node.n_id;
		     param_in = trad_list env n_decl_to_decl node.n_param_in;
		     param_out = trad_list env n_decl_to_decl node.n_param_out;
		   } in 
  let sigop_pre = trad_list env n_condition_to_condition node.n_pre in
  let sigop_post = trad_list env n_condition_to_condition node.n_post in
  let sig_operation = { sigop_decl = sigop_decl;
			sigop_pre = sigop_pre;
			sigop_post = sigop_post;
		      } in
  { machine = machine;
    sig_sees = sees;
    sig_operation = sig_operation;
  }

let translate prog =
  let node = prog.n_node in
  let env = Utils.make_env (N_Env.elements node.n_env) in
  let bsig = bsig_translator env node in
  let bimpl = bimpl_translator env node prog.n_includes in
  { env = env;
    signature = bsig;
    implementation = bimpl;
  }
