(* Florian Thibord  --  Projet CERCLES *)

open Ast_repr_b
open Ast_repr_norm
open Utils




(*
  TODO : 

  ecrire translator (passer de ast_n a ast_b
  retrieve_vars
  bimpl_translator
  bsig_printer
*)

let id_to_bid env id =
  let bid = Env.find id env in bid

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
  | NA_Slice (id, e_list) -> BA_Slice 
    (id_to_bid env id, (List.map (fun (e1, e2) -> (n_expr_to_b_expr env e1, n_expr_to_b_expr env e2)) e_list))
  | NA_Index (id, e_list) -> BA_Index (id_to_bid env id, (List.map (n_expr_to_b_expr env) e_list))

let rec n_type_to_b_type env = function
  | NT_Base b -> BT_Base b
  | NT_Array (t,e) -> BT_Array (n_type_to_b_type env t, n_expr_to_b_expr env e)

let nlp_to_blp env = function
  | NLP_Ident id -> BLP_Ident (id_to_bid env id)
  | NLP_Tuple id_list -> BLP_Tuple (List.map (id_to_bid env) id_list)

let n_decl_to_decl env (id, _) =
  id_to_bid env id

let n_condition_to_condition env (id, t, e) =
  (id_to_bid env id, n_type_to_b_type env t, n_expr_to_b_expr env e)

let rec trad_list env to_call = function
  | [] -> []
  | elt::l -> (to_call env elt)::(trad_list env to_call l)

let get_concrete_vars env reg =
  id_to_bid env reg.n_reg_lpid

let get_invariant env reg =
  (id_to_bid env reg.n_reg_lpid, n_type_to_b_type env reg.n_reg_type)

let get_initialisation env reg =
  (id_to_bid env reg.n_reg_lpid, n_expr_to_b_expr env reg.n_reg_ini)

let bimpl_translator env node =
  let implem_name = String.capitalize (node.n_id ^ "_i") in
  let refines = String.capitalize (node.n_id) in
  let sees = Utils.sees_list in
  let imports = Utils.imports_list in
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
  let (eqs, regs) = List.partition (fun eq -> match eq with N_Registre _ -> false | _ -> true) node.n_eqs in
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
  { machine = machine;
    sig_sees = sees;
    sigop_decl = sigop_decl;
    sigop_pre = sigop_pre;
    sigop_post = sigop_post;
  }

let translate node =
  let env = Utils.make_env (N_Env.elements node.n_env) in
  let bsig = bsig_translator env node in
  let bimpl = bimpl_translator env node in
  { env = env;
    signature = bsig;
    implementation = bimpl;
  }
