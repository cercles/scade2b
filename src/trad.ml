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
  let (_,_,bid) = (Env.find id env) in bid

let rec n_expr_to_b_expr env = function
  | NE_Ident id ->  BE_Ident (id_to_bid env id)
  | NE_Tuple e_list -> BE_Tuple (List.map n_expr_to_b_expr e_list)
  | NE_Value v -> BE_Value v
  | NE_Array ar -> BE_Array (n_array_to_b_array ar)
  | NE_Bop (bop, e1, e2) -> BE_Bop (bop, n_expr_to_b_expr e1, n_expr_to_b_expr e2)
  | NE_Unop (unop, e) -> BE_Unop (unop, n_expr_to_b_expr e)
  | NE_Sharp e_list -> BE_Sharp (List.map n_expr_to_b_expr e_list)

and n_array_to_b_array env ppt = function 
  | NA_Def e_list -> BA_Def (List.map n_expr_to_b_expr e_list)
  | NA_Caret (e1, e2) -> BA_Caret (n_expr_to_b_expr e1, n_expr_to_b_expr e2)
  | NA_Concat (e1, e2) -> BA_Concat (n_expr_to_b_expr e1, n_expr_to_b_expr e2)
  | NA_Slice (id, e_list) -> BA_Slice (List.map (fun (e1, e2) -> (n_expr_to_b_expr e1, n_expr_to_b_expr e2)) e_list)
  | NA_Index (id, e_list) -> BA_Index (List.map n_expr_to_b_expr e_list)

let rec n_type_to_b_type env = function
  | NT_Base b -> BT_Base b
  | NT_Array (t,e) -> BT_Array (n_type_to_b_type t, n_expr_to_b_expr env e)

let nlp_to_blp env = function
  | NLP_Ident id -> BLP_Ident (id_to_bid env id)
  | NLP_Tuple id_list -> BLP_Tuple (List.map (id_to_bid env) id_list)


let retrieve_vars node env =
  []

let bimpl_translator env node =
  let implem_name = node.n_id ^ "_i" in
  let refines = node.n_id in
  let sees = Utils.sees_list in
  let imports = Utils.imports_list in
  let concrete_vars = ref [] in
  let invariant = ref [] in 
  let initialisation = ref [] in
  let translate_eqs env eqs = []
    (* let translator = function *)
    (*   | N_Alternative a -> *)
    (* 	  { alt_lp =... *)
    (* 	  } *)
    (*   | N_Fonction f -> *)
    (*   | N_Operation o -> *)
    (*   | N_Registre r -> *)
    (* in *)
  in
  let reg_ids = retrieve_reg_ids node.n_eqs in
  let env_without_regs =
    List.filter (fun (id, t) -> not(List.mem id reg_ids)) (N_Env.elements n_env) in
  let vars = retrieve_vars node env in
  let eqs = translate_eqs env node.n_eqs in
  let op_decl = { id = ""; 
		  param_in = [];
		  param_out = [];
		} in
  let operations = { op_decl = op_decl;
		     vars = vars;
		     op_1 = eqs;
		     op_2 = [];
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

let n_decl_to_decl env (id, t) =
  let (_,_,bid) = Env.find id env in
  (bid, n_type_to_b_type t)

let n_condition_to_condition env (id, t, e) =
  let (_,_,bid) = Env.find id env in
  (bid, n_type_to_b_type t, n_expr_to_expr e)

let rec trad_list env to_call = function
  | [] -> []
  | elt::l -> (to_call env elt)::(trad_list env to_call l)

let bsig_translator env prog =
  let machine = String.capitalize prog.n_id in
  let sees = Utils.sees_list in
  let sigop_d = { id = prog.n_id;
		  param_in = trad_list env n_decl_to_decl prog.n_param_in;
		  param_out = trad_list env n_decl_to_decl prog.n_param_out;
		} in 
  let sigop_pre = trad_list env n_condition_to_condition prog.n_pre in
  let sigop_post = trad_list env n_condition_to_condition prog.n_post in
  { machine = machine;
    sig_sees = sees;
    sigop_decl = sigop_d;
    sigop_pre = sigop_pre;
    sigop_post = sigop_post;
  }


let translate prog =
  let env = Utils.make_env prog.n_env in
  let bsig = bsig_translator env prog in
  let bimpl = bimpl_translator env prog in
  { env = env;
    signature = bsig;
    implementation = bimpl;
  }
