(* Florian Thibord  --  Projet CERCLES *)

open Ast_repr_b
open Ast_scade_norm
open Ast_scade
open Ast_base
open Ast_kcg
open Ast_prog
open Call_graph



(*** RECONNAISSANCE DE L'INITIALISATION D'UN REGISTRE PAR UNE ENTREE DU NOEUD ***)

(*  1) Côté appelé                                                              *)

let is_linked eqs ins v =
  let linked_fold = function
    N_Operation op -> (
      match op.n_op_lp, op.n_op_expr with 
	  NLP_Ident lp, NE_Ident i when lp = v -> 
	    if List.exists (fun (id, typ) -> id = i) ins then Some i else None
	| _ -> None )
    | _ -> None
  in
  List.fold_left (fun id eq -> match linked_fold eq with Some i -> Some i | None -> id) None eqs

let remove_from_ins ins i =
  let index_ref = ref 0 in
  let ins, _ = List.fold_left (fun (acc, index) (ident, typ) -> 
				 if i = ident then (index_ref := index; (acc, index + 1))
				 else ((ident, typ) :: acc, index + 1)
			      ) ([], 0) ins in
  ins, !index_ref

let remove_from_pres pres i =
  let cond_ref = ref ("", NT_Base T_Bool, None) in
  let pres =  List.fold_left (fun acc ((ident, _ , _) as pre) -> 
		    if i = ident then (cond_ref := pre; acc) 
		    else pre :: acc) [] pres in
  pres, !cond_ref
    
let build_lambda cond index = 
  let id, _, _ = cond in
  { n_l_ident = id;
    n_l_cond = cond;
    n_l_index = index;
  }

let search_input_in_reg ast_n =
  let eqs = ast_n.n_eqs in
  let ins = ast_n.n_param_in in 
  let pres = ast_n.n_pre in
  let lambdas = [] in
  let eqs_out = ref [] in
  let rec fun_rec eqs ins pres lambdas =
    match eqs with
    | [] -> 
      {ast_n with 
	n_lambdas = lambdas; 
	n_env = Env_builder.make_params_ident ast_n.n_env lambdas;
	n_param_in = ins; 
	n_pre = pres; 
	n_eqs = (List.rev !eqs_out)}
    | eq :: eqs_bis -> begin
      match eq with
      | N_Registre reg -> (
	match reg.n_reg_ini with
	  NE_Ident v -> (
	    match is_linked eqs ins v with
	      Some i ->
		let pres, cond = remove_from_pres pres i in
		let ins, index = remove_from_ins ins i in
		eqs_out := (N_Registre {reg with n_reg_ini = NE_Ident i}) :: !eqs_out;
		fun_rec eqs_bis ins pres ((build_lambda cond index) :: lambdas)
	    | None ->
	      eqs_out := eq :: !eqs_out;
	      fun_rec eqs_bis ins pres lambdas
	    )
	| _ ->
	  eqs_out := eq :: !eqs_out;
	  fun_rec eqs_bis ins pres lambdas)
      | _ as eq ->
	eqs_out := eq :: !eqs_out;
	fun_rec eqs_bis ins pres lambdas
    end
  in
  fun_rec eqs ins pres lambdas


	

(*  2) Côté appelant                                                            *)

let check_imports_params ast_b imports = 
  let eqs = ast_b.implementation.operation.op_1 in
  let imports_out = ref [] in
  let find_in_imp_list imp_id inst_id =
    List.find (fun imp -> (imp.call_name = imp_id) && (imp.instance_id = inst_id)) imports
  in
  let cip_fun_rec eq =
    match eq with
      Call c -> (
	let imp = find_in_imp_list c.call_id c.call_instance in
	let params_index_list, instname = imp.params_index, imp.instance_id in
	match params_index_list with 
	  | None -> 
	      imports_out := { b_import_name = imp.call_name; 
			       b_params_expr = None;
			       b_instance_id = instname; } :: !imports_out; eq
	  | Some param_index_list ->
	      if (List.length param_index_list) = 0 then (
		imports_out :=  { b_import_name = imp.call_name; 
				  b_params_expr = None;
				  b_instance_id = instname; } :: !imports_out; eq)
	      else (
		if instname = c.call_instance then
		  ( 
		    let _, params_op, params_m =
		      List.fold_left (fun (index, params_op, params_m) param_expr -> 
					if List.mem index param_index_list then
					  (index+1, params_op, (List.nth c.call_params index) :: params_m)
					else
					  (index+1, (List.nth c.call_params index) :: params_op, params_m)
				     ) (0, [], []) c.call_params 
		    in
		    imports_out :=  { b_import_name = imp.call_name; 
				      b_params_expr = Some params_m;
				      b_instance_id = instname; } :: !imports_out;
		    Call {c with call_params = params_op}
		  )
		else (
		  imports_out := { b_import_name = imp.call_name; 
				   b_params_expr = None;
				   b_instance_id = instname; } :: !imports_out; eq)
	      )
      )
      | _ -> eq
  in
  let eqs_out = List.map cip_fun_rec eqs in 
  { ast_b with implementation =
      { ast_b.implementation with 
	imports = !imports_out; 
	operation = { ast_b.implementation.operation with op_1 = eqs_out };
      }
  }
