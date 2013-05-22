(* Florian Thibord  --  Projet CERCLES *)

open Ast_repr_b
open Ast_repr_norm
open Utils

let bimpl_translator env node =
  let implementation = node.n_id ^ "_i" in
  let refines = node.n_id in
  let sees = Utils.sees_list in
  let imports = Utils.imports_list in
  let concrete_vars = ref [] in
  let invariant = ref [] in 
  let initialisation = ref [] in
  let translate_eqs env eqs =
    let translator = function
      | N_Alternative a ->
	  { alt_lp =...
	  }
      | N_Fonction f ->
      | N_Operation o ->
      | N_Registre r ->
  in
  let vars = retrieve_vars node env in
  let eqs = translate_eqs env node.n_eqs in
  let operations = (vars, eqs) in
  { implementation = implementation;
    refines = refines;
    sees = sees;
    imports = imports;
    concrete_variables = concrete_vars;
    invariant = invariant;
    initialisation = initialisation;
    operations = operations;
  }





(* A REECRIRE AVEC UNE MAP *)
let retrieve_env node =
  let n_env = node.n_env in 
  Utils.id_and_bid_list (N_Env.elements n_env)


let translate prog =
  let env = retrieve_env prog in
  let bsig = bsig_translator prog env in
  let bimpl = bimpl_translator prog env in
  { env = env;
    signature = bsig;
    implementation = bimpl;
  }
