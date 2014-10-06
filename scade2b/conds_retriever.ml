(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == conds_retriever.ml                                                    == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

open Ast_repr_b
open Ast_scade_norm
open Ast_scade
open Ast_base
open Ast_kcg
open Ast_prog
open Normalizer
open Utils

exception Failed

(***************************** ERROR MACHINE *******************************)

let add_non_existing_cond cond_list decl_list =
  List.fold_left (fun cond_l (id, t) ->
    if (List.exists (fun (id_bis, _, _) -> id = id_bis) cond_l)
    then cond_l else (id, t, None) :: cond_l) cond_list decl_list


let retrieve_ast_b p_ast prog =
  let pres, posts =
    try
      let const_ids = List.map (fun c -> c.c_id) prog.consts in
      let enums_ids = List.concat (List.map (fun e -> e.p_enum_list) prog.enum_types) in
      let p_ins, p_outs = p_ast.p_param_in, p_ast.p_param_out in
      let n_assumes = List.map 
	(Normalizer.handle_assume p_ins (const_ids@enums_ids)) p_ast.p_assumes in
      let n_guarantees = List.map 
	(Normalizer.handle_guarantee p_outs (const_ids@enums_ids)) p_ast.p_guarantees in
      let n_ins, n_outs = Normalizer.p_decl_to_n_decl p_ins, Normalizer.p_decl_to_n_decl p_outs in
      let n_assumes = add_non_existing_cond n_assumes n_ins in
      let n_guarantees = add_non_existing_cond n_guarantees n_outs in
      let env = Env_builder.make_env [] n_assumes n_guarantees ([p_ast.p_id]@const_ids@enums_ids) in
      let b_pres = List.map (Trad.n_condition_to_condition env) n_assumes in
      let b_posts = List.map (Trad.n_condition_to_condition env) n_guarantees in
      b_pres, b_posts
    with _ -> [], []
  in
  { name = p_ast.p_id;
    m_params = [];
    m_see_const = false;
    m_see_enum = false;
    m_imports = [];
    m_constraints = [];
    m_concrete_vars = [];
    m_invariant = [];
    m_initialisation = [];
    op_in_params = (let ins, _ = List.split p_ast.p_param_in in ins);
    op_out_params = (let outs, _ = List.split p_ast.p_param_out in outs);
    m_vars = (let vars, _ = List.split p_ast.p_vars in vars);
    abs_pre_condition = pres;
    abs_post_condition = posts;
    imp_substitutions = [];
  }

