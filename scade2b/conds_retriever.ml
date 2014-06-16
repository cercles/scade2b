(* Florian Thibord  --  Projet CERCLES *)

open Ast_repr_b
open Ast_scade_norm
open Ast_scade
open Ast_base
open Ast_kcg
open Ast_prog
open Ast_xml
open Call_graph

exception Failed

(***************************** ERROR MACHINE *******************************)

let rec xml_to_scade_type arraytypes type_ident =
  match type_ident with
  | "kcg_int" -> PT_Base T_Int
  | "kcg_real" -> PT_Base T_Float
  | "kcg_bool" -> PT_Base T_Bool
  | _ -> 
    try
      let array_type = List.find (fun array_t -> array_t.name = type_ident) arraytypes in
      xml_to_scade_array_type arraytypes array_type
    with Not_found -> PT_Base (T_Enum type_ident)

and xml_to_scade_array_type arraytypes array_type =
  let cell_type, size = array_type.celltype, array_type.size in
  let p_expr = PE_Value (Int (size))
    (* try (\* SIZE doit etre un type string -> changer parser_xml *\) *)
    (*   let dim = int_of_string size in *)
    (*   PE_Value (Int (size)) *)
    (* with Failure _ -> PE_Ident(size)  *)
  in
  PT_Array (xml_to_scade_type arraytypes cell_type, p_expr)

let xml_to_scade_decl arraytypes decl = 
  (decl.var_id, xml_to_scade_type arraytypes decl.var_type)

(* TODO : En se passant du xml, on pourra récupérer les vars locales en plus *)
let compute_conditions_error_m conditions consts enums node_xml arraytypes =
  try
    let assumes, guarantees = conditions in
    let ins_xml, outs_xml = node_xml.ins, node_xml.outs in
    let p_ins, p_outs = 
      List.map (xml_to_scade_decl arraytypes) ins_xml, 
      List.map (xml_to_scade_decl arraytypes) outs_xml
    in
    let const_ids = List.map (fun c -> c.c_id) consts in
    let enums_ids = List.concat (List.map (fun e -> e.p_enum_list) enums) in
    let n_assumes = List.map (Normalizer.handle_assume p_ins (const_ids@enums_ids)) assumes in
    let n_guarantees = List.map (Normalizer.handle_guarantee p_outs (const_ids@enums_ids)) guarantees in
    let n_ins, n_outs = Normalizer.p_decl_to_n_decl p_ins, Normalizer.p_decl_to_n_decl p_outs in
    let add_non_existing_cond cond_list decl_list =
      List.fold_left (fun cond_l (id, t) ->
    	if (List.exists (fun (id_bis, _, _) -> id = id_bis) cond_l)
    	then cond_l else (id, t, None) :: cond_l) cond_list decl_list
    in
    let n_assumes = add_non_existing_cond n_assumes n_ins in
    let n_guarantees = add_non_existing_cond n_guarantees n_outs in
    let env = Env_builder.make_env [] n_assumes n_guarantees ([node_xml.xml_node_name]@const_ids@enums_ids) in
    let b_pres = List.map (Trad.n_condition_to_condition_pred env) n_assumes in
    let b_posts = List.map (Trad.n_condition_to_condition env) n_guarantees in
    (b_pres, b_posts)
  with _ -> raise Failed (* TODO : manager les erreurs *)
