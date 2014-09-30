(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == babsterror_generator.ml                                               == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

open Format
open Ast_repr_b
open Ast_base
open Ast_xml
open Utils
open Printer


let print_bid ppt id =
  let id_ok = Env_builder.make_b_ident_without_env id in
  fprintf ppt "%s" id_ok

let print_idlist_comma ppt l =
  print_list print_bid ppt l

let print_type ppt typ =
  match typ with
    | "kcg_int" -> fprintf ppt "%s" "INT"
    | "kcg_real" -> fprintf ppt "%s" "REAL"
    | "kcg_bool" -> fprintf ppt "%s" "BOOL"
    | "array_int_2" -> fprintf ppt "%s" "seq(INT)"
    | "array_int_2_2" -> fprintf ppt "%s" "seq(seq(INT))"
    | _ as a -> fprintf ppt "%s" a

let print_then_condition ppt output =
  fprintf ppt "%a : ( ii | ii : %a )" print_bid output.var_id print_type output.var_type

let print_pre_condition ppt input =
  fprintf ppt "%a : %a" print_bid input.var_id print_type input.var_type

let print_thenlist ppt l =
  print_list print_then_condition ~sep:"||" ~break:true ppt l

let print_prelist ppt l =
  print_list print_pre_condition ~sep:" &" ~break:true ppt l

let print_op_decl ppt xml_decl =
  let in_ids = List.map (fun var -> var.var_id) xml_decl.in_params in
  let out_ids = List.map (fun var -> var.var_id) xml_decl.out_params in
  if (List.length out_ids = 0) && (List.length in_ids = 0) then
    fprintf ppt "%s" xml_decl.name
  else if (List.length out_ids = 0) then
    fprintf ppt "%s(%a)" xml_decl.name print_idlist_comma (List.rev in_ids)
  else if (List.length in_ids = 0) then
    fprintf ppt "%a <-- %s" print_idlist_comma (List.rev out_ids) xml_decl.name
  else
    fprintf ppt "%a <-- %s(%a)"
      print_idlist_comma (List.rev out_ids)
      xml_decl.name
      print_idlist_comma (List.rev in_ids)

let print_operation_wc ppt xml_decl =
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n@[<v 3> PRE@,@[<v>%a@]@]@\n@[<v 3> THEN@,@[<v>%a@]@]@\n END"
    print_op_decl xml_decl
    print_prelist xml_decl.in_params
    print_thenlist xml_decl.out_params

let print_operation conditions ppt xml_decl =
  let pres, posts = conditions in
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n@[<v 3> PRE@,@[<v>%a@]@]@\n@[<v 3> THEN@,@[<v>%a@]@]@\n END"
    print_op_decl xml_decl
    Babst_generator.print_pre_list pres
    Babst_generator.print_post_list posts

let print_id_machine ppt id_machine =
  fprintf ppt "M_%s" id_machine

let print_machine_wc ppt xml_decl =
  fprintf ppt
    "MACHINE %a@\n%a@\nEND"
    print_id_machine xml_decl.name
    print_operation_wc xml_decl

let print_machine conditions ppt xml_decl =
  fprintf ppt
    "MACHINE %a@\n%a@\nEND"
    print_id_machine xml_decl.name
    (print_operation conditions) xml_decl

let print_machine_base_wc xml_node file =
    fprintf (formatter_of_out_channel file) "%a@." print_machine_wc xml_node

let print_machine_base xml_node conditions file =
    fprintf (formatter_of_out_channel file) "%a@." (print_machine conditions) xml_node

let generate_without_cond node_xml main_dir =
  let node_name = node_xml.name in
  let babst_err =
    open_out (Filename.concat main_dir ("M_" ^ node_name ^ ".mch")) in
  print_machine_base_wc node_xml babst_err ;
  close_out babst_err

let generate node_xml main_dir conditions =
  let node_name = node_xml.name in
  let babst_err =
    open_out (Filename.concat main_dir ("M_" ^ node_name ^ ".mch")) in
  print_machine_base node_xml conditions babst_err ;
  close_out babst_err
