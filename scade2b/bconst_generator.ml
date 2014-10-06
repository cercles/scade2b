(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == bconst_generator.ml                                                   == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

open Format
open Ast_kcg
open Ast_base
open Ast_repr_b
open Ast_prog
open Ast_scade_norm
open Printer


let print_property ppt = function
  | Const_Base (id, t, expr) -> 
    fprintf ppt "%a : %a & %a = %a "
      print_bid id
      print_basetype t
      print_bid id
      print_expr expr 
  | Const_Fun (id, t, e_list, expr) ->
    fprintf ppt "%a : %a & %a = %a "
      print_bid id
      (print_array_type t id) e_list
      print_bid id
      print_expr expr

let print_properties_list ppt l =
  print_list print_property ~sep:" & " ~break:true ppt l

let print_properties ppt = function
  | [] -> ()
  | const_list ->
      fprintf ppt "PROPERTIES @\n@[<v 3>   %a@]@\n"
        print_properties_list (List.map Utils.p_const_to_b_const const_list)


let print_concrete_constants ppt = function
  | [] -> ()
  | const_id_list ->
      fprintf ppt "CONCRETE_CONSTANTS %a@\n" print_idlist_comma const_id_list


let print_machine ppt const_list =
  fprintf ppt
    "MACHINE M_Const@\n%a%aEND"
    print_concrete_constants (List.map (fun cst -> cst.c_id) const_list)
    print_properties const_list 

let print_m_const const_list file prog_env =
    with_env prog_env (fun () ->
        fprintf (formatter_of_out_channel file) "%a@." print_machine const_list
    )
