(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_kcg
open Ast_base
open Ast_repr_b
open Ast_prog
open Ast_scade_norm
open Printer

(* let rec print_dim_list ppt = function *)
(*   | [] -> () *)
(*   | [BE_Value (Int i)] -> fprintf ppt "0 .. %a" print_value (Int (i-1)) *)
(*   | BE_Value (Int i) :: l -> fprintf ppt "0 .. %a, %a " print_value (Int (i-1)) print_dim_list l *)
(*   | [d] -> fprintf ppt "0 .. (%a-1)" print_expr d *)
(*   | d :: l -> fprintf ppt "0 .. (%a-1), %a " print_expr d print_dim_list l *)

(* let print_array_type t ppt e_list = *)
(*   fprintf ppt "(%a) --> %a" print_dim_list e_list print_basetype t *)





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
      (print_array_type t) e_list
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
    "MACHINE M_Consts@\n%a%aEND"
    print_concrete_constants (List.map (fun cst -> cst.c_id) const_list)
    print_properties const_list 

let print_m_const const_list file prog_env =
    with_env prog_env (fun () ->
        fprintf (formatter_of_out_channel file) "%a@." print_machine const_list
    )
