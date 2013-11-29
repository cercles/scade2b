(* Florian Thibord  --  Projet CERCLES *)


open Format
open Ast_repr_b
open Ast_base
open Xml_utils


let print_bid ppt id =
  fprintf ppt "%s" id

let rec print_idlist_comma ppt = function
  | [] -> ()
  | [id] -> fprintf ppt "%a" print_bid id
  | id::l -> fprintf ppt "%a, %a" print_bid id print_idlist_comma l

let print_type ppt typ =
  match typ with
    | "kcg_int" -> fprintf ppt "%s" "INT"
    | "kcg_real" -> fprintf ppt "%s" "REAL"
    | "kcg_bool" -> fprintf ppt "%s" "BOOL"
    | "array_int_2" -> fprintf ppt "%s" "(0 .. 1) --> INT"
    | "array_int_2_2" -> fprintf ppt "%s" "(0 .. 1, 0 .. 1) --> INT"
    | _ as a -> fprintf ppt "%s" a

let print_then_condition ppt (id, typ) =
  fprintf ppt "%s :: { ii | ii : %a }" id print_type typ

let print_pre_condition ppt (id, typ) =
  fprintf ppt "%s : %a" id print_type typ

let rec print_thenlist ppt = function 
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_then_condition c
  | c::l -> fprintf ppt "%a||@,%a" print_then_condition c print_thenlist l 

let rec print_prelist ppt = function 
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_pre_condition c
  | c::l -> fprintf ppt "%a &@,%a" print_pre_condition c print_prelist l 

let print_op_decl ppt xml_decl =
  let ins, _ = List.split xml_decl.xmlu_ins in
  let outs, _ = List.split xml_decl.xmlu_outs in
  fprintf ppt "%a <-- %s(%a)"
    print_idlist_comma outs
    xml_decl.xmlu_node_name
    print_idlist_comma ins

let print_operation ppt xml_decl =
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n@[<v 3> PRE@,@[<v>%a@]@]@\n@[<v 3> THEN@,@[<v>%a@]@]@\n END"
    print_op_decl xml_decl
    print_prelist xml_decl.xmlu_ins
    print_thenlist xml_decl.xmlu_outs

let print_id_machine ppt id_machine =
  fprintf ppt "M_%s" id_machine


let print_machine ppt xml_decl =
  fprintf ppt
    "MACHINE %a@\n%a @\nEND"
    print_id_machine xml_decl.xmlu_node_name
    print_operation xml_decl


let print_m xml_decl file =
    fprintf (formatter_of_out_channel file) "%a@." print_machine xml_decl
