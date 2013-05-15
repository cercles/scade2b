(* Florian Thibord  --  Projet CERCLES *)

open Ast_norm_repr

(* regarde si toutes les paires d'unes liste sont égales *)
let a_b_list_equals l=
  List.for_all (fun (a, b) -> a = b) l


let sees_list = []

let imports_list = ["Bound"]

let string_of_list l = 
  List.fold_left (fun res str -> res^", "^str ) (List.hd l) (List.tl l)

let string_of_reglist l = 
  let head = List.hd l in
  List.fold_left (fun res reg -> res^", "^reg.reg_id ) head.reg_id (List.tl l)
