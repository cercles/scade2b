(* Florian Thibord  --  Projet CERCLES *)


(* regarde si toutes les paires d'unes liste sont égales *)
let a_b_list_equals l=
  List.for_all (fun (a, b) -> a = b) l
