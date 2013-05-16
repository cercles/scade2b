(* Florian Thibord  --  Projet CERCLES *)

open Ast_repr_norm
open Ast_repr


(* a_b_list_equals (l: ('a, 'a) list) returns true if a = b for every pairs *)
let a_b_list_equals l=
  List.for_all (fun (a, b) -> a = b) l

(* Define the machines accessed in the SEES clause *)
let sees_list = []
(* Define the machines accessed in the IMPORT clause (A AUTOMATISER) *)
let imports_list = ["Bound"]

(* string_of_list (l: string list) returns the concat of every strings in list *)
let string_of_list l = 
  List.fold_left (fun res str -> res^", "^str ) (List.hd l) (List.tl l)

(* idem for l of type: n_reg list *)
(* let string_of_reglist l =  *)
(*   let head = List.hd l in *)
(*   List.fold_left (fun res reg -> res^", "^reg.reg_lp ) head.reg_lp (List.tl l) *)


exception Two_ident of (string * string)

let find_ident_in_pexpr expr =
  let id = ref "" in
  let rec ident_finder = function
    | PE_Ident iden -> if (!id <> "" && !id <> iden) then raise (Two_ident (!id, iden)) else id := iden
    | PE_Tuple elist -> List.iter ident_finder elist
    | PE_Value v -> ()
    | PE_Array array -> idarray_finder array
    | PE_App (id, elist) -> List.iter ident_finder elist
    | PE_Bop (bop, e1, e2) -> ident_finder e1; ident_finder e2
    | PE_Unop (unop, exp) -> ident_finder exp
    | PE_Fby (e1, e2) -> ident_finder e1; ident_finder e2
    | PE_Pre exp -> ident_finder exp
    | PE_If (e1, e2, e3) -> ident_finder e1; ident_finder e2; ident_finder e3
    | PE_Sharp elist -> List.iter ident_finder elist
  and idarray_finder = function
    | PA_Def elist -> List.iter ident_finder elist
    | PA_Caret (e1, e2) -> ident_finder e1; ident_finder e2 
    | PA_Concat (e1, e2) -> ident_finder e1; ident_finder e2 
    | PA_Slice (iden, _) -> if (!id <> "" && !id <> iden) then raise (Two_ident (!id, iden)) else id := iden
    | PA_Index (iden, _) -> if (!id <> "" && !id <> iden) then raise (Two_ident (!id, iden)) else id := iden
  in
  ident_finder expr;
  !id

