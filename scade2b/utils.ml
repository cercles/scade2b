(* Florian Thibord  --  Projet CERCLES *)

open Ast_repr_b
open Ast_scade_norm
open Ast_scade
open Ast_base
open Ast_kcg
open Ast_prog
open Call_graph


(*************************** DIVERS ***************************)

(* a_b_list_equals (l: ('a, 'a) list) returns true if a = b for every pairs (parser_scade) *)
let a_b_list_equals l=
  List.for_all (fun (a, b) -> a = b) l

(* Find the type related to a variable (normalizer) *)
let rec find_type id declist =
  match declist with
  | [] -> None
  | (ident, typ)::l -> if ident = id then Some typ else find_type id l

(* caret_to_def v n returns [v; ...; v] with n index (trad) *)
and caret_to_def e1 e2 = 
  let rec funrec v dim acc =
    if dim = 0 then acc
    else funrec v (dim-1) (v :: acc)
  in
  NA_Def (funrec e1 e2 [])


exception Two_ident_used_in_condition of (string * string)

(* Find an ident in an expr. Used in handle_assume/guarantee (normalizer), find the ident linked to a condition *)
let find_ident_in_pexpr expr consts =
  let is_not_const id = not(List.mem id consts) in
  let id = ref "" in
  let rec ident_finder = function
    | PE_Ident iden -> 
        if (!id <> iden && !id <> "" && (is_not_const iden) && (is_not_const !id))
	then raise (Two_ident_used_in_condition (!id, iden)) 
	else (if (is_not_const iden) then id := iden)
    | PE_Value v -> ()
    | PE_Array array -> idarray_finder array
    | PE_Call (_, _, elist) -> List.iter ident_finder elist
    | PE_Op_Arith1 (_, e) -> ident_finder e
    | PE_Op_Arith2 (_, e1, e2)
    | PE_Op_Relat (_, e1, e2) -> ident_finder e1;ident_finder e2
    | PE_Op_Sharp elist -> List.iter ident_finder elist
    | PE_Op_Not e -> ident_finder e
    | PE_Op_Logic (_, e1, e2) -> ident_finder e1 ; ident_finder e2
    | PE_Fby (e1, e2, e3) -> ident_finder e1; ident_finder e2; ident_finder e3
    | PE_If (e1, e2, e3) -> ident_finder e1; ident_finder e2; ident_finder e3
  and idarray_finder = function
    | PA_Def elist -> List.iter ident_finder elist
    | PA_Caret (e1, e2) -> ident_finder e1; ident_finder e2 
    | PA_Concat (e1, e2) -> ident_finder e1; ident_finder e2 
    | PA_Slice (iden, _) -> if (!id <> "" && !id <> iden) then 
	raise (Two_ident_used_in_condition (!id, iden)) 
      else id := iden
    | PA_Index (iden, _) -> if (!id <> "" && !id <> iden) then 
	raise (Two_ident_used_in_condition (!id, iden)) 
      else id := iden
    | PA_Reverse iden -> if (!id <> "" && !id <> iden) then 
	raise (Two_ident_used_in_condition (!id, iden)) 
      else id := iden	
  in
  ident_finder expr;
  !id

(* Find out if a constant and/or an enum is used in the node *)
let find_const_enum_in_node ast_option consts enums =
  let const_found = ref false in
  let enum_found = ref false in
  let consts = List.map (fun c -> c.c_id) consts in
  let enum_ids, enum_elts = 
    List.split (List.map (fun e -> e.p_enum_id, e.p_enum_list) enums) in
  let enum_elts = List.concat enum_elts in
  let rec id_expr_finder = function
    | PE_Ident id -> 
      const_found := !const_found || (List.mem id consts);
      enum_found := !enum_found || (List.mem id enum_elts)
    | PE_Value v -> ()
    | PE_Array array -> idarray_finder array
    | PE_Call (_, _, elist) -> List.iter id_expr_finder elist
    | PE_Op_Arith1 (_, e) -> id_expr_finder e
    | PE_Op_Arith2 (_, e1, e2)
    | PE_Op_Relat (_, e1, e2) -> id_expr_finder e1;id_expr_finder e2
    | PE_Op_Sharp elist -> List.iter id_expr_finder elist
    | PE_Op_Not e -> id_expr_finder e
    | PE_Op_Logic (_, e1, e2) -> id_expr_finder e1 ; id_expr_finder e2
    | PE_Fby (e1, e2, e3) -> id_expr_finder e1; id_expr_finder e2; id_expr_finder e3
    | PE_If (e1, e2, e3) -> id_expr_finder e1; id_expr_finder e2; id_expr_finder e3
  and idarray_finder = function
    | PA_Def elist -> List.iter id_expr_finder elist
    | PA_Caret (e1, e2) -> id_expr_finder e1; id_expr_finder e2 
    | PA_Concat (e1, e2) -> id_expr_finder e1; id_expr_finder e2 
    | PA_Slice (id, _) -> 
      const_found := !const_found || (List.mem id consts);
      enum_found := !enum_found || (List.mem id enum_elts)
    | PA_Index (id, _) -> 
      const_found := !const_found || (List.mem id consts);
      enum_found := !enum_found || (List.mem id enum_elts)
    | PA_Reverse id -> 
      const_found := !const_found || (List.mem id consts);
      enum_found := !enum_found || (List.mem id enum_elts)
  in
  let rec enum_const_type_finder = function
    | PT_Base t -> 
      begin
	match t with 
	| T_Enum id -> enum_found := !enum_found || (List.mem id enum_ids)
	| _ -> ()
      end;
    | PT_Array (t, e) -> enum_const_type_finder t; id_expr_finder e
  in
  match ast_option with
  | None -> !const_found, !enum_found
  | Some ast -> 
    let (_, eq_expr_list) = List.split ast.p_eqs in
    List.iter id_expr_finder (eq_expr_list@ast.p_assumes@ast.p_guarantees);
    let (_, types) = List.split (ast.p_param_in@ast.p_param_out@ast.p_vars) in
    List.iter enum_const_type_finder types;
    !const_found, !enum_found

let const_enums_used_in_prog nodes =
  List.fold_left (fun (const_used, enum_used) node -> 
    let n_const_used, n_enum_used = node.sees_cond in
    (const_used || n_const_used, enum_used || n_enum_used)
  ) (false, false) nodes

(* (trad) *)
let sees_list sees_cond =
  match sees_cond with
  | (false, false) -> [] 
  | (false, true) -> ["M_Enum"] 
  | (true, false) -> ["M_Consts"] 
  | (true, true) -> ["M_Consts"; "M_Enum"] 

(* Fonctions de rennomage d'ident (trad) *)
let rec rename_id_expr old new_i = function
  | NE_Ident i -> if i = old then NE_Ident new_i else NE_Ident i
  | NE_Value v -> NE_Value v
  | NE_Array ar -> NE_Array (rename_id_array old new_i ar)
  | NE_Op_Arith1 (op, e) -> NE_Op_Arith1 (op, rename_id_expr old new_i e)
  | NE_Op_Arith2 (op, e1, e2) -> NE_Op_Arith2 (op, rename_id_expr old new_i e1,
                                                   rename_id_expr old new_i e2)
  | NE_Op_Relat (op, e1, e2) -> NE_Op_Relat (op, rename_id_expr old new_i e1,
                                                 rename_id_expr old new_i e2)
  | NE_Op_Logic (op, e1, e2) -> NE_Op_Logic (op, rename_id_expr old new_i e1,
                                                 rename_id_expr old new_i e2)
  | NE_Op_Sharp (e_list) -> NE_Op_Sharp (List.map (rename_id_expr old new_i) e_list)
  | NE_Op_Not e -> NE_Op_Not (rename_id_expr old new_i e)
and rename_id_array old new_i = function
  | NA_Def e_list -> NA_Def (List.map (rename_id_expr old new_i) e_list)
  | NA_Caret (e1, e2) -> NA_Caret (rename_id_expr old new_i e1, rename_id_expr old new_i e2)
  | NA_Concat (e1, e2) -> NA_Concat (rename_id_expr old new_i e1, rename_id_expr old new_i e2)
  | NA_Slice (i, e_list) -> 
    NA_Slice ((if i = old then new_i else i), 
	      (List.map (fun (e1, e2) ->
		(rename_id_expr old new_i e1, rename_id_expr old new_i e2)) e_list))
  | NA_Index (i, e_list) -> 
    NA_Index ((if i = old then new_i else i), 
	      (List.map (rename_id_expr old new_i) e_list))
  | NA_Reverse i -> NA_Reverse (if i = old then new_i else i)


(* Creation de l'expression a substituer a l'id d'un tableau pour l'impression des conditions *)
let make_subst_id index_id dims tab_id =
  let rec fun_rec size =
    if size = 1 then tab_id else "conc(" ^ (fun_rec (size-1)) ^ ")"
  in
  (fun_rec (List.length dims)) ^ "(" ^ index_id ^ ")"
    

(************** CHANGEMENT DE LA REPRESENTATION DES CONSTANTES **************)
    
(* ast_repr to ast_repr_b functions *)

let rec p_expr_to_b_expr = function
  | PE_Ident id ->  BE_Ident id
  | PE_Value v -> BE_Value v
  | PE_Array ar -> BE_Array (p_array_to_b_array ar)
  | PE_Op_Arith1 (op, e) -> BE_Op_Arith1 (op, p_expr_to_b_expr e)
  | PE_Op_Arith2 (op, e1, e2) -> BE_Op_Arith2 (op, p_expr_to_b_expr e1, p_expr_to_b_expr e2)
  | PE_Op_Relat (op, e1, e2) -> BE_Pred (BP_Op_Relat (op, p_expr_to_b_expr e1, p_expr_to_b_expr e2))
  | _ -> assert false

and p_array_to_b_array = function
  | PA_Def e_list -> BA_Def (List.map p_expr_to_b_expr e_list)
  | PA_Caret (e1, PE_Value (Int i)) -> p_array_to_b_array (caret_to_def_bis e1 i)
  | PA_Caret (e1, e2) -> BA_Caret (p_expr_to_b_expr e1, p_expr_to_b_expr e2)
  | PA_Concat (e1, e2) -> BA_Concat (p_expr_to_b_expr e1, p_expr_to_b_expr e2)
  | PA_Slice (id, e_list) -> 
      BA_Slice (id, 
		(List.map (fun (e1, e2) -> 
			     (p_expr_to_b_expr e1, p_expr_to_b_expr e2)) e_list))
  | PA_Index (id, e_list) -> 
      BA_Index (id, (List.map p_expr_to_b_expr e_list))
  | PA_Reverse id -> BA_Reverse id

and caret_to_def_bis e1 e2 = 
  let rec funrec v dim acc =
    if dim = 0 then acc
    else funrec v (dim-1) (v :: acc)
  in
  PA_Def (funrec e1 e2 [])

let p_const_to_b_const const =
  let flatten_array a =
    let base_t = ref T_Bool in (* default ref *)
    let rec fun_rec = function
      | PT_Base t -> base_t := t; []
      | PT_Array (t, expr) -> (p_expr_to_b_expr expr) :: (fun_rec t)
    in
    (!base_t, fun_rec a)
  in
  let (id, t, e) = (const.c_id, const.c_typ, const.c_expr) in 
  match t with 
    | PT_Base typ -> Const_Base (id, typ, p_expr_to_b_expr e)
    | PT_Array (_, _) -> (
      let typ, dims = flatten_array t in 
      Const_Fun (id, typ, dims, p_expr_to_b_expr e))


(*************************** MISC INITIALISATION ***************************)

let create_dir_output dir_output =
  if not(Sys.file_exists dir_output) then Unix.mkdir dir_output 0o764

(***************************** LIBRARY STUFF *******************************)

let library_list = [
"to_int", "ML_math" ; 
"to_real", "ML_math"
]


