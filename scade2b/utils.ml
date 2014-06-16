(* Florian Thibord  --  Projet CERCLES *)

open Ast_repr_b
open Ast_scade_norm
open Ast_scade
open Ast_base
open Ast_kcg
open Ast_prog
open Call_graph
open Ast_xml
open Xml_utils

(*************************** IDENT COLLISION FUNCTIONS ***************************)

exception Underscore of string
exception Reserved of string
exception Character of string
exception Collision of string

let reserved_b_words = ["ABSTRACT_CONSTANTS"; "ABSTRACT_VARIABLES"; "ANY"; "ASSERT"; "ASSERTIONS"; "BE"; "BEGIN"; "BOOL"; "CASE"; "CHOICE"; "CONCRETE_CONSTANTS"; "CONCRETE_VARIABLES"; "CONSTANTS"; "CONSTRAINTS"; "DEFINITIONS"; "DO"; "EITHER"; "ELSE"; "ELSIF"; "END"; "EXTENDS"; "FALSE"; "FIN"; "FIN1"; "IF"; "IMPLEMENTATION"; "IMPORTS"; "IN"; "INCLUDES"; "INITIALISATION"; "INT"; "INTEGER"; "INTER"; "INVARIANT"; "LET"; "LOCAL_OPERATIONS"; "MACHINE"; "MAXINT"; "MININT"; "NAT"; "NAT1"; "NATURAL"; "NATURAL1"; "OF"; "OPERATIONS"; "OR"; "PI"; "POW"; "POW1"; "PRE"; "PROMOTES"; "PROPERTIES"; "REFINES"; "REFINEMENT"; "SEES"; "SELECT"; "SETS"; "SIGMA"; "STRING"; "THEN"; "TRUE"; "UNION"; "USES"; "VALUES"; "VAR"; "VARIANT"; "VARIABLES"; "WHEN"; "WHERE"; "WHILE"; "arity"; "bin"; "bool"; "btree"; "card"; "closure"; "closure1"; "conc"; "const"; "dom"; "father"; "first"; "fnc"; "front"; "id"; "infix"; "inter"; "iseq"; "iseq1"; "iterate"; "last"; "left"; "max"; "min"; "mirror"; "mod"; "not"; "or"; "perm"; "postfix"; "pred"; "prefix"; "prj1"; "prj2"; "ran"; "rank"; "rec"; "rel"; "rev"; "right"; "seq"; "seq1"; "size"; "sizet"; "skip"; "son"; "sons"; "struct"; "subtree"; "succ"; "tail"; "top"; "tree"; "union"]

let is_reserved ident = List.mem ident reserved_b_words

let is_b_compliant ident = 
  if String.length ident < 2 then raise (Character ident)
  else if ident.[0] = '_' then raise (Underscore ident)
  else if is_reserved ident then raise (Reserved ident)
  else true

let check_no_collision ident env =
  if (Env.exists (fun _ (bid, _, _) -> ident = bid) env) then raise (Collision ident)
  else true

let make_b_ident ident env = 
  let new_ident = ref "" in
  let switch_underscore s = 
    try 
      new_ident := (String.sub s 1 ((String.length s)-1));
      false
    with 
    | Invalid_argument _ -> failwith ("String error in switch_underscore(Utils) with " ^ s)
  in
  let double_character s = new_ident := s^s; false in
  let add_underscore s = new_ident := s^"_"; false in
  let ident_check ident =
    try 
      (is_b_compliant ident) && (check_no_collision ident env)
    with
    | Underscore s ->
        switch_underscore s 
    | Character s ->
        double_character s
    | Reserved s ->
        add_underscore s
    | Collision s ->
        add_underscore s
  in
  let rec ident_generator ident =
    if ident_check ident then ident else ident_generator !new_ident
  in
  ident_generator ident

let make_env id_type_expr_list =
  List.fold_left
    (fun env (ident, typ, expr) -> Env.add ident ((make_b_ident ident env), typ, expr) env)
    Env.empty id_type_expr_list

let make_params_ident env lambda_list =
  List.fold_left (fun env lambda -> let (param, typ, expr) = Env.find lambda.n_l_ident env in
    Env.add lambda.n_l_ident ((String.uncapitalize param), typ, expr) env) env lambda_list


let make_b_ident_without_env ident = 
  let new_ident = ref "" in
  let switch_underscore s = 
    try 
      new_ident := (String.sub s 1 ((String.length s)-1));
      false
    with 
    | Invalid_argument _ -> failwith ("String error in switch_underscore(Utils) with " ^ s)
  in
  let double_character s = new_ident := s^s; false in
  let add_underscore s = new_ident := s^"_"; false in
  let ident_check ident =
    try 
      (is_b_compliant ident)
    with
    | Underscore s ->
        switch_underscore s 
    | Character s ->
        double_character s
    | Reserved s ->
        add_underscore s
    | Collision s ->
        add_underscore s
  in
  let rec ident_generator ident =
    if ident_check ident then ident else ident_generator !new_ident
  in
  ident_generator ident



(******************** GLOBAL ENV CHECK *************************)

let check_no_collision_global ident env =
  if (Env.exists (fun _ bid -> ident = bid) env) then raise (Collision ident)
  else true

let make_b_ident_global ident env = 
  let new_ident = ref "" in
  let switch_underscore s = 
    try 
      new_ident := (String.sub s 1 ((String.length s)-1));
      false
    with 
    | Invalid_argument _ -> failwith ("String error in switch_underscore(Utils) with " ^ s)
  in
  let double_character s = new_ident := s^s; false in
  let add_underscore s = new_ident := s^"_"; false in
  let ident_check ident =
    try 
      (is_b_compliant ident) && (check_no_collision_global ident env)
    with
    | Underscore s ->
        switch_underscore s 
    | Character s ->
        double_character s
    | Reserved s ->
        add_underscore s
    | Collision s ->
        add_underscore s
  in
  let rec ident_generator ident =
    if ident_check ident then ident else ident_generator !new_ident
  in
  ident_generator ident



(******************** INSTANCES ENV BUILDER *************************)

let make_inst_ident inst =
  let node_name, import_name, inst_id = inst in
  try
    (String.sub node_name 0 1) ^ (String.sub import_name 0 1) ^ inst_id 
  with Invalid_argument _ -> Printf.printf " ERROR !!!!!! : %s %s" node_name import_name; node_name ^ import_name ^ inst_id

let check_no_collision_global ident env =
  if (Env_instances.exists (fun _ bid -> ident = bid) env) then raise (Collision ident)
  else true

let make_instance_bid instance env = 
  let new_ident = ref "" in
  let switch_underscore s = 
    try 
      new_ident := (String.sub s 1 ((String.length s)-1));
      false
    with 
    | Invalid_argument _ -> failwith ("String error in switch_underscore(Utils) with " ^ s)
  in
  let double_character s = new_ident := s^s; false in
  let add_underscore s = new_ident := s^"_"; false in
  let ident_check ident =
    try 
      (is_b_compliant ident) && (check_no_collision_global ident env)
    with
    | Underscore s ->
        switch_underscore s 
    | Character s ->
        double_character s
    | Reserved s ->
        add_underscore s
    | Collision s ->
        add_underscore s
  in
  let rec ident_generator ident =
    if ident_check ident then ident else ident_generator !new_ident
  in
  let ident = make_inst_ident instance in
  ident_generator ident




(*** RECONNAISSANCE DE L'INITIALISATION D'UN REGISTRE PAR UNE ENTREE ***)

(*                          1) Normalisation                           *)


let is_linked eqs ins v =
  let linked_fold = function
    N_Operation op -> (
      match op.n_op_lp, op.n_op_expr with 
	  NLP_Ident lp, NE_Ident i when lp = v -> 
	    if List.exists (fun (id, typ) -> id = i) ins then Some i else None
	| _ -> None )
    | _ -> None
  in
  List.fold_left (fun id eq -> match linked_fold eq with Some i -> Some i | None -> id) None eqs


let remove_from_ins ins i =
  let index_ref = ref 0 in
  let ins, _ = List.fold_left (fun (acc, index) (ident, typ) -> 
				 if i = ident then (index_ref := index; (acc, index + 1))
				 else ((ident, typ) :: acc, index + 1)
			      ) ([], 0) ins in
  ins, !index_ref

let remove_from_pres pres i =
  let cond_ref = ref ("", NT_Base T_Bool, None) in
  let pres =  List.fold_left (fun acc ((ident, _ , _) as pre) -> 
		    if i = ident then (cond_ref := pre; acc) 
		    else pre :: acc) [] pres in
  pres, !cond_ref
    
let build_lambda cond index = 
  let id, _, _ = cond in
  { n_l_ident = id;
    n_l_cond = cond;
    n_l_index = index;
  }


let search_input_in_reg eqs ins pres lambdas =
  let eqs_out = ref [] in
  let rec fun_rec eqs ins pres lambdas =
    match eqs with
    | [] -> (ins, pres, lambdas, (List.rev !eqs_out))
    | eq :: eqs_bis -> begin
      match eq with
      | N_Registre reg -> (
	match reg.n_reg_ini with
	  NE_Ident v -> (
	    match is_linked eqs ins v with
	      Some i ->
		let pres, cond = remove_from_pres pres i in
		let ins, index = remove_from_ins ins i in
		eqs_out := (N_Registre {reg with n_reg_ini = NE_Ident i}) :: !eqs_out;
		fun_rec eqs_bis ins pres ((build_lambda cond index) :: lambdas)
	    | None ->
	      eqs_out := eq :: !eqs_out;
	      fun_rec eqs_bis ins pres lambdas
	    )
	| _ ->
	  eqs_out := eq :: !eqs_out;
	  fun_rec eqs_bis ins pres lambdas)
      | _ as eq ->
	eqs_out := eq :: !eqs_out;
	fun_rec eqs_bis ins pres lambdas
    end
  in
  fun_rec eqs ins pres lambdas


	
(*                          2) Traduction                           *)

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


