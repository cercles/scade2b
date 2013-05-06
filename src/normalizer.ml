(* Florian Thibord  --  Projet CERCLES *)

(* Extraction des registres, 
   Vérification que les opérations sont atomiques?
   Vérifs de bases en plus nécessaires?
   Différentier pre/post conditions *)

open Ast_base
open Ast_repr
open Ast_norm_repr

exception Assert_id_error
exception Register_error 




(* Fonctions de passage de ast_repr à ast_norm_repr *)

let rec p_expr_to_n_expr = function
  | PE_Ident iden -> NE_Ident iden
  | PE_Tuple elist -> NE_Tuple (List.map (fun e -> p_expr_to_n_expr e) elist)
  | PE_Value v -> NE_Value v
  | PE_Array array -> NE_Array (p_array_to_n_array array)
  | PE_App (id, elist) -> NE_App (id, (List.map (fun e -> p_expr_to_n_expr e) elist))
  | PE_Bop (bop, e1, e2) -> NE_Bop (bop, p_expr_to_n_expr e1, p_expr_to_n_expr e2)
  | PE_Unop (unop, e) -> NE_Unop (unop, p_expr_to_n_expr e)
  | PE_Fby (e1, e2) -> failwith "Arrow misssed in normalizer"
  | PE_Pre e -> failwith "Pre misssed in normalizer"
  | PE_If (e1, e2, e3) -> NE_If (p_expr_to_n_expr e1, p_expr_to_n_expr e2, p_expr_to_n_expr e3)
  | PE_Sharp elist -> NE_Sharp (List.map (fun e -> p_expr_to_n_expr e) elist)

and p_array_to_n_array = function
  | PA_Def elist -> NA_Def (List.map (fun e -> p_expr_to_n_expr e) elist)
  | PA_Caret (e1, e2) -> NA_Caret (p_expr_to_n_expr e1, p_expr_to_n_expr e2)
  | PA_Concat (e1, e2) -> NA_Concat (p_expr_to_n_expr e1, p_expr_to_n_expr e2)
  | PA_Slice (id, l) -> NA_Slice (id, (List.map (fun (e1, e2) -> p_expr_to_n_expr e1, p_expr_to_n_expr e2) l))

let plp_to_nlp = function
  | PLP_Ident id -> NLP_Ident id
  | PLP_Tuple ids -> NLP_Tuple ids

let rec p_type_to_n_type = function
  | PT_Base b -> NT_Base b
  | PT_Array (t, e) -> NT_Array (p_type_to_n_type t, p_expr_to_n_expr e)

let p_decl_to_n_decl declist = 
  List.map (fun (id, p_type) -> (id, p_type_to_n_type p_type)) declist








(* ATTENTION AUX TUPLES *) 


(* AJOUTER un p_expr to n_expr *)



(* New type used for pre/post conditions *)
type pre_post = Pre of n_condition | Post of n_condition

(* Find the type related to a declaration *)
let rec find_type id declist =
  match declist with
  | [] -> None
  | (ident, typ)::l -> if ident = id then Some typ else find_type id l


(* Transform an assert into a pre/post condition *)
let handle_assert main_node asser = 
  let id = ref "" in
  let rec ident_finder eq =
    match eq with
      PE_Ident iden -> if !id <> "" then raise Assert_id_error else id := iden
    | PE_Tuple elist -> List.iter (fun eq -> ident_finder eq) elist
    | PE_Value v -> ()
    | PE_Array array -> () (* gérer array! *)
    | PE_App (id, elist) -> ()
    | PE_Bop (bop, e1, e2) -> ident_finder e1; ident_finder e2
    | PE_Unop (unop, exp) -> ident_finder exp
    | PE_Fby (e1, e2) -> ident_finder e1; ident_finder e2
    | PE_Pre exp -> ident_finder exp
    | PE_If (e1, e2, e3) -> ident_finder e1; ident_finder e2; ident_finder e3
    | PE_Sharp elist -> List.iter (fun eq -> ident_finder eq) elist
  in
  ident_finder asser;
  match find_type !id main_node.p_param_in with
  | Some typ -> Pre (!id, p_type_to_n_type typ, p_expr_to_n_expr asser)
  | None -> match find_type !id main_node.p_param_out with
    | Some typ -> Post (!id, p_type_to_n_type typ, p_expr_to_n_expr asser)
    | None -> raise Assert_id_error


(* Two steps: 
   1. create a register ( c -> pre x with type t becomes reg_id = t, c, x 
   2. replace the register in the old equation by its new ident 
   This fun returns the new register and the new equation *)
let handle_register main_node= 
  let cpt = ref 0 in
  function 
  | P_Eq (l, PE_Fby (a, PE_Pre (PE_Ident id))) -> 
    begin 
      incr cpt;
      let typ = match find_type id (main_node.p_param_in@main_node.p_param_out@main_node.p_vars) with
	| Some typ -> typ
	| None -> raise Register_error
      in
      let reg = { reg_id = "reg"^(string_of_int !cpt);
		  reg_type = p_type_to_n_type typ;
		  reg_ini = p_expr_to_n_expr a;
		  reg_var = NE_Ident id;
		} in
      let new_equation = (plp_to_nlp l, NE_Ident reg.reg_id) in
      (reg, new_equation)
    end
  | _ -> raise Register_error
    



module L = Set.Make(
  struct
    type t = ident
    let compare = compare
  end
)

module EQs = Set.Make(
  struct 
    (* 1 equation, ses ids à gauche et à droite *)
    type t = n_equation * L.t * L.t
    let compare = compare 
  end
)

(* A reecrire en fonctionnel *)
let ident_of_expr expr =
  let id = ref L.empty in 
  let rec idexpr_rec = function
    | NE_Ident iden -> id := L.add iden !id
    | NE_Tuple elist -> List.iter (fun e -> idexpr_rec e) elist
    | NE_Value v -> ()
    | NE_Array array -> idarray_rec array
    | NE_App (id, elist) -> List.iter (fun eq -> idexpr_rec eq) elist
    | NE_Bop (bop, e1, e2) -> idexpr_rec e1; idexpr_rec e2 
    | NE_Unop (unop, exp) -> idexpr_rec exp
    | NE_If (e1, e2, e3) -> idexpr_rec e1; idexpr_rec e2; idexpr_rec e3
    | NE_Sharp elist -> List.iter (fun eq -> idexpr_rec eq) elist
  and idarray_rec = function
    |NA_Def elist -> List.iter (fun eq -> idexpr_rec eq) elist
    |NA_Caret (e1, e2) -> idexpr_rec e1; idexpr_rec e2 
    |NA_Concat (e1, e2) -> idexpr_rec e1; idexpr_rec e2 
    |NA_Slice (iden, l) -> id := L.add iden !id;
	List.iter (fun (e1, e2) -> idexpr_rec e1; idexpr_rec e2) l
  in
  idexpr_rec expr;
  !id

let ident_of_left = function
  | NLP_Ident id -> L.add id L.empty
  | NLP_Tuple idl -> List.fold_left (fun s id ->  L.add id s) L.empty idl

let ident_of_eq (lp, expr) = (ident_of_left lp, ident_of_expr expr)
  
let scheduler eqs inputs =
  let rec schedul_rec res l eqs =
    if EQs.is_empty eqs then res else begin
      let pred = fun l e_idset -> L.is_empty (L.diff e_idset l) in
      let (ok,nok) = 
	EQs.fold
	  (fun ((_, _, e_idset) as eq) (ok,nok) -> 
	     if pred l e_idset then (eq::ok, nok) else (ok, EQs.add eq nok)
	  ) eqs ([], EQs.empty) in 
      let l' = 	List.fold_left (fun s (_, l_idset, _) -> L.union s l_idset) l ok in
      let res' = List.fold_left (fun r (eq, _, _) -> eq::r) res ok in
      let eqs' = nok in
      schedul_rec res' l' eqs'
    end
  in
  let eqs = List.fold_left 
    (fun acc eq -> 
       let (left_set, expr_set) = ident_of_eq eq in
       EQs.add (eq, left_set, expr_set) acc) EQs.empty eqs in
  let l = List.fold_left (fun acc id -> L.add id acc) L.empty inputs in
  schedul_rec [] l eqs
    

let check_atomicite eq = true (* TODO? *)



(* 
   Parcours de eq_list, et pour chaque eq :
   si Assert -> vérifier que sur un seul ident, puis regarder si param_in ou param_out
   si FBY -> vérifier que c'est : C -> pre X . Puis céer registre x, type, C.
   Vérifier Atomicité: en parcourant et en recréant l'arbre? 
*)

let folder main_node = 

  let registres = ref [] in
  let pre = ref [] in
  let post = ref [] in
  let eqs = ref [] in

  let rec fold eq_list =
    match eq_list with
      | [] -> ()
      | eq::l -> 
	  begin 
	match eq with 
	  | P_Eq (l, r) -> 
	      begin 
		match r with
		  | PE_Fby (a, b) -> let (reg, new_eq) = handle_register main_node eq in
		    registres := reg:: !registres;
		    eqs := new_eq:: !eqs
		  | _ -> eqs := (plp_to_nlp l, p_expr_to_n_expr r):: !eqs
	      end
	  | P_Assert expr ->  
	      match (handle_assert main_node expr) with
	      | Post (id, typ, e) -> post := (id, typ, e):: !post
	      | Pre (id, typ, e) -> pre := (id, typ, e):: !pre
	  end
  in
  fold main_node.p_eqs;
  let inputs = p_decl_to_n_decl main_node.p_param_in in
  let scheduled_eqs = 
    let (id_inputs, _) = List.split inputs in 
    scheduler !eqs id_inputs in
  let node_out =
    { n_id = main_node.p_id;
      n_param_in = inputs;
      n_param_out = p_decl_to_n_decl main_node.p_param_out;
      n_vars = p_decl_to_n_decl main_node.p_vars;
      n_reg = !registres;
      n_pre = !pre;
      n_post = !post;
      n_eqs = scheduled_eqs; } in
  node_out


(* extraction du main *)
(* appel fold eq_list *)
let normalize (ast:prog) main =
  let main_node =
    try 
      List.find (fun node -> node.p_id = main) ast
    with Not_found -> assert false
  in
  folder main_node
