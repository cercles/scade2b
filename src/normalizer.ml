(* Florian Thibord  --  Projet CERCLES *)

(* Extraction des registres, 
   Vérification que les opérations sont atomiques?
   Vérifs de bases en plus nécessaires?
   Différentier pre/post conditions *)

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

let p_decl_to_n_decl (id, p_type) = (id, p_type_to_n_type p_type)








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
  in
  ident_finder asser;
  match find_type !id (main_node.p_param_in) with
  | Some typ -> Pre (!id, p_type_to_n_type typ, p_expr_to_n_expr asser)
  | None -> match find_type id main_node.p_param_out with
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
			       eqs := eq:: !eqs
	    | _ -> eqs := (plp_to_nlp l, p_expr_to_n_expr r):: !eqs
	  end
	| P_Assert expr ->  
	  match (handle_assert main_node eq) with
	  | Post (id, typ, expr) -> post := (id, typ, expr):: !post
	  | Pre (id, typ, expr) -> pre := (id, typ, expr):: !pre
      end
  in
  fold main_node.p_eqs;

  (* APPEL DU SCHEDULER ICI POUR ORDONNANCER LES EQUATIONS !eqs *)

  let node_out = ref
    { n_id = main_node.p_id;
      n_param_in = p_decl_to_n_decl main_node.p_param_in;
      n_param_out = p_decl_to_n_decl main_node.p_param_out;
      n_vars = p_decl_to_n_decl main_node.p_vars;
      n_reg = !registres;
      n_pre = !pre;
      n_post = !post;
      n_eqs = !eqs; } in
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
  
