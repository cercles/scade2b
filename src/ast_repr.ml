(* Florian Thibord  --  Projet CERCLES *)

(* Probleme des tableax multi dimension à résoudre (penser liste de tableaux)*)

open Ast_base
 
type value = 
  Bool of bool 
| Int of int 
| Float of float

type p_expression =
  PE_Ident of ident
| PE_Tuple of p_expression list
| PE_Value of value
| PE_Array of p_array_expr
| PE_App of ident * p_expression list
| PE_Bop of bop * p_expression * p_expression
| PE_Unop of unop * p_expression
| PE_Fby of p_expression * p_expression
| PE_Pre of p_expression
| PE_If of p_expression * p_expression * p_expression

and p_array_expr = 
  Slice of ident * p_expression * p_expression (* marche aussi pour une seule case *)
| Concat of p_array_expr * p_array_expr (* verifier qu'il s'agit de 2 tableaux *)

type plp_item =
  PLP_Ident of ident
| PLP_Array of ident * p_expression * p_expression (* Array of ident * array_smthg *)
 
type p_left_part = 
  PLP_Item of plp_item
| PLP_Tuple of plp_item list

type p_equation = 
| P_Eq of p_left_part * p_expression
| P_Assert of p_expression

type p_typ =
  T_Bool 
| T_Int 
| T_Float
| T_Array of p_typ * p_expression

type p_decl = ident * p_typ

type p_node = 
  {  p_id: ident; 
     p_param_in: decl list; 
     p_param_out: decl list; 
     p_vars: decl list; 
     p_eqs: p_equation list; }

type prog = p_node list

