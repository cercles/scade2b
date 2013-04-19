(* Florian Thibord  --  Projet CERCLES *)

open Ast_base

type p_left_part = 
  PLP_Ident of ident
| PLP_Tuple of ident list

type p_expression =
  PE_Ident of ident
| PE_Tuple of p_expression list
| PE_Value of value
| PE_App of ident * p_expression list
| PE_Bop of bop * p_expression * p_expression
| PE_Unop of unop * p_expression
| PE_Fby of p_expression * p_expression
| PE_Pre of p_expression
| PE_If of p_expression * p_expression * p_expression
 
type p_equation = 
| P_Eq of p_left_part * p_expression
| P_Assert of p_expression

type p_node = 
  {  p_id: ident; 
     p_param_in: decl list; 
     p_param_out: decl list; 
     p_vars: decl list; 
     p_eqs: p_equation list; }

type p_prog = p_node list

