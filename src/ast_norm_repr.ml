(* Florian Thibord  --  Projet CERCLES *)

open Ast_base

type n_left_part = 
  NLP_Ident of ident
| NLP_Tuple of ident list

(* AJOUTER UN TYPE POUR METTRE EN EVIDENCE L'ATOMICITE *)
type n_expression =
  NE_Ident of ident
| NE_Tuple of n_expression list
| NE_Value of value
| NE_App of ident * n_expression list
| NE_Bop of bop * n_expression * n_expression
| NE_Unop of unop * n_expression
| NE_If of n_expression * n_expression * n_expression
 
type n_equation = n_left_part * n_expression

type n_condition = ident * typ * n_expression

type n_registre = ident * typ * value

type n_node = 
  {  n_id: ident; 
     n_param_in: decl list; 
     n_param_out: decl list; 
     n_vars: decl list; 
     n_reg: n_registre list;
     n_pre: n_condition list;
     n_post: n_condition list;
     n_eqs: n_equation list; }

