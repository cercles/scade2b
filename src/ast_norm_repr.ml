(* Florian Thibord  --  Projet CERCLES *)

open Ast_base

type n_expression =
  NE_Ident of ident
| NE_Tuple of n_expression list
| NE_Value of value
| NE_Array of n_array_expr
| NE_App of ident * n_expression list
| NE_Bop of bop * n_expression * n_expression
| NE_Unop of unop * n_expression
| NE_If of n_expression * n_expression * n_expression
| NE_Sharp of n_expression list

and n_array_expr = 
  NA_Def of n_expression list
| NA_Caret of n_expression * n_expression
| NA_Concat of n_expression * n_expression
| NA_Slice of ident * (n_expression * n_expression) list
| NA_Index of ident * n_expression list
 
type n_left_part = 
  NLP_Ident of ident
| NLP_Tuple of ident list
 
type n_equation = n_left_part * n_expression

type n_type =
  NT_Base of base_type
| NT_Array of n_type * n_expression

type n_condition = ident * n_type * n_expression

type n_registre = 
  { reg_id: ident;
    reg_type: n_type;
    reg_ini: n_expression;
    reg_var: n_expression;
  }

type n_decl = ident * n_type

type n_node = 
  {  n_id: ident; 
     n_param_in: n_decl list; 
     n_param_out: n_decl list; 
     n_vars: n_decl list; 
     n_reg: n_registre list;
     n_pre: n_condition list;
     n_post: n_condition list;
     n_eqs: n_equation list; }


(* type nlp_item = *)
(*   NLP_Ident of ident *)
(* | NLP_Array of ident * n_expression * n_expression (\* Array of ident * array_smthg *\) *)
 
(* type n_left_part =  *)
(*   NLP_Item of nlp_item *)
(* | NLP_Tuple of nlp_item list *)
 
