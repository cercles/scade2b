(* Florian Thibord  --  Projet CERCLES *)

open Ast_base

type n_expression =
  NE_Ident of ident
| NE_Tuple of n_expression list
| NE_Value of value
| NE_Bop of bop * n_expression * n_expression
| NE_Unop of unop * n_expression
| NE_Sharp of n_expression list
| NE_Array of n_array_expr

and n_array_expr = 
  NA_Def of n_expression list
| NA_Caret of n_expression * n_expression
| NA_Concat of n_expression * n_expression
| NA_Slice of ident * (n_expression * n_expression) list
| NA_Index of ident * n_expression list

type n_type =
  NT_Base of base_type
| NT_Array of n_type * n_expression
 
type n_left_part = 
  NLP_Ident of ident
| NLP_Tuple of ident list

type n_alternative =
  { alt_lp: n_left_part;
    alt_cond: n_expression;
    alt_then: n_expression;
    alt_else: n_expression;
  }

type n_fonction =
  { fun_lp: n_left_part;
    fun_id: ident;
    fun_params: n_expression list;
  }

type n_operation =
  { op_lp: n_left_part;
    op_expr: n_expression;
  }

type n_registre = 
  { reg_lp: n_left_part;
    reg_ini: n_expression;
    reg_val: n_expression;
    reg_type: n_type;
  }
 
type n_equation = 
  N_Alternative of n_alternative
| N_Fonction of n_fonction
| N_Operation of n_operation
| N_Registre of n_registre

type n_condition = 
  ident * n_type * n_expression

type n_decl = 
  ident * n_type

type n_node = 
  { n_id: ident; 
    n_param_in: n_decl list; 
    n_param_out: n_decl list; 
    n_vars: n_decl list; 
    n_pre: n_condition list;
    n_post: n_condition list;
    n_eqs: n_equation list; 
  }
