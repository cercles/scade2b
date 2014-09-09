(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == ast_repr_b.ml                                                         == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

open Ast_base

type b_expression =
  BE_Ident of ident
| BE_Value of value
| BE_Op_Arith1 of op_arith1 * b_expression
| BE_Op_Arith2 of op_arith2 * b_expression * b_expression
| BE_Array of array_expr
| BE_Pred of b_predicate

and b_predicate =
| BP_Op_Logic of op_logic * b_predicate * b_predicate
| BP_Op_Relat of op_relat * b_expression * b_expression
| BP_Not of b_predicate
| BP_Expr of b_expression

and array_expr =
  BA_Def of b_expression list
| BA_Caret of b_expression * b_expression
| BA_Concat of b_expression * b_expression
| BA_Slice of ident * (b_expression * b_expression) list
| BA_Index of ident * b_expression list
| BA_Reverse of ident

type left_part =
  BLP_Ident of ident
| BLP_Tuple of ident list

type alternative =
  { alt_lp: left_part;
    alt_cond: b_expression;
    alt_then: b_expression;
    alt_else: b_expression;
  }

type call =
  { call_lp: left_part;
    call_id: ident;
    call_params: b_expression list;
    call_instance: ident;
  }

type simpl =
  { simpl_lp: left_part;
    simpl_expr: b_expression;
  }

type substitution = 
  Simpl of simpl 
| Call of call 
| Alt of alternative

type set =
  base_type * b_expression list option

type imports = 
  { b_import_name : ident; 
    b_params_expr : b_expression list option; 
    b_instance_id : ident;
  } 

type b_elt =
  { name : ident;
    m_params : ident list;
    m_see_const : bool;
    m_see_enum : bool;
    m_imports : imports list;
    m_constraints : (ident * set * b_expression option) list;
    m_concrete_vars : ident list;
    m_invariant : (ident * set * b_expression option) list;
    m_initialisation : (ident * b_expression) list;
    op_in_params : ident list;
    op_out_params : ident list;
    m_vars : ident list;
    abs_pre_condition : (ident * set * b_expression option) list;
    abs_post_condition : (ident * set * b_expression option) list;
    imp_substitutions : substitution list;
  }
