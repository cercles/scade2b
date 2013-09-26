(* Florian Thibord  --  Projet CERCLES *)

open Ast_base

type b_expression =
  BE_Ident of ident
| BE_Value of value
| BE_Op_Arith of op_arith * b_expression list
| BE_Op_Logic of op_logic * b_expression list
| BE_Array of array_expr

and array_expr =
  BA_Def of b_expression list
| BA_Caret of b_expression * b_expression
| BA_Concat of b_expression * b_expression
| BA_Slice of ident * (b_expression * b_expression) list
| BA_Index of ident * b_expression list

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
  }

type op_base =
  { op_lp: left_part;
    op_expr: b_expression;
  }

type registre =
  { reg_lpid: ident;
    reg_val: b_expression;
  }

type substitution =
  Alternative of alternative
| Call of call
| Op_Base of op_base

type initialisation =
  ident * b_expression

type condition =
  Base_no_expr of ident * base_type * ident
| Fun_no_expr of ident * base_type * b_expression list * ident
| Base_expr of ident * base_type * b_expression * ident
| Fun_expr of ident * base_type * b_expression list * b_expression * ident

type op_decl =
  { id: ident;
    param_in: ident list;
    param_out: ident list;
  }

type impl_operation =
  { op_decl: op_decl;
    vars: ident list;
    op_1: substitution list;
    op_2: registre list;
  }

type b_impl =
  { name: ident;
    refines: ident;
    sees: ident list;
    imports: ident list;
    concrete_variables: ident list;
    invariant: condition list;
    initialisation: initialisation list;
    operation: impl_operation;
  }

type abst_operation =
  { abstop_decl: op_decl;
    abstop_pre: condition list;
    abstop_post: condition list;
  }

type b_abst =
  { machine: ident;
    abst_sees: ident list;
    abst_operation: abst_operation;
  }

type prog =
  { machine_abstraite: b_abst;
    implementation: b_impl;
  }
