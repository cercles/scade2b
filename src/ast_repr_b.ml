(* Florian Thibord  --  Projet CERCLES *)

open Ast_base

type expression =
  BE_Ident of ident
| BE_Tuple of expression list
| BE_Value of value
| BE_Bop of bop * expression * expression
| BE_Unop of unop * expression
| BE_Sharp of expression list
| BE_Array of array_expr

and array_expr =
  BA_Def of expression list
| BA_Caret of expression * expression
| BA_Concat of expression * expression
| BA_Slice of ident * (expression * expression) list
| BA_Index of ident * expression list

(* TODO: remplacer bt_array en bt_fun, car on utilise plus des arrays mais des fonctions. *)

type b_type = 
  BT_Base of base_type
| BT_Array of b_type * expression

type left_part =
  BLP_Ident of ident
| BLP_Tuple of ident list

type alternative =
  { alt_lp: left_part;
    alt_cond: expression;
    alt_then: expression;
    alt_else: expression;
  }

type fonction =
  { fun_lp: left_part;
    fun_id: ident;
    fun_params: expression list;
  }

type operation =
  { op_lp: left_part;
    op_expr: expression;
  }

type registre =
  { reg_lpid: ident;
    reg_val: expression;
  }

type equation =
  Alternative of alternative
| Fonction of fonction
| Operation of operation

type initialisation =
  ident * expression

type condition =
  ident * b_type * expression

type op_decl =
  { id: ident;
    param_in: ident list;
    param_out: ident list;
  }

type operations =
  { op_decl: op_decl;
    vars: ident list;
    op_1: equation list;
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
    operations: operations;
  }

type sig_operation =
  { sigop_decl: op_decl;
    sigop_pre: condition list;
    sigop_post: condition list;
  }

type b_sig =
  { machine: ident;
    sig_sees: ident list;
    sig_operation: sig_operation;
  }

module Env = Map.Make(
  struct
    type t = ident
    let compare = compare
  end
)

type env = (ident * Ast_repr_norm.n_expression option) Env.t

type prog =
  { env: env;
    signature: b_sig;
    implementation: b_impl;
  }
