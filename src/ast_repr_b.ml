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


(* séparer les registres tuples en plusieurs registres? *)
type registre = 
  { reg_lp: left_part;
    reg_ini: expression;
    reg_val: expression;
    reg_type: b_type;
  }
 
type equation = 
  Alternative of alternative
| Fonction of fonction
| Operation of operation
| Registre of registre

type condition = 
  ident * b_type * expression

type decl = 
  ident * b_type

type b_impl =
    { implementation: ident; 
      refines: ident; 
      sees: ident list;
      imports: ident list;
      concrete_variables: ident list;
      invariant: registre list;
      initialisation: registre list;
      operations: ident list * equation list; 
    }

type b_sig =
    { machine: ident;
      sees: ident list;
      op_decl: decl list * ident * decl list
      op_pre: condition list;
      op_post: condition list;
  }

module Env = Map.Make(String);

type env = Env.t

type prog =
  {
     env: env;
     signature: b_sig;
     implementation: b_impl;
  }
