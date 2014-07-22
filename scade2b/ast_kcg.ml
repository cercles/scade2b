(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == ast_kcg.ml                                                            == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

open Ast_base
open Ast_scade
open Ast_repr_b


type kcg_enum = 
  { p_enum_id : ident;
    p_enum_list : ident list;
  }

type kcg_const = 
  { c_id : ident;
    c_typ : p_type;
    c_expr : p_expression;
  }
 
type b_const =
  Const_Base of ident * base_type * b_expression
| Const_Fun of ident * base_type * b_expression list * b_expression

module T_Node = Map.Make(
  struct
    type t = ident
    let compare = compare
  end
)

type t_node_map = string T_Node.t

type t_prog = 
  { node_map : t_node_map;
    const_list : kcg_const list;
    enum_list : kcg_enum list;
  }
