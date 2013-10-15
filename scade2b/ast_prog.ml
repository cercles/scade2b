(* Florian Thibord  --  Projet CERCLES *)


open Ast_base
open Ast_repr
open Ast_repr_b

type p_const = 
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
    const_list : p_const list;
  }
