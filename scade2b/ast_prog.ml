(* Florian Thibord  --  Projet CERCLES *)


open Ast_base
open Ast_repr

type t_const = 
  { id : ident;
    typ : p_type;
    expr : p_expression;
  }
 
module T_Node = Map.Make(
  struct
    type t = ident
    let compare = compare
  end
)

type t_node_map = string T_Node.t

type t_prog = 
  { node_map : t_node_map;
    const_list : t_const list;
  }
