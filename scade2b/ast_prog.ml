(* Florian Thibord  --  Projet CERCLES *)

open Ast_base


type p_enum = 
  { p_enum_id : ident;
    p_enum_list : ident list;
  }

type p_const = 
  { c_id : ident;
    c_typ : p_type;
    c_expr : p_expression;
  }

type instance =
  { inst_name : ident;
    inst_id : ident;
  }

type var_decl =
  { var_id : ident;
    var_type : ident;
  }

type locals =
  { local_id : ident;
    local_var : ident;
  }

type node_decl =
  { node_name : ident;
    
    is_root : bool;
    ins : var_decl list;
    outs : var_decl list;
    locals : locals list;
    instances : instance list;
  }

type prog =
  { nodes : node list;
    enum_types : enum_types list;
    consts : const list;
  }
