(* Florian Thibord  --  Projet CERCLES *)

open Ast_base

type arraytype =
    { name : ident;
      celltype : ident;
      size : int;
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
      local_target : ident;
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
      arraytypes : arraytype list;
    }
