(* Florian Thibord  --  Projet CERCLES *)


open Ast_base

(* AST en sortie de parser_xml *)

type xml_option =
    ScadeName of ident 
  | InstName of ident
  | TargetName of ident
  | TargetType of ident
  | CellType of ident
  | Size of ident

type xml_options = xml_option list

type xml_node_balise = 
    Input of xml_options 
  | Output of xml_options
  | Local of xml_options
  | NodeInstance of xml_options

type xml_nbs = xml_node_balise list

type xml_prog_balise =
    Node of xml_options * xml_nbs
  | Root of xml_options * xml_nbs
  | ArrayType of xml_options

type xml_prog_list = xml_prog_balise list

(* AST en sortie de Enum_to_Records *)

type xml_arraytype =
    { name : ident;
      celltype : ident;
      size : int;
    }

type xml_instance =
    { inst_name : ident;
      inst_id : ident;
    }

type xml_var_decl =
    { var_id : ident;
      var_type : ident;
    }

type xml_locals =
    { local_id : ident;
      local_target : ident;
    }

type xml_node_decl =
    { xml_node_name : ident;
      is_root : bool;
      ins : xml_var_decl list;
      outs : xml_var_decl list;
      locals : xml_locals list;
      instances : xml_instance list;
    }

type xml_prog = 
    { xml_nodes : xml_node_decl list;
      xml_arraytypes : xml_arraytype list;
    }
