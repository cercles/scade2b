(* Florian Thibord  --  Projet CERCLES *)


open Ast_base


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

type xml_prog = xml_prog_balise list



(*   PREMIER JET DE L'ENV
type xml_instance =
  { xml_inst_name : ident;
    xml_inst_id : ident;
  }

type xml_var_decl =
  { xml_var_id : ident;
    xml_var_type : ident;
  }

type xml_locals =
  { xml_local_id : ident;
    xml_local_var : ident;
  }

type xml_node_decl =
  { xml_node_name : ident;
    xml_is_root : bool;
    xml_ins : xml_var_decl list;
    xml_outs : xml_var_decl list;
    xml_locals : xml_locals list;
    xml_instances : instance list;
  }

type xml_prog =
  { xml_nodes : xml_node_decl list;
    xml_types : xml_types list;
  }
*)
