(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == ast_xml.ml                                                            == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

open Ast_base

(* AST en sortie de parser_xml *)

type xml_option_name =
  | ScadeName
  | InstName
  | TargetName
  | TargetType
  | CellType
  | Size

type xml_option = xml_option_name * ident

type xml_options = xml_option list

type xml_node_balise = 
    Input of xml_options 
  | Output of xml_options
  | Local of xml_options
  | NodeInstance of xml_options

type xml_nbs = xml_node_balise list

type xml_prog_balise =
    Node of xml_options * xml_nbs

type xml_prog_list = xml_prog_balise list

(* AST en sortie de Enum_to_Records *)

type xml_instances =
    { inst_name : ident;
      inst_id : ident;
    }

type xml_var_decl =
    { var_id : ident;
      var_type : ident;
    }

type xml_node_decl =
    { name : ident;
      in_params : xml_var_decl list;
      out_params : xml_var_decl list;
      instances : xml_instances list;
    }

type xml_prog = 
    { xml_nodes : xml_node_decl list;
    }
