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


