(* Florian Thibord  --  Projet CERCLES *)

open Ast_xml
open Format

let rec print_ast_xml ast file =
  fprintf (formatter_of_out_channel file) "___AST XML PRINTER___@\n@\n@\n%a@." 
    print_prog_list ast

and print_prog_list ppt = function
  | [] -> ()
  | [p] -> fprintf ppt "%a" print_prog p
  | p :: l -> fprintf ppt "%a%a" print_prog p print_prog_list l
	    
and print_prog ppt nbs = 
    match nbs with
      | Node (options, node_bs) -> fprintf ppt "%a @\n@\n" (print_node options) node_bs
      | Root (options, node_bs) -> fprintf ppt "%a @\n@\n" (print_root options) node_bs
      | ArrayType options -> fprintf ppt "%a @\n@\n" print_arraytype options

and print_node options ppt node_bs =
  fprintf ppt "Node : %a  %a"
    print_option_list options 
    print_node_list node_bs

and print_root options ppt node_bs =
  fprintf ppt "Root : %a  %a"
    print_option_list options
    print_node_list node_bs

and print_arraytype ppt options =
  fprintf ppt "ArrayType : %a"
    print_option_list options

and print_node_list ppt = function
  | [] -> ()
  | [n] -> fprintf ppt "%a" print_node_bs n
  | n :: l -> fprintf ppt "%a %a" print_node_bs n print_node_list l

and print_node_bs ppt = function
  | Input options -> fprintf ppt "@\n  Input -> %a"  print_option_list options
  | Output options -> fprintf ppt "@\n  Output -> %a" print_option_list options
  | Local options -> fprintf ppt "@\n  Local -> %a" print_option_list options
  | NodeInstance options -> fprintf ppt "@\n  NodeInstance -> %a" print_option_list options

and print_option_list ppt = function
  | [] -> ()
  | [o] -> fprintf ppt "%a" print_option o
  | o :: l -> fprintf ppt "%a %a" print_option o print_option_list l

and print_option ppt = function
  | ScadeName id -> fprintf ppt " ScadeName : %s -" id
  | InstName id -> fprintf ppt " InstName : %s -" id
  | TargetName id -> fprintf ppt " TargetName : %s -" id
  | TargetType id -> fprintf ppt " TargetType : %s -" id
  | CellType id -> fprintf ppt " CellType : %s -" id
  | Size id -> fprintf ppt " Size : %s -" id
