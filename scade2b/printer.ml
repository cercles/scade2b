open Ast_base
open Format

let print_bid ppt id =
  fprintf ppt "%s" id

let rec print_idlist_comma ppt = function
  | [] -> ()
  | [id] -> fprintf ppt "%a" print_bid id
  | id::l -> fprintf ppt "%a, %a" print_bid id print_idlist_comma l

let print_value ppt = function
  | Bool b -> fprintf ppt "%s" (if b then "TRUE" else "FALSE")
  | Int i -> fprintf ppt "%d" i
  | Float f -> fprintf ppt "%f" f

