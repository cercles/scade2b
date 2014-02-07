open Ast_base
open Ast_scade_norm
open Format

let env = ref None

let print_bid ppt id =
    let bid = match !env with
      | None -> id
      | Some e -> Env.find id e
    in
    fprintf ppt "%s" bid

let with_env e f =
    (match !env with
        | Some _ -> failwith "with_env: env should be None"
        | None -> ());
    env := Some e;
    let r = f () in
    env := None;
    r

let rec print_idlist_comma ppt = function
  | [] -> ()
  | [id] -> fprintf ppt "%a" print_bid id
  | id::l -> fprintf ppt "%a, %a" print_bid id print_idlist_comma l

let print_value ppt = function
  | Bool b -> fprintf ppt "%s" (if b then "TRUE" else "FALSE")
  | Int i -> fprintf ppt "%d" i
  | Float f -> fprintf ppt "%f" f

let print_op_arith1 ppt = function
  | Op_minus -> fprintf ppt "-"
  | Op_cast_real -> fprintf ppt "REAL"
  | Op_cast_int -> fprintf ppt "INT"

let print_op_arith2 ppt = function
  | Op_eq -> fprintf ppt "="
  | Op_neq -> fprintf ppt "/="
  | Op_lt -> fprintf ppt "<"
  | Op_le -> fprintf ppt "<="
  | Op_gt -> fprintf ppt ">"
  | Op_ge -> fprintf ppt ">="
  | Op_add -> fprintf ppt "+"
  | Op_sub -> fprintf ppt "-"
  | Op_mul -> fprintf ppt "*"
  | Op_div -> fprintf ppt "/"
  | Op_mod -> fprintf ppt "mod"
  | Op_div_f -> fprintf ppt "/"

let print_op_logic ppt = function
  | Op_and -> fprintf ppt "&"
  | Op_or -> fprintf ppt "or"
  | Op_xor -> assert false
