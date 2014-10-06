%{
(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == parser_kcg.mly                                                        == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

  open Ast_scade
  open Ast_base
  open Ast_kcg


  (* Check if all the types in the list are the same. Used to check array coherence *)
  let check_type list =
    let typ = List.hd list in
    List.iter
      (fun t -> if t <> typ then raise Parsing.Parse_error else ()) list;
    typ

  (* Used for the distinction between Slice and Index 
  if e_list is [(a,b), (c,d), (e,f)] with a=b and c=d and e=f then it's an index [a,c,e] else it's a slice*)
  let handle_slice id elist =
    if Utils.a_b_list_equals elist then
      let (l, _) = List.split elist in
      PA_Index (id, l)
    else
      PA_Slice (id, elist)

  type node_const_enum = 
      Node of (string * string) 
    | Function of (string * string) 
    | Const of kcg_const
    | Enum of kcg_enum

  let prog_builder node_const_enum_list =
    let (node_map, cst_list, enu_list) = 
      List.fold_left (fun (acc_node, acc_const, acc_enum) n_c_e -> 
	match  n_c_e with
	| Node (id, node) -> (T_Node.add id node acc_node, acc_const, acc_enum)
	| Function (id, node) -> (T_Node.add id node acc_node, acc_const, acc_enum)
	| Const cst -> (acc_node, cst :: acc_const, acc_enum)
	| Enum e -> (acc_node, acc_const, e :: acc_enum)
      ) (T_Node.empty, [], []) node_const_enum_list
    in
    { node_map = node_map; const_list = cst_list; enum_list = enu_list }

%}


%token <string * string> NODE FUNCTION
%token CONST
%token TYPE
%token ENUM
%token IMPORTED
%token <char> CHAR
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COLON SEMICOL COMMA DOT DOTDOT
%token CARET EQ
%token T_BOOL T_INT T_REAL
%token <bool> BOOL
%token <int> INT
%token <float> REAL
%token <string> IDENT
%token EOF

%left CARET 

%start prog
%type <Ast_kcg.t_prog> prog

%%

prog :
 | node_const_enum_list EOF { prog_builder $1 }
;

node_const_enum_list :
 |   { [] }
 | const_block node_const_enum_list { $1 @ $2 }
 | NODE node_const_enum_list { (Node $1) :: $2 }
 | FUNCTION node_const_enum_list { (Function $1) :: $2 }
 | enum_block node_const_enum_list { $1 @ $2 }
 | error node_const_enum_list { $2 }
;

enum_block:
| TYPE enum_decl_list { $2 }
;

enum_decl_list:
| enum_decl { [$1] }
| enum_decl enum_decl_list { $1::$2 }
;

enum_decl:
| imported_opt IDENT EQ
  ENUM LBRACE id_list RBRACE SEMICOL { Enum { p_enum_id = $2
                                            ; p_enum_list = $6
                                            }
                                     }
;

imported_opt:
|          { }
| IMPORTED { }
;

id_list :
 | IDENT { [$1] } 
 | IDENT COMMA id_list { $1 :: $3 }
;

const_block:
| CONST const_decl_list { $2 }
;

const_decl_list:
| const_decl { [$1] }
| const_decl const_decl_list { $1::$2 }
;

const_decl:
| IDENT COLON typ EQ expr SEMICOL { Const { c_id = $1; c_typ = $3; c_expr = $5; } }
;

typ :
 | base_type { PT_Base $1 }
 | array_type { $1 }
;

base_type :
 | T_BOOL { T_Bool }
 | T_INT { T_Int }
 | T_REAL { T_Float }
;

/* 2 facons de déclarer un tableau */
array_type :
 | LBRACKET typ_list RBRACKET { let type_list = $2 in
				let typ = check_type type_list in
				PT_Array (typ, PE_Value (Int (List.length type_list))) }
 | typ CARET expr { PT_Array ($1, $3) }
;

typ_list :
 | typ { [$1] }
 | typ COMMA typ_list { $1 :: $3 }
;

expr :
 | INT { PE_Value (Int $1) }
 | BOOL { PE_Value (Bool $1) }
 | REAL { PE_Value (Float $1) }
 | array_expr { PE_Array $1 }
;

expr_list :
 |   { [] }
 | expr { [$1] }
 | expr COMMA expr_list { $1 :: $3 }
;

array_expr :
 | LBRACKET expr_list RBRACKET { PA_Def $2 } 
 | IDENT array_list { handle_slice $1 $2 }
 | expr CARET expr { PA_Caret ($1, $3) }
;

array_list :
 | LBRACKET expr RBRACKET { [($2, $2)] }
 | LBRACKET expr DOTDOT expr RBRACKET { [($2, $4)] }
 | LBRACKET expr RBRACKET array_list { ($2, $2) :: $4 }
 | LBRACKET expr DOTDOT expr RBRACKET array_list { ($2, $4) :: $6 }
;

%%

