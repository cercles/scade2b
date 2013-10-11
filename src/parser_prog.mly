%{
  (* Florian Thibord  --  Projet CERCLES *)

  open Ast_repr
  open Ast_base


  module Map_node = Map.Make(
    struct
      type t = ident
      let compare = compare
    end
  )

  type map_node = string Map_node.t

  (* Check if all the types in the list are the same. Used to check array coherence *)
  let check_type list =
    let typ = List.hd list in
    List.iter
      (fun t -> if t <> typ then raise Parsing.Parse_error else ()) list;
    typ

%}


%token NODE TEL CONST
%token LPAREN RPAREN LBRACKET RBRACKET COLON SEMICOL COMMA
%token CARET 
%token T_BOOL T_INT T_REAL
%token <bool> BOOL
%token <int> INT
%token <float> REAL
%token <string> IDENT
%token EOF

%left CARET 

%start prog
%type (* TO CHANGE *) 

%%

prog :
 | node_const_list EOF { $1 }
;

node_const_list :
 |   { }
 | node node_const_list { }
 | const node_const_list { }
;

node :
 | NODE IDENT TEXT TEL semi_opt {  }
;

const :
 | CONST IDENT COLON typ EQ expr SEMICOL {  }
;

id_list :
 | IDENT { [$1] }
 | IDENT COMMA id_list { $1 :: $3 }
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

array_expr :
 | LBRACKET expr_list RBRACKET { PA_Def $2 } /* OK POUR TABLEAU MULTI-DIM */
 | expr CARET expr { PA_Caret ($1, $3) }
;

expr_list :
 |   { [] }
 | expr { [$1] }
 | expr COMMA expr_list { $1 :: $3 }
;

semi_opt :
 |   { () }
 | SEMICOL { () }
;

%%
