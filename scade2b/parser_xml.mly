%{
  (* Florian Thibord  --  Projet CERCLES *)

%}


%token CHEV_IN CHEV_OUT NOEXPNODE NODEINSTANCE ROOTNODE SCADENAME
%token QUOTES EQ
%token <string> IDENT
%token EOF

%start prog
%type <(*TODO*)> prog

%%

prog :
 | node_const_list EOF { prog_builder $1 }
;

node_const_list :
 |   { [] }
 | const node_const_list { (Const $1) :: $2 }
 | NODE node_const_list { (Node $1) :: $2 }
 | FUNCTION node_const_list { (Function $1) :: $2 }
;

const :
 | CONST IDENT COLON typ EQ expr SEMICOL { {id = $2;
					    typ = $4;
					    expr = $6;} }
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
