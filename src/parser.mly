%{
  (* Florian Thibord  --  Projet CERCLES *)

  open Ast_repr
  open Ast_base

  (* Check if all the types in the list are the same. Used to check array coherence *)
  let check_type list =
    let typ = List.hd list in
    List.iter (fun t -> if t <> typ then raise Parsing.Parse_error else ()) list;
    typ

%}

%token NODE RETURNS LET TEL VAR CONST ASSERT
%token IF THEN ELSE
%token PRE FBY 
%token PLUS MINUS MULT DIV DIV_INT MOD 
%token EQ NEQ INF INFEQ SUP SUPEQ
%token AND OR NOT XOR SHARP
%token LPAREN RPAREN LBRACKET RBRACKET COLON SEMICOL COMMA DOTDOT CARET CONCAT  /* these three last are array ops */
%token T_BOOL T_INT T_REAL
%token <bool> BOOL
%token <int> INT
%token <float> REAL
%token <string> IDENT
%token EOF

%nonassoc THEN
%nonassoc ELSE
%right FBY
%left OR
%left XOR
%left AND
%left EQ NEQ INF INFEQ SUP SUPEQ 
%left CARET CONCAT
%left PLUS MINUS    
%left MULT DIV DIV_INT MOD
%nonassoc uminus
%nonassoc NOT PRE

%start prog
%type <Ast_repr.prog> prog

%%

prog : 
 | node_list EOF { $1 }
;

node_list :
 |   { [] }
 | node node_list { $1::$2 }
;

node :
 | NODE IDENT LPAREN decl RPAREN RETURNS LPAREN decl RPAREN SEMICOL 
   var_decl 
   LET eq_list TEL semi_opt
   { { p_id = $2; p_param_in = $4; p_param_out = $8; p_vars = $11; p_eqs = $13; } }
;

var_decl :
 |   { [] } 
 | VAR decl { $2 }
;

decl :
 | id_list COLON typ semi_opt { let typ = $3 in
				List.map (fun id -> (id, typ)) $1 }
 | id_list COLON typ SEMICOL decl { let typ = $3 in
				    (List.map (fun id -> (id, typ)) $1) @ $5 }
;

id_list :
 | IDENT { [$1] }
 | IDENT COMMA id_list { $1::$3 }
;

typ :
 | T_BOOL { T_Bool }
 | T_INT { T_Int } 
 | T_REAL { T_Float }
 | LBRACKET typ_list RBRACKET { let type_list = $2 in
				let typ = check_type type_list in
				T_Array (typ, (List.length type_list)) }
 | typ CARET INT { T_Array ($1, $3) }
;

typ_list :
 | typ { [$1] }
 | typ COMMA typ_list { $1::$3 }
;

eq_list :
 | ASSERT expr SEMICOL { [P_Assert $2] }
 | ASSERT expr SEMICOL eq_list { (P_Assert $2)::$4 }
 | left_part EQ expr SEMICOL { [P_Eq ($1, $3)] }
 | left_part EQ expr SEMICOL eq_list { (P_Eq ($1, $3))::$5 }
;

left_part :
 | IDENT { PLP_Ident $1 }
 | LPAREN IDENT COMMA id_list RPAREN { PLP_Tuple ($2::$4) }
;

expr :
 | IDENT { PE_Ident $1 }
 | INT { PE_Value (Int $1) }
 | BOOL { PE_Value (Bool $1) } 
 | REAL { PE_Value (Float $1) }
/* | array_expr { $1 } */
 | expr PLUS expr { PE_Bop (Op_add, $1, $3) }
 | expr MINUS expr { PE_Bop (Op_sub, $1, $3) }
 | expr MULT expr { PE_Bop (Op_mul, $1, $3) }
 | expr DIV expr { PE_Bop (Op_div_f, $1, $3) }
 | expr DIV_INT expr { PE_Bop (Op_div, $1, $3) }
 | expr MOD expr { PE_Bop (Op_mod, $1, $3) }
 | expr EQ expr { PE_Bop (Op_eq, $1, $3) }
 | expr NEQ expr { PE_Bop (Op_neq, $1, $3) }
 | expr INF expr { PE_Bop (Op_lt, $1, $3) }
 | expr INFEQ expr { PE_Bop (Op_le, $1, $3) }
 | expr SUP expr { PE_Bop (Op_gt, $1, $3) }
 | expr SUPEQ expr { PE_Bop (Op_ge, $1, $3) }
 | expr AND expr { PE_Bop (Op_and, $1, $3) }
 | expr OR expr { PE_Bop (Op_or, $1, $3) }
 | expr XOR expr { PE_Bop (Op_xor, $1, $3) }
 | MINUS expr { PE_Unop (Op_minus, $2) }
 | NOT expr { PE_Unop (Op_not, $2) }
 | expr FBY expr { PE_Fby ($1, $3) }
 | PRE expr { PE_Pre $2 }
 | IF expr THEN expr ELSE expr { PE_If ($2, $4, $6) }
 | IDENT LPAREN expr_list RPAREN { PE_App ($1, $3) }
 | LPAREN expr COMMA expr_list RPAREN { PE_Tuple ($2::$4) }
 | LPAREN expr RPAREN { $2 }

/* ajouter sharp */
;

expr_list :
 |   { [] }
 | expr { [$1] }
 | expr COMMA expr_list { $1::$3 }
;

/*
array_expr :
 | {()}
    AFAIRE 
    LBRACKET expr_list RBRACKET { PE_Value (Array $2) } 
    ajouter filtrage et concat 
;
*/

semi_opt :
 |   { () }
 | SEMICOL { () }
;

%%
