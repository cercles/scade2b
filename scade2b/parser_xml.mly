%{
  (* Florian Thibord  --  Projet CERCLES *)

  open Utils

%}

%token <string * string>OPTION
%token NOEXPNODE NODEINSTANCE ROOTNODE SCADENAME MODEL
%token QUOTES EQ SLASH CHEV_IN CHEV_OUT 
%token <string> IDENT
%token EOF

%start model
%type <Utils.xml_prog> model



%%

model :
 | CHEV_IN MODEL option_list CHEV_OUT balise_model_list CHEV_IN SLASH MODEL CHEV_OUT EOF 
     { $5 }
;

balise_model_list :
 |   { XML_prog.empty }
 | CHEV_IN NOEXPNODE option_list CHEV_OUT balise_node_list CHEV_IN SLASH NOEXPNODE CHEV_OUT balise_model_list
     { XML_prog.add (List.hd $3) $5 $10 }
 | CHEV_IN ROOTNODE option_list CHEV_OUT balise_node_list CHEV_IN SLASH ROOTNODE CHEV_OUT balise_model_list
     { XML_prog.add (List.hd $3) $5 $10 }
 | balise_dummy balise_model_list { $2 }
;

balise_node_list :
 |   { [] }
 | CHEV_IN NODEINSTANCE option_list CHEV_OUT balise_dummy_list CHEV_IN SLASH NODEINSTANCE CHEV_OUT balise_node_list
     { (List.hd $3) :: $10 }
 | CHEV_IN NODEINSTANCE option_list SLASH CHEV_OUT balise_node_list
     { (List.hd $3) :: $6 }
 | balise_dummy balise_node_list { $2 }
;

balise_dummy_list :
 |   { () }
 | balise_dummy balise_dummy_list { () }
;

balise_dummy :
 | CHEV_IN IDENT option_list CHEV_OUT { () }
 | CHEV_IN IDENT option_list SLASH CHEV_OUT { () }
 | CHEV_IN SLASH IDENT CHEV_OUT { () }
;

option_list :
 |   { [] }
 | SCADENAME EQ QUOTES IDENT QUOTES option_list { $4 :: $6 }
 | OPTION option_list { $2 }
;

%%
