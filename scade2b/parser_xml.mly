%{
  (* Florian Thibord  --  Projet CERCLES *)

  open Utils

  let remove_package_name s =
    try 
      let index = String.index s ':' in
      if s.[index+1] = ':' then 
	String.sub s (index+2) ((String.length s) - (index+2))
      else s
    with 
    | Not_found -> s
    | Invalid_argument e -> Printf.printf "\n!Warning : invalid argument in node package name"; s
      
%}

%token NOEXPNODE NODEINSTANCE ROOTNODE SCADENAME MODEL
%token UNUSED
%token EQ SLASH CHEV_IN CHEV_OUT 
%token <string> VALUE
%token <string> IDENT
%token EOF

%start model
%type <Utils.xml_prog> model



%%

model :
 | CHEV_IN MODEL option_list CHEV_OUT balise_model_list EOF 
     { $5 }
;

balise_model_list :
 | CHEV_IN SLASH MODEL CHEV_OUT { XML_prog.empty }
 | CHEV_IN NOEXPNODE option_list CHEV_OUT balise_node_list balise_model_list
     { XML_prog.add (List.hd $3) $5 $6 }
 | CHEV_IN ROOTNODE option_list CHEV_OUT balise_node_list balise_model_list
     { XML_prog.add (List.hd $3) $5 $6 }
 | balise_dummy balise_model_list { $2 }
;

balise_node_list :
 | CHEV_IN SLASH NOEXPNODE CHEV_OUT { [] }
 | CHEV_IN SLASH ROOTNODE CHEV_OUT { [] }
 | CHEV_IN NODEINSTANCE option_list CHEV_OUT balise_dummy balise_node_list
     { {node_name = (List.hd $3); params_m = None} :: $6 }
 | CHEV_IN NODEINSTANCE option_list SLASH CHEV_OUT balise_node_list
     { {node_name = (List.hd $3); params_m = None} :: $6 }
 | balise_dummy balise_node_list { $2 }
;

/*
balise_dummy_list :
 |   { () }
 | balise_dummy balise_dummy_list { $2 }
;*/

balise_dummy :
 | CHEV_IN SLASH NODEINSTANCE CHEV_OUT { () }
 | CHEV_IN UNUSED option_list CHEV_OUT { () }
 | CHEV_IN UNUSED option_list SLASH CHEV_OUT { () }
 | CHEV_IN SLASH UNUSED CHEV_OUT { () }
;

option_list :
 |   { [] }
 | SCADENAME EQ VALUE option_list { (remove_package_name $3) :: $4 }
 | IDENT EQ VALUE option_list { $4 }
;

%%
