%{
  (* Florian Thibord  --  Projet CERCLES *)

  open Ast_xml

  let make_opt value option = 
    match option with
      | "scadeName" -> Some (ScadeName (Xml_utils.remove_package_name value))
      | "instName" -> Some (InstName value)
      | "targetName" -> Some (TargetName value)
      | "targetType" -> Some (TargetType value)
      | "cellType" -> Some (CellType value)
      | "size" -> Some (Size value)
      | _ -> None
	  
%}

%token MODEL ROOTNODE NOEXPNODE NODEINSTANCE INPUT OUTPUT LOCAL ARRAYTYPE
%token EQ SLASH CHEV_IN CHEV_OUT
%token <string> VALUE
%token <string> IDENT
%token EOF

%start model
%type <Ast_xml.xml_prog> model

%%

  model :
| CHEV_IN MODEL option_list CHEV_OUT balise_model_list EOF
      { $5 }
  ;

  balise_model_list :
| CHEV_IN SLASH MODEL CHEV_OUT { [] }
| CHEV_IN NOEXPNODE option_list CHEV_OUT balise_node_list balise_model_list
      { (Node ($3, $5)) :: $6 }
| CHEV_IN ROOTNODE option_list CHEV_OUT balise_node_list balise_model_list
	  { (Root ($3, $5)) :: $6 }
| CHEV_IN ARRAYTYPE option_list SLASH CHEV_OUT balise_model_list
	      { (ArrayType $3) :: $6 }
| balise_dummy balise_model_list { $2 }
  ;

  balise_node_list :
| CHEV_IN SLASH NOEXPNODE CHEV_OUT { [] }
| CHEV_IN SLASH ROOTNODE CHEV_OUT { [] }
| CHEV_IN NODEINSTANCE option_list CHEV_OUT balise_dummy balise_node_list
      { (NodeInstance $3) :: $6 }
| CHEV_IN NODEINSTANCE option_list SLASH CHEV_OUT balise_node_list
	  { (NodeInstance $3) :: $6 }
| CHEV_IN INPUT option_list SLASH CHEV_OUT balise_node_list 
	      { (Input $3) :: $6 }
| CHEV_IN OUTPUT option_list SLASH CHEV_OUT balise_node_list
		  { (Output $3) :: $6 }
| CHEV_IN LOCAL option_list SLASH CHEV_OUT balise_node_list
		      { (Local $3) :: $6 }
| balise_dummy balise_node_list { $2 }
  ;

  option_list :
|   { [] }
| IDENT EQ VALUE option_list 
	{ match make_opt $3 $1 with
	    | Some a -> a :: $4
	    | None -> $4 
	}
  ;

  balise_dummy :
| CHEV_IN SLASH NODEINSTANCE CHEV_OUT { () }
| CHEV_IN SLASH IDENT CHEV_OUT { () }
| CHEV_IN IDENT option_list CHEV_OUT { () }
| CHEV_IN IDENT option_list SLASH CHEV_OUT { () }
  ;

%%
