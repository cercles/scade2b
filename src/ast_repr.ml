(* Florian Thibord  --  Projet CERCLES *)

(* Probleme des tableaux multi dimension à résoudre (penser liste de tableaux) *)

open Ast_base
 

type p_expression =
  PE_Ident of ident
| PE_Tuple of p_expression list
| PE_Value of value
| PE_Array of p_array_expr
| PE_App of ident * p_expression list
| PE_Bop of bop * p_expression * p_expression
| PE_Unop of unop * p_expression
| PE_Fby of p_expression * p_expression
| PE_Pre of p_expression
| PE_If of p_expression * p_expression * p_expression
| PE_Sharp of p_expression list

and p_array_expr = 
  PA_Def of p_expression list (* Initaialisation avec [e1, e2, ...] (ex: [3,4,5] ) *)
| PA_Caret of p_expression * p_expression (* Initialisation avec e1^ e2 (ex: false^4) *)
| PA_Concat of p_array_expr * p_array_expr
| PA_Slice of ident * (p_expression * p_expression) list
(* list pour gerer la multi dimension. slice est aussi utilisé pour l'index, dans ce cas la borne est unitaire *)

type plp_item =
  PLP_Ident of ident
| PLP_Array of ident * p_expression * p_expression (* Array of ident * array_smthg *)
 
type p_left_part = 
  PLP_Item of plp_item
| PLP_Tuple of plp_item list

type p_equation = 
  P_Eq of p_left_part * p_expression
| P_Assert of p_expression

type p_type =
  PT_Base of base_type
| PT_Array of p_type * p_expression

type p_decl = ident * p_type

type p_node = 
  {  p_id: ident; 
     p_param_in: p_decl list; 
     p_param_out: p_decl list; 
     p_vars: p_decl list; 
     p_eqs: p_equation list; }
    
type prog = p_node list
