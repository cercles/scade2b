(* Florian Thibord  --  Projet CERCLES *)


type ident = string

type bop =
  Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge
| Op_add | Op_sub | Op_mul | Op_div | Op_mod
| Op_add_f | Op_sub_f | Op_mul_f | Op_div_f
| Op_and | Op_or | Op_xor | Op_sharp

type unop = 
  Op_not | Op_minus

(* Si on fais un type supérieur pour différencier Array des autres, les Arrays de Arrays sont plus compliqués *)
type typ =
  T_Bool 
| T_Int 
| T_Float
| T_Array of typ * int

type value = 
  Bool of bool 
| Int of int 
| Float of float
| Array of value list

type decl = ident * typ
