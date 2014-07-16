(* Florian Thibord  --  Projet CERCLES *)

open Ast_repr_b
open Ast_scade_norm
open Ast_scade
open Ast_base
open Ast_kcg
open Ast_prog


(**************** IDENT COLLISION FUNCTIONS ********************)

exception Underscore of string
exception Reserved of string
exception Character of string
exception Collision of string

let reserved_b_words = ["ABSTRACT_CONSTANTS"; "ABSTRACT_VARIABLES"; "ANY"; "ASSERT"; "ASSERTIONS"; "BE"; "BEGIN"; "BOOL"; "CASE"; "CHOICE"; "CONCRETE_CONSTANTS"; "CONCRETE_VARIABLES"; "CONSTANTS"; "CONSTRAINTS"; "DEFINITIONS"; "DO"; "EITHER"; "ELSE"; "ELSIF"; "END"; "EXTENDS"; "FALSE"; "FIN"; "FIN1"; "IF"; "IMPLEMENTATION"; "IMPORTS"; "IN"; "INCLUDES"; "INITIALISATION"; "INT"; "INTEGER"; "INTER"; "INVARIANT"; "LET"; "LOCAL_OPERATIONS"; "MACHINE"; "MAXINT"; "MININT"; "NAT"; "NAT1"; "NATURAL"; "NATURAL1"; "OF"; "OPERATIONS"; "OR"; "PI"; "POW"; "POW1"; "PRE"; "PROMOTES"; "PROPERTIES"; "REFINES"; "REFINEMENT"; "SEES"; "SELECT"; "SETS"; "SIGMA"; "STRING"; "THEN"; "TRUE"; "UNION"; "USES"; "VALUES"; "VAR"; "VARIANT"; "VARIABLES"; "WHEN"; "WHERE"; "WHILE"; "arity"; "bin"; "bool"; "btree"; "card"; "closure"; "closure1"; "conc"; "const"; "dom"; "father"; "first"; "fnc"; "front"; "id"; "infix"; "inter"; "iseq"; "iseq1"; "iterate"; "last"; "left"; "max"; "min"; "mirror"; "mod"; "not"; "or"; "perm"; "postfix"; "pred"; "prefix"; "prj1"; "prj2"; "ran"; "rank"; "rec"; "rel"; "rev"; "right"; "seq"; "seq1"; "size"; "sizet"; "skip"; "son"; "sons"; "struct"; "subtree"; "succ"; "tail"; "top"; "tree"; "union"]

let is_reserved ident = List.mem ident reserved_b_words

let is_b_compliant ident = 
  if String.length ident < 2 then raise (Character ident)
  else if ident.[0] = '_' then raise (Underscore ident)
  else if is_reserved ident then raise (Reserved ident)
  else true

let switch_underscore s = 
  try 
    (String.sub s 1 ((String.length s)-1));
  with 
  | Invalid_argument _ -> "a"^s

let double_character s = s^s 

let add_underscore s = s^"_"


(********************* LOCAL ENV BUILD *************************)

let check_no_collision ident env =
  if (Env.exists (fun _ (bid, _, _) -> ident = bid) env) then raise (Collision ident)
  else true

let make_b_ident ident env = 
  let new_ident = ref "" in
  let ident_check ident =
    try 
      (is_b_compliant ident) && (check_no_collision ident env)
    with
    | Underscore s ->
        new_ident := switch_underscore s; false
    | Character s ->
        new_ident := double_character s; false
    | Reserved s ->
        new_ident := add_underscore s; false
    | Collision s ->
        new_ident := add_underscore s; false
  in
  let rec ident_generator ident =
    if ident_check ident then ident else ident_generator !new_ident
  in
  ident_generator ident

let global_collision ident global_ids =
  List.mem ident global_ids

let make_dummy_cond ids =
  List.map (fun id -> (id, NT_Base T_Int, None)) ids

(* Normaliser.normalize_node *)
let make_env vars assumes guarantees global_ids =
  (* Construction de l'environnement *)
  let vars_cond = List.map (fun (id, t) -> (id, t, None)) vars in
  let globals_cond = make_dummy_cond global_ids in (* consts, enums, node_id *)
  List.fold_left
    (fun env (ident, typ, expr) -> 
      Env.add ident ((make_b_ident ident env), typ, expr) env
    )
    Env.empty (globals_cond@assumes@guarantees@vars_cond) (* globals_cond must be first in list *)




(******************** GLOBAL ENV BUILD *************************)

(* not the same env as local (no type, no condition)*)
let check_no_collision_global ident env =
  if (Env.exists (fun _ bid -> ident = bid) env) then raise (Collision ident)
  else true

(* check_collision_global à la place de check_collision*)
let make_b_ident_global ident env = 
  let new_ident = ref "" in
  let ident_check ident =
    try 
      (is_b_compliant ident) && (check_no_collision_global ident env)
    with
    | Underscore s ->
        new_ident := switch_underscore s; false
    | Character s ->
        new_ident := double_character s; false
    | Reserved s ->
        new_ident := add_underscore s; false
    | Collision s ->
        new_ident := add_underscore s; false
  in
  let rec ident_generator ident =
    if ident_check ident then ident else ident_generator !new_ident
  in
  ident_generator ident



(******************* INSTANCES ENV BUILDER **********************)

let make_inst_ident inst =
  let node_name, import_name, inst_id = inst in
  try
    (String.sub node_name 0 1) ^ (String.sub import_name 0 1) ^ inst_id 
  with Invalid_argument _ -> "a"^inst_id

let check_no_collision_instances ident env =
  if (Env_instances.exists (fun _ bid -> ident = bid) env) then raise (Collision ident)
  else true

let make_instance_bid instance env = 
  let new_ident = ref "" in
  let ident_check ident =
    try 
      (is_b_compliant ident) && (check_no_collision_instances ident env)
    with
    | Underscore s ->
        new_ident := switch_underscore s; false
    | Character s ->
        new_ident := double_character s; false
    | Reserved s ->
        new_ident := add_underscore s; false
    | Collision s ->
        new_ident := add_underscore s; false
  in
  let rec ident_generator ident =
    if ident_check ident then ident else ident_generator !new_ident
  in
  let ident = make_inst_ident instance in
  ident_generator ident



(********************  ERROR MACHINE ENV  *************************)

(*** babsterror_generator ***)
let make_b_ident_without_env ident = 
  let new_ident = ref "" in
  let ident_check ident =
    try 
      (is_b_compliant ident)
    with
    | Underscore s ->
        new_ident := switch_underscore s; false
    | Character s ->
        new_ident := double_character s; false
    | Reserved s ->
        new_ident := add_underscore s; false
  in
  let rec ident_generator ident =
    if ident_check ident then ident else ident_generator !new_ident
  in
  ident_generator ident



(******************** LAMBDA UNCAPITALIZE *************************)

(* Reg_ini_input.search_input_in_reg *)
let make_params_ident env lambda_list = 
  List.fold_left (fun env lambda -> 
    let (param, typ, expr) = Env.find lambda.n_l_ident env in
    Env.add lambda.n_l_ident ((String.uncapitalize param), typ, expr) env
  ) env lambda_list

