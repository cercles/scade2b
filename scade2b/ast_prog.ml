(* Florian Thibord  --  Projet CERCLES *)

open Ast_base
open Ast_scade
open Ast_scade_norm
open Ast_xml
open Ast_kcg

type node =
    { node_name : ident;
      node_xml : xml_node_decl;
      ast_scade : p_node option;
    }

type env_prog = ident Env.t

module Env_instances = Map.Make(
  struct
    type t = ident * ident * ident (* node name * import name * inst id*)
    let compare = compare
  end
)

type env_instances = ident Env_instances.t

type prog =
    { nodes : node list;
      enum_types : kcg_enum list;
      consts : kcg_const list;
      arraytypes : xml_arraytype list;
      env_prog : env_prog;
      env_instances : env_instances;
    }
