(* Florian Thibord  --  Projet CERCLES *)

open Ast_base
open Ast_scade
open Ast_scade_norm
open Ast_xml
open Ast_kcg
open Call_graph

type node =
    { node_name : ident;
      node_xml : xml_node_decl;
      ast_scade : p_node option;
      sees_cond : bool * bool; (* M_Consts, M_Enum *)
    }

type env_prog = ident Env.t

module Env_instances = Map.Make(
  struct
    type t = ident * ident * ident (* node name * called node name * inst id *)
    let compare = compare
  end
)

type env_instances = ident Env_instances.t

type s2b_parameters =
  { ast_scade_print : bool;
    ast_norm_print : bool;
    ast_schedeqs_print : bool;
    xml_present : bool;
    dir_output : string;
    m_consts_used : bool;
    m_enum_used : bool;
  }

type prog =
    { nodes : node list;
      call_map : nodename_calls_map;
      enum_types : kcg_enum list;
      consts : kcg_const list;
      arraytypes : xml_arraytype list;
      env_prog : env_prog;
      env_instances : env_instances;
      s2b_params : s2b_parameters;
    }

