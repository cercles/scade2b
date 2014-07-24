(*
  +----------+              +-----------------+
  | dir/KCG/ |   scade2b    | dir/Machines_B/ |
  |          |              |                 |
  | Source   |  --------->  |     Actual      |
  |  code    |              |     output      |
  +----------+              +-----------------+
                                |
  +-----------+                 |
  | dir/spec/ |                 |
  |           |                 |
  | Expected  | -------------+  |
  |  output   |              |  |
  +-----------+              |  |
                             v  v
   \___  ___/            +-----------+
       \/                | dir.diff  |
                         |           |
     in git              |  Unified  | ----> If empty: OK
                         |   diff    |
                         +-----------+
*)

open OUnit2

(** 3.12 compat functions *)
module Compat = struct
  let (|>) x f = f x

  module String = struct
    let trim s =
      let is_space = function
        | ' ' | '\012' | '\n' | '\r' | '\t' -> true
        | _ -> false in
      let len = String.length s in
      let i = ref 0 in
      while !i < len && is_space (String.get s !i) do
        incr i
      done;
      let j = ref (len - 1) in
      while !j >= !i && is_space (String.get s !j) do
        decr j
      done;
      if !i = 0 && !j = len - 1 then
        s
      else if !j >= !i then
        String.sub s !i (!j - !i + 1)
      else
        ""

    include String
  end
end

open Compat

let issuffix sfx s =
  let sfx_len = String.length sfx in
  let s_len = String.length s in
  let ext = String.sub s (s_len - sfx_len) sfx_len in
  ext = sfx

let rec map_option f = function
  | [] -> []
  | x::xs ->
      let ys = map_option f xs in
      begin match f x with
      | Some y -> y::ys
      | None -> ys
      end

let read_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  s

(**
 * Tests can be of several types:
 *)
type test_kind =
  | TestOK of string (** success expected, compare output with reference output *)
  | TestFail of string (** failure expected, compare error message *)

let make_test d =
  let testpath = "tests/" ^ d in
  match () with
  | _ when issuffix ".test" d -> Some (TestOK testpath)
  | _ when issuffix ".fail" d -> Some (TestFail testpath)
  | _ -> None

let find_tests () =
  Sys.readdir "tests"
  |> Array.to_list
  |> map_option make_test

let check_exec = "./_obuild/scade2b_cov/scade2b_cov.asm"

type obj_coverage =
  | ObjNotCovered
  | ObjCovered

let find_objectives () =
  let lines =
    "tests/objectives.txt"
      |> read_file
      |> String.trim
      |> Str.split (Str.regexp "\n")
  in
  let objs = Hashtbl.create 0 in
  List.iter (fun str -> Hashtbl.add objs str ObjNotCovered) lines;
  objs

let all_matches pattern s =
  let rec go start =
    try
      let next = Str.search_forward pattern s start in
      let r = Str.matched_string s in
      r :: go (next+1)
    with Not_found -> []
  in
  go 0


let collect_objectives dir =
  let pattern = Str.regexp "--@.*$" in
  (dir ^ "/KCG/kcg_xml_filter_out.scade")
  |> read_file
  |> String.trim
  |> all_matches pattern
  |> List.map (fun s -> String.trim (String.sub s 3 (String.length s - 3)))

let comp_tests dirs objs =
  let update_objectives obj_list =
    List.iter (fun obj ->
      let msg = "Objective '" ^ obj ^ "' is unknown" in
      assert_equal ~msg true (Hashtbl.mem objs obj);
      Hashtbl.replace objs obj ObjCovered
    ) obj_list
  in
  let run_test s =
    let d, check_diff, exp_code = match s with
      | TestOK d -> d, true, 0
      | TestFail d -> d, false, 1
    in (d>:: fun ctxt ->
      let opts =
        try
          (d ^ "/options.txt")
          |> read_file
          |> String.trim
          |> Str.split (Str.regexp " ")
        with Sys_error _ -> []
      in
      let all_opts = opts @ [d ^ "/"] in
      let obj_of_this_test = collect_objectives d in
      let exit_code =
        let n =
          try
            (d ^ "/exitcode.txt")
            |> read_file
            |> String.trim
            |> int_of_string
          with Sys_error _ -> exp_code
        in
        Unix.WEXITED n
      in
      let buf = Buffer.create 0 in
      let foutput =
        Stream.iter (Buffer.add_char buf)
      in
      let backtrace = false in
      assert_command ~ctxt ~exit_code ~foutput ~backtrace check_exec all_opts;
      begin if check_diff then
        assert_command ~ctxt ~env:[||] "diff" ["-Nru"; d ^ "/spec" ; d ^ "/Machines_B"]
      end;
      let exp_output =
        try
          Some (read_file (d ^ "/output.txt"))
        with Sys_error _ -> None
      in
      begin match exp_output with
      | Some spec -> assert_equal ~printer:(fun s -> s) spec (Buffer.contents buf)
      | None -> () end;
      (* Everything went fine: clear objectives *)
      update_objectives obj_of_this_test
      )
  in
  let test_objs_covered = "Objectives covered">:: fun ctxt ->
    Hashtbl.iter (fun obj cov ->
      non_fatal ctxt (fun ctxt ->
        if cov <> ObjCovered then
          todo obj
      )
    ) objs
  in
  "scade2b">:::
    [ "Unit tests" >::: List.map run_test dirs
    ; "Objectives" >::: [test_objs_covered]
    ]

let perform_test test =
  let conf = OUnitConf.default () in
    OUnitCore.run_test_tt
      conf
      (OUnitLoggerStd.std_logger conf OUnitLogger.shard_default)
      OUnitRunner.sequential_runner
      OUnitChooser.simple
      test

let main () =
  let tests = find_tests () in
  let objectives = find_objectives () in
  perform_test (comp_tests tests objectives)

let _ = main ()
