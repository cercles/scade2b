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

let comp_tests dirs =
  "scade2b">:::
  List.map (fun s ->
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
      match exp_output with
      | Some spec -> assert_equal ~printer:(fun s -> s) spec (Buffer.contents buf)
      | None -> ()
      )
    ) dirs

let main () =
  let tests = find_tests () in
  run_test_tt_main (comp_tests tests)

let _ = main ()
