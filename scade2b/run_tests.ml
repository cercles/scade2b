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

let issuffix sfx s =
  let sfx_len = String.length sfx in
  let s_len = String.length s in
  let ext = String.sub s (s_len - sfx_len) sfx_len in
  ext = sfx

let find_tests () =
  Sys.readdir "tests"
  |> Array.to_list
  |> List.filter (issuffix ".test")
  |> List.map (fun d -> "tests/" ^ d)

let check_exec = "./_obuild/scade2b_cov/scade2b_cov.asm"

let comp_tests dirs =
  List.map (fun d ->
    (d, `Quick, fun () ->
      OUnit.assert_command check_exec [d ^ "/"];
      OUnit.assert_command "diff" ["-Nru"; d ^ "/spec" ; d ^ "/Machines_B"];
  )) dirs

let main () =
  let tests = find_tests () in
  Alcotest.run "scade2b" [("compile", comp_tests tests)]

let _ = main ()
