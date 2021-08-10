open OUnit2
open Exercises

let tests = "test suite for exercises" >::: [
  "list expr 1" >:: (fun _ -> assert_equal [1;2;3;4;5] (list_expr_1 ()) ~printer:(fun v -> String.concat " " (List.map string_of_int v)));
]

let _ = run_test_tt_main tests