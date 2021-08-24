open OUnit2
open Exercises
(* open Printf *)

let tests = "test suite for exercises" >::: [
  "repeat_0" >:: (fun _ -> assert_equal 4 (repeat double 0 4) ~printer:string_of_int);
  "repeat_2" >:: (fun _ -> assert_equal 16 (repeat double 2 4) ~printer:string_of_int)
]

let _ = run_test_tt_main tests