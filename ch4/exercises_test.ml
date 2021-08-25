open OUnit2
open Exercises
(* open Printf *)

let tests = "test suite for exercises" >::: [
  "repeat_0" >:: (fun _ -> assert_equal 4 (repeat double 0 4) ~printer:string_of_int);
  "repeat_2" >:: (fun _ -> assert_equal 16 (repeat double 2 4) ~printer:string_of_int);
  "product_left_empty" >:: (fun _ -> assert_equal 1.0 (product_left []) ~printer:string_of_float);
  "product_left_1" >:: (fun _ -> assert_equal 5.0 (product_left [5.0]) ~printer:string_of_float);
  "product_right_empty" >:: (fun _ -> assert_equal 1.0 (product_right []) ~printer:string_of_float);
  "product_right_1" >:: (fun _ -> assert_equal 5.0 (product_right [5.0]) ~printer:string_of_float);
]

let _ = run_test_tt_main tests