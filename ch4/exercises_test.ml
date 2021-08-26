open OUnit2
open Exercises
(* open Printf *)

let print_list to_string v = "[" ^ String.concat ";" (List.map to_string v) ^ "]"

let tests = "test suite for exercises" >::: [
  "repeat_0" >:: (fun _ -> assert_equal 4 (repeat double 0 4) ~printer:string_of_int);
  "repeat_2" >:: (fun _ -> assert_equal 16 (repeat double 2 4) ~printer:string_of_int);
  "product_left_empty" >:: (fun _ -> assert_equal 1.0 (product_left []) ~printer:string_of_float);
  "product_left_1" >:: (fun _ -> assert_equal 5.0 (product_left [5.0]) ~printer:string_of_float);
  "product_right_empty" >:: (fun _ -> assert_equal 1.0 (product_right []) ~printer:string_of_float);
  "product_right_1" >:: (fun _ -> assert_equal 5.0 (product_right [5.0]) ~printer:string_of_float);
  "cliplist_map" >:: (fun _ -> assert_equal [0;0;2;4;6;8;10;10;10] (cliplist_map [-2;0;2;4;6;8;10;12;14]) ~printer:(print_list string_of_int));
  "cliplist_rec" >:: (fun _ -> assert_equal [0;0;2;4;6;8;10;10;10] (cliplist_rec [-2;0;2;4;6;8;10;12;14]) ~printer:(print_list string_of_int));
  "sum_cube_odd" >:: (fun _ -> assert_equal ((1*1*1) + (3*3*3) + (5*5*5)) (sum_cube_odd 5) ~printer:string_of_int);
  "sum_cube_odd_pipeline" >:: (fun _ -> assert_equal ((1*1*1) + (3*3*3) + (5*5*5)) (sum_cube_odd_pipeline 5) ~printer:string_of_int);
]

let _ = run_test_tt_main tests