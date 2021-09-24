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
  "exists_rec_empty" >:: (fun _ -> assert_equal false (exists_rec (fun x -> x = 3) []) ~printer:string_of_bool);
  "exists_rec_false" >:: (fun _ -> assert_equal false (exists_rec (fun x -> x = 3) [1;2;4]) ~printer:string_of_bool);
  "exists_rec_true" >:: (fun _ -> assert_equal true (exists_rec (fun x -> x = 3) [1;2;3]) ~printer:string_of_bool);
  "exists_fold_empty" >:: (fun _ -> assert_equal false (exists_fold (fun x -> x = 3) []) ~printer:string_of_bool);
  "exists_fold_false" >:: (fun _ -> assert_equal false (exists_fold (fun x -> x = 3) [1;2;4]) ~printer:string_of_bool);
  "exists_fold_true" >:: (fun _ -> assert_equal true (exists_fold (fun x -> x = 3) [1;2;3]) ~printer:string_of_bool);
  "exists_lib_empty" >:: (fun _ -> assert_equal false (exists_lib (fun x -> x = 3) []) ~printer:string_of_bool);
  "exists_lib_false" >:: (fun _ -> assert_equal false (exists_lib (fun x -> x = 3) [1;2;4]) ~printer:string_of_bool);
  "exists_lib_true" >:: (fun _ -> assert_equal true (exists_lib (fun x -> x = 3) [1;2;3]) ~printer:string_of_bool);
  "budget_r_none_remaining" >:: (fun _ -> assert_equal 0. (budget_r [1.;2.;3.] 6.) ~printer:string_of_float);
  "budget_r" >:: (fun _ -> assert_equal 0.5 (budget_r [1.;2.;3.] 6.5) ~printer:string_of_float);
  "budget_l_none_remaining" >:: (fun _ -> assert_equal 0. (budget_l [1.;2.;3.] 6.) ~printer:string_of_float);
  "budget_l" >:: (fun _ -> assert_equal 0.5 (budget_l [1.;2.;3.] 6.5) ~printer:string_of_float);
  "uncurry" >:: (fun _ -> assert_equal [1;2;3;4;5] ((uncurry List.append) ([1;2;3], [4;5])));
  "curry" >:: (fun _ -> assert_equal [1;2;3;4;5] ((curry (uncurry List.append)) [1;2;3] [4;5]));
  "map_composition" >:: (fun _ -> assert_equal [1;3;5;7] (map_compose (fun x -> x - 1) (fun y -> y * 2) [1;2;3;4]));
  "greater_than_3" >:: (fun _ -> assert_equal ["james";"freddie";"joyce"] (greater_than_3 ["jo";"james";"tom";"freddie";"joyce";"kai"]) ~printer:(print_list (fun x -> x)));
  "add_1" >:: (fun _ -> assert_equal [2.;3.;4.] (add_1 [1.;2.;3.]) ~printer:(print_list string_of_float));
  "join" >:: (fun _ -> assert_equal "hi,bye" (join ["hi";"bye"] ",") ~printer:(fun x -> x));
]

let _ = run_test_tt_main tests