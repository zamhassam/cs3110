open OUnit2
open Exercises

let tests = "test suite for exercises" >::: [
  "list expr 1" >:: (fun _ -> assert_equal [1;2;3;4;5] (list_expr_1 ()) ~printer:(fun v -> String.concat " " (List.map string_of_int v)));
  "list expr 2" >:: (fun _ -> assert_equal [1;2;3;4;5] (list_expr_2 ()) ~printer:(fun v -> String.concat " " (List.map string_of_int v)));
  "list expr 3" >:: (fun _ -> assert_equal [1;2;3;4;5] (list_expr_3 ()) ~printer:(fun v -> String.concat " " (List.map string_of_int v)));
  "product_zero" >:: (fun _ -> assert_equal 1 (product []) ~printer:string_of_int);
  "product_one" >:: (fun _ -> assert_equal 5 (product [5]) ~printer:string_of_int);
  "product_many" >:: (fun _ -> assert_equal 10 (product [5;2]) ~printer:string_of_int);
]

let _ = run_test_tt_main tests