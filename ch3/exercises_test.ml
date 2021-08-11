open OUnit2
open Exercises

let tests = "test suite for exercises" >::: [
  "list expr 1" >:: (fun _ -> assert_equal [1;2;3;4;5] (list_expr_1 ()) ~printer:(fun v -> String.concat " " (List.map string_of_int v)));
  "list expr 2" >:: (fun _ -> assert_equal [1;2;3;4;5] (list_expr_2 ()) ~printer:(fun v -> String.concat " " (List.map string_of_int v)));
  "list expr 3" >:: (fun _ -> assert_equal [1;2;3;4;5] (list_expr_3 ()) ~printer:(fun v -> String.concat " " (List.map string_of_int v)));
  "product_zero" >:: (fun _ -> assert_equal 1 (product []) ~printer:string_of_int);
  "product_one" >:: (fun _ -> assert_equal 5 (product [5]) ~printer:string_of_int);
  "product_many" >:: (fun _ -> assert_equal 10 (product [5;2]) ~printer:string_of_int);
  "concat_zero" >:: (fun _ -> assert_equal "" (concat []) ~printer:(fun v -> v));
  "concat_one" >:: (fun _ -> assert_equal "james" (concat ["james"]) ~printer:(fun v -> v));
  "concat_many" >:: (fun _ -> assert_equal "jamesjoyce" (concat ["james";"joyce"]) ~printer:(fun v -> v));
  "patterns 1 false" >:: (fun _ -> assert_equal false (patterns_1 ["blue";"bigred"]) ~printer:string_of_bool);
  "patterns 1 true" >:: (fun _ -> assert_equal true (patterns_1 ["bigred";"blue"]) ~printer:string_of_bool);
  "patterns_2 false" >:: (fun _ -> assert_equal false (patterns_2 [1;2;3]) ~printer:string_of_bool);
  "patterns_2 true_2" >:: (fun _ -> assert_equal true (patterns_2 [1;2]) ~printer:string_of_bool);
  "patterns_2 true_4" >:: (fun _ -> assert_equal true (patterns_2 [1;2;3;4]) ~printer:string_of_bool);
]

let _ = run_test_tt_main tests