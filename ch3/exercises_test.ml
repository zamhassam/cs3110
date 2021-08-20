open OUnit2
open Exercises
open Printf

let print_list to_string v = "[" ^ String.concat ";" (List.map to_string v) ^ "]"

let print_option to_string v =
  match v with
  | None -> "None"
  | Some x -> "Some " ^ (to_string x)

let tests = "test suite for exercises" >::: [
  "list expr 1" >:: (fun _ -> assert_equal [1;2;3;4;5] (list_expr_1 ()) ~printer:(print_list string_of_int));
  "list expr 2" >:: (fun _ -> assert_equal [1;2;3;4;5] (list_expr_2 ()) ~printer:(print_list string_of_int));
  "list expr 3" >:: (fun _ -> assert_equal [1;2;3;4;5] (list_expr_3 ()) ~printer:(print_list string_of_int));
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
  "patterns_3_false" >:: (fun _ -> assert_equal false (patterns_3 [1;2;3]) ~printer:string_of_bool);
  "patterns_3_true" >:: (fun _ -> assert_equal true (patterns_3 [2;2;3]) ~printer:string_of_bool);
  "library_1_invalid_length" >:: (fun _ -> assert_equal 0 (library_1 [1;2;3;4]) ~printer:string_of_int);
  "library_1_valid_length" >:: (fun _ -> assert_equal 5 (library_1 [1;2;3;4;5;6]) ~printer:string_of_int);
  "library_2" >:: (fun _ -> assert_equal [6;5;4;3;2;1] (library_2 [2;1;3;6;5;4]) ~printer:(print_list string_of_int));
  "library_puzzle_1" >:: (fun _ -> assert_equal 4 (library_puzzle_1 [2;1;3;6;5;4]) ~printer:string_of_int);
  "library_puzzle_2_false" >:: (fun _ -> assert_equal false (library_puzzle_2 [2;1;3;6;5;4]) ~printer:string_of_bool);
  "library_puzzle_2_true" >:: (fun _ -> assert_equal true (library_puzzle_2 [2;1;3;0;5;4]) ~printer:string_of_bool);
  "take_happy_path" >:: (fun _ -> assert_equal [1;2;3] (take 3 [1;2;3;4;5]) ~printer:(print_list string_of_int));
  "take_list_smaller_than_n" >:: (fun _ -> assert_equal [1;2;3;4;5] (take 6 [1;2;3;4;5]) ~printer:(print_list string_of_int));
  "drop_happy_path" >:: (fun _ -> assert_equal [4;5] (drop 3 [1;2;3;4;5]) ~printer:(print_list string_of_int));
  "drop_list_smaller_than_n" >:: (fun _ -> assert_equal [] (drop 7 [1;2;3;4;5]) ~printer:(print_list string_of_int));
  "is_unimodal_nil" >:: (fun _ -> assert_equal true (is_unimodal []) ~printer:string_of_bool);
  "is_unimodal_constant" >:: (fun _ -> assert_equal true (is_unimodal [5;5;5]) ~printer:string_of_bool);
  "is_unimodal_true_on_way_up" >:: (fun _ -> assert_equal true (is_unimodal [2;3;4;5]) ~printer:string_of_bool);
  "is_unimodal_false_on_way_up" >:: (fun _ -> assert_equal false (is_unimodal [2;3;4;5;4;6]) ~printer:string_of_bool);
  "is_unimodal_true_on_way_down" >:: (fun _ -> assert_equal true (is_unimodal [2;3;4;5;4;3]) ~printer:string_of_bool);
  "is_unimodal_false_on_way_down" >:: (fun _ -> assert_equal false (is_unimodal [2;3;4;5;4;3;4;3]) ~printer:string_of_bool);
  "is_unimodal_true_only_down" >:: (fun _ -> assert_equal true (is_unimodal [6;5;4;3;2;1]) ~printer:string_of_bool);
  "add_to_each_1" >:: (fun _ -> assert_equal [[1]] (add_to_each 1 [[]]) ~printer:(print_list (print_list string_of_int)));
  "add_to_each_2" >:: (fun _ -> assert_equal [[2];[2;1]] (add_to_each 2 [[];[1]]) ~printer:(print_list (print_list string_of_int)));
  "add_to_each_3" >:: (fun _ -> assert_equal [[3];[3;2];[3;2;1]] (add_to_each 3 [[];[2];[2;1]]) ~printer:(print_list (print_list string_of_int)));
  "powerset_1" >:: (fun _ -> assert_equal  [[1];[]] (powerset [1]) ~printer:(print_list (print_list string_of_int)));
  "powerset_2" >:: (fun _ -> assert_equal  [[1;2];[1];[2];[]] (powerset [1;2]) ~printer:(print_list (print_list string_of_int)));
  "powerset_3" >:: (fun _ -> assert_equal  [[1;2;3];[1;2];[1;3];[1];[2;3];[2];[3];[]] (powerset [1;2;3]) ~printer:(print_list (print_list string_of_int)));
  "safe_hd_none" >:: (fun _ -> assert_equal None (safe_hd []));
  "safe_hd_some" >:: (fun _ -> assert_equal (Some 1) (safe_hd [1]));
  "safe_tl_none" >:: (fun _ -> assert_equal None (safe_tl []));
  "safe_tl_some" >:: (fun _ -> assert_equal (Some 1) (safe_tl [3;2;1]));
  "max_hp_none" >:: (fun _ -> assert_equal (None) (max_hp []));
  "max_hp_some_1" >:: (fun _ -> assert_equal (Some {name="charizard";hp=77;ptype=Fire}) (max_hp [{name="charizard";hp=77;ptype=Fire}]));
  "max_hp_some_3" >:: (fun _ -> assert_equal (Some {name="charizard";hp=77;ptype=Fire}) (max_hp [{name="squirtle";hp=55;ptype=Fire};{name="charizard";hp=77;ptype=Fire};{name="geodude";hp=44;ptype=Fire}]));
  "is_before_false" >:: (fun _ -> assert_equal false (is_before (2021, 2, 28) (2021, 1, 13)) ~printer:string_of_bool);
  "is_before_true" >:: (fun _ -> assert_equal true (is_before (2021, 1, 13) (2021, 2, 28)) ~printer:string_of_bool);
  "is_before_true" >:: (fun _ -> assert_equal true (is_before (2021, 1, 13) (2021, 3, 8)) ~printer:string_of_bool);
  "earliest_none" >:: (fun _ -> assert_equal None (earliest []));
  "earliest_some" >:: (fun _ -> assert_equal (Some (2021, 1, 13)) (earliest [(2021, 2, 28);(2021, 1, 13);(2021, 3, 8)]) ~printer:(print_option (fun x -> let (year, month, date) = x in sprintf "%d,%d,%d" year month date)));
]

let _ = run_test_tt_main tests