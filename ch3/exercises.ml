let list_expr_1 () = [1;2;3;4;5]

let list_expr_2 () = 1::2::3::4::5::[]

let list_expr_3 () = [1] @ [2;3;4] @ [5]

let rec product = function
  | [] -> 1
  | hd::tl -> hd * (product tl)

let rec concat = function
  | [] -> ""
  | hd::tl -> hd ^ (concat tl)

let patterns_1 = function
  | "bigred"::tl -> true
  | _ -> false