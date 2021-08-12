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

let patterns_2 = function
  | h1::h2::[] -> true
  | h1::h2::h3::h4::[] -> true
  | _ -> false

let patterns_3 = function
  | h1::h2::tl -> h1 = h2
  | _ -> false

let library_1 (x : int list) = 
  match (List.nth_opt x (5 - 1)) with
  | Some n -> n
  | None -> 0

let library_2 (x : int list) = 
  List.rev (List.sort Stdlib.compare x)

let library_puzzle_1 (x: 'a list) =
  List.hd (List.rev x)

let library_puzzle_2 (x: int list) =
  List.mem 0 x

let rec take (n: int) (x: 'a list) =
  if n = 0 || x = [] then 
    []
  else 
    (List.hd x)::(take (n - 1) (List.tl x))

let rec drop (n: int) (x: 'a list) =
  if x = [] then
    []
  else if n = 0 then
    (List.hd x)::(drop 0 (List.tl x))
  else
    drop (n - 1) (List.tl x)