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

let rec take_tl_rec n input output =
  if n = 0 || input = [] then 
    output
  else
    take_tl_rec (n - 1) (List.tl input) ((List.hd input)::output)

let rec take (n: int) (x: 'a list) =
  List.rev (take_tl_rec n x [])

let rec drop_tl_rec n input output =
  if input = [] then
    output
  else if n = 0 then
    drop_tl_rec 0 (List.tl input) (List.hd input::output)
  else
    drop_tl_rec (n - 1) (List.tl input) output

let rec drop (n: int) (x: 'a list) =
  List.rev (drop_tl_rec n x [])