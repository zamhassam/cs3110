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

let rec is_unimodal_rec (on_way_up: bool) (prev: int) (x: int list) =
  if on_way_up then
    match x with
    | hd::tl -> 
      if hd >= prev then
        (* keep going up *)
        is_unimodal_rec on_way_up hd tl 
      else
        (* flip to going down *)
        is_unimodal_rec false hd tl
    | [] -> true
  else
    match x with
    | hd::tl -> if hd <= prev then is_unimodal_rec on_way_up hd tl else false
    | [] -> true

let is_unimodal (x: int list) =
  match x with
  | hd::tl -> is_unimodal_rec true hd x
  | [] -> true

let rec add_to_each (x: int) (lst: int list list) =
  match lst with
  | hd::tl -> (x::hd)::(add_to_each x tl)
  | [] -> []

let rec powerset (x: int list) =
  match x with
  | hd::tl -> let powerset_tail = (powerset tl) in (add_to_each hd powerset_tail) @ powerset_tail
  | [] -> [[]]

(* let rec print_int_list = function
  | [] -> ()
  | ht::t -> 
    print_int ht;
    print_newline ();
    print_int_list t; *)

(* let print_int_list_iter (lst: int list) =
    List.iter (
      fun lst -> (
        print_int lst; 
        print_newline ()
      )
    ) lst *)

type student = { first_name: string; last_name: string; gpa: float}

let student_1 = {first_name = "John"; last_name = "Smith"; gpa = 1.2}

let student_2 = function
  | {first_name; last_name; gpa} -> (first_name, last_name)

let student_3 first_name last_name gpa =
  {first_name = first_name; last_name = last_name; gpa = gpa}

type poketype = Normal | Fire | Water

type pokemon = { name: string; hp: int; ptype: poketype }

let charizard = {name = "charizard"; hp = 78; ptype = Fire}

let squirtle = {name = "squirtle"; hp = 44; ptype = Water}

let safe_hd (x: 'a list) : 'a option =
  match x with
  | hd::tl -> Some hd
  | [] -> None

let rec safe_tl (x: 'a list) : 'a option =
  match x with
  | hd::[] -> Some hd
  | hd::tl -> safe_tl tl
  | [] -> None