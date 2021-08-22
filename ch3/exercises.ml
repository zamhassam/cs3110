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

let rec max_hp (pokemon_lst: pokemon list) : pokemon option =
  match pokemon_lst with
  | [] -> None
  | hd::tl -> 
    match (max_hp tl) with
    | None -> Some hd
    | Some p -> if p.hp > hd.hp then Some p else Some hd

let is_before (before: int * int * int) (after: int * int * int) : bool =
  let (before_year, before_month, before_date) = before in
    let (after_year, after_month, after_date) = after in
      if before_year < after_year then
        true
      else if before_year > after_year then
        false
      else if before_month < after_month then
        true
      else if before_month > after_month then
        false
      else if before_date < after_date then
        true
      else
        false


let rec earliest (dates: (int * int * int) list) : (int * int * int) option =
  match dates with
  | [] -> None
  | hd::tl ->
    match (earliest tl) with
    | None -> Some hd
    | Some d -> if (is_before hd d) then Some hd else Some d

(* insert a binding from key k to value v in association list d *)
let insert k v d = (k,v)::d

(* find the value v to which key k is bound, if any, in the assocation list *)
let rec lookup k = function
  | [] -> None
  | (k',v)::t -> if k=k' then Some v else lookup k t

let assoc_list () =
  let assoc_lst = insert 3 "three" (insert 2 "two" (insert 1 "one" [])) in
    lookup 2 assoc_lst

type suit = Club | Spade | Heart | Diamond

type rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King

type card = { suit : suit ; rank : rank }

let all_cards () =
  [
    {suit = Heart; rank = Three},
    {suit = Club; rank = Jack}
  ]

type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x:int) : sign = 
  if x = 0 then
    Zero
  else if x > 0 then
    Pos
  else
    Neg

let quadrant : int * int -> quad option = fun (x,y) ->
  match ((sign x), (sign y)) with
    | (Pos, Pos) -> Some I
    | (Neg, Pos) -> Some II
    | (Neg, Neg) -> Some III
    | (Pos, Neg) -> Some IV
    | _ -> None

type 'a tree = 
  | Leaf 
  | Node of 'a * 'a tree * 'a tree

let rec depth_rec (cur: 'a tree) (champion: int) (cur_depth: int) : int =
  match cur with
  | Leaf -> if cur_depth > champion then cur_depth else champion
  | Node (_, left, right) -> 
    max 
    (depth_rec left champion (cur_depth + 1)) 
    (depth_rec right champion (cur_depth + 1))

let depth (x: 'a tree) : int =
  depth_rec x 0 0

let rec same_shape (a: 'a tree) (b: 'a tree) : bool =
  match (a, b) with
  | (Leaf, Leaf) -> true
  | (Node (val1, a_left, a_right), Node (val2, b_left, b_right)) -> 
    val1 = val2 
    && (same_shape a_left b_left) 
    && (same_shape a_right b_right)
  | (Node (_, _, _), Leaf) -> false
  | (Leaf, Node (_, _, _)) -> false

let rec list_max_rec lst champion =
  match lst with
  | [] -> champion
  | hd::tl -> max champion (list_max_rec tl (max hd champion))

let list_max (lst: int list) : int =
  match lst with
    | [] -> raise (Failure "empty")
    | hd::tl -> list_max_rec tl hd

let list_max_string (lst: int list) : string =
  match lst with
    | [] -> "empty"
    | hd::tl -> string_of_int (list_max_rec tl hd)