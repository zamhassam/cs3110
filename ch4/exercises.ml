let double x = 2*x
let square x = x*x
let twice f x = f (f x)
let quad = twice double
let fourth = twice square

let rec repeat (f: 'a -> 'a) (n: int) (x: 'a) : 'a =
  match n with
  | 0 -> x
  | _ -> repeat f (n - 1) (f x)

let product_left (x: float list) : float =
  match x with
  | [] -> 1.0
  | _ -> List.fold_left ( *. ) 1.0 x

let product_right (x: float list) : float =
  match x with
  | [] -> 1.0
  | _ -> List.fold_right ( *. ) x 1.0

let clip n = 
  if n < 0 then 0 
  else if n > 10 then 10
  else n

let cliplist_map (x: int list) : int list =
  List.map clip x

let rec cliplist_rec (x: int list) : int list =
  match x with
  | [] -> []
  | hd::tl -> (clip hd) :: (cliplist_rec tl)

let (--) i j =
  let rec from i j l =
    if i>j then l
    else from i (j-1) (j::l)
    in from i j []

let sum_cube_odd (n: int) : int =
  List.fold_right (fun x y -> x + y) 
                  (List.map (fun x -> x * x * x) 
                            (List.filter (fun x -> (x mod 2) != 0)
                                         (0 -- n)))
                  0

let sum_cube_odd_pipeline (n: int) : int =
  let is_odd x = (x mod 2) != 0 in
  let cube x = x * x * x in
  (0 -- n) 
  |> List.filter is_odd
  |> List.map cube
  |> List.fold_left (+) 0

let rec exists_rec (f: ('a -> bool)) (lst: 'a list) : bool =
  match lst with
  | [] -> false
  | hd::tl -> (f hd) || (exists_rec f tl)

let exists_fold (f: ('a -> bool)) (lst: 'a list) : bool =
  List.fold_right (fun x acc -> acc || (f x)) lst false

let exists_lib (f: ('a -> bool)) (lst: 'a list) : bool =
  List.exists f lst