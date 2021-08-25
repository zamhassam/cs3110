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
  if x = [] then
    1.0
  else
    List.fold_left ( *. ) 1.0 x

let product_right (x: float list) : float =
  if x = [] then
    1.0
  else
    List.fold_right ( *. ) x 1.0