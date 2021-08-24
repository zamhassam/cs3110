let double x = 2*x
let square x = x*x
let twice f x = f (f x)
let quad = twice double
let fourth = twice square

let rec repeat (f: 'a -> 'a) (n: int) (x: 'a) : 'a =
  match n with
  | 0 -> x
  | _ -> repeat f (n - 1) (f x)