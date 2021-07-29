(* int *)
(* string *)

42 * 10;;
3.14 /. 2.0;;
let rec pow m n =
  if n = 0.0 then 1.0 else m *. pow m (n -. 1.0) in
  pow 2.0 3.0;;

42 = 42;;
"hi" = "hi";;
"hi" == "hi";;

assert true;;
assert false;;
assert (2110 <> 3110);;

if 2 > 1 then 42 else 7;;

let double x = x * 2 in double 7;;

let cube x = x *. x *. x in cube 3.;;
let sign_of x =
  if x > 0 then 1
  else if x < 0 then -1
  else 0 in sign_of (-5);;
let area_of_circle radius =
  Float.pi *. (radius *. radius) in area_of_circle 2.;;

assert ((let rms x y = sqrt(((x*.x)+.(y*.y)/.2.)) in rms 2. 10.) > 7.3);;

let valid_date d m =
  if ((m = "Jan" || m = "Mar" || m = "May" || m = "Jul" || m = "Aug" || m = "Oct" || m = "Dec") && d >= 1 && d <= 31) then true else if ((m = "Apr" || m = "June" || m = "Sep" || m = "Nov") && d >=1 && d <= 30) then true else if (m = "Feb" && d >= 1 && d <= 28) then true else false in valid_date 31 "Nov";;

let rec fib n =
  if n = 0 then 0
  else if n = 1 then 1
  else fib(n - 1) + fib(n - 2) in fib 4;;

let fast_fib n =
  let rec fib_helper i minus2 minus1 = if i = 0 then minus2 + minus1 else fib_helper (i - 1) minus1 (minus1 + minus2) in
    if n = 0 then 0
    else if n = 1 then 1
    else fib_helper (n - 2) 0 1 in  fast_fib 4;;

let rec divide (n:float) (d:float) =
  if d > n then 0 else (1 + divide(n -. d) d) in divide 10. 3.;;

let (+/.) (x:float) (y:float) =
  (x +. y) /. 2. in 1. +/. 2.;;

let rec print_int_list = function 
| [] -> () 
| h::t -> print_int h;
          print_newline ();
          print_int_list t in print_int_list [1;2;3];;

let print_int_list' lst = 
  List.iter (fun x -> 
      print_int x;
      print_newline ();
    ) lst in print_int_list' [1;2;3];;