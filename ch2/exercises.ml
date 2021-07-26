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