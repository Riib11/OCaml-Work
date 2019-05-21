(* labeled arguments *)
let f ~x:i ~y:j = i - j;;

f ~x:1 ~y:2 = f ~y:2 ~x:1;;

(* optional arguments *)
let g ?(x=1) y = x - y;;

