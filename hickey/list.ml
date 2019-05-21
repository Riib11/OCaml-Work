let xs = [1,2,3];;

(* the above doesn't work like you think -
 * it treats the 1,2,3 as a tuple rather than 3 list elements.
 * so instead, write one of the following equivalent expressions: *)

let xs = 1::2::3::[];;
let xs = [1;2;3];;

(* pattern matching on lists *)

let rec last = function
      []    -> []
    | h::[] -> [h]
    | h::t  -> last t
;;

let rec map f = function
      []   -> []
    | h::t -> f h :: map f t
;;


let rec zip = function
      ([],    []   ) -> []
    | (x::xs, y::ys) -> (x,y) :: zip (xs,ys)
;;


