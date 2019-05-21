let inc = fun i -> i + i;;

let sum = fun i j -> i + j;;

let sumcurry = fun i -> (fun j -> i + j);;
let add1 = sumcurry 1;;
let add1' = sum 1;; (* same as above *)

(* alternative syntax for function *)
let sum i j = i + j;;
