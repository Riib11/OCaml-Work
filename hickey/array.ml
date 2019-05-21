(* arrays are fixed-length lists *)
let a = Array.make 10 1;; (* creates a 10-length array filled by 1 *)
let a = [|1; 2; 3; 4;|];; (* array braces *)

a.(0);; (* access array element *)
a.(0) <- 2;; (* mutate array element *)
