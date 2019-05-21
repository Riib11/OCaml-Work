(* 'exn' is short for 'exception' *)

(* let f = raise (Failure "abort! abort!") *)

(* let _ = 1 + raise (Failure ":(") * 21 *)

(* try except *)

let head ls = match ls with
| [] -> raise (Failure "head of empty list")
| x::_ -> x

(* evaluates to `head 1` if no exception, otherwise pattern matches exception *)
let head_default ls default =
  try head ls with
    Failure _ -> default;;

let x = head_default [0;1;2] (-1);;
let x = head [0;1;2];;

(* define custom exceptions *)

exception SomeException;;
