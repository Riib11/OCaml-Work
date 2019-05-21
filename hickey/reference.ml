(* imperitive style in ocaml *)

(*
let factorial i =
    let j = ref 1 in
    for k := 2 to i do
        j := !j * k
    done;
    !j
;;
*)

let factorial i =
    let j = ref 1 in
    let i = ref i in
    while !i > 0 do
        j := !j * !i;
        i := !i - 1
    done;
    !j
;;

let f_was_called = false

let f x =
    f_was_called = true;;
