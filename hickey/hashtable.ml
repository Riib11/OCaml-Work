let random_numbers = [| 0x04a018c6; 0x5ba7b0f2; 0x04dcf08b; 0x1e5a22cc; 0x2523b9ea |]
let random_length = Array.length random_numbers

type hash_info = { mutable hash_index:int; mutable hash_value:int }

let hash_char info c =
  let i = Char.code c in
  let index = (info.hash_index + i + 1) mod random_length in
  info.hash_value <- (info.hash_value * 3) lxor random_numbers.(index);
  info.hash_index <- index
;;

let hash s =
  let info = { hash_index=0; hash_value=0 } in
  for i = 0 to String.length s do
    hash_char info s.[i]
  done;
  info.hash_value
;;

(* examples of some entries *)

type 'a hash_entry = { key:string; value:'a }
type 'a hash_table = 'a hash_entry list array

let create () = Array.make 101 []

let add table key value =
  let index = (hash key) mod (Array.length table) in
  table.(index) <- { key=key; value=value } :: table.(index)

let f = function
  | 0 -> 1
  | _ -> 0


let rec find_entry key = function
  | { key=key'; value=value } :: _ when key' = key -> value
  | _ :: entries -> find_entry key entries
  | _ -> raise Not_found

let find table key =
  let index = (hash key) mod (Array.length table) in
  find_entry key table.(index)
