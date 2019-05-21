type expression =
| Const of int
| Var of string
| Sum of expression * expression
| Diff of expression * expression
| Prod of expression * expression
;;

exception Unbound_variable of string;;

let rec eval env exp =
  match exp with
  | Const c -> c
  | Var v -> (try List.assoc v env with Not_found -> raise (Unbound_variable v))
  | Sum  (f,g) -> eval env f + eval env g
  | Diff (f,g) -> eval env f - eval env g
  | Prod (f,g) -> eval env f * eval env g
;;

(* let result = eval ["x",1 ; "y",3] (Prod (Sum (Var "x", Const 2), Var "y")) in *)
let result = eval [] (Const 1) in
output_char stdout (char_of_int result);
output_char stdout '\n';;
