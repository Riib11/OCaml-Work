type point2 = { x:int; y:int };;
type point3 = { x:int; y:int; z:int };;
(* automatically detects record fit *)
let p = {x=1; y=2};;

(* neat field access *)
type person = { firstname: string; lastname: string };;
let dave = { firstname="Dave"; lastname="Betrol" };;
Printf.printf "%s\n" dave.firstname;;
