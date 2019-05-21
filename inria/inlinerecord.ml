type t = Point of { width:int ; mutable x:float ; mutable y:float }

let v = Point { width=10 ; x=0.0 ; y=0.0 }

(** Now for some definitions of [print_point] *)
let print_point p =
  match p with
  | Point { x ; y ; _ } -> Printf.printf "%f,%f" x y
