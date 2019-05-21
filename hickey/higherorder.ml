let derive f dx =
    (fun x -> (f (x +. dx) -. f x) /. dx);;


let rec power i x =
    if i = 0 then
        1.0
    else
        x *. (power (i-1) x);;

let dx = 1e-10;;

let f   = power 3;;
let f'  = derive f dx;;
let f'' = derive f' dx;;

let g'  x = 3.0 *. (power 2 x);; (* should have f'  = g' *)
let g'' x = 6.0 *. x;;           (* should have f'' = g'' *)
