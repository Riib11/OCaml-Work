(* the `'a . 'a -> 'a` is the same idea as `forall 'a, 'a -> 'a` *)
type idref = { mutable id : 'a . 'a -> 'a };;

let r = { id=fun x -> x };;

let g s = (s.id 1, s.id true);;

r.id <- (fun x -> print_string "called `id`\n"; x);;

g r;;
