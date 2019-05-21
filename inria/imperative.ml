let add_vect v1 v2 =
  let len = min (Array.length v1) (Array.length v2) in
  let res = Array.make len 0.0 in
  for i = 0 to len - 1 do
    res.(i) <- v1.(i) +. v2.(i)
  done;
  res

let dot_vect v1 v2 =
  let len = min (Array.length v1) (Array.length v2) in
  let res = Array.make len 0.0 in
  for i = 0 to len - 1 do
    res.(i) <- v1.(i) *. v2.(i)
  done;
  res

type mutable_point = { mutable x: float; mutable y: float }

let translate p dx dy =
  p.x <- p.x +. dx; p.y <- p.y +. dy


