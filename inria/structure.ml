module PriorityQueue = struct
  type priority = int
  type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
  let empty = Empty
  let rec insert queue p e =
    match queue with
    | Empty -> Node (p, e, Empty, Empty)
    | Node(p', e, l, r) ->
      if p <= p'
          then Node(p, e, insert r p e, l)
          else Node(p, e, r, insert l p e)
  exception Queue_is_empty
  let rec remove_head q = match q with
    | Empty -> raise Queue_is_empty
    | Node(p, e, l, Empty) -> l
    | Node(p, e, Empty, r) -> r
    | Node(p, e, (Node(lp, le, _, _) as l), (Node(rp, re, _, _) as r)) ->
      if lp <= rp
        then Node(lp, le, remove_head l, r)
        else Node(rp, re, l, remove_head r)
  let extract q = match q with
    | Empty -> raise Queue_is_empty
    | Node(p, e, _, _) -> (p, e, remove_head q)
end
