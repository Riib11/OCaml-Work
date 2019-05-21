type comparison = LT | EQ | GT

module type ORDERED_TYPE = sig
  type t
  val compare : t -> t -> comparison
end

module Set = functor (Element : ORDERED_TYPE) -> struct
  type element = Element.t
  let empty = []
  let rec add x s =
    match s with
    | [] -> [x]
    | h::t ->
      match Element.compare x h with
      | EQ -> s
      | LT -> x :: s
      | GT -> h :: add x t
  let rec member x s =
    match s with
    | [] -> false
    | h::t ->
      match Element.compare x h with
      | EQ -> true
      | LT -> false
      | GT -> member x t
end

module OrderedString = struct
  type t
  let compare x y = if x = y then EQ else if x < y then LT else GT
end

module type SETFUNCTOR = functor (Element : ORDERED_TYPE) -> sig
  type element = Element.t (* concrete *)
  type set                 (* abstract *)
  val empty : set
  val add : element -> set -> set
  val member : element -> set -> bool
end

(* module AbstractSet = (Set : SETFUNCTOR) *)
