module rec A :
  sig
    type t = Leaf of string | Node of ASet.t
    val compare : t -> t -> int
  end =
  struct
    type t = Leaf of string | Node of ASet.t
    let compare t1 t2 =
      match (t1, t2) with
      | (Leaf s1, Leaf s2) -> Pervasives.compare s1 s2
      | (Leaf  _, Node _ ) -> 1
      | (Node  _, Leaf _ ) -> 1
      | (Node n1, Node n2) -> ASet.compare n1 n2
    end
  and
    ASet : Set.S with type elt = A.t =
      Set.Make(A)

(* the above module has the following simple specification *)

(*
module rec A :
  sig
    type t = Leaf of string | Node of ASet.t
    val compare : t -> t -> int
  end
  and ASet : Set.S with type elt = A.t
*)
