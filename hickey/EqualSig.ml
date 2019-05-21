module type EqualSig = sig
  type t
  val equal : t -> t -> bool
end

module MakeSet (Equal : EqualSig) = struct
  open Equal
  type elt = Equal.t
  type t = elt list
  let empty = []
  let mem x s = List.exists (equal x) s
  let add = (::)
  let find x s = List.find (equal x) s
end
