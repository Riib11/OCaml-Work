module type PRIORITYQUEUE = sig
  type priority = int
  type 'a queue
  val empty : 'a queue
  val insert : 'a queue -> priority -> 'a -> 'a queue
  val extract : 'a queue -> priority * 'a * 'a queue
end
