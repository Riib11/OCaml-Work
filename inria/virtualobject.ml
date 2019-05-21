class virtual abstract_point x_init = object (this)
  method virtual get_x : int
  method get_offset = this#get_x - x_init
  method virtual move : int -> unit
end

class point x_init = object(this)
  inherit abstract_point x_init
  val mutable x = x_init
  method get_x = x
  method move d = x <- this#get_x + d
end
