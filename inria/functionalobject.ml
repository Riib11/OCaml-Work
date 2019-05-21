class point x_init = object (this)
  val mutable x = x_init
  method get_x = x
  method move d = {< x = x + d >}
end
