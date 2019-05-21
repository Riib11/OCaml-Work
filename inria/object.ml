class point = object
  val mutable x = 0
  method get_x = x
  method move d = x <- x + d
end

let p = new point;;
print_int (p#get_x);
print_newline ();
p#move 2;
print_int (p#get_x);
print_newline ()

(*  or a different way to define point, making it more of a constructor *)

class point x_init = object
 val mutable x = x_init
 method get_x = x
 method move d = x <- x + d
end

(* can have an explicit `self`/`this` variable *)

class point x_init = object(this)
  val mutable x = x_init
  method get_x = x
  method move d = x <- this#get_x + d
  method print = print_int this#get_x
end
