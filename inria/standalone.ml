(*  example of a standalone OCaml program *)

let rec fib n =
  if n < 2 then
    1
  else
    fib (n-2) + fib (n-1)
;;

let main () =
  try
    let arg = int_of_string Sys.argv.(1) in
    print_string "fib ";
    print_int arg;
    print_string " = ";
    print_int (fib arg);
    print_newline ();
    exit 0
  with
  | Invalid_argument _ -> raise (Failure "required argument: int")
;;

main ();;
