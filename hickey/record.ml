type db_enty =
    { name   : string;
      height : float;
      phone  : string;
      salary : float }
;;

type db_entry' =
    { name   : string
    ; height : float
    ; phone  : string
    ; salary : float  }
;;

let jason =
    { name   = "Jason"
    ; height = 6
    ; phone  = "123-456-7890"
    ; salary = 100 }
;;

(* automatically sees that jason has type db_salary *)

(* functional record update *)

let dave = { jason with name = "Dave"; height = 5.9 };;


