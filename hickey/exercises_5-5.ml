(* 5.4 *)

let db =
    ["John", "x3456", 50.1;
     "Jane", "x1234", 107.3;
     "Joan", "unlisted", 12.7]
;;

let find_salary name =
    let rec f = function
        (name', phone, salary)::t ->
            if name' = name then
                salary
            else
                f t
      | [] -> raise Not_found
    in f db
;;
