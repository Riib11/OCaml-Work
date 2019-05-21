type db_entry =
    { name : string
    ; mutable salary : int }
;;

let jason = { name="Jason"; salary=100 };;
jason.salary <- 200;; (* impute update *)
