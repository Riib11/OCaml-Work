let name_of_binary_digit digit =
  try
    List.assoc digit [0, "zero"; 1, "one"]
  with
  | Not_found -> "not a binary digit"
;;

print_endline (name_of_binary_digit 0);
print_endline (name_of_binary_digit 1);
print_endline (name_of_binary_digit 2);

let tmp_set_ref refr newval func =
  let oldval = !ref in
  try
    ref := newval;
    let res = func () in
    ref := oldval;
    res
  with x ->
    ref := oldval;
    raise x
