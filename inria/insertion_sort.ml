let insertion_sort a =
  for i = 0 to Array.length a - 1 do
    let val_i = a.(i) in
    let j = ref i in
    while !j > 0 && val_i < a.(!j - i) do
      a.(!j) <- a.(!j - i);
      j := !j - 1
    done;
    a.(!j) <- val_i
  done
