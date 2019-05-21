let rec f i j =
    if i = 0 then
        j
    else
        g (j - 1)
and g j =
    if j mod 3 = 0 then
        j
    else
        f (j - 1) j;;
