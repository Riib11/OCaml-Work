let rec power i x =
    if i = 0 then
        1
    else
        x * (power (i-1) x);;
