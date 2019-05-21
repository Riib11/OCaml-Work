let current_rand = ref 0

let random () =
  current_rand := !current_rand * 25713 + 1345;
  !current_rand
