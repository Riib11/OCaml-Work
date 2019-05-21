(* there are inptut and output channels *)

let () =
  output_string stdout "Hello World!\n";
  output_string stdout "> ";
  flush stdout
;;

let a = input_line stdin;;
