(* loop *)
let rec unique already_read =
  output_string "> ";
  flush stdout;
  let line = input_line in
    if not (List.mem line already_read) then begin
      output_string stdout line;
      output_char stdout "\n";
      unique (line :: already_read)
    end else
      unique already_read

  try unique [] with End_of_file -> ();;
