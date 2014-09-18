open Core.Std
open Lexing

let shell () =
  (* Print the REPL header *)
  print_string "MiniLAM. ";
  print_string
    (match Sys.os_type with
     | "Unix" -> "Ctrl-D"
     | "Win32" -> "Ctrl-Z"
     | _ -> "EOF");
  print_endline " to exit.";
  let rec loop () =
    printf "MiniLAM> %!";
    match In_channel.input_line stdin with
    | None -> print_endline "\nGood bye."
    | Some line -> 
      begin
        let lexbuf = Lexing.from_string line in
        try
          lexbuf
          |> Parser.prog Lexer.read
          |> List.map ~f:Syntax.string_of_term
          |> List.iter ~f:print_endline
        with
        | Lexer.Error msg -> print_endline msg
        | Parser.Error ->
          print_endline (Lexer.err_with_info lexbuf "Syntax Error")
      end;
      loop ()
  in
  loop ()

let _ = shell ()
