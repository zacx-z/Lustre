open Parser
open Lexer
open Printf

let _ =
    try
        let lexbuf = Lexing.from_channel (open_in "input.lustre")  in
            let result = Parser.file Lexer.initial lexbuf in
                print_string "OK"
    with
    (Parse_Error str) ->
        print_string (sprintf "Error: %s\n" str);
        exit 0
    | _ ->
        print_string "Error!\n";
        exit 0

