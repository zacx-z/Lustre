open Parser
open Lexer
open Printf
open List
open Type
open Printf

let format_var_type t = match t with
    Bool -> "bool"
  | Int -> "int"
  | Char -> "char"
  | Real -> "real"
  | Ident v -> v

let rec print_list func lst = match lst with
    fst :: snd :: rest -> func fst; print_char ','; print_list func (snd::rest)
  | [s] -> func s
  | [] -> ()

let rec print_program program = 
    print_string (sprintf "The program contains %d nodes:\n" (length program.nodes));
    iter print_node program.nodes

and print_node (name, node) = print_node_head node.header

and print_node_head (t, name, args, rets) =
    print_node_type t;
    print_string name; print_newline ();
    print_string "   - input: ";
    print_params args;
    print_string "   - output: ";
    print_params rets

and print_node_type t = match t with
    Node -> print_string " - node:"
  | Function -> print_string " - function:"

and print_params pms = 
    iter print_var_def pms
    and print_var_def (ids, var_type, _) = print_list (function x -> match x with 
        Clock v -> print_string v
      | V v -> print_string v) ids;
        print_char ':';
        print_string (format_var_type var_type);
        print_newline ()

let _ =
    try
        let lexbuf = Lexing.from_channel (open_in "input.lustre")  in
            let result = Parser.file Lexer.initial lexbuf in
                print_program result
    with
    (Parse_Error str) ->
        print_string (sprintf "Error: %s\n" str);
        exit 0
    | e ->
        print_string "Error!\n";
        raise e

