open Parser
open Lexer
open Printf
open List
open Type
open Printf
open Int32

let find_replace func lst = match fold_left (fun (found, res) elem ->
        if found
        then (true, elem :: res)
        else match func elem with
            None -> (false, elem :: res)
          | Some ne -> (true, ne :: res)
    ) (false, []) lst with
        (true, res) -> res
      | (false, _) -> raise Not_found

let format_var_type = function
    TBool -> "bool"
  | TInt -> "int"
  | TChar -> "char"
  | TReal -> "real"
  | TIdent v -> v

let rec print_list func lst = match lst with
    fst :: snd :: rest -> func fst; print_char ','; print_list func (snd::rest)
  | [s] -> func s
  | [] -> ()

let print_scope { vars = vlst } =
    print_string "scope:\n";
    iter (fun (name, var) -> print_string name) vlst;
    print_newline ()

let print_context = iter print_scope

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
    and print_var_def (ids, var_type, _) = print_list (fun x ->
        print_string (fst x)) ids;
        print_char ':';
        print_string (format_var_type var_type);
        print_newline ()

let lookup context name = assoc name (find (fun scp -> mem_assoc name scp.vars) context).vars
and bind_var context (name:string) value : scope list =
    find_replace (
        fun scp -> let tbl = scp.vars in if mem_assoc name tbl
        then Some { scp with vars = (name, {(assoc name tbl) with value = value}) :: (remove_assoc name tbl) }
        else None) context

let get_val context x =
    let check value =
        if value == VUndefined then
            raise (Failure "Undefined value")
        else value
    in match x with
        VIdent varname -> check (lookup context varname).value
      | t -> t

let vmult context a b : value =
    match (get_val context a, get_val context b) with
        (VInt x, VInt y) -> VInt (mul x y)
      | _ -> raise (Failure "Can't multiply")

let rec eval_expr context expr =
    let eval = eval_expr context in
    match expr with
        Mult (a,b) -> vmult context (eval a) (eval b)
      | RValue v -> v

let run_node { header=(_, _, args, rets); equations=eqs } input =
    (* Build vars *)
    let vars_table = map (fun v -> (v.name, v))
    and bind_var_list vars vals = map (fun vr -> {vr with value = hd (assoc vr.name
vals)}) vars in
        let context = [{ vars = vars_table (bind_var_list (make_var_list args) input);
                         stype = SInput };
                       { vars = vars_table (make_var_list rets);
                         stype = SOutput }]
        in let result = fold_left (fun context (lhs, expr) -> match (hd lhs) with
            LIdent name -> bind_var context name (eval_expr context expr)
          | Underscore -> context) context eqs
        in print_context result;
           iter (fun (name, vr) -> print_value vr.value)
                (find (fun scp -> scp.stype == SOutput) result).vars


let _ =
    try
        let lexbuf = Lexing.from_channel (open_in "input.lus")  in
            let result = Parser.file Lexer.initial lexbuf in
                print_program result;
                run_node (assoc "main" result.nodes) [("x", map (fun x ->
                    VInt (of_int x)) [10;3;56;4;3;2])]

    with
    (Parse_Error str) ->
        print_string (sprintf "Error: %s\n" str);
        exit 0
    | e ->
        print_string "Error!\n";
        raise e

