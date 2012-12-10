open Parser
open Lexer
open Printf
open List
open Type
open Printf
open Int32

type var = {
    name : string;
    is_clock : bool;
    vtype : var_type;
    value : tval list
}
and tval = Undefined | Val of value
and context = { local : (string * var) list; input : (string * value list) list }

let make_var (namelist, t, csexpr) = map (function varid -> 
    {name = (fst varid); is_clock = (snd varid); vtype = t; value = [] }) namelist
let make_var_list lst = concat (map make_var lst)

let print_var_list vlst  =
    print_string "vars:\n";
    print_list (fun (name, var) -> print_string name) vlst;
    print_newline ()

let print_context context = print_var_list context.local

let rec print_program program =
    printf "The program contains %d nodes:\n" (length program.nodes);
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
and print_params pms = iter print_var_def pms and print_var_def (ids, var_type, _) = print_list (fun x ->
        print_string (fst x)) ids;
        print_char ':';
        print_string (format_var_type var_type);
        print_newline ()

let next_clock context =
    try
        let grab_input name = if mem_assoc name context.input
        then Val (hd (assoc name context.input))
        else Undefined in
        { local = map (fun (name, vr) -> (name, { vr with value = (grab_input vr.name) :: vr.value })) context.local;
          input = map (fun (name, lst) -> (name, tl lst)) context.input; }
    with
        Failure t when t = "hd" || t = "tl" -> raise (Failure "reach the end of input")

let lookup context name = assoc name context.local
and bind_var context (name:string) (value:value) : context =
    let tbl = context.local in
    let vr = assoc name tbl in
    { context with local =
        (name, {vr with value = (Val value) :: tl vr.value}) :: (remove_assoc name tbl) }


let rec eval_expr context eqs expr =
    let get_val x =
        let check var : value =
            match hd var.value with
              Undefined -> snd (solve_var context eqs var.name)
            | Val v -> v in
        match x with
            VIdent varname -> check (lookup context varname)
          | t -> t in
    let eval = eval_expr context eqs in
        match expr with
          Add    (a, b) -> vadd (eval a) (eval b)
        | Minus  (a, b) -> vminus (eval a) (eval b)
        | Mult   (a, b) -> vmult (eval a) (eval b)
        | Divide (a, b) -> vdivide (eval a) (eval b)
        | Div    (a, b) -> vdiv (eval a) (eval b)
        | Mod    (a, b) -> vmod (eval a) (eval b)
        | Neg a         -> vneg (eval a)
        | RealConv a    -> vreal_conv (eval a)
        | IntConv a     -> vint_conv (eval a)
        | RValue v      -> get_val v
        | Elist lst     -> VList (map (eval_expr context eqs) lst)
        | Temp a        -> raise (Failure "Not supported")

and solve_var context eqs varname : context * value =
    let value = hd (lookup context varname).value in
    match value with
      Undefined ->
        let eq = find
                 (fun (lhs, expr) -> exists
                    (function LIdent name -> name = varname | _ -> false) lhs) eqs in

        let result = eval_expr context eqs (snd eq)

        and bind_lhs context (vr,v) = match vr with
          LIdent varname -> bind_var context varname v
        | Underscore -> context

        and lhs = fst eq in

        ((match result with
          VList lst -> fold_left bind_lhs context (combine lhs lst)
        | t -> bind_lhs context (hd lhs, t)), result)
    | Val v -> (context, v)

let run_node { header=(_, _, args, rets); equations=eqs } input =
    (* Build vars *)
    let vars_table = map (fun v -> (v.name, v)) in
    let output_vars = make_var_list rets in
    let context = { local = vars_table
                            (append (make_var_list args)
                            (output_vars));
                    input = input } in
    let rec cycle context =
    let (context, output) = fold_left (fun (context, res) var -> let (c, r) =
        (solve_var context eqs var.name)
                                                        in (c, r::res))
                    (context, []) output_vars in

    print_list print_value output;
    print_newline ();
    cycle (next_clock context) in cycle (next_clock context)


let _ =
    try
        let lexbuf = Lexing.from_channel (open_in "input.lus")  in
            let result = Parser.file Lexer.initial lexbuf in
                print_program result;
                run_node (assoc "main" result.nodes) [("x", map (fun x ->
                    VInt (of_int x)) [10;3;56;4;3;2])]

    with
    (Parse_Error str) ->
        printf "Error: %s\n" str;
        exit 0

