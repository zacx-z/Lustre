open Parser
open Lexer
open Printf
open List
open Type
open Printf
open Int32

type context = { local : scope; }

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


let print_scope { vars = vlst } =
    print_string "scope:\n";
    print_list (fun (name, var) -> print_string name) vlst;
    print_newline ()

let print_context context = print_scope context.local

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

and print_params pms =
    iter print_var_def pms
    and print_var_def (ids, var_type, _) = print_list (fun x ->
        print_string (fst x)) ids;
        print_char ':';
        print_string (format_var_type var_type);
        print_newline ()

let lookup context name = assoc name context.local.vars
and bind_var context (name:string) value : context =
    let tbl = context.local.vars in
    { context with local =
        { context.local with vars = (name, {(assoc name tbl) with value = value}) :: (remove_assoc name tbl) } }


let vmult a b =
    match (a, b) with
        (VInt x, VInt y) -> VInt (mul x y)
      | (VReal x, VReal y) -> VReal (x *. y)
      | _ -> raise (Failure "Can't multiply")

let vadd a b =
    match (a, b) with
        (VInt x, VInt y) -> VInt (add x y)
      | (VReal x, VReal y) -> VReal (x +. y)
      | _ -> raise (Failure "Can't add")

let vminus a b =
    match (a, b) with
        (VInt x, VInt y) -> VInt (sub x y)
      | (VReal x, VReal y) -> VReal (x -. y)
      | _ -> raise (Failure "Can't minus")

let vdivide a b =
    match (a, b) with
        (VReal x, VReal y) -> VReal (x /. y)
      | _ -> raise (Failure "Can't divide")

let vdiv a b =
    match (a, b) with
        (VInt x, VInt y) -> VInt (div x y)
      | _ -> raise (Failure "Can't div")

let vmod a b =
    match (a, b) with
        (VInt x, VInt y) -> VInt (rem x y)
      | _ -> raise (Failure "Can't mod")

let vneg a =
    match a with
        VInt x -> VInt (neg x)
      | VReal x -> VReal (-.x)
      | _ -> raise (Failure "Can't negate")

let vreal_conv a =
    match a with
        VInt x -> VReal (to_float x)
      | VReal x -> VReal x
      | _ -> raise (Failure "Can't convert to real")

let vint_conv a =
    match a with
        VInt x -> VInt x
      | VReal x -> VInt (of_float x)
      | _ -> raise (Failure "Can't convert to int")

let rec eval_expr context eqs expr =
    let get_val x =
        let check var : value =
            if var.value == VUndefined then
                snd (solve_var context eqs var.name)
            else var.value
        in match x with
            VIdent varname -> check (lookup context varname)
          | t -> t
            in let eval = eval_expr context eqs
                in match expr with
                    Add (a, b)    -> vadd (eval a) (eval b)
                  | Minus (a, b)  -> vminus (eval a) (eval b)
                  | Mult (a, b)   -> vmult (eval a) (eval b)
                  | Divide (a, b) -> vdivide (eval a) (eval b)
                  | Div (a, b)    -> vdiv (eval a) (eval b)
                  | Mod (a, b)    -> vmod (eval a) (eval b)
                  | Neg a         -> vneg (eval a)
                  | RealConv a    -> vreal_conv (eval a)
                  | IntConv a     -> vint_conv (eval a)
                  | RValue v      -> get_val v
                  | Elist lst     -> VList (map (eval_expr context eqs) lst)
                  | Temp a        -> raise (Failure "Not supported")

and solve_var context eqs varname : context * value =
    let value = (lookup context varname).value in
        if value == VUndefined then
            let eq = find (fun (lhs, expr) -> exists
            (function LIdent name -> name = varname | _ -> false) lhs) eqs in
            let result = eval_expr context eqs (snd eq)
            and bind_lhs context (vr,v) = match vr with
                LIdent varname -> bind_var context varname v
              | Underscore -> context
            and lhs = fst eq
            in ((match result with
                VList lst -> fold_left bind_lhs context (combine lhs lst)
              | t -> bind_lhs context (hd lhs, t)), result)
        else (context, value)

let run_node { header=(_, _, args, rets); equations=eqs } input =
    (* Build vars *)
    let vars_table = map (fun v -> (v.name, v))
    and bind_var_list vars vals = map (fun vr -> {vr with value = hd (assoc vr.name
vals)}) vars in
        let output_vars = make_var_list rets in
        let context = { local = { vars = vars_table
                                (append (bind_var_list (make_var_list args) input)
                                (output_vars))} } in
        let (context, output) = fold_left (fun (context, res) var -> let (c, r) = (solve_var context eqs var.name)
                                                          in (c, r::res))
                        (context, []) output_vars
        in print_context context;
           iter print_value output


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
    | e ->
        print_string "Error!\n";
        raise e

