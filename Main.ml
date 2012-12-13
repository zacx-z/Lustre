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

let split_string c str =
    let prepend i = function
          (a, b) :: rst when a = i + 1 -> (i, b) :: rst
        | lst -> (i, i + 1) :: lst
        in
    let rec split' c str i =
        if i = String.length str
        then []
        else let ch = String.get str i in
            if ch = c
            then split' c str (i + 1)
            else prepend i (split' c str (i + 1))
    in map (fun (a, b) -> String.sub str a (b - a)) (split' c str 0)

let transpose = function
      [] -> []
    | matrix -> fold_right (fun line res -> map (fun (l, r) -> l :: r)
                           (combine line res)) matrix (map (fun l -> []) (hd matrix))

exception Type_mismatch of var_type * value
exception Cyclic_dependence of string
exception Invalid_if of expr
exception Not_in_equation of string
exception Case_not_match of value

type var = {
    name : string;
    is_clock : bool;
    vtype : var_type;
    value : tval list
}
and tval = Undefined | Evaluating | Val of value
and context = {
    local : (string * var) list;
    clock : int
}

let assert_type vtype value = match value with
  Val v -> if not (check_type vtype v)
           then raise (Type_mismatch (vtype, v))
           else ()
| _ -> ()

let make_var (namelist, t, csexpr) = map (function varid -> 
    {name = (fst varid); is_clock = (snd varid); vtype = t; value = [] }) namelist
let make_var_list lst = concat (map make_var lst)

let print_tval = function
    Undefined -> print_string "Undefined"
  | Evaluating -> print_string "Evaluating"
  | Val v -> print_value v

let print_var_list vlst  =
    print_string "vars:\n";
    iter (fun (name, var) -> printf "%s:  " name; print_list print_tval var.value;print_newline() ) vlst;
    print_newline ()

let print_context context =
    print_string "-------- context --------\n";
    print_var_list context.local

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
and print_params pms = print_list print_var_def pms; print_newline ()
and print_var_def (ids, var_type, _) = print_list (fun x ->
        print_string (fst x)) ids;
        print_char ':';
        print_string (format_var_type var_type)

let next_clock context input =
    try
        let grab_input name = if mem_assoc name input
        then Val (hd (assoc name input))
        else Undefined in
        ({ local = map (fun (name, vr) -> (name, { vr with value =
                                                   let v = grab_input vr.name in assert_type vr.vtype v; v :: vr.value
                                           })) context.local;
           clock = context.clock + 1
        }, map (fun (name, lst) -> (name, tl lst)) input)
    with
        Failure t when t = "hd" || t = "tl" -> raise (Failure "reach the end of input")

let pre context = if context.clock > 0 
    then let (prelst, cur) = split (map (fun (name, vr) ->
    ((name, { vr with value = tl vr.value }), hd vr.value)) context.local) in
    ({ local = prelst; clock = context.clock - 1 }, Some cur)
    else (context, None)

let restore_pre context sav = match sav with
      Some cur -> { local = map (fun ((name, vr), v) -> (name, { vr with value
    = v :: vr.value})) (combine context.local cur); clock = context.clock + 1 }
    | None -> context

let fstclock context = let (fstlst, rst) = split (map (fun (name, vr) ->
    (let rv = rev vr.value in (name, { vr with value = [hd rv] }), tl rv)) context.local) in
    ({ local = fstlst; clock = 1 }, (context.clock, rst))

let restore_fc context rst = { local = map (fun ((name, vr), rv) ->
    (name, { vr with value = rev (vr.value @ rv) })) (combine context.local (snd rst)); clock = (fst rst) }

let lookup context name = assoc name context.local
let bind_var context (name:string) (value:tval) : context =
    let tbl = context.local in
    { context with local =
        find_replace (fun (_n, vr) -> if _n = name
            then (assert_type vr.vtype value;
                 Some (name, {vr with value = value :: tl vr.value}))
            else None ) tbl }

let hdv lst = if lst = [] then Val VNil else hd lst

let rec eval_expr context eqs expr : context * value =
    let get_val x =
        let eval varname =
            match hdv (lookup context varname).value with
              Undefined -> solve_var context eqs varname
            | Evaluating -> print_context context;raise (Cyclic_dependence varname)
            | Val v -> (context, v) in
        match x with
          VIdent varname -> eval varname
        | t -> (context, t) in
    let eval2 op a b =
        let (c1, ra) = eval_expr context eqs a in
        let (c2, rb) = eval_expr c1 eqs b in
        (c2, op ra rb)
    and eval1 op a =
        let (c, r) = eval_expr context eqs a in (c, op r)
        in match expr with
          Add    (a, b) -> eval2 vadd       a b
        | Minus  (a, b) -> eval2 vminus     a b
        | Mult   (a, b) -> eval2 vmult      a b
        | Divide (a, b) -> eval2 vdivide    a b
        | Div    (a, b) -> eval2 vdiv       a b
        | Mod    (a, b) -> eval2 vmod       a b
        | Neg        a  -> eval1 vneg       a
        | RealConv   a  -> eval1 vreal_conv a
        | IntConv    a  -> eval1 vint_conv  a

        | RValue     v  -> get_val v

        | Elist    lst  -> let (c, r) = (fold_right (fun expr (context, res) ->
                           let (c, r) = eval_expr context eqs expr in (c, r ::
                               res) ) lst (context, [])) in (c, VList r)

        | Pre        a  -> let eval_pre context =
                               let (precon, cur) = pre context in
                               let (c, r) = eval_expr precon eqs a in (restore_pre c cur, r)
                           in eval_pre context

        | Arrow  (a, b) -> if context.clock == 1
                           then let (fstcon, rst) = fstclock context in
                                let (c, r) = eval_expr fstcon eqs a in (restore_fc fstcon rst, r)
                           else eval_expr context eqs b

        | Not        a  -> eval1 vnot  a
        | And    (a, b) -> eval2 vand  a b
        | Or     (a, b) -> eval2 vor   a b
        | Xor    (a, b) -> eval2 vxor  a b

        | Eq     (a, b) -> eval2 veq   a b
        | Ne     (a, b) -> eval2 vne   a b
        | Lt     (a, b) -> eval2 vlt   a b
        | Gt     (a, b) -> eval2 vgt   a b
        | Lteq   (a, b) -> eval2 vlteq a b
        | Gteq   (a, b) -> eval2 vgteq a b

        | If  (c, a, b) -> let (con, cv) = eval_expr context eqs c in (match cv with
                             VBool v -> if v then eval_expr con eqs a else eval_expr con eqs b
                           | _ -> raise (Invalid_if c))
        | Case   (a, p) -> let (c, v) = eval_expr context eqs a in
                           let (_, b) = try find (function (PUnderscore, _) -> true | (PValue t, _) -> t = v) p
                                        with Not_found -> raise (Case_not_match v) in
                           eval_expr c eqs b

        | Temp          -> raise (Failure "Not supported")

and solve_var context eqs varname : context * value =
    let value = hdv (lookup context varname).value in
    match value with
      Undefined -> (
        let context = bind_var context varname Evaluating in
        let eq = try find
                 (fun (lhs, expr) -> exists
                    (function LIdent name -> name = varname | _ -> false) lhs) eqs
                 with Not_found -> raise (Not_in_equation varname) in

        let (context, result) = eval_expr context eqs (snd eq)

        and bind_lhs (vr,v) context = match vr with
          LIdent varname -> bind_var context varname (Val v)
        | Underscore -> context

        and lhs = fst eq in

        match result with
          VList lst -> (fold_right bind_lhs (combine lhs lst) context, assoc (LIdent varname) (combine lhs lst))
        | t -> (bind_lhs (hd lhs, t) context, t))
    | Val v -> (context, v)
    | _ -> assert false

let run_node { header=(_, _, args, rets); locals = locals; equations=eqs } input =
    (* Build vars *)
    let vars_table = map (fun v -> (v.name, v)) in
    let output_vars = make_var_list rets in
    let context = { local = vars_table
                           (concat [(make_var_list args);
                                    (output_vars);
                                    (make_var_list locals)]);
                    clock = 0 } in
    let rec cycle (context, input) =
    let (context, output) = fold_right (fun var (context, res) -> let (c, r) =
        (solve_var context eqs var.name)
                                                        in (c, r::res))
                    output_vars (context, []) in

    print_list print_value output;
    print_newline ();
    cycle (next_clock context input) in cycle (next_clock context input)


let read_data_in fname =
    let parse_line str = map parse (split_string ' ' str) in
    let i = open_in fname in
    let res = (let rec get () =
        try let n = parse_line (input_line i) in n :: get ()
        with End_of_file -> [] in
        get ()) in close_in i; res


let _ =
    try
        let lexbuf = Lexing.from_channel (open_in "code.lus")  in
            let result = Parser.file Lexer.initial lexbuf in
                print_program result;
                let node = assoc "main" result.nodes
                and get_args (_, _, args, _) = args in
                let in_argsname = (map (fun vr -> vr.name) (make_var_list (get_args node.header))) in
                run_node node
                         (combine in_argsname (transpose (read_data_in "in.data")))

    with
    (Parse_Error str) ->
        printf "Error: %s\n" str;
        exit 0

