open Parser
open Lexer
open Lexing
open Printf
open List
open Type
open Printf
open Int32

#install_printer expr_formatter;;

(* Generic Helper Functions *)
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

let rec n_list = function
  0 -> []
| n -> n_list (n - 1) @ [n - 1]

let rec select lst = function
  0 -> hd lst
| n -> select (tl lst) (n - 1)

(* Interpreting Exceptions *)
exception Multi_var_defs of string * string * string
exception Type_mismatch of var_type * value
exception Cyclic_dependence of string * string
exception Invalid_if of expr * value
exception Not_in_equations of string * string
exception Case_not_match of expr * value
exception Invalid_expr_in_when of expr * value
exception Not_same_clock of expr * value list
exception Not_same_clock' of clock_expr * value list
exception Not_same_length of expr
exception Not_same_length' of lvalue list * expr
exception Node_not_found of string
(* Mediate Exceptions *)
exception Failure_val of value

(* Types for Interpretation *)
type var = {
    name : string;
    is_clock : bool;
    vtype : var_type;
    value : tval list
}
and tval = Undefined | Evaluating | Val of value
and context = {
    local : (string * var) list;
    node_name : string;
    clock : int;
    eqs : (lvalue list * expr) list;
    program : program;
    node_ins : (id * (string * context)) list;
}
(* Three-value Logic for Clock Deduction *)
and tbool = TTrue | TFalse | TUnknown

let check_type vtype value = match value with
  Val v -> if not (check_type vtype v)
           then raise (Type_mismatch (vtype, v))
| _ -> ()

(* Helpers *)
let make_var (namelist, t, csexpr) = map (function varid -> 
    {name = (fst varid); is_clock = (snd varid); vtype = t; value = [] }) namelist
let make_var_list lst = concat (map make_var lst)

(* Print Functions *)
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
    printf "In the node: %s\n" context.node_name;
    printf "Clock: %d\n" context.clock;
    print_var_list context.local

let rec print_program program =
    printf "The program contains %d node(s):\n" (length program.nodes);
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


let get_node name program =
    try assoc name program.nodes
    with Not_found -> raise (Node_not_found name)

(* Context Manipulations *)

let next_clock context input =
    try
        let grab_input name = if mem_assoc name input
        then Val (hd (assoc name input))
        else Undefined in
        ({ context with
           local = map (fun (name, vr) -> (name, { vr with value =
                                                   let v = grab_input vr.name in check_type vr.vtype v; v :: vr.value
                                           })) context.local;
           clock = context.clock + 1
        }, map (fun (name, lst) -> (name, tl lst)) input)
    with
        Failure t when t = "hd" || t = "tl" -> raise (Failure "reach the end of input")

let pre context = if context.clock > 0
    then let (prelst, cur) = split (map (fun (name, vr) ->
    ((name, { vr with value = tl vr.value }), hd vr.value)) context.local) in
    ({ context with local = prelst; clock = context.clock - 1 }, Some cur)
    else (context, None)

let restore_pre context sav = match sav with
      Some cur -> { context with local = map (fun ((name, vr), v) -> (name, { vr with value
    = v :: vr.value})) (combine context.local cur); clock = context.clock + 1 }
    | None -> context

let fstclock context = let (fstlst, rst) = split (map (fun (name, vr) ->
    (let rv = rev vr.value in (name, { vr with value = [hd rv] }), tl rv)) context.local) in
    ({ context with local = fstlst; clock = 1 }, (context.clock, rst))

let restore_fc context rst = { context with local = map (fun ((name, vr), rv) ->
    (name, { vr with value = rev (vr.value @ rv) })) (combine context.local (snd rst)); clock = (fst rst) }

(* remove all the evaluating flag*)
let clean_context context = if context.clock > 0
    then { context with local = map (fun (name, vr) -> (name,
         if hd vr.value = Evaluating then { vr with value = Undefined :: tl vr.value } else vr)) context.local }
    else context

(* finding and binding variables *)
let lookup context name = assoc name context.local
let bind_var context (name:string) (value:tval) : context =
    let tbl = context.local in
    { context with local =
        find_replace (fun (_n, vr) -> if _n = name
            then (check_type vr.vtype value;
                 Some (name, {vr with value = value :: tl vr.value}))
            else None ) tbl }

let hdv lst = if lst = [] then Val VNil else hd lst

let rec length_expr context expr =
    let l = length_expr context
    and (@=) len b =
        if length_expr context b != len then raise (Not_same_length expr);
        len in
    match expr with
      Add    (a, b) -> l a @= b
    | Minus  (a, b) -> l a @= b
    | Mult   (a, b) -> l a @= b
    | Divide (a, b) -> l a @= b
    | Div    (a, b) -> l a @= b
    | Mod    (a, b) -> l a @= b
    | Neg        a  -> l a
    | RealConv   a  -> l a
    | IntConv    a  -> l a
    | RValue     v  -> 1
    | Elist    lst  -> fold_right (+) (map l lst) 0
    | Pre        a  -> l a
    | Current    a  -> l a
    | Arrow  (a, b) -> l a @= b
    | When   (a, c) -> l a
    | Not        a  -> l a
    | And    (a, b) -> l a @= b
    | Or     (a, b) -> l a @= b
    | Xor    (a, b) -> l a @= b
    | Eq     (a, b) -> l a @= b
    | Ne     (a, b) -> l a @= b
    | Lt     (a, b) -> l a @= b
    | Gt     (a, b) -> l a @= b
    | Lteq   (a, b) -> l a @= b
    | Gteq   (a, b) -> l a @= b
    | If  (c, a, b) -> l a @= b
    | Case   (a, p) -> (match map snd p with
                         h :: rst -> fold_left (@=) (l h) rst
                       | []       -> 1 (* error indeed *))
    | Apply  (id, name, args) -> match (assoc name context.program.nodes).header with
                                 (_, _, _, rets) -> length (make_var_list rets)


type compile_context = { mutable apply_id : int }
let rec precompile' context expr =
    let p = precompile' context in
    match expr with
      Add    (a, b) -> Add     (p a, p b)
    | Minus  (a, b) -> Minus   (p a, p b)
    | Mult   (a, b) -> Mult    (p a, p b)
    | Divide (a, b) -> Divide  (p a, p b)
    | Div    (a, b) -> Div     (p a, p b)
    | Mod    (a, b) -> Mod     (p a, p b)
    | Neg        a  -> Neg     (p a)
    | RealConv   a  -> RealConv(p a)
    | IntConv    a  -> IntConv (p a)
    | RValue     v  -> RValue  v
    | Elist    lst  -> Elist   (map p lst)
    | Pre        a  -> Pre     (p a)
    | Current    a  -> Current (p a)
    | Arrow  (a, b) -> Arrow   (p a, p b)
    | When   (a, c) -> When    (p a, c)
    | Not        a  -> Not     (p a)
    | And    (a, b) -> And     (p a, p b)
    | Or     (a, b) -> Or      (p a, p b)
    | Xor    (a, b) -> Xor     (p a, p b)
    | Eq     (a, b) -> Eq      (p a, p b)
    | Ne     (a, b) -> Ne      (p a, p b)
    | Lt     (a, b) -> Lt      (p a, p b)
    | Gt     (a, b) -> Gt      (p a, p b)
    | Lteq   (a, b) -> Lteq    (p a, p b)
    | Gteq   (a, b) -> Gteq    (p a, p b)
    | If  (c, a, b) -> If (p c, p a, p b)
    | Case   (a, s) -> Case    (p a, map (fun (pat, b) -> (pat, p b)) s)
    | Apply  (id, name, args) -> context.apply_id <- context.apply_id + 1; Apply (context.apply_id, name, args)

let precompile = precompile' { apply_id = 0 }

let build_context program node node_name =
    let build_vars_table { header=(_, _, args, rets); locals = locals; equations = eqs } =
        let vars_table = map (fun v -> (v.name, v))
                      @. concat @. map snd
                      @. fold_left (fun cur (sname, lst) ->
                                    iter (fun v ->
                                          iter (fun (sname', s) ->
                                              if exists (fun v' -> v'.name = v.name) s
                                              then raise (Multi_var_defs (v.name, sname, sname'))) cur) lst;
                                    cur @ [(sname,lst)]) []
                      @. map (fun (sname, lst) -> (sname, fold_right
                             (fun v cur -> if exists (fun v' -> v'.name = v.name) cur
                                           then raise (Multi_var_defs (v.name, sname, sname));
                                           v::cur) lst []))  in
        vars_table [("input", (make_var_list args));
                    ("output", (make_var_list rets));
                    ("local", (make_var_list locals))] in
    match node with { header=(_, _, args, rets); locals = locals; equations = eqs } ->
    { local = build_vars_table node;
      node_name = node_name;
      clock = 0;
      eqs = map (fun (lhs, expr) ->  (lhs, precompile expr)) eqs;
      program = program;
      node_ins = [] }

(* Main Calculation *)

(* n:int refers the position of the element in the list expression *)
let rec eval_expr context n expr: context * value =
    let get_val x =
        let eval varname = solve_var context varname in
        match x with
          VIdent varname -> eval varname
        | t -> (context, t) in
    let eval_lst context lst =
        fold_right (fun expr (context, res) ->
        let (c, r) = eval_expr context n expr in (c, r :: res) )
        lst (context, []) in
    let eval_lst' lst =
        let check_clock_lst (context, lst) =
            if (for_all (fun x -> x = VNone) lst || (not (exists (fun x-> x = VNone) lst)))
            then (context, lst)
            else raise (Not_same_clock (expr, lst)) in
        check_clock_lst (eval_lst context lst) in

    let eval2 op a b =
        let (c, t) = eval_lst' [a;b] in match t with [ra;rb] -> (c, op ra rb) | _ -> assert false
    and eval1 op a =
        let (c, t) = eval_lst' [a] in match t with [r] -> (c, op r) | _ -> assert false

    and arrow a b = if context.clock = 1 then a else b in

    (* for Pre and Current *)
    let rec eval_pre context a =
        let (precon, cur) = pre context in
        let (c, r) = eval_expr precon n a in

        if r = VNone & c.clock != 0
        then let (c', r') = eval_pre c a in (restore_pre c' cur, r')
        else (restore_pre c cur, r)

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

    | Elist    lst  -> (match fold_left (fun (m, r) a ->
                                                  if r = None then
                                                      let m' = m + (length_expr context a) in
                                                      if m' > n then (m, Some a)
                                                      else (m', None)
                                                  else (m, r)) (0, None) lst with
                         (m, Some a) -> eval_expr context (n - m) a
                       | _ -> assert false)


    | Pre        a  -> if deduce_clock (clean_context context) n a then
                           eval_pre context a
                       else (context, VNone)

    | Current    a  -> let (c, r) = eval_expr context n a in
                       if r = VNone
                       then eval_pre c a
                       else (c, r)

    | Arrow  (a, b) -> eval2 arrow a b
    | When   (a, c) -> (try let (nc, cr) = eval_clock_expr context c in
                       match cr with
                         VBool v -> let (c, r) = eval_expr nc n a in
                                    if r = VNone then raise (Not_same_clock (expr, [r; cr]));
                                    if v
                                    then (c, r)
                                    else (c, VNone)
                       | VNone   -> let (c, r) = eval_expr nc n a in
                                    if r != VNone then raise (Not_same_clock (expr, [r; cr]));
                                    (c, VNone)
                       | _       -> raise (Invalid_expr_in_when (expr, cr))
                       with Failure_val v-> raise (Invalid_expr_in_when (expr, v)))

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

    | If  (c, a, b) -> assert (length_expr context c = 1);
                       let (con, cv) = eval_expr context 0 c in (match cv with
                         VBool v -> if v then eval_expr con n a else eval_expr con n b
                       | v -> raise (Invalid_if (expr, v)))
    | Case   (a, p) -> assert (length_expr context a = 1);
                       let (c, v) = eval_expr context 0 a in
                       let (_, b) = try find (function (PUnderscore, _) -> true | (PValue t, _) -> t = v) p
                                    with Not_found -> raise (Case_not_match (expr, v)) in
                       eval_expr c n b

    | Apply  (id, name, args) -> let node = get_node name context.program in
                                 let sync_context context' context =
                                     match node.header with (_, _, fs, _) ->
                                     let rec calc c' c =
                                         let ct = if c'.clock > c.clock
                                                  then calc (fst (pre c')) c
                                                  else c in
                                         if c'.clock > c.clock
                                         then let (_, vals) = eval_lst context' args in
                                            fst (next_clock ct (combine (map (fun vr -> vr.name) (make_var_list fs)) (map (fun v -> [v]) vals)))
                                         else ct
                                     in calc context' context in
                                 let (found, nc) = (try (true, snd (assoc id context.node_ins))
                                                    with Not_found ->
                                                        (false, build_context context.program node name)) in
                                 let outname = match node.header with (_, _, _, rets) ->
                                     let out_vars = concat (map (fun (ids, _, _) -> map fst ids) rets) in
                                     select out_vars n in
                                 let (c, r) = solve_var (sync_context context nc) outname in
                                 ({ context with node_ins = if found
                                     then find_replace
                                        (fun (id', (name, _)) ->
                                         if id = id'
                                         then Some (id', (name, c))
                                         else None) context.node_ins
                                     else (id, (name, c)) :: context.node_ins }, r)

and solve_var context varname : context * value =
    let eqs = context.eqs in
    let value = hdv (lookup context varname).value in
    match value with
      Undefined -> (
        let context = bind_var context varname Evaluating
        and meet_varname = function LIdent name -> name = varname | _ -> false in
        let eq = try find
                 (fun (lhs, expr) -> exists meet_varname lhs) eqs
                 with Not_found -> raise (Not_in_equations (context.node_name, varname)) in

        let (context, result) = let lhs = fst eq in
                                eval_expr context (fst (find (meet_varname @. snd)
                                                             (combine (n_list (length lhs)) lhs)))
                                                  (snd eq)

        in bind_var context varname (Val result), result)
    | Val v -> (context, v)
    | Evaluating -> print_context context; raise (Cyclic_dependence (context.node_name, varname))

and eval_clock_expr context expr =
    let v_not = function
      VBool v -> VBool (not v)
    | VNone -> VNone
    | t -> raise (Failure_val t)
    and v_eq v1 v2 = match (v1, v2) with
      (VNone, VNone) -> VNone
    | (VNone, _) -> raise (Not_same_clock' (expr, [v1;v2]))
    | (_, VNone) -> raise (Not_same_clock' (expr, [v1;v2]))
    | (a, b) -> VBool (a = b)
    in match expr with
      CWhen varname -> solve_var context varname
    | CNot  varname -> let (c, r) = solve_var context varname in (c, v_not r)
    | CMatch (var1, var2) -> let (c1, r1) = solve_var context var1 in
                             let (c2, r2) = solve_var c1 var2 in
                             (c2, v_eq r1 r2)

and deduce_clock context n expr = match deduce_clock' context n expr with
      TTrue -> true
    | TFalse -> false
    | TUnknown -> true

and deduce_clock' context n expr =
    (*A weird three-value logic*)
    let (@-) a b = if a = TTrue || (a = TUnknown && b = TTrue) then TTrue
                   else if a = TUnknown && b = TUnknown then TUnknown
                   else TFalse
    and (@&) a b = if (a = TTrue || a = TUnknown) && b then TTrue else TFalse
    and to_tb a = if a then TTrue else TFalse in

    let deduce = deduce_clock' context n
    and deduce_val = function
      VIdent varname -> (match hdv (lookup context varname).value with
                        Evaluating -> TUnknown
                      | Undefined  ->
                        let nc = bind_var context varname Evaluating in
                        let eq = try find
                                (fun (lhs, expr) -> exists
                                    (function LIdent name -> name = varname | _ -> false) lhs) context.eqs
                                with Not_found -> raise (Not_in_equations (context.node_name, varname)) in
                        deduce_clock' nc 0 (snd eq)
                      | Val v      -> to_tb (not (v = VNone)))
    | t -> TTrue

    in match expr with
      Add    (a, b) -> deduce a @- deduce b
    | Minus  (a, b) -> deduce a @- deduce b
    | Mult   (a, b) -> deduce a @- deduce b
    | Divide (a, b) -> deduce a @- deduce b
    | Div    (a, b) -> deduce a @- deduce b
    | Mod    (a, b) -> deduce a @- deduce b
    | Neg        a  -> deduce a
    | RealConv   a  -> deduce a
    | IntConv    a  -> deduce a

    | RValue     v  -> assert (n = 0); deduce_val v

    | Elist    lst  -> fold_right (@-) (map deduce lst) TUnknown (*I think values in an expression list are of the same clock*)

    | Not        a  -> deduce a
    | And    (a, b) -> deduce a @- deduce b
    | Or     (a, b) -> deduce a @- deduce b
    | Xor    (a, b) -> deduce a @- deduce b
    | Eq     (a, b) -> deduce a @- deduce b
    | Ne     (a, b) -> deduce a @- deduce b
    | Lt     (a, b) -> deduce a @- deduce b
    | Gt     (a, b) -> deduce a @- deduce b
    | Lteq   (a, b) -> deduce a @- deduce b
    | Gteq   (a, b) -> deduce a @- deduce b

    | If  (c, a, b) -> deduce c @- deduce a @- deduce b
    | Case   (a, p) -> deduce a @- fold_right (@-) (map (deduce @. snd) p) TUnknown (*Is it lazy? Maybe needing some optimization*)

    | Pre        a  -> deduce a
    | Current    a  -> TTrue
    | When   (a, b) -> deduce a @& (let (c, r) = eval_clock_expr context b in (match r with VBool v -> v | _ -> raise (Invalid_expr_in_when (expr, r))))
    | Arrow  (a, b) -> deduce a @- deduce b

    | Apply  (id, name, args) -> TTrue (* TODO *)


(* Entry *)
let run node node_name program input =
    match node with { header=(_, _, args, rets); locals = locals; equations = eqs } ->
    (* build context *)
    let output_vars = make_var_list rets in
    let context = build_context program node node_name in
    let _ = iter (fun (lhs, expr) -> if (length lhs != length_expr context expr)
                                     then raise (Not_same_length' (lhs, expr))) eqs in
    (* calculate a cycle *)
    let rec cycle (context, input) =
    let (context, output) = fold_right (fun var (context, res) ->
                                        let (c, r) = (solve_var context var.name)
                                        in (c, r::res))
                                        output_vars (context, []) in

    print_list print_value output;
    print_newline ();
    (* iterate: move on to next clock *)
    cycle (next_clock context input)

    in (* start *)
    cycle (next_clock context input)


let read_data_in fname =
    let parse_line str = map parse (split_string ' ' str) in
    let i = open_in fname in
    let res = (let rec get () =
        try let n = parse_line (input_line i) in n :: get ()
        with End_of_file -> [] in
        get ()) in close_in i; res


let _ =
    try
        let filename = "code.lus" in
        let lexbuf = Lexing.from_channel (open_in filename)  in
        let _ = lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = filename };
                lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } in
            let result = Parser.file Lexer.initial lexbuf in
                print_program result;
                let node = get_node "main" result
                and get_args (_, _, args, _) = args in
                let in_argsname = (map (fun vr -> vr.name) (make_var_list (get_args node.header))) in
                run node
                    "main"
                    result
                    (combine in_argsname (transpose (read_data_in "in.data")))

    with
    (Parse_Error str) ->
        printf "Error: %s\n" str;
        exit 0

