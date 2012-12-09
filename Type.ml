open List
open Printf
open Int32

type node_type = Node | Function
and var_def = var_id list * var_type * cs_expr option
and var_id = string * bool
and var_type = TBool | TInt | TChar | TReal | TIdent of string
and cs_expr = Is of string | Not of string | Match of string * string
and lvalue = LIdent of string | Underscore
and value = VBool of bool | VInt of int32 | VChar of char | VReal of float
          | VIdent of string
          | VUndefined
and expr = RValue of value | Elist of expr list | Temp of int
           | IntConv  of expr
           | RealConv of expr
           | Neg     of expr
           | Add     of expr * expr
           | Minus   of expr * expr
           | Mult    of expr * expr
           | Divide  of expr * expr
           | Div     of expr * expr
           | Mod     of expr * expr


type node = {
    header : node_type * string * var_def list * var_def list;
    equations : (lvalue list * expr) list
}

type program = {
    nodes : (string * node) list;
    types : int list
}

type var = {
    name : string;
    is_clock : bool;
    vtype : var_type;
    value : value
}
and scope = {
    vars : (string * var) list;
    stype : scope_type
}
and scope_type = SInput | SOutput | SLocal

(* Helpers *)
let node_name (_, name, _, _) = name (* receive header and return name*)

let make_var (namelist, t, csexpr) = map (function varid -> 
    {name = (fst varid); is_clock = (snd varid); vtype = t; value = VUndefined}) namelist
let make_var_list lst = concat (map make_var lst)

let print_value value = match value with
    VBool b -> print_string (sprintf "Bool: %b" b)
  | VInt i -> print_string (sprintf "Int: %d" (to_int i))
  | VChar c -> print_string (sprintf "Char: %c" c)
  | VReal r -> print_string (sprintf "Real: %f" r)
  | VIdent v -> print_string (sprintf "Undefined: %s" v)
  | VUndefined -> print_string "Undefined"
