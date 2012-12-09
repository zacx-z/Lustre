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
          | VList of value list
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
}
and scope_type = SInput | SOutput | SLocal

(* Helpers *)
let node_name (_, name, _, _) = name (* receive header and return name*)

let make_var (namelist, t, csexpr) = map (function varid -> 
    {name = (fst varid); is_clock = (snd varid); vtype = t; value = VUndefined}) namelist
let make_var_list lst = concat (map make_var lst)

let rec print_list func lst = match lst with
    fst :: snd :: rest -> func fst; print_char ','; print_list func (snd::rest)
  | [s] -> func s
  | [] -> ()

let rec print_value value = match value with
    VBool b -> printf "Bool: %b" b
  | VInt i -> printf "Int: %d" (to_int i)
  | VChar c -> printf "Char: %c" c
  | VReal r -> printf "Real: %f" r
  | VIdent v -> printf "Undefined: %s" v
  | VList v -> print_string "VList: "; print_list print_value v
  | VUndefined -> printf "Undefined"
