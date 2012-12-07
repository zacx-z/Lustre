type node_type = Node | Function
and var_def = var_id list * var_type * cs_expr option
and var_id = Clock of string | V of string
and var_type = Bool | Int | Char | Real | Ident of string
and cs_expr = Is of string | Not of string | Match of string * string
and lvalue = I of string | Underscore
and rvalue = True | False | CInt of int32 | CChar of char | CReal of float
           | Var of string
and expr = RValue of rvalue | Elist of expr list | Temp of int
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

(* Helpers *)
let node_name (_, name, _, _) = name (* receive header and return name*)
