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
          | VNil
(* and stream = { run: value list -> stream * value } *)
and expr = RValue   of value | Elist of expr list | Temp of int
         | IntConv  of expr
         | RealConv of expr
         | Neg      of expr
         | Add      of expr * expr
         | Minus    of expr * expr
         | Mult     of expr * expr
         | Divide   of expr * expr
         | Div      of expr * expr
         | Mod      of expr * expr


type node = {
    header : node_type * string * var_def list * var_def list;
    equations : (lvalue list * expr) list
}

type program = {
    nodes : (string * node) list;
    types : int list
}

(* Calculation *)
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


(* Helpers *)
let node_name (_, name, _, _) = name (* receive header and return name*)

let rec print_list func lst = match lst with
    fst :: snd :: rest -> func fst; print_char ','; print_list func (snd::rest)
  | [s] -> func s
  | [] -> ()

let rec print_value value = match value with
    VBool  b   -> printf "Bool: %b" b
  | VInt   i   -> printf "Int: %d" (to_int i)
  | VChar  c   -> printf "Char: %c" c
  | VReal  r   -> printf "Real: %f" r
  | VIdent v   -> printf "Undefined: %s" v
  | VList  v   -> print_string "VList: "; print_list print_value v
  | VNil       -> print_string "Nil"
