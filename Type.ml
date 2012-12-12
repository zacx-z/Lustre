open List
open Printf
open Int32

type node_type = Node | Function
and var_def = var_id list * var_type * cs_expr option
and var_id = string * bool
and var_type = TBool | TInt | TChar | TReal | TIdent of string
and cs_expr = CIs of string | CNot of string | CMatch of string * string
and lvalue = LIdent of string | Underscore
and value = VBool of bool | VInt of int32 | VChar of char | VReal of float
          | VIdent of string
          | VList of value list
          | VNil
(* and stream = { run: value list -> stream * value } *)
and expr = RValue   of value | Elist of expr list | Temp
         | IntConv  of expr
         | RealConv of expr
         | Neg      of expr
         | Add      of expr * expr
         | Minus    of expr * expr
         | Mult     of expr * expr
         | Divide   of expr * expr
         | Div      of expr * expr
         | Mod      of expr * expr

         | Pre      of expr
         | Arrow    of expr * expr
         | When     of expr * expr

         | Not      of expr
         | And      of expr * expr
         | Or       of expr * expr
         | Xor      of expr * expr

         | Eq       of expr * expr
         | Ne       of expr * expr
         | Lt       of expr * expr
         | Gt       of expr * expr
         | Lteq     of expr * expr
         | Gteq     of expr * expr

         | If       of expr * expr * expr
         | Case     of expr * (pattern * expr) list
and pattern = PUnderscore | PValue of value


type node = {
    header : node_type * string * var_def list * var_def list;
    locals : var_def list;
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
    | (VNil, VNil) -> VNil
    | _ -> raise (Failure "Can't multiply")

let vadd a b =
    match (a, b) with
      (VInt x, VInt y) -> VInt (add x y)
    | (VReal x, VReal y) -> VReal (x +. y)
    | (VNil, VNil) -> VNil
    | _ -> raise (Failure "Can't add")

let vminus a b =
    match (a, b) with
      (VInt x, VInt y) -> VInt (sub x y)
    | (VReal x, VReal y) -> VReal (x -. y)
    | (VNil, VNil) -> VNil
    | _ -> raise (Failure "Can't minus")

let vdivide a b =
    match (a, b) with
      (VReal x, VReal y) -> VReal (x /. y)
    | (VNil, VNil) -> VNil
    | _ -> raise (Failure "Can't divide")

let vdiv a b =
    match (a, b) with
      (VInt x, VInt y) -> VInt (div x y)
    | (VNil, VNil) -> VNil
    | _ -> raise (Failure "Can't div")

let vmod a b =
    match (a, b) with
      (VInt x, VInt y) -> VInt (rem x y)
    | (VNil, VNil) -> VNil
    | _ -> raise (Failure "Can't mod")

let vneg a =
    match a with
      VInt x -> VInt (neg x)
    | VReal x -> VReal (-.x)
    | VNil -> VNil
    | _ -> raise (Failure "Can't negate")

let vreal_conv a =
    match a with
      VInt x -> VReal (to_float x)
    | VReal x -> VReal x
    | VNil -> VNil
    | _ -> raise (Failure "Can't convert to real")

let vint_conv a =
    match a with
      VInt x -> VInt x
    | VReal x -> VInt (of_float x)
    | VNil -> VNil
    | _ -> raise (Failure "Can't convert to int")


let vnot a =
    match a with
      VBool x -> VBool (not x)
    | VNil -> VNil
    | _ -> raise (Failure "Can't Not")

let vand a b =
    match (a, b) with
      (VBool x, VBool y) -> VBool (x && y)
    | (VNil, VNil) -> VNil
    | _ -> raise (Failure "Can't And")

let vor a b =
    match (a, b) with
      (VBool x, VBool y) -> VBool (x || y)
    | (VNil, VNil) -> VNil
    | _ -> raise (Failure "Can't Or")

let vxor a b =
    match (a, b) with
      (VBool x, VBool y) -> VBool ((x && (not y)) || ((not x) && y))
    | (VNil, VNil) -> VNil
    | _ -> raise (Failure "Can't Xor")

let veq a b =
    match (a, b) with
      (VNil, VNil) -> VNil
    | (VBool x, VBool y) -> VBool (x = y)
    | (VInt x, VInt y)   -> VBool (compare x y = 0)
    | (VReal x, VReal y) -> VBool (x = y)
    | (VChar x, VChar y) -> VBool (x = y)
    | _ -> raise (Failure "Can't eq")

let vne a b =
    match (a, b) with
      (VNil, VNil) -> VNil
    | (VBool x, VBool y) -> VBool (x != y)
    | (VInt x, VInt y)   -> VBool (compare x y != 0)
    | (VReal x, VReal y) -> VBool (x != y)
    | (VChar x, VChar y) -> VBool (x != y)
    | _ -> raise (Failure "Can't ne")

let vlt a b =
    match (a, b) with
      (VNil, VNil) -> VNil
    | (VInt x, VInt y)   -> VBool (compare x y < 0)
    | (VReal x, VReal y) -> VBool (x < y)
    | _ -> raise (Failure "Can't lt")

let vgt a b =
    match (a, b) with
      (VNil, VNil) -> VNil
    | (VInt x, VInt y)   -> VBool (compare x y > 0)
    | (VReal x, VReal y) -> VBool (x > y)
    | _ -> raise (Failure "Can't gt")

let vlteq a b =
    match (a, b) with
      (VNil, VNil) -> VNil
    | (VInt x, VInt y)   -> VBool (compare x y <= 0)
    | (VReal x, VReal y) -> VBool (x <= y)
    | _ -> raise (Failure "Can't gteq")

let vgteq a b =
    match (a, b) with
      (VNil, VNil) -> VNil
    | (VInt x, VInt y)   -> VBool (compare x y >= 0)
    | (VReal x, VReal y) -> VBool (x >= y)
    | _ -> raise (Failure "Can't gteq")

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

let format_var_type = function
    TBool -> "bool"
  | TInt -> "int"
  | TChar -> "char"
  | TReal -> "real"
  | TIdent v -> v

let parse str = match str.[0] with
    '\'' -> assert (String.length str = 3 && str.[2] = '\''); VChar str.[1]
  | c when c = 't' || c = 'f' -> VBool (c = 't')
  | c when (let i = int_of_char c in i >= int_of_char '0' && i <= int_of_char '9')
         -> if String.contains str '.'
            then VReal (float_of_string str)
            else VInt (of_int (int_of_string str))
  | _ -> raise (Failure "Invalid input")

let check_type vtype value = match (vtype, value) with
    (TBool, VBool _) -> true
  | (TInt,  VInt _)  -> true
  | (TChar, VChar _) -> true
  | (TReal, VReal _) -> true
  | (_, VNil) -> true
  | _ -> false
