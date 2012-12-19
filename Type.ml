open List
open Printf
open Int32

(*function composition*)
let (@.) f g x = f (g x)

type node_type = Node | Function
and var_def = var_id list * var_type * clock_expr option
and var_id = string * bool
and var_type = TBool | TInt | TChar | TReal | TIdent of string
and lvalue = LIdent of string | Underscore
and value = VBool of bool | VInt of int32 | VChar of char | VReal of float
          | VIdent of string
          | VList of value list
          | VNil  (*produced by pre operator*)
          | VNone (*not in clock cycle *)
(* and stream = { run: value list -> stream * value } *)
and expr = RValue   of value
         | Elist    of expr list
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
         | Current  of expr
         | Arrow    of expr * expr
         | When     of expr * clock_expr

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

         | Temp
and pattern = PUnderscore | PValue of value
and clock_expr = CWhen of string | CNot of string | CMatch of string * string


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

exception Not_same_clock
exception Invalid_op1 of string * value
exception Invalid_op2 of string * value * value

let vmult a b =
    match (a, b) with
      (VInt x, VInt y) -> VInt (mul x y)
    | (VReal x, VReal y) -> VReal (x *. y)
    | (VNil, _) -> VNil
    | (_, VNil) -> VNil
    | (VNone, VNone) -> VNone
    | (a, b) when a = VNone || b = VNone -> raise Not_same_clock
    | _ -> raise (Invalid_op2 ("multiply", a, b))

let vadd a b =
    match (a, b) with
      (VInt x, VInt y) -> VInt (add x y)
    | (VReal x, VReal y) -> VReal (x +. y)
    | (VNil, _) -> VNil
    | (_, VNil) -> VNil
    | (VNone, VNone) -> VNone
    | (a, b) when a = VNone || b = VNone -> raise Not_same_clock
    | _ -> raise (Invalid_op2 ("add", a, b))

let vminus a b =
    match (a, b) with
      (VInt x, VInt y) -> VInt (sub x y)
    | (VReal x, VReal y) -> VReal (x -. y)
    | (VNil, _) -> VNil
    | (_, VNil) -> VNil
    | (VNone, VNone) -> VNone
    | (a, b) when a = VNone || b = VNone -> raise Not_same_clock
    | _ -> raise (Invalid_op2 ("minus", a, b))

let vdivide a b =
    match (a, b) with
      (VReal x, VReal y) -> VReal (x /. y)
    | (VNil, _) -> VNil
    | (_, VNil) -> VNil
    | (VNone, VNone) -> VNone
    | (a, b) when a = VNone || b = VNone -> raise Not_same_clock
    | _ -> raise (Invalid_op2 ("divide", a, b))

let vdiv a b =
    match (a, b) with
      (VInt x, VInt y) -> VInt (div x y)
    | (VNil, _) -> VNil
    | (_, VNil) -> VNil
    | (VNone, VNone) -> VNone
    | (a, b) when a = VNone || b = VNone -> raise Not_same_clock
    | _ -> raise (Invalid_op2 ("div", a, b))

let vmod a b =
    match (a, b) with
      (VInt x, VInt y) -> VInt (rem x y)
    | (VNil, _) -> VNil
    | (_, VNil) -> VNil
    | (VNone, VNone) -> VNone
    | (a, b) when a = VNone || b = VNone -> raise Not_same_clock
    | _ -> raise (Invalid_op2 ("mod", a, b))

let vneg a =
    match a with
      VInt x -> VInt (neg x)
    | VReal x -> VReal (-.x)
    | VNil -> VNil
    | VNone -> VNone
    | _ -> raise (Invalid_op1 ("negate", a))

let vreal_conv a =
    match a with
      VInt x -> VReal (to_float x)
    | VReal x -> VReal x
    | VNil -> VNil
    | VNone -> VNone
    | _ -> raise (Invalid_op1 ("convert to real", a))

let vint_conv a =
    match a with
      VInt x -> VInt x
    | VReal x -> VInt (of_float x)
    | VNil -> VNil
    | VNone -> VNone
    | _ -> raise (Invalid_op1 ("convert to int", a))


let vnot a =
    match a with
      VBool x -> VBool (not x)
    | VNil -> VNil
    | VNone -> VNone
    | _ -> raise (Invalid_op1 ("not", a))

let vand a b =
    match (a, b) with
      (VBool x, VBool y) -> VBool (x && y)
    | (VNil, _) -> VNil
    | (_, VNil) -> VNil
    | (VNone, VNone) -> VNone
    | (a, b) when a = VNone || b = VNone -> raise Not_same_clock
    | _ -> raise (Invalid_op2 ("and", a, b))

let vor a b =
    match (a, b) with
      (VBool x, VBool y) -> VBool (x || y)
    | (VNil, _) -> VNil
    | (_, VNil) -> VNil
    | (VNone, VNone) -> VNone
    | (a, b) when a = VNone || b = VNone -> raise Not_same_clock
    | _ -> raise (Invalid_op2 ("or", a, b))

let vxor a b =
    match (a, b) with
      (VBool x, VBool y) -> VBool ((x && (not y)) || ((not x) && y))
    | (VNil, _) -> VNil
    | (_, VNil) -> VNil
    | (VNone, VNone) -> VNone
    | (a, b) when a = VNone || b = VNone -> raise Not_same_clock
    | _ -> raise (Invalid_op2 ("xor", a, b))

let veq a b =
    match (a, b) with
      (VNil, _) -> VNil
    | (_, VNil) -> VNil
    | (VBool x, VBool y) -> VBool (x = y)
    | (VInt x, VInt y)   -> VBool (compare x y = 0)
    | (VReal x, VReal y) -> VBool (x = y)
    | (VChar x, VChar y) -> VBool (x = y)
    | (VNone, VNone) -> VNone
    | (a, b) when a = VNone || b = VNone -> raise Not_same_clock
    | _ -> raise (Invalid_op2 ("equal", a, b))

let vne a b =
    match (a, b) with
      (VNil, _) -> VNil
    | (_, VNil) -> VNil
    | (VBool x, VBool y) -> VBool (x != y)
    | (VInt x, VInt y)   -> VBool (compare x y != 0)
    | (VReal x, VReal y) -> VBool (x != y)
    | (VChar x, VChar y) -> VBool (x != y)
    | (VNone, VNone) -> VNone
    | (a, b) when a = VNone || b = VNone -> raise Not_same_clock
    | _ -> raise (Invalid_op2 ("not equal", a, b))

let vlt a b =
    match (a, b) with
      (VNil, _) -> VNil
    | (_, VNil) -> VNil
    | (VInt x, VInt y)   -> VBool (compare x y < 0)
    | (VReal x, VReal y) -> VBool (x < y)
    | (VNone, VNone) -> VNone
    | (a, b) when a = VNone || b = VNone -> raise Not_same_clock
    | _ -> raise (Invalid_op2 ("<", a, b))

let vgt a b =
    match (a, b) with
      (VNil, _) -> VNil
    | (_, VNil) -> VNil
    | (VInt x, VInt y)   -> VBool (compare x y > 0)
    | (VReal x, VReal y) -> VBool (x > y)
    | (VNone, VNone) -> VNone
    | (a, b) when a = VNone || b = VNone -> raise Not_same_clock
    | _ -> raise (Invalid_op2 (">", a, b))

let vlteq a b =
    match (a, b) with
      (VNil, _) -> VNil
    | (_, VNil) -> VNil
    | (VInt x, VInt y)   -> VBool (compare x y <= 0)
    | (VReal x, VReal y) -> VBool (x <= y)
    | (VNone, VNone) -> VNone
    | (a, b) when a = VNone || b = VNone -> raise Not_same_clock
    | _ -> raise (Invalid_op2 ("<=", a, b))

let vgteq a b =
    match (a, b) with
      (VNil, _) -> VNil
    | (_, VNil) -> VNil
    | (VInt x, VInt y)   -> VBool (compare x y >= 0)
    | (VReal x, VReal y) -> VBool (x >= y)
    | (VNone, VNone) -> VNone
    | (a, b) when a = VNone || b = VNone -> raise Not_same_clock
    | _ -> raise (Invalid_op2 (">=", a, b))

(* Helpers *)
let node_name (_, name, _, _) = name (* receive header and return name*)

let rec print_list func = function
    fst :: snd :: rest -> func fst; print_string ", "; print_list func (snd::rest)
  | [s] -> func s
  | [] -> ()

let rec format_value = function
    VBool  b   -> sprintf "Bool: %b" b
  | VInt   i   -> sprintf "Int: %d" (to_int i)
  | VChar  c   -> sprintf "Char: %c" c
  | VReal  r   -> sprintf "Real: %f" r
  | VIdent v   -> sprintf "Undefined: %s" v
  | VList  v   -> "VList: " ^ String.concat ", " (map format_value v)
  | VNil       -> "Nil"
  | VNone      -> "Nothing"

let rec format_v = function
    VBool  b   -> string_of_bool b
  | VInt   i   -> string_of_int (to_int i)
  | VChar  c   -> String.make 1 c
  | VReal  r   -> string_of_float r
  | VIdent v   -> v
  | VList  v   -> "(" ^ String.concat ", " (map format_v v) ^ ")"
  | VNil       -> "_"
  | VNone      -> " "

let format_var_type = function
    TBool -> "bool"
  | TInt -> "int"
  | TChar -> "char"
  | TReal -> "real"
  | TIdent v -> v

let print_value = print_string @. format_value
and print_v = print_string @. format_v

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
  | (_, VNone) -> true
  | _ -> false

let rec format_expr = let f = format_expr in function
  RValue   v     -> format_v v
| Elist    lst   -> "(" ^ String.concat ", " (map f lst) ^ ")"
| IntConv  a     -> "int " ^ f a
| RealConv a     -> "real " ^ f a
| Neg      a     -> "-" ^ f a
| Add     (a, b) -> f a ^ " + " ^ f b
| Minus   (a, b) -> f a ^ " - " ^ f b
| Mult    (a, b) -> f a ^ " * " ^ f b
| Divide  (a, b) -> f a ^ " / " ^ f b
| Div     (a, b) -> f a ^ " div " ^ f b
| Mod     (a, b) -> f a ^ " mod " ^ f b

| Pre      a     -> "pre " ^ f a
| Current  a     -> "current " ^ f a
| Arrow   (a, b) -> f a ^ " -> " ^ f b
| When    (a, b) -> f a ^ " when " ^ format_clock_expr b

| Not      a     -> "not" ^ f a
| And     (a, b) -> f a ^ "and" ^ f b
| Or      (a, b) -> f a ^ "or"  ^ f b
| Xor     (a, b) -> f a ^ "xor" ^ f b

| Eq      (a, b) -> f a ^ " = "  ^ f b
| Ne      (a, b) -> f a ^ " != " ^ f b
| Lt      (a, b) -> f a ^ " < "  ^ f b
| Gt      (a, b) -> f a ^ " > "  ^ f b
| Lteq    (a, b) -> f a ^ " <= " ^ f b
| Gteq    (a, b) -> f a ^ " >= " ^ f b

| If      (c, a, b) -> "if" ^ f c ^ "then" ^ f a ^ "else" ^ f b
| Case    (a, p) -> "case" ^ f a ^ "\n" ^ String.concat "\n" (map (fun (p, e) -> "|" ^ match p with PUnderscore -> "_" | PValue v -> format_v v ^ f e) p)

| Temp           -> raise (Failure "Not supported")

and format_clock_expr = function
  CWhen v -> v
| CNot v -> "not" ^ v
| CMatch (v1, v2) -> v1 ^ "match" ^ v2

let print_expr = print_string @. format_expr

let expr_formatter ppf = Format.fprintf ppf "%s" @. format_expr

