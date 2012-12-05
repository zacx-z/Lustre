%{
open Camlcoq
open Parse_aux
open Untyped
open Names
open Printf
open List

let coqchar_of x =
    coqint_of_camlint (Int32.of_int (Char.code x))

let coqint_of x =
    coqint_of_camlint x

let coqpos_of x =
    positive_of_camlint x

let cur_nonlocal_ident = ref None

let cur_nonlocal () =
  match !cur_nonlocal_ident with
    | None -> assert false
    | Some x -> x

let define_ident idtype =
  let id = cur_nonlocal () in
  let i = get_ident_info id in
  if i.defined <> IUNDEFINED then
    parse_error (sprintf "duplicated definition of identifier %s"
      (format_ident_verbose id))
  else
    i.defined <- idtype;
    i.loc <- get_current_loc ()

(* I wonder why this function does not appear in the OCaml
   standard library *)
let rec replicate n a =
  if n = 0 then [a]
  else a :: replicate (n-1) a

type csexpr =
    CSwhen of string
  | CSnot of string
  | CSmatch of string * BinPos.positive
  | CSdefault

type local_var = {
    var_ident     : string;
    var_is_clock  : bool;
    var_type      : coq_type;
    var_csexpr    : csexpr;
  }

let define_vars ps vartype =
  let i = cur_nonlocal () in
  iter (fun x -> append_local i vartype x.var_ident) ps

let format_var_str i v =
  match get_local i v with
    | None -> sprintf "???_%s" v
    | Some x -> format_var i x

let warn_var_clock ps =
  let i = cur_nonlocal () in
  iter (fun x ->
    if x.var_is_clock then
      parse_warn (sprintf "%s is declared as a clock variable, but this takes no effect"
        (format_var_str i x.var_ident))
    else () ) ps

let get_local' str =
  let i = cur_nonlocal () in
  match get_local i str with
    | Some x    -> x
    | None      -> parse_error (sprintf "local variable not declared: %s" str)

let from_csexpr cs =
  match cs with
  | CSwhen  s        -> Cwhen  (get_local' s)
  | CSnot   s        -> Cnot   (get_local' s)
  | CSmatch (s, ii)  -> Cmatch (get_local' s, ii)
  | CSdefault        -> Cdefault

let define_local lst eq =
  match eq with
    | (Some v, e) -> begin
        let vstr = format_var (cur_nonlocal ()) v in
        if mem_assoc v lst then
          parse_error (sprintf "local variable %s defined multiple times" vstr)
        else match v with
          | Vinp _ -> parse_error (sprintf "input parameter %s must not be defined" vstr)
          | Vinq _ -> parse_error (sprintf "input parameter %s must not be defined" vstr)
          | Vloc _ -> (v,e) :: lst
          | Vout _ -> (v,e) :: lst
      end
    | (None, _) -> lst

let define_locals' n lst local_cat local_cat_str =
  let rec define_locals'' i num =
    if i > n then Enil
    else
      let v = local_cat num in
      let x = try assoc v lst
              with Not_found -> parse_error (
                                  sprintf "%s %s not defined"
                                  local_cat_str
                                  (format_var (cur_nonlocal ()) v)
                                ) in
      Econs (x, define_locals'' (i+1) (Datatypes.S num)) in

  define_locals'' 1 Datatypes.O

let define_locals node lst =
  {
    node with
    dloc = define_locals' (length node.tloc) lst (fun x -> Vloc x) "local variable";
    dout = define_locals' (length node.tout) lst (fun x -> Vout x) "output parameter";
  }
%}

/* package */
%token PACKAGE END
%token PRIVATE PUBLIC
%token OPEN
%token COLONCOLON

/* top-level */
%token TYPE CONST NODE FUNCTION
%token RETURNS VAR LET TEL
%token CLOCK
%token EOF

/* operators */
%token PLUS MINUS MULT DIVIDE MOD DIV         /* arithmetic */
%token AND OR XOR NOT SHARP                   /* logic      */
%token EQ NE LT LTEQ GT GTEQ                  /* relation   */
%token WHEN PRE FBY ARROW TIMES MATCH MERGE   /* tempo      */
%token IF THEN ELSE CASE OF                   /* switch     */
%token DOT                                    /* struct     */
%token CARET AT REVERSE DOTDOT                /* array      */

/* brackets */
%token LPAREN   RPAREN
%token LBRACKET RBRACKET
%token LBRACE   RBRACE
%token LANGLE   RANGLE

/* delimiters */
%token COLON SEMICOLON COMMA PIPE

/* constants */
%token <int32> CONST_INT
%token TRUE FALSE
%token <float> CONST_REAL
%token <char> CONST_CHAR

/* types */
%token BOOL INT REAL CHAR
%token ENUM

/* others */
%token <string> IDENT
%token UNDERSCORE


/* precedences */
%nonassoc   TIMES
%nonassoc   ELSE
%nonassoc   ARROW
%nonassoc   OR XOR
%nonassoc   AND
%nonassoc   EQ NE LT LTEQ GT GTEQ
%left       PLUS MINUS
%left       MULT DIVIDE MOD DIV
%nonassoc   UPLUS UMINUS
%nonassoc   PRE
%nonassoc   REVERSE INT REAL
%nonassoc   WHEN
%nonassoc   NOT
%nonassoc   CARRET
%nonassoc   AT
%nonassoc   DOT INDEX




%start file
%type <Untyped.program> file
%%

file:
  | decls EOF                 { $1 }
  ;

decls:
  | TYPE type_decls decls     { { $3 with prog_type  = $2 @ $3.prog_type;   } }
  | CONST const_decls decls   { { $3 with prog_const = $2 @ $3.prog_const;  } }
  | user_op_decl decls        { { $2 with prog_node  = $1 ::$2.prog_node;   } }
  | /* empty */               {
      let main_node_name = !Clflags.option_mainnode in
      let main_node = add_nonlocal main_node_name in
      { prog_type = [];
        prog_const = [];
        prog_node = [];
        prog_main = main_node; }
    }
  ;

type_decls:
  | type_decl                           { [$1] }
  | type_decl type_decls                { $1::$2 }

type_decl:
  | type_decl0 EQ type_expr SEMICOLON      { ($1, $3) }
  ;

type_decl0:
  | ident_nonlocal        { cur_nonlocal_ident := Some $1; define_ident ITYPE; $1 }
  ;

type_expr:
  | BOOL                        { Tbool                       }
  | INT                         { Tint                        }
  | REAL                        { Treal                       }
  | CHAR                        { Tchar                       }
  | ident_nonlocal              { Tnonlocal $1                }
//  | LBRACE field_decls RBRACE   { Branch("construct", $2) }
//  | type_expr CARET expr        { Branch("array", [$1;$3]) }
  ;

// 
// field_decls:
//      field_decl                      { [$1] }
//  |   field_decl COMMA field_decls    { $1::$3 }
// ;
// 
// field_decl:
//      IDENT COLON type_expr   { Branch("field", [Leaf $1; $3]) }
// ;

const_decls:
  | const_decl              { [$1] }
  | const_decl const_decls  { $1::$2 }
  ;

const_decl:
  | const_decl0 COLON type_expr EQ expr SEMICOLON   { (($1, $3), $5) }
  ;

const_decl0:
  | ident_nonlocal         { cur_nonlocal_ident := Some $1; define_ident ICONST; $1 }
  ;

user_op_decl:
  | user_op_decl1            { (cur_nonlocal (), $1) }
  ;

user_op_decl1:
  | node_header SEMICOLON                               { $1                   }
  | node_header equations_single SEMICOLON              { define_locals $1 $2  }
  | node_header  LET equations TEL optional_semicolon   { define_locals $1 $3  }
  | node_header2 LET equations TEL optional_semicolon   { define_locals $1 $3  }
  ;

node_header:
  | op_kind ident_nonlocal params RETURNS params {
      cur_nonlocal_ident := Some $2;
      define_ident INODE;
      let (inp, inq) = partition (fun p -> p.var_is_clock) $3 in
      define_vars inp INP;
      define_vars inq INQ;
      define_vars $5  OUT;
      warn_var_clock $5;
      {
        tinp = map (fun x -> (x.var_type, from_csexpr x.var_csexpr)) inp;
        tinq = map (fun x -> (x.var_type, from_csexpr x.var_csexpr)) inq;
        tloc = [];
        tout = map (fun x -> (x.var_type, from_csexpr x.var_csexpr)) $5;
        
        dloc = Enil;
        dout = Enil;
        
        input_clock = map (fun x -> x.var_is_clock) $3;
      }
    }
  ;

node_header2:
  | node_header local_block {
      define_vars $2 LOC;
      warn_var_clock $2;
      {
        $1 with
        tloc = map (fun x -> (x.var_type, from_csexpr x.var_csexpr)) $2;
      }
    }
  ;

/* Ignoring the difference between a node and a function */
op_kind:
  | NODE        { () }
  | FUNCTION    { () }
  ;

local_block:
  | VAR var_decls2                  { $2    }
  ;

var_decls2:
  | /* empty */                     { []      }
  | var_decl SEMICOLON var_decls2   { $1 @ $3 }
  ;

params:
  | LPAREN RPAREN                   { []    }
  | LPAREN var_decls RPAREN         { $2    }
  ;

var_decls:
  | var_decl                        { $1      }
  | var_decl SEMICOLON var_decls    { $1 @ $3 }
  ;

var_decl:
  | var_ids COLON type_expr when_decl { map (fun (i,c) -> {
      var_ident    = i;
      var_is_clock = c;
      var_type     = $3;
      var_csexpr   = $4;
    }) $1 }
  ;

when_decl:
  | WHEN csexpr             { $2                                }
  | /* empty */             { CSdefault                         }
  ;

csexpr:
  | IDENT                       { CSwhen $1                               }
  | NOT IDENT                   { CSnot $2                                }
  | IDENT MATCH ident_nonlocal  { CSmatch ($1, $3)                        }
  ;

var_ids:
  | var_id                  { [$1]      }
  | var_id COMMA var_ids    { $1 :: $3  }
  ;

var_id:
  | CLOCK IDENT             { ($2, true)  }
  | IDENT                   { ($1, false) }
  ;

optional_semicolon:
  | SEMICOLON               { () }
  | /* empty */             { () }
  ;

equations_single:
  | equation                      { fold_left define_local [] $1 }
  ;

equations:
  | equation SEMICOLON equations  { fold_left define_local $3 $1 }
  | /* empty */                   { [] }
  ;

equation:
  | lhs EQ expr        {
      let pos = ref Datatypes.O in
      map (fun x ->
        let p = !pos in
        pos := Datatypes.S !pos;
        (x, Eextract (p, Datatypes.length $1, $3))
      ) $1
    }
  ;

lhs:
  | lhs_id                { [$1]    }
  | lhs_id COMMA lhs      { $1::$3  }
  ;

lhs_id:
  | ident_local           { Some $1 }
  | UNDERSCORE            { None    }
  ;


/* Local variables will shadow any non-local variable.
   There are no warning reported. */
ident_expr:
  | IDENT   {
      let nodeid = cur_nonlocal () in
      match get_local nodeid $1 with
        | None -> Econst (Snonlocal (add_nonlocal $1))
        | Some x -> Evar x
    }
  ;

ident_nonlocal:
  | IDENT     { add_nonlocal $1 }
  ;

ident_label:
  | IDENT     { add_label $1    }
  ;

ident_local:
  | IDENT     { get_local' $1   }
  ;

clock_expr:
  | ident_local                       { Pwhen  $1               }
  | NOT ident_local                   { Pnot   $2               }
  | ident_local MATCH ident_nonlocal  { Pmatch ($1,$3)          }
  ;

elist:
  | /* empty */         { Enil              }
  | expr                { Econs ($1, Enil)  }
  | expr COMMA elist    { Econs ($1, $3)    }

expr:
  | ident_expr          { $1              }
  | const               { Econst $1       }
  | list_expr           { $1              }
  | tempo_expr          { $1              }
  | arith_expr          { $1              }
  | relation_expr       { $1              }
  | bool_expr           { $1              }
  | switch_expr         { $1              }
  | apply_expr          { $1              }
//   | array_expr      { $1 }
//   | struct_expr     { $1 }
  ;

const:
  | TRUE            { Sbool true                        }
  | FALSE           { Sbool false                       }
  | CONST_CHAR      { Schar (coqchar_of $1)             }
  | CONST_INT       { Sint  (coqint_of $1)              }
  | CONST_REAL      { Sreal $1                          }
  ;

const_patt:
  | TRUE            { Sbool true                        }
  | FALSE           { Sbool false                       }
  | CONST_CHAR      { Schar (coqchar_of $1)             }
  | CONST_INT       { Sint  (coqint_of $1)              }
  | MINUS CONST_INT { Sint  (coqint_of (Int32.neg $2))  }
  | CONST_REAL      { Sreal $1                          }
  ;

list_expr:
  | LPAREN elist RPAREN      { Elist $2                              }
  ;

tempo_expr:
  | PRE expr                { Epre $2                               }
  | expr ARROW expr         { Earrow ($1, $3)                       }
  | expr WHEN clock_expr    { Ewhen ($1, $3)                        }
//  | FBY LPAREN elist SEMICOLON CONST_INT SEMICOLON elist RPAREN
//                            { Efby ($3, coqint_of $5, $7)           }
  ;

arith_expr:
  | MINUS expr %prec UMINUS { Eunop  (Oneg , $2    )    }
  | PLUS expr  %prec UPLUS  { Eunop  (Opos , $2    )    }
  | INT expr                { Eunop  (Oint , $2    )    }
  | REAL expr               { Eunop  (Oreal, $2    )    }
  | expr PLUS expr          { Ebinop (Oadd , $1, $3)    }
  | expr MINUS expr         { Ebinop (Osub , $1, $3)    }
  | expr MULT expr          { Ebinop (Omul , $1, $3)    }
  | expr DIVIDE expr        { Ebinop (Odivr, $1, $3)    }
  | expr DIV expr           { Ebinop (Odivi, $1, $3)    }
  | expr MOD expr           { Ebinop (Omod , $1, $3)    }
  ;

relation_expr:
  | expr EQ expr            { Ebinop (Oeq  , $1, $3)    }
  | expr NE expr            { Ebinop (One  , $1, $3)    }
  | expr LT expr            { Ebinop (Olt  , $1, $3)    }
  | expr GT expr            { Ebinop (Ogt  , $1, $3)    }
  | expr LTEQ expr          { Ebinop (Ole  , $1, $3)    }
  | expr GTEQ expr          { Ebinop (Oge  , $1, $3)    }
  ;

bool_expr:
  | NOT expr                { Eunop  (Onot , $2    )    }
  | expr AND expr           { Ebinop (Oand , $1, $3)    }
  | expr OR expr            { Ebinop (Oor  , $1, $3)    }
  | expr XOR expr           { Ebinop (Oxor , $1, $3)    }
  ;

switch_expr:
  | IF expr THEN expr ELSE expr             { Eif ($2, $4, $6)            }
  | LPAREN CASE expr OF case_exprs RPAREN   { Ecase ($3, fst $5, snd $5)  }
  ;

case_exprs:
  | PIPE UNDERSCORE COLON expr              { ([],             Econs ($4,    Enil  )) }
  | PIPE case_expr                          { ([fst $2],       Econs (snd $2,Enil  )) }
  | PIPE case_expr case_exprs               { (fst $2::fst $3, Econs (snd $2,snd $3)) }
  ;

case_expr:
  | const_patt COLON expr                   { ($1, $3)                    }
  ;

apply_expr:
  | ident_nonlocal LPAREN elist RPAREN      { Eapply ($1, $3)             }
  ;

// 
// struct_expr:
//      expr DOT IDENT      { Branch( "field_access", [$1; Leaf $3]) }
//  |   LBRACE label_exprs RBRACE
//                          { Branch( "construct_with_type", $2) } 
// ;
// 
// label_exprs:
//      label_expr      { [$1] }
//  |   label_expr COMMA label_exprs    { $1::$3 }
// ;
// 
// label_expr:
//      IDENT COLON expr    { Branch("label_expr", [Leaf $1; $3]) }
// ;
// 
// array_expr:
//      expr index      { Branch("array_index", [$1; $2]) }
//  |   expr CARET expr { Branch("array_dim", [$1; $3]) }
// ;
// 
// index:
//      LBRACK expr RBRACK { $2 }
// ;
// 
