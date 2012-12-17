%{
open Type
open Int32
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
%type <Type.program> file
%%

file:
  | decls EOF                 { $1 }
  ;

decls:
  | TYPE type_decls decls                 { $3 }
  | CONST const_decls decls               { $3 }
  | user_op_decl decls                    { { $2 with nodes = (node_name
  $1.header, $1) :: $2.nodes; } }
  | /* empty */                           { { types=[]; nodes=[] } }
  ;

type_decls:
  | type_decl                             { 1 }
  | type_decl type_decls                  { 1 }

type_decl:
  | type_decl0 EQ type_expr SEMICOLON     { 1 }
  ;

type_decl0:
  | ident_nonlocal                        { 1 }
  ;

type_expr:
  | BOOL                                  { TBool }
  | INT                                   { TInt }
  | REAL                                  { TReal }
  | CHAR                                  { TChar }
  | ident_nonlocal                        { TIdent $1 }
//  | LBRACE field_decls RBRACE           { 1 }
//  | type_expr CARET expr                { 1 }
  ;

// 
// field_decls:
//      field_decl                        { 1 }
//  |   field_decl COMMA field_decls      { 1 }
// ;
// 
// field_decl:
//      IDENT COLON type_expr             { 1 }
// ;

const_decls:
  | const_decl                            { 1 }
  | const_decl const_decls                { 1 }
  ;

const_decl:
  | const_decl0 COLON type_expr EQ expr SEMICOLON   { 1 }
  ;

const_decl0:
  | ident_nonlocal                        { 1 }
  ;

user_op_decl:
  | node_header SEMICOLON
  { { header=$1; locals = []; equations=[] } }
  | node_header equations_single SEMICOLON
  { { header=$1; locals = []; equations=[] } }
  | node_header optional_semicolon LET equations TEL optional_semicolon
  { { header=$1; locals = []; equations=$4 } }
  | node_header2 optional_semicolon LET equations TEL optional_semicolon
  { { header=fst $1; locals = snd $1; equations=$4 } }
  ;

node_header:
  | op_kind ident_nonlocal params RETURNS params        { ($1, $2, $3, $5) }
  ;

node_header2:
  | node_header local_block               { ($1, $2) }
  ;

/* Ignoring the difference between a node and a function */
op_kind:
  | NODE                                  { Node }
  | FUNCTION                              { Function }
  ;

local_block:
  | VAR var_decls2                        { $2 }
  ;

var_decls2:
  | /* empty */                           { [] }
  | var_decl SEMICOLON var_decls2         { $1 :: $3 }
  ;

params:
  | LPAREN RPAREN                         { [] }
  | LPAREN var_decls RPAREN               { $2 }
  ;

var_decls:
  | var_decl                              { [$1] }
  | var_decl SEMICOLON var_decls          { $1 :: $3 }
  ;

var_decl:
  | var_ids COLON type_expr when_decl     { ($1, $3, $4) }
  ;

when_decl:
  | WHEN csexpr                           { Some $2 }
  | /* empty */                           { None }
  ;

csexpr:
  | IDENT                                 { CWhen $1 }
  | NOT IDENT                             { CNot $2 }
  | IDENT MATCH ident_nonlocal            { CMatch ($1, $3) }
  ;

var_ids:
  | var_id                                { [$1] }
  | var_id COMMA var_ids                  { $1 :: $3 }
  ;

var_id:
  | CLOCK IDENT                           { ($2, true) }
  | IDENT                                 { ($1, false) }
  ;

optional_semicolon:
  | SEMICOLON                             { 1 }
  | /* empty */                           { 1 }
  ;

equations_single:
  | equation                              { [$1] }
  ;

equations:
  | equation SEMICOLON equations          { $1 :: $3 }
  | /* empty */                           { [] }
  ;

equation:
  | lhs EQ expr                           { ($1, $3) }
  ;

lhs:
  | lhs_id                                { [$1] }
  | lhs_id COMMA lhs                      { $1 :: $3 }
  ;

lhs_id:
  | ident_local                           { LIdent $1 }
  | UNDERSCORE                            { Underscore }
  ;


/* Local variables will shadow any non-local variable.
   There are no warning reported. */
ident_expr:
  | IDENT                                 { VIdent $1 }
  ;

ident_nonlocal:
  | IDENT                                 { $1 }
  ;

ident_label:
  | IDENT                                 { $1 }
  ;

ident_local:
  | IDENT                                 { $1 }
  ;

clock_expr:
  | ident_local                           { CWhen $1  }
  | NOT ident_local                       { CNot $2 }
  | ident_local MATCH ident_nonlocal      { CMatch ($1, $3) }
  ;

elist:
  | /* empty */                           { [] }
  | expr                                  { [$1] }
  | expr COMMA elist                      { $1 :: $3 }

expr:
  | ident_expr                            { RValue $1 }
  | const                                 { RValue $1 }
  | list_expr                             { $1 }
  | tempo_expr                            { $1 }
  | arith_expr                            { $1 }
  | relation_expr                         { $1 }
  | bool_expr                             { $1 }
  | switch_expr                           { $1 }
  | apply_expr                            { Temp }
//   | array_expr      
//   | struct_expr     
  ;

const:
  | TRUE                                  { VBool true }
  | FALSE                                 { VBool false }
  | CONST_CHAR                            { VChar $1 }
  | CONST_INT                             { VInt $1 }
  | CONST_REAL                            { VReal $1 }
  ;

const_patt:
  | TRUE                                  { VBool true }
  | FALSE                                 { VBool false }
  | CONST_CHAR                            { VChar $1 }
  | CONST_INT                             { VInt $1 }
  | MINUS CONST_INT                       { VInt (neg $2) }
  | CONST_REAL                            { VReal $1 }
  ;

list_expr:
  | LPAREN elist RPAREN                   { Elist $2 }
  ;

tempo_expr:
  | PRE expr                              { Pre $2 }
  | expr ARROW expr                       { Arrow ($1, $3) }
  | expr WHEN clock_expr                  { When ($1, $3) }
//  | FBY LPAREN elist SEMICOLON CONST_INT SEMICOLON elist RPAREN
//                            
  ;

arith_expr:
  | MINUS expr %prec UMINUS               { Neg $2 }
  | PLUS expr  %prec UPLUS                { $2 }
  | INT expr                              { IntConv $2 } //TODO is this?
  | REAL expr                             { RealConv $2 } //TODO
  | expr PLUS expr                        { Add    ($1,$3) }
  | expr MINUS expr                       { Minus  ($1,$3) }
  | expr MULT expr                        { Mult   ($1,$3) }
  | expr DIVIDE expr                      { Divide ($1,$3) }
  | expr DIV expr                         { Div    ($1,$3) }
  | expr MOD expr                         { Mod    ($1,$3) }
  ;

relation_expr:
  | expr EQ expr                          { Eq ($1, $3) }
  | expr NE expr                          { Ne ($1, $3) }
  | expr LT expr                          { Lt ($1, $3) }
  | expr GT expr                          { Gt ($1, $3) }
  | expr LTEQ expr                        { Lteq ($1, $3) }
  | expr GTEQ expr                        { Gteq ($1, $3) }
  ;

bool_expr:
  | NOT expr                              { Not $2 }
  | expr AND expr                         { And ($1, $3) }
  | expr OR expr                          { Or ($1, $3) }
  | expr XOR expr                         { Xor ($1, $3) }
  ;

switch_expr:
  | IF expr THEN expr ELSE expr           { If ($2, $4, $6) }
  | LPAREN CASE expr OF case_exprs RPAREN { Case ($3, $5) }
  ;

case_exprs:
  | PIPE UNDERSCORE COLON expr            { [(PUnderscore, $4)] }
  | PIPE case_expr                        { [$2] }
  | PIPE case_expr case_exprs             { $2 :: $3 }
  ;

case_expr:
  | const_patt COLON expr                 { (PValue $1, $3) }
  ;

apply_expr:
  | ident_nonlocal LPAREN elist RPAREN    { 1 }
  ;

// 
// struct_expr:
//      expr DOT IDENT      
//  |   LBRACE label_exprs RBRACE
//                           
// ;
// 
// label_exprs:
//      label_expr      
//  |   label_expr COMMA label_exprs    
// ;
// 
// label_expr:
//      IDENT COLON expr    
// ;
// 
// array_expr:
//      expr index      
//  |   expr CARET expr 
// ;
// 
// index:
//      LBRACK expr RBRACK 
// ;
// 
