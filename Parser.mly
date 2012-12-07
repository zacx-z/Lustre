%{
open Type
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
  | BOOL                                  { Bool }
  | INT                                   { Int }
  | REAL                                  { Real }
  | CHAR                                  { Char }
  | ident_nonlocal                        { Ident $1 }
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
  { { header=$1; equations=[] } }
  | node_header equations_single SEMICOLON
  { { header=$1; equations=[] } }
  | node_header optional_semicolon LET equations TEL optional_semicolon
  { { header=$1; equations=$4 } }
  | node_header2 optional_semicolon LET equations TEL optional_semicolon
  { { header=$1; equations=$4 } }
  ;

node_header:
  | op_kind ident_nonlocal params RETURNS params        { ($1, $2, $3, $5) }
  ;

node_header2:
  | node_header local_block               { $1 }
  ;

/* Ignoring the difference between a node and a function */
op_kind:
  | NODE                                  { Node }
  | FUNCTION                              { Function }
  ;

local_block:
  | VAR var_decls2                        { 1 }
  ;

var_decls2:
  | /* empty */                           { 1 }
  | var_decl SEMICOLON var_decls2         { 1 }
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
  | IDENT                                 { Is $1 }
  | NOT IDENT                             { Not $2 }
  | IDENT MATCH ident_nonlocal            { Match ($1, $3) }
  ;

var_ids:
  | var_id                                { [$1] }
  | var_id COMMA var_ids                  { $1 :: $3 }
  ;

var_id:
  | CLOCK IDENT                           { Clock $2 }
  | IDENT                                 { V $1 }
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
  | ident_local                           { I $1 }
  | UNDERSCORE                            { Underscore }
  ;


/* Local variables will shadow any non-local variable.
   There are no warning reported. */
ident_expr:
  | IDENT                                 { Var $1 }
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
  | ident_local                           { 1 }
  | NOT ident_local                       { 1 }
  | ident_local MATCH ident_nonlocal      { 1 }
  ;

elist:
  | /* empty */                           { [] }
  | expr                                  { [$1] }
  | expr COMMA elist                      { $1 :: $3 }

expr:
  | ident_expr                            { RValue $1 }
  | const                                 { RValue $1 }
  | list_expr                             { $1 }
  | tempo_expr                            { Temp $1 }
  | arith_expr                            { $1 }
  | relation_expr                         { Temp $1 }
  | bool_expr                             { Temp $1 }
  | switch_expr                           { Temp $1 }
  | apply_expr                            { Temp $1 }
//   | array_expr      
//   | struct_expr     
  ;

const:
  | TRUE                                  { True }
  | FALSE                                 { False }
  | CONST_CHAR                            { CChar $1 }
  | CONST_INT                             { CInt $1 }
  | CONST_REAL                            { CReal $1 }
  ;

const_patt:
  | TRUE                                  { 1 }
  | FALSE                                 { 1 }
  | CONST_CHAR                            { 1 }
  | CONST_INT                             { 1 }
  | MINUS CONST_INT                       { 1 }
  | CONST_REAL                            { 1 }
  ;

list_expr:
  | LPAREN elist RPAREN                   { Elist $2 }
  ;

tempo_expr:
  | PRE expr                              { 1 }
  | expr ARROW expr                       { 1 }
  | expr WHEN clock_expr                  { 1 }
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
  | expr EQ expr                          { 1 }
  | expr NE expr                          { 1 }
  | expr LT expr                          { 1 }
  | expr GT expr                          { 1 }
  | expr LTEQ expr                        { 1 }
  | expr GTEQ expr                        { 1 }
  ;

bool_expr:
  | NOT expr                              { 1 }
  | expr AND expr                         { 1 }
  | expr OR expr                          { 1 }
  | expr XOR expr                         { 1 }
  ;

switch_expr:
  | IF expr THEN expr ELSE expr           { 1 }
  | LPAREN CASE expr OF case_exprs RPAREN { 1 }
  ;

case_exprs:
  | PIPE UNDERSCORE COLON expr            { 1 }
  | PIPE case_expr                        { 1 }
  | PIPE case_expr case_exprs             { 1 }
  ;

case_expr:
  | const_patt COLON expr                 { 1 }
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
