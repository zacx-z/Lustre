{
open Parser
open Parse_aux
open Lexing
open Printf

let error = parse_error

(* overflow is not checked; large constants will be taken modulo 2^32 *)
let parse_int base s =
  let v = ref 0l in
  for i = 0 to String.length s - 1 do
    v := Int32.mul !v (Int32.of_int base);
    let c = s.[i] in
    let digit =
      if      c >= '0' && c <= '9' then Char.code c - Char.code '0'
      else if c >= 'A' && c <= 'F' then Char.code c - Char.code 'A' + 10
      else if c >= 'a' && c <= 'f' then Char.code c - Char.code 'a' + 10
      else assert false in
    if digit >= base then assert false;
    v := Int32.add !v (Int32.of_int digit)
  done;
  !v

(* relies on 'float_of_string' in OCaml library *)
let parse_float s =
  try float_of_string s
  with Failure _ -> error (sprintf "bad float literal '%s'" s)
}


let decdigit = ['0'-'9']
let octdigit = ['0'-'7']
let bindigit = ['0'-'1']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let letter   = ['a'-'z' 'A'-'Z']

let decnum = '0' | ['1'-'9']['0'-'9']*

let ident = (letter | '_') (letter | '_' | decdigit)*

let character = [ '~' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '-' '+'
                  '/' ';' ':' ''' '"' '?' '<' '>' '=' '{' '}' ]
                | letter | decdigit | ' ' | '\t'

let exponent = ['e' 'E']['+' '-']? decdigit+
let fraction = '.' decdigit+
let decfloat = (decdigit* fraction)
             | (decdigit+ exponent)
             | (decdigit* fraction exponent)
             | (decdigit+ '.')
             | (decdigit+ '.' exponent)

let whitespace = [' ' '\t' '\r']
let newline    = '\n'

rule initial = parse
    (* whitespaces *)
    | whitespace                { initial lexbuf                        }
    | newline                   { new_line lexbuf; initial lexbuf       }

    (* comments *)
    | "/*"                      { comment lexbuf;        initial lexbuf }
    | "--"                      { onelinecomment lexbuf; initial lexbuf }

    (* packages *)
    | "package"                 { PACKAGE       }
    | "end"                     { END           }
    | "private"                 { PRIVATE       }
    | "public"                  { PUBLIC        }
    | "open"                    { OPEN          }
    | "::"                      { COLONCOLON    }

    (* top-level *)
    | "type"                    { TYPE          }
    | "const"                   { CONST         }
    | "node"                    { NODE          }
    | "function"                { FUNCTION      }
    
    | "returns"                 { RETURNS       }
    | "var"                     { VAR           }
    | "let"                     { LET           }
    | "tel"                     { TEL           }
    | "clock"                   { CLOCK         }
    | eof                       { EOF           }

    (* arithmetic *)
    | '+'                       { PLUS          }
    | '-'                       { MINUS         }
    | '*'                       { MULT          }
    | '/'                       { DIVIDE        }
    | "mod"                     { MOD           }
    | "div"                     { DIV           }

    (* logic *)
    | "and"                     { AND           }
    | "or"                      { OR            }
    | "xor"                     { XOR           }
    | "not"                     { NOT           }
    | '#'                       { SHARP         }

    (* relation *)
    | '='                       { EQ            }
    | "<>"                      { NE            }
    | '<'                       { LT            }
    | '>'                       { GT            }
    | "<="                      { LTEQ          }
    | ">="                      { GTEQ          }

    (* tempo *)
    | "when"                    { WHEN          }
    | "pre"                     { PRE           }
    | "fby"                     { FBY           }
    | "->"                      { ARROW         }
    | "times"                   { TIMES         }
    | "match"                   { MATCH         }
    | "merge"                   { MERGE         }

    (* switch *)
    | "if"                      { IF            }
    | "then"                    { THEN          }
    | "else"                    { ELSE          }
    | "case"                    { CASE          }
    | "of"                      { OF            }

    (* struct *)
    | '.'                       { DOT           }

    (* array *)
    | '^'                       { CARET         }
    | '@'                       { AT            }
    | "reverse"                 { REVERSE       }
    | ".."                      { DOTDOT        }

    (* brackets *)
    | '('                       { LPAREN        }
    | ')'                       { RPAREN        }
    | '['                       { LBRACKET      }
    | ']'                       { RBRACKET      }
    | '{'                       { LBRACE        }
    | '}'                       { RBRACE        }
    | "<<"                      { LANGLE        }
    | ">>"                      { RANGLE        }

    (* delimiters *)
    | ':'                       { COLON         }
    | ';'                       { SEMICOLON     }
    | ','                       { COMMA         }
    | '|'                       { PIPE          }

    (* types *)
    | "bool"                    { BOOL          }
    | "int"                     { INT           }
    | "real"                    { REAL          }
    | "char"                    { CHAR          }
    | "enum"                    { ENUM          }

    (* constants *)
    | decnum as n               { CONST_INT (parse_int 10 n)        }
    | "0"  (octdigit+ as n)     { CONST_INT (parse_int 8  n)        }
    | "0b" (bindigit+ as n)     { CONST_INT (parse_int 2  n)        }
    | "0x" (hexdigit+ as n)     { CONST_INT (parse_int 16 n)        }

    | "true"                    { TRUE                              }
    | "false"                   { FALSE                             }

    | decfloat as f             { CONST_REAL (parse_float f)        }

    | ''' (character as c) '''  { CONST_CHAR c                      }

    (* others *)
    | '_'                       { UNDERSCORE                        }
    | ident as i                { IDENT i                           }
    | _                         { error "invalid symbol";           }

and comment = parse
    | newline                   { new_line lexbuf; comment lexbuf   }
    | "*/"                      { ()                                }
    | eof                       { error "unterminated comment"      }
    | _                         { comment lexbuf                    }

and onelinecomment = parse
    | newline                   { new_line lexbuf; ()               }
    | eof                       { ()                                }
    | _                         { onelinecomment lexbuf             }

