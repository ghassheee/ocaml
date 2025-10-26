{
open Support
open Parser


let reservedWords = [
  (* Keywords *)
    ("All",     fun i -> ALL i);
    ("Exists",  fun i -> SOME i);
    ("Top",     fun i -> TOP i);
    ("ref",     fun i -> REF i);
    ("Ref",     fun i -> REFTYPE i);
    ("letrec",  fun i -> LETREC i);
    ("fix",     fun i -> FIX i);
    ("Float",   fun i -> FLOAT i);
    ("*.",      fun i -> TIMESFLOAT i);
    ("String",  fun i -> STRING i);
    ("case",    fun i -> CASE i);
    ("of",      fun i -> OF i);
    ("as",      fun i -> AS i);
    ("unit",    fun i -> UNIT i);
    ("Unit",    fun i -> UNITTYPE i);
    ("where",   fun i -> WHERE i);
    ("in",      fun i -> IN i);
    ("let",     fun i -> LET i);
    ("Bool",    fun i -> BOOL i);
    ("Nat",     fun i -> NAT i);
    ("\\",      fun i -> LAM i);
    ("if",      fun i -> IF i);
    ("then",    fun i -> THEN i);
    ("else",    fun i -> ELSE i);
    ("true",    fun i -> TRUE i);
    ("false",   fun i -> FALSE i);
    ("succ",    fun i -> SUCC i);
    ("pred",    fun i -> PRED i);
    ("iszero",  fun i -> ISZERO i);
  
  (* Symbols *)
    ("_",       fun i -> USCORE i);
    ("'",       fun i -> APOSTROPHE i);
    ("\"",      fun i -> DQUOTE i);
    ("!",       fun i -> BANG i);
    ("#",       fun i -> HASH i);
    ("$",       fun i -> TRIANGLE i);
    ("*",       fun i -> STAR i);
    ("|",       fun i -> VBAR i);
    (".",       fun i -> DOT i);
    (";",       fun i -> SEMI i);
    (",",       fun i -> COMMA i);
    ("/",       fun i -> SLASH i);
    (":",       fun i -> COLON i);
    ("::",      fun i -> COLONCOLON i);
    ("=",       fun i -> EQ i);
    ("==",      fun i -> EQEQ i);
    ("[",       fun i -> LSQUARE i); 
    ("<",       fun i -> LT i);
    ("{",       fun i -> LCUR i); 
    ("(",       fun i -> LPAREN i); 
    ("<-",      fun i -> LEFTARROW i); 
    ("{|",      fun i -> LCURBAR i); 
    ("[|",      fun i -> LSQUAREBAR i); 
    ("}",       fun i -> RCUR i);
    (")",       fun i -> RPAREN i);
    ("]",       fun i -> RSQUARE i);
    (">",       fun i -> GT i);
    ("|}",      fun i -> BARRCUR i);
    ("|>",      fun i -> BARGT i);
    ("|]",      fun i -> BARRSQUARE i);
    ("\n",      fun i -> NEWLINE i); 
    (";;",      fun i -> DSEMI i); 

  (* Special compound symbols: *)
    (":=",      fun i -> COLONEQ i);
    ("->",      fun i -> ARROW i);
    ("=>",      fun i -> DARROW i);
    ("==>",     fun i -> DDARROW i);
]

(* Support functions *)
let fos                     =   float_of_string
let ios                     =   int_of_string 

type tokentbl               =   (string, info->token) Hashtbl.t
let  symbolTable:tokentbl   =   Hashtbl.create 1024
let _                       =   List.iter (fun(str,f)->Hashtbl.add symbolTable str f) reservedWords
let initCapital str         =   let s=String.get str 0 in s>='A'&&s<='Z'  

let createID i str          =   (* info -> string -> token *)
  try   Hashtbl.find symbolTable str i
  with _ -> if initCapital str then UCID {i=i;v=str} else LCID {i=i;v=str}

let startLex                =   ref dummy
let lineno                  =   ref 1
and start                   =   ref 0

(* filename *) 
let filename                =   ref ""
let set_filename f          =   (if not (Filename.is_implicit f)
                                    then filename := f 
                                    else filename := Filename.concat (Sys.getcwd()) f );
                                start := 0; lineno := 1
(* comment nest depth *) 
let depth                   =   ref 0

(* line number *) 
let newline lexbuf          =   incr lineno; start := (Lexing.lexeme_start lexbuf)
let info    lexbuf          =   createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)
let text                    =   Lexing.lexeme

(* string lexbuf *) 
let str_buf                 =   ref (Bytes.create 2048)
let str_end                 =   ref 0
let reset_str ()            =   str_end := 0
let add_char c              =
    let x           =   !str_end in
    let buf         =   !str_buf in
    if x=Bytes.length buf
    then (
        let newbuf   = Bytes.create (x*2) in
        Bytes.blit buf 0 newbuf 0 x;
        Bytes.set newbuf x c;
        str_buf     := newbuf;
        str_end     := x+1
    ) else (
        Bytes.set buf x c;
        str_end       := x+1
    )
let get_str ()                   = Bytes.to_string(Bytes.sub (!str_buf) 0 (!str_end))


let extractLineno yytxt offset  = ios(Bytes.to_string(Bytes.sub yytxt offset(Bytes.length yytxt-offset)))
let out_of_char x fi            = if x>255 then error fi"Illegal Char" else Char.chr x 
}


(* The main body of the lexical analyzer *)

let digit                   = ['0'-'9'] 
let init                    = ['a'-'z' 'A'-'Z' '_' ]
let tail                    = ['a'-'z' 'A'-'Z' '_' '0'-'9' '\'']
let tabs                    = [' ' '\009' '\012']
let op                      = ['~' '%' '\\' '+' '-' '&' '|' ':' '`' '$']
let symbol                  = ['*' '#' '/' '!' '?' '^' '(' ')' '{' '}' '[' ']' '<' '>' '.' ';' '_' ',' '=' '\'']
let nl                      = tabs*("\r")?"\n"
let comment                 = "/*" 
let comment_end             = "*/"  

rule token              = parse
| "#show"                   { show lexbuf                                           }   
| "#load"                   { load lexbuf                                           }   
| tabs+                     { token lexbuf                                          }
| "()"                      { UNIT(info lexbuf)                              }
| "[]"                      { NIL(info lexbuf)                               } 
| nl                        { newline lexbuf; token lexbuf                          }
| digit+                    { INTV{i=info lexbuf;v=ios(text lexbuf)}         }
| "*."                      { TIMESFLOAT(info lexbuf)                        }  
| digit+ '.' digit+         { FLOATV{i=info lexbuf;v=fos(text lexbuf)}       }
| init tail*                { createID (info lexbuf) (text lexbuf)                  }
| ":=" | "<:" | "<-" | "->" | "=>" | "==>" | "{|" | "|}" | "<|" | "|>" 
| "[|" | "|]" | "=="        { createID (info lexbuf) (text lexbuf)                  }
| op+                       { createID (info lexbuf) (text lexbuf)                  }
| symbol                    { createID (info lexbuf) (text lexbuf)                  }
| "\""                      { reset_str(); startLex:=info lexbuf; string lexbuf      } 
| ";;" nl                   { DSEMI(info lexbuf)                        }
| eof                       { EOF(info lexbuf)                               }
| comment_end               { error (info lexbuf) "Unmatched end of comment"        } 
| comment                   { depth:=1;startLex:=info lexbuf;comment lexbuf;token lexbuf } 
| _                         { error (info lexbuf) "Illegal character"               }

and show                = parse
| "context"                 { SHOWCONTEXT(info lexbuf)                              }
| _                         { show lexbuf                                           }
and load                = parse
| '"'                       { LOAD{i = !startLex; v=get_str()}                      }
| eof                       { error(!startLex)"String not terminated"               } 
| '\\'                      { add_char(escaped lexbuf)             ; load lexbuf    } 
| '\n'                      { add_char('\n') ; newline lexbuf      ; load lexbuf    } 
| _                         { add_char(Lexing.lexeme_char lexbuf 0); load lexbuf    } 
| "\""                      { reset_str();startLex:=info lexbuf    ; load lexbuf    } 

and comment             = parse
| comment                   { depth:=succ !depth; comment lexbuf                    } 
| comment_end               { depth:=pred !depth; if !depth>0 then comment lexbuf   } 
| eof                       { error (!startLex) "Comment not terminated"            } 
| [^ '\n']                  { comment lexbuf                                        }
| "\n"                      { newline lexbuf; comment lexbuf                        } 

and string              = parse
| '"'                       { STRINGV{i= !startLex; v=get_str()}                    }
| eof                       { error(!startLex)"String not terminated"               } 
| '\\'                      { add_char(escaped lexbuf)              ; string lexbuf } 
| '\n'                      { add_char('\n') ; newline lexbuf       ; string lexbuf } 
| _                         { add_char(Lexing.lexeme_char lexbuf 0) ; string lexbuf } 
and escaped             = parse
| 'n'	                    { '\n'                                                  }
| 't'	                    { '\t'                                                  }
| '\\'	                    { '\\'                                                  }
| '"'                       { '\034'                                                }
| '\''	                    { '\''                                                  }
| digit digit digit         { out_of_char (ios(text lexbuf))(info lexbuf)           }
| [^ '"' '\\' 't' 'n' '\''] { error (info lexbuf) "Illegal character constant"      }




