{
open Support
open Parser

let reservedWords = [
  (* Keywords *)
  ("Bool",    fun i -> BOOL i);
  ("\\",    fun i -> LAMBDA i);
  ("if",    fun i -> IF i);
  ("then",  fun i -> THEN i);
  ("else",  fun i -> ELSE i);
  ("true",  fun i -> TRUE i);
  ("false", fun i -> FALSE i);
  ("succ",  fun i -> SUCC i);
  ("pred",  fun i -> PRED i);
  ("iszero",fun i -> ISZERO i);
  
  (* Symbols *)
  ("_",     fun i -> USCORE i);
  ("'",     fun i -> APOSTROPHE i);
  ("\"",    fun i -> DQUOTE i);
  ("!",     fun i -> BANG i);
  ("#",     fun i -> HASH i);
  ("$",     fun i -> TRIANGLE i);
  ("*",     fun i -> STAR i);
  ("|",     fun i -> VBAR i);
  (".",     fun i -> DOT i);
  (";",     fun i -> SEMI i);
  (",",     fun i -> COMMA i);
  ("/",     fun i -> SLASH i);
  (":",     fun i -> COLON i);
  ("::",    fun i -> COLONCOLON i);
  ("=",     fun i -> EQ i);
  ("==",    fun i -> EQEQ i);
  ("[",     fun i -> LSQUARE i); 
  ("<",     fun i -> LT i);
  ("{",     fun i -> LCURLY i); 
  ("(",     fun i -> LPAREN i); 
  ("<-",    fun i -> LEFTARROW i); 
  ("{|",    fun i -> LCURLYBAR i); 
  ("[|",    fun i -> LSQUAREBAR i); 
  ("}",     fun i -> RCURLY i);
  (")",     fun i -> RPAREN i);
  ("]",     fun i -> RSQUARE i);
  (">",     fun i -> GT i);
  ("|}",    fun i -> BARRCURLY i);
  ("|>",    fun i -> BARGT i);
  ("|]",    fun i -> BARRSQUARE i);
  ("\n",    fun i -> NEWLINE i); 
  (";;",    fun i -> DSEMI i); 

  (* Special compound symbols: *)
  (":=",    fun i -> COLONEQ i);
  ("->",    fun i -> ARROW i);
  ("=>",    fun i -> DARROW i);
  ("==>",   fun i -> DDARROW i);
]

(* Support functions *)

type buildfun               =   info -> token
let (symbolTable :(string,buildfun) Hashtbl.t) 
                            =   Hashtbl.create 1024
let _                       =   List.iter (fun (str,f) -> Hashtbl.add symbolTable str f) reservedWords

let isInitialCapital str    = let s = Bytes.get str 0 in s >= 'A' && s <= 'Z'  

let createID i str          =   (* info -> string -> token *)
  try (Hashtbl.find symbolTable str) i
  with _ -> 
      if isInitialCapital str 
        then UCID {i=i;v=str} 
        else LCID {i=i;v=str}

let lineno                  =   ref 1
and depth                   =   ref 0
and start                   =   ref 0
and filename                =   ref ""
and startLex                =   ref dummyinfo
let create inFile stream    =   if not (Filename.is_implicit inFile) 
                                    then filename   := inFile
                                    else filename   := Filename.concat (Sys.getcwd()) inFile;
                                lineno := 1; start := 0; Lexing.from_channel stream
let newline lexbuf          =   incr lineno; start := (Lexing.lexeme_start lexbuf)
let info    lexbuf          =   createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)
let text                    =   Lexing.lexeme
let stringBuffer            =   ref (Bytes.create 2048)
let stringEnd               =   ref 0
let resetStr ()             =   stringEnd := 0
let addStr ch               =
    let x                       =   !stringEnd in
    let buffer                  =   !stringBuffer in
    if x = Bytes.length buffer 
    then begin
        let newBuffer   = Bytes.create (x*2) in
        Bytes.blit buffer 0 newBuffer 0 x;
        Bytes.set newBuffer x ch;
        stringBuffer    := newBuffer;
        stringEnd       := x+1
    end 
else begin
        Bytes.set buffer x ch;
        stringEnd := x+1
    end
let getStr ()               = Bytes.sub (!stringBuffer) 0 (!stringEnd)

let extractLineno yytext offset =
  int_of_string (Bytes.sub yytext offset (Bytes.length yytext - offset))

}


(* The main body of the lexical analyzer *)

let digit       = ['0'-'9'] 
let init        = ['a'-'z' 'A'-'Z' '_' ]
let tail        = ['a'-'z' 'A'-'Z' '_' '0'-'9' '\'']
let tabs        = [' ' '\009' '\012']
let op          = ['~' '%' '\\' '+' '-' '&' '|' ':' '`' '$']
let symbol      = ['*' '#' '/' '!' '?' '^' '(' ')' '{' '}' '[' ']' '<' '>' '.' ';' '_' ',' '=' '\'']
let nl          = tabs*("\r")?"\n"
let comment     = "/*" 
let comment_end = "*/"  

rule token = parse
  tabs+                 { token lexbuf }
| nl                    { newline lexbuf; token lexbuf }
| digit+                { INTV{i=info lexbuf; v=int_of_string (text lexbuf)} }
| digit+ '.' digit+     { FLOATV{i=info lexbuf; v=float_of_string (text lexbuf)} }
| init tail*            { createID (info lexbuf) (text lexbuf) }
| ":=" | "<:" | "<-" | "->" | "=>" | "==>" | "{|" | "|}" | "<|" | "|>" 
| "[|" | "|]" | "=="    { createID (info lexbuf) (text lexbuf) }
| op+                   { createID (info lexbuf) (text lexbuf) }
| symbol                { createID (info lexbuf) (text lexbuf) }
| ";;" nl               { DSEMI(info lexbuf) }
| eof                   { EOF(info lexbuf)   }
| comment_end           { error (info lexbuf) "Unmatched end of comment" } 
| comment               { depth := 1; startLex := info lexbuf; comment lexbuf; token lexbuf } 
| _                     { error (info lexbuf) "Illegal character" }

and comment = parse
  comment               { depth := succ !depth; comment lexbuf } 
| comment_end           { depth := pred !depth; if !depth > 0 then comment lexbuf } 
| eof                   { error (!startLex) "Comment not terminated" } 
| [^ '\n']              { comment lexbuf }
| "\n"                  { newline lexbuf; comment lexbuf } 

and escaped = parse
  'n'	                { '\n'      }
| 't'	                { '\t'      }
| '\\'	                { '\\'      }
| '"'                   { '\034'    }
| '\''	                { '\''      }
| digit digit digit     { let x = int_of_string(text lexbuf) in 
    if  x > 255 
        then    error (info lexbuf) "Illegal character constant"
        else    Char.chr x    
}
| [^ '"' '\\' 't' 'n' '\'']
                        { error (info lexbuf) "Illegal character constant" }

(*  *)
