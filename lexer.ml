# 1 "lexer.mll"
 
open Support.Error

let reservedWords = [
  (* Keywords *)
  ("if",    fun i -> Parser.IF i);
  ("then",  fun i -> Parser.THEN i);
  ("else",  fun i -> Parser.ELSE i);
  ("true",  fun i -> Parser.TRUE i);
  ("false", fun i -> Parser.FALSE i);
  ("succ",  fun i -> Parser.SUCC i);
  ("pred",  fun i -> Parser.PRED i);
  ("iszero",fun i -> Parser.ISZERO i);
  
  (* Symbols *)
  ("_",     fun i -> Parser.USCORE i);
  ("'",     fun i -> Parser.APOSTROPHE i);
  ("\"",    fun i -> Parser.DQUOTE i);
  ("!",     fun i -> Parser.BANG i);
  ("#",     fun i -> Parser.HASH i);
  ("$",     fun i -> Parser.TRIANGLE i);
  ("*",     fun i -> Parser.STAR i);
  ("|",     fun i -> Parser.VBAR i);
  (".",     fun i -> Parser.DOT i);
  (";",     fun i -> Parser.SEMI i);
  (",",     fun i -> Parser.COMMA i);
  ("/",     fun i -> Parser.SLASH i);
  (":",     fun i -> Parser.COLON i);
  ("::",    fun i -> Parser.COLONCOLON i);
  ("=",     fun i -> Parser.EQ i);
  ("==",    fun i -> Parser.EQEQ i);
  ("[",     fun i -> Parser.LSQUARE i); 
  ("<",     fun i -> Parser.LT i);
  ("{",     fun i -> Parser.LCURLY i); 
  ("(",     fun i -> Parser.LPAREN i); 
  ("<-",    fun i -> Parser.LEFTARROW i); 
  ("{|",    fun i -> Parser.LCURLYBAR i); 
  ("[|",    fun i -> Parser.LSQUAREBAR i); 
  ("}",     fun i -> Parser.RCURLY i);
  (")",     fun i -> Parser.RPAREN i);
  ("]",     fun i -> Parser.RSQUARE i);
  (">",     fun i -> Parser.GT i);
  ("|}",    fun i -> Parser.BARRCURLY i);
  ("|>",    fun i -> Parser.BARGT i);
  ("|]",    fun i -> Parser.BARRSQUARE i);
  ("\n",    fun i -> Parser.NEWLINE i); 
  (";;",    fun i -> Parser.DOUBLESEMI i); 

  (* Special compound symbols: *)
  (":=",    fun i -> Parser.COLONEQ i);
  ("->",    fun i -> Parser.ARROW i);
  ("=>",    fun i -> Parser.DARROW i);
  ("==>",   fun i -> Parser.DDARROW i);
]

(* Support functions *)

type buildfun               =   info -> Parser.token
let (symbolTable :(string,buildfun) Hashtbl.t) 
                            =   Hashtbl.create 1024
let _                       =   List.iter (fun (str,f) -> Hashtbl.add symbolTable str f) reservedWords

let createID i str          =   (* info -> string -> token *)
  try (Hashtbl.find symbolTable str) i
  with _ -> if (String.get str 0) >= 'A' && (String.get str 0) <= 'Z' 
                then Parser.UCID {i=i;v=str}
                else Parser.LCID {i=i;v=str}

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
let stringBuffer            =   ref (String.create 2048)
let stringEnd               =   ref 0
let resetStr ()             =   stringEnd := 0
let addStr ch               =
    let x                       =   !stringEnd in
    let buffer                  =   !stringBuffer in
    if x = String.length buffer 
    then begin
        let newBuffer   = String.create (x*2) in
        String.blit buffer 0 newBuffer 0 x;
        String.set newBuffer x ch;
        stringBuffer    := newBuffer;
        stringEnd       := x+1
    end 
else begin
        String.set buffer x ch;
        stringEnd := x+1
    end
let getStr ()               = String.sub (!stringBuffer) 0 (!stringEnd)

let extractLineno yytext offset =
  int_of_string (String.sub yytext offset (String.length yytext - offset))


# 107 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\243\255\246\255\248\255\022\000\080\000\069\000\093\000\
    \008\000\103\000\009\000\073\000\117\000\092\000\165\000\197\000\
    \018\001\254\255\001\000\154\000\004\000\028\001\038\001\250\255\
    \080\000\159\000\247\255\005\000\245\255\244\255\134\000\251\255\
    \252\255\253\255\096\000\103\000\255\255\254\255\062\001\249\255\
    \072\001\251\255\252\255\253\255\254\255\255\255\087\001\250\255\
    ";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\007\000\007\000\007\000\006\000\
    \007\000\006\000\007\000\007\000\006\000\007\000\006\000\004\000\
    \002\000\255\255\012\000\000\000\255\255\255\255\003\000\255\255\
    \005\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\003\000\003\000\255\255\255\255\255\255\255\255\
    \006\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\000\000\255\255\000\000\000\000\032\000\000\000\
    \000\000\000\000\255\255\255\255\000\000\000\000\039\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    ";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\019\000\017\000\017\000\019\000\018\000\017\000\026\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \019\000\003\000\000\000\003\000\007\000\007\000\007\000\003\000\
    \003\000\003\000\005\000\007\000\003\000\012\000\003\000\004\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\014\000\006\000\013\000\011\000\003\000\003\000\
    \029\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\008\000\007\000\003\000\003\000\015\000\
    \007\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\010\000\009\000\003\000\007\000\028\000\
    \025\000\007\000\007\000\007\000\023\000\023\000\024\000\023\000\
    \007\000\023\000\007\000\007\000\007\000\007\000\023\000\037\000\
    \031\000\036\000\007\000\000\000\007\000\000\000\023\000\007\000\
    \000\000\007\000\007\000\007\000\000\000\000\000\000\000\000\000\
    \007\000\007\000\007\000\019\000\017\000\023\000\019\000\020\000\
    \025\000\026\000\000\000\025\000\027\000\000\000\000\000\007\000\
    \034\000\000\000\000\000\023\000\000\000\035\000\000\000\000\000\
    \000\000\007\000\019\000\000\000\000\000\007\000\000\000\025\000\
    \000\000\000\000\000\000\007\000\023\000\000\000\000\000\007\000\
    \000\000\007\000\007\000\007\000\000\000\000\000\000\000\000\000\
    \007\000\007\000\007\000\000\000\000\000\007\000\000\000\000\000\
    \023\000\007\000\000\000\007\000\000\000\000\000\000\000\007\000\
    \000\000\000\000\023\000\007\000\023\000\007\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\015\000\000\000\000\000\000\000\
    \000\000\007\000\000\000\007\000\000\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\000\000\
    \002\000\007\000\000\000\000\000\000\000\007\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \000\000\007\000\000\000\007\000\015\000\000\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \021\000\000\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \042\000\000\000\000\000\000\000\000\000\041\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\000\000\000\000\000\000\000\000\033\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\043\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\045\000\000\000\000\000\000\000\
    \000\000\000\000\044\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\018\000\000\000\000\000\020\000\027\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
    \006\000\007\000\007\000\007\000\008\000\010\000\011\000\011\000\
    \007\000\013\000\007\000\009\000\009\000\009\000\024\000\034\000\
    \030\000\035\000\009\000\255\255\009\000\255\255\013\000\007\000\
    \255\255\012\000\012\000\012\000\255\255\255\255\255\255\255\255\
    \012\000\009\000\012\000\019\000\019\000\009\000\019\000\019\000\
    \025\000\025\000\255\255\025\000\025\000\255\255\255\255\012\000\
    \030\000\255\255\255\255\012\000\255\255\030\000\255\255\255\255\
    \255\255\007\000\019\000\255\255\255\255\007\000\255\255\025\000\
    \255\255\255\255\255\255\009\000\009\000\255\255\255\255\009\000\
    \255\255\014\000\014\000\014\000\255\255\255\255\255\255\255\255\
    \014\000\012\000\014\000\255\255\255\255\012\000\255\255\255\255\
    \013\000\007\000\255\255\007\000\255\255\255\255\255\255\014\000\
    \255\255\255\255\014\000\009\000\009\000\009\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\015\000\255\255\255\255\255\255\
    \255\255\012\000\255\255\012\000\255\255\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\255\255\
    \000\000\014\000\255\255\255\255\255\255\014\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \255\255\014\000\255\255\014\000\015\000\255\255\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \016\000\255\255\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \038\000\255\255\255\255\255\255\255\255\038\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\255\255\255\255\255\255\255\255\030\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\038\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\038\000\255\255\255\255\255\255\
    \255\255\255\255\038\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\038\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 120 "lexer.mll"
                        ( token lexbuf )
# 308 "lexer.ml"

  | 1 ->
# 121 "lexer.mll"
                        ( newline lexbuf; token lexbuf )
# 313 "lexer.ml"

  | 2 ->
# 122 "lexer.mll"
                        ( Parser.INTV{i=info lexbuf; v=int_of_string (text lexbuf)} )
# 318 "lexer.ml"

  | 3 ->
# 123 "lexer.mll"
                        ( Parser.FLOATV{i=info lexbuf; v=float_of_string (text lexbuf)} )
# 323 "lexer.ml"

  | 4 ->
# 124 "lexer.mll"
                        ( createID (info lexbuf) (text lexbuf) )
# 328 "lexer.ml"

  | 5 ->
# 126 "lexer.mll"
                        ( createID (info lexbuf) (text lexbuf) )
# 333 "lexer.ml"

  | 6 ->
# 127 "lexer.mll"
                        ( createID (info lexbuf) (text lexbuf) )
# 338 "lexer.ml"

  | 7 ->
# 128 "lexer.mll"
                        ( createID (info lexbuf) (text lexbuf) )
# 343 "lexer.ml"

  | 8 ->
# 129 "lexer.mll"
                        ( Parser.HOGE(info lexbuf) )
# 348 "lexer.ml"

  | 9 ->
# 130 "lexer.mll"
                        ( Parser.EOF(info lexbuf)   )
# 353 "lexer.ml"

  | 10 ->
# 131 "lexer.mll"
                        ( error (info lexbuf) "Unmatched end of comment" )
# 358 "lexer.ml"

  | 11 ->
# 132 "lexer.mll"
                        ( depth := 1; startLex := info lexbuf; comment lexbuf; token lexbuf )
# 363 "lexer.ml"

  | 12 ->
# 133 "lexer.mll"
                        ( error (info lexbuf) "Illegal character" )
# 368 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment lexbuf =
   __ocaml_lex_comment_rec lexbuf 30
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 136 "lexer.mll"
                        ( depth := succ !depth; comment lexbuf )
# 380 "lexer.ml"

  | 1 ->
# 137 "lexer.mll"
                        ( depth := pred !depth; if !depth > 0 then comment lexbuf )
# 385 "lexer.ml"

  | 2 ->
# 138 "lexer.mll"
                        ( error (!startLex) "Comment not terminated" )
# 390 "lexer.ml"

  | 3 ->
# 139 "lexer.mll"
                        ( comment lexbuf )
# 395 "lexer.ml"

  | 4 ->
# 140 "lexer.mll"
                        ( newline lexbuf; comment lexbuf )
# 400 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

and escaped lexbuf =
   __ocaml_lex_escaped_rec lexbuf 38
and __ocaml_lex_escaped_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 143 "lexer.mll"
                      ( '\n'      )
# 412 "lexer.ml"

  | 1 ->
# 144 "lexer.mll"
                      ( '\t'      )
# 417 "lexer.ml"

  | 2 ->
# 145 "lexer.mll"
                       ( '\\'      )
# 422 "lexer.ml"

  | 3 ->
# 146 "lexer.mll"
                        ( '\034'    )
# 427 "lexer.ml"

  | 4 ->
# 147 "lexer.mll"
                       ( '\''      )
# 432 "lexer.ml"

  | 5 ->
# 148 "lexer.mll"
                        ( let x = int_of_string(text lexbuf) in 
    if  x > 255 
        then    error (info lexbuf) "Illegal character constant"
        else    Char.chr x    
)
# 441 "lexer.ml"

  | 6 ->
# 154 "lexer.mll"
                        ( error (info lexbuf) "Illegal character constant" )
# 446 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_escaped_rec lexbuf __ocaml_lex_state

;;

