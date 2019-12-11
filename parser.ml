type token =
  | SHOWCONTEXT of (Support.Error.info)
  | UNIV of (Support.Error.info)
  | PI of (Support.Error.info)
  | SIGMA of (Support.Error.info)
  | LETREC of (Support.Error.info)
  | FIX of (Support.Error.info)
  | WHERE of (Support.Error.info)
  | IN of (Support.Error.info)
  | LET of (Support.Error.info)
  | BOOL of (Support.Error.info)
  | NAT of (Support.Error.info)
  | SUCC of (Support.Error.info)
  | PRED of (Support.Error.info)
  | ISZERO of (Support.Error.info)
  | LAMBDA of (Support.Error.info)
  | IF of (Support.Error.info)
  | THEN of (Support.Error.info)
  | ELSE of (Support.Error.info)
  | TRUE of (Support.Error.info)
  | FALSE of (Support.Error.info)
  | UCID of (string  Support.Error.withinfo)
  | LCID of (string  Support.Error.withinfo)
  | INTV of (int     Support.Error.withinfo)
  | FLOATV of (float   Support.Error.withinfo)
  | STRINGV of (string  Support.Error.withinfo)
  | APOSTROPHE of (Support.Error.info)
  | DQUOTE of (Support.Error.info)
  | ARROW of (Support.Error.info)
  | BANG of (Support.Error.info)
  | BARGT of (Support.Error.info)
  | BARRCURLY of (Support.Error.info)
  | BARRSQUARE of (Support.Error.info)
  | COLON of (Support.Error.info)
  | COLONCOLON of (Support.Error.info)
  | COLONEQ of (Support.Error.info)
  | COLONHASH of (Support.Error.info)
  | COMMA of (Support.Error.info)
  | DARROW of (Support.Error.info)
  | DDARROW of (Support.Error.info)
  | DOT of (Support.Error.info)
  | EOF of (Support.Error.info)
  | EQ of (Support.Error.info)
  | EQEQ of (Support.Error.info)
  | EXISTS of (Support.Error.info)
  | GT of (Support.Error.info)
  | HASH of (Support.Error.info)
  | LCURLY of (Support.Error.info)
  | LCURLYBAR of (Support.Error.info)
  | LEFTARROW of (Support.Error.info)
  | LPAREN of (Support.Error.info)
  | LSQUARE of (Support.Error.info)
  | LSQUAREBAR of (Support.Error.info)
  | LT of (Support.Error.info)
  | RCURLY of (Support.Error.info)
  | RPAREN of (Support.Error.info)
  | RSQUARE of (Support.Error.info)
  | SEMI of (Support.Error.info)
  | SLASH of (Support.Error.info)
  | STAR of (Support.Error.info)
  | TRIANGLE of (Support.Error.info)
  | USCORE of (Support.Error.info)
  | VBAR of (Support.Error.info)
  | NEWLINE of (Support.Error.info)
  | DOUBLESEMI of (Support.Error.info)

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Format
open Support.Error
open Support.Pervasive
open Syntax
open Type
open Eval

let pe = print_endline 
# 79 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* SHOWCONTEXT *);
  258 (* UNIV *);
  259 (* PI *);
  260 (* SIGMA *);
  261 (* LETREC *);
  262 (* FIX *);
  263 (* WHERE *);
  264 (* IN *);
  265 (* LET *);
  266 (* BOOL *);
  267 (* NAT *);
  268 (* SUCC *);
  269 (* PRED *);
  270 (* ISZERO *);
  271 (* LAMBDA *);
  272 (* IF *);
  273 (* THEN *);
  274 (* ELSE *);
  275 (* TRUE *);
  276 (* FALSE *);
  277 (* UCID *);
  278 (* LCID *);
  279 (* INTV *);
  280 (* FLOATV *);
  281 (* STRINGV *);
  282 (* APOSTROPHE *);
  283 (* DQUOTE *);
  284 (* ARROW *);
  285 (* BANG *);
  286 (* BARGT *);
  287 (* BARRCURLY *);
  288 (* BARRSQUARE *);
  289 (* COLON *);
  290 (* COLONCOLON *);
  291 (* COLONEQ *);
  292 (* COLONHASH *);
  293 (* COMMA *);
  294 (* DARROW *);
  295 (* DDARROW *);
  296 (* DOT *);
    0 (* EOF *);
  297 (* EQ *);
  298 (* EQEQ *);
  299 (* EXISTS *);
  300 (* GT *);
  301 (* HASH *);
  302 (* LCURLY *);
  303 (* LCURLYBAR *);
  304 (* LEFTARROW *);
  305 (* LPAREN *);
  306 (* LSQUARE *);
  307 (* LSQUAREBAR *);
  308 (* LT *);
  309 (* RCURLY *);
  310 (* RPAREN *);
  311 (* RSQUARE *);
  312 (* SEMI *);
  313 (* SLASH *);
  314 (* STAR *);
  315 (* TRIANGLE *);
  316 (* USCORE *);
  317 (* VBAR *);
  318 (* NEWLINE *);
  319 (* DOUBLESEMI *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\003\000\003\000\001\000\001\000\
\004\000\004\000\004\000\006\000\006\000\007\000\007\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\005\000\005\000\
\005\000\009\000\009\000\009\000\009\000\009\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\012\000\000\000\000\000"

let yylen = "\002\000\
\000\000\003\000\002\000\002\000\002\000\003\000\001\000\003\000\
\001\000\002\000\002\000\000\000\002\000\002\000\002\000\001\000\
\001\000\001\000\003\000\003\000\003\000\003\000\005\000\005\000\
\001\000\001\000\006\000\006\000\006\000\006\000\001\000\002\000\
\003\000\003\000\002\000\002\000\002\000\003\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\001\000\000\000\040\000\000\000\000\000\000\000\041\000\
\042\000\000\000\000\000\000\000\000\000\000\000\043\000\044\000\
\000\000\000\000\045\000\007\000\000\000\047\000\000\000\000\000\
\000\000\000\000\031\000\000\000\016\000\000\000\000\000\017\000\
\018\000\000\000\000\000\000\000\000\000\000\000\039\000\035\000\
\036\000\037\000\000\000\000\000\000\000\010\000\000\000\000\000\
\011\000\046\000\000\000\000\000\000\000\000\000\032\000\000\000\
\003\000\004\000\000\000\000\000\000\000\000\000\000\000\033\000\
\034\000\000\000\000\000\000\000\000\000\000\000\000\000\015\000\
\038\000\008\000\000\000\000\000\002\000\000\000\005\000\020\000\
\021\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\000\000\000\000\000\000\000\000\023\000\024\000\
\027\000\028\000\029\000\030\000"

let yydgoto = "\003\000\
\022\000\028\000\058\000\023\000\024\000\046\000\049\000\035\000\
\025\000\026\000\027\000\051\000"

let yysindex = "\040\000\
\001\000\000\000\000\000\000\000\096\255\096\255\236\254\000\000\
\000\000\071\255\071\255\071\255\240\254\045\000\000\000\000\000\
\227\254\228\254\000\000\000\000\045\000\000\000\214\254\252\254\
\010\255\071\255\000\000\047\255\000\000\096\255\096\255\000\000\
\000\000\096\255\016\255\016\255\002\255\004\255\000\000\000\000\
\000\000\000\000\038\255\066\255\096\255\000\000\096\255\045\000\
\000\000\000\000\018\255\001\000\062\255\063\255\000\000\023\255\
\000\000\000\000\216\254\016\255\016\255\239\254\096\255\000\000\
\000\000\045\000\045\000\096\255\045\000\059\255\059\255\000\000\
\000\000\000\000\072\255\073\255\000\000\023\000\000\000\000\000\
\000\000\000\000\059\255\104\255\107\255\231\254\099\255\045\000\
\045\000\000\000\045\000\045\000\045\000\045\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\041\255\142\255\000\000\000\000\000\000\000\000\000\000\045\255\
\039\255\199\255\000\000\119\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\053\255\055\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\120\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\073\000\000\000\049\000\232\255\000\000\000\000\000\000\058\000\
\242\255\000\000\254\255\000\000"

let yytablesize = 350
let yytable = "\044\000\
\020\000\037\000\063\000\059\000\047\000\043\000\050\000\040\000\
\041\000\042\000\063\000\045\000\048\000\052\000\093\000\078\000\
\054\000\004\000\005\000\006\000\064\000\065\000\079\000\055\000\
\007\000\008\000\009\000\010\000\011\000\012\000\013\000\014\000\
\053\000\072\000\015\000\016\000\082\000\039\000\019\000\038\000\
\001\000\002\000\066\000\063\000\067\000\080\000\081\000\056\000\
\004\000\005\000\006\000\084\000\085\000\059\000\087\000\007\000\
\008\000\009\000\010\000\011\000\012\000\013\000\014\000\036\000\
\021\000\015\000\016\000\017\000\018\000\019\000\068\000\073\000\
\004\000\095\000\096\000\025\000\097\000\098\000\099\000\100\000\
\008\000\009\000\069\000\075\000\076\000\077\000\063\000\060\000\
\061\000\015\000\016\000\062\000\039\000\019\000\025\000\021\000\
\012\000\029\000\030\000\031\000\009\000\025\000\070\000\012\000\
\071\000\032\000\033\000\009\000\013\000\057\000\014\000\091\000\
\088\000\089\000\092\000\013\000\094\000\014\000\048\000\021\000\
\083\000\019\000\019\000\019\000\074\000\086\000\090\000\000\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\000\000\000\000\019\000\019\000\000\000\019\000\019\000\039\000\
\034\000\000\000\000\000\000\000\039\000\000\000\000\000\039\000\
\039\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\
\039\000\039\000\000\000\039\000\039\000\000\000\000\000\000\000\
\019\000\000\000\000\000\000\000\000\000\019\000\000\000\019\000\
\000\000\000\000\039\000\000\000\000\000\000\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\039\000\000\000\
\000\000\000\000\000\000\000\000\000\000\039\000\000\000\000\000\
\000\000\026\000\026\000\000\000\039\000\026\000\026\000\026\000\
\000\000\000\000\026\000\026\000\026\000\026\000\026\000\026\000\
\026\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\026\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\026\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\026\000\000\000\026\000\000\000\
\000\000\000\000\004\000\005\000\006\000\026\000\000\000\000\000\
\000\000\007\000\008\000\009\000\010\000\011\000\012\000\013\000\
\014\000\000\000\000\000\015\000\016\000\017\000\018\000\019\000\
\004\000\005\000\006\000\000\000\000\000\000\000\000\000\007\000\
\008\000\009\000\010\000\011\000\012\000\013\000\014\000\000\000\
\000\000\015\000\016\000\017\000\018\000\019\000\004\000\005\000\
\006\000\021\000\000\000\000\000\000\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\014\000\000\000\000\000\015\000\
\016\000\000\000\039\000\019\000\000\000\000\000\000\000\021\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\021\000"

let yycheck = "\014\000\
\000\000\022\001\028\001\028\000\033\001\022\001\021\000\010\000\
\011\000\012\000\028\001\041\001\041\001\056\001\040\001\056\001\
\007\001\002\001\003\001\004\001\035\000\036\000\063\001\026\000\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\037\001\048\000\019\001\020\001\054\001\022\001\023\001\060\001\
\001\000\002\000\041\001\028\001\041\001\060\000\061\000\001\001\
\002\001\003\001\004\001\066\000\067\000\078\000\069\000\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\006\000\
\049\001\019\001\020\001\021\001\022\001\023\001\033\001\054\001\
\002\001\088\000\089\000\037\001\091\000\092\000\093\000\094\000\
\010\001\011\001\017\001\022\001\022\001\063\001\028\001\030\000\
\031\000\019\001\020\001\034\000\022\001\023\001\056\001\049\001\
\056\001\002\001\003\001\004\001\056\001\063\001\045\000\063\001\
\047\000\010\001\011\001\063\001\056\001\063\001\056\001\008\001\
\041\001\041\001\008\001\063\001\018\001\063\001\000\000\049\001\
\063\000\002\001\003\001\004\001\052\000\068\000\078\000\255\255\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\255\255\255\255\019\001\020\001\255\255\022\001\023\001\002\001\
\049\001\255\255\255\255\255\255\007\001\255\255\255\255\010\001\
\011\001\255\255\255\255\255\255\255\255\255\255\255\255\040\001\
\019\001\020\001\255\255\022\001\023\001\255\255\255\255\255\255\
\049\001\255\255\255\255\255\255\255\255\054\001\255\255\056\001\
\255\255\255\255\037\001\255\255\255\255\255\255\063\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\049\001\255\255\
\255\255\255\255\255\255\255\255\255\255\056\001\255\255\255\255\
\255\255\003\001\004\001\255\255\063\001\007\001\008\001\009\001\
\255\255\255\255\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\028\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\037\001\255\255\255\255\040\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\054\001\255\255\056\001\255\255\
\255\255\255\255\002\001\003\001\004\001\063\001\255\255\255\255\
\255\255\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\255\255\019\001\020\001\021\001\022\001\023\001\
\002\001\003\001\004\001\255\255\255\255\255\255\255\255\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\255\255\
\255\255\019\001\020\001\021\001\022\001\023\001\002\001\003\001\
\004\001\049\001\255\255\255\255\255\255\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\255\255\255\255\019\001\
\020\001\255\255\022\001\023\001\255\255\255\255\255\255\049\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\049\001"

let yynames_const = "\
  "

let yynames_block = "\
  SHOWCONTEXT\000\
  UNIV\000\
  PI\000\
  SIGMA\000\
  LETREC\000\
  FIX\000\
  WHERE\000\
  IN\000\
  LET\000\
  BOOL\000\
  NAT\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  LAMBDA\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  UCID\000\
  LCID\000\
  INTV\000\
  FLOATV\000\
  STRINGV\000\
  APOSTROPHE\000\
  DQUOTE\000\
  ARROW\000\
  BANG\000\
  BARGT\000\
  BARRCURLY\000\
  BARRSQUARE\000\
  COLON\000\
  COLONCOLON\000\
  COLONEQ\000\
  COLONHASH\000\
  COMMA\000\
  DARROW\000\
  DDARROW\000\
  DOT\000\
  EOF\000\
  EQ\000\
  EQEQ\000\
  EXISTS\000\
  GT\000\
  HASH\000\
  LCURLY\000\
  LCURLYBAR\000\
  LEFTARROW\000\
  LPAREN\000\
  LSQUARE\000\
  LSQUAREBAR\000\
  LT\000\
  RCURLY\000\
  RPAREN\000\
  RSQUARE\000\
  SEMI\000\
  SLASH\000\
  STAR\000\
  TRIANGLE\000\
  USCORE\000\
  VBAR\000\
  NEWLINE\000\
  DOUBLESEMI\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
                                        ( fun ctx   ->  [],[]                                   )
# 388 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 104 "parser.mly"
                                        ( let _,ctx' = _1 [] in pr_ctx ctx';
                                          fun ctx   ->  [],ctx'                                 )
# 398 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 106 "parser.mly"
                                        ( fun ctx   ->  [],ctx                                  )
# 406 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'oneREPL) in
    Obj.repr(
# 107 "parser.mly"
                                        ( let _,ev_ctx  = _1 [] in  
                                          let cmds,_    = _2 ev_ctx in 
                                          let ev_ctx'   = process_commands ev_ctx cmds in 
                                          fun ctx   ->  [],ev_ctx'                              )
# 417 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 112 "parser.mly"
                                        ( fun ctx   ->  let cmd,ctx'   = _1 ctx in [cmd],ctx'   )
# 425 "parser.ml"
               : 'oneREPL))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'oneREPL) in
    Obj.repr(
# 113 "parser.mly"
                                        ( fun ctx   ->  let cmd,ctx'   = _1 ctx in 
                                                        let cmds,ctx'' = _3 ctx' in cmd::cmds,ctx''  )
# 435 "parser.ml"
               : 'oneREPL))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 117 "parser.mly"
                                        ( fun ctx   ->  [],ctx                                  )
# 442 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    Obj.repr(
# 118 "parser.mly"
                                        ( fun ctx   ->  let cmd,ctx  = _1 ctx in 
                                                        let cmds,ctx = _3 ctx in cmd::cmds,ctx  )
# 452 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'TermWrap) in
    Obj.repr(
# 122 "parser.mly"
                                        ( fun ctx   ->  let t = _1 ctx in Eval(tmInfo t,t),ctx  )
# 459 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'TyBinder) in
    Obj.repr(
# 123 "parser.mly"
                                        ( fun ctx   ->  Bind(_1.i,_1.v,_2 ctx),addname ctx _1.v )
# 467 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 124 "parser.mly"
                                        ( fun ctx   ->  Bind(_1.i,_1.v,_2 ctx),addname ctx _1.v )
# 475 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
                                        ( fun ctx   ->  BindTyVar                               )
# 481 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 127 "parser.mly"
                                        ( fun ctx   ->  BindTyAbb(_2 ctx)                       )
# 489 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 129 "parser.mly"
                                        ( fun ctx   ->  BindVar(_2 ctx)                       )
# 497 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 130 "parser.mly"
                                        ( fun ctx   ->  BindAbb(_2 ctx,None)                  )
# 505 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 134 "parser.mly"
                                        ( fun ctx   ->  Universe(_1,0)                           )
# 512 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 135 "parser.mly"
                                        ( fun ctx   ->  Bool(_1)                               )
# 519 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 136 "parser.mly"
                                        ( fun ctx   ->  Nat(_1)                                )
# 526 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 137 "parser.mly"
                                        ( fun ctx   ->  App(_2,App(_2,Pi(_2),_1 ctx),Abs(_2,"_",_1 ctx,_3 ctx)))
# 535 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 138 "parser.mly"
                                        ( fun ctx   ->  App(_1,App(_1,Pi(_1),_2 ctx),_3 ctx))
# 544 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 139 "parser.mly"
                                        ( fun ctx   ->  App(_1,App(_1,Sigma(_1),_2 ctx),_3 ctx))
# 553 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 140 "parser.mly"
                                        ( _2 )
# 562 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'TermWrap) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 144 "parser.mly"
                                        ( fun ctx   ->  Let(_2,_3.v,_5 ctx,_1(addname ctx _3.v)) )
# 573 "parser.ml"
               : 'TermWrap))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 145 "parser.mly"
                                        ( fun ctx   ->  Let(_2,_3.v,_5 ctx,_1(addname ctx _3.v)) )
# 584 "parser.ml"
               : 'TermWrap))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 146 "parser.mly"
                                        ( _1                                                    )
# 591 "parser.ml"
               : 'TermWrap))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 148 "parser.mly"
                                        ( _1                                                    )
# 598 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 149 "parser.mly"
                                        ( fun ctx   ->  Let(_1,_2.v,_4 ctx,_6(addname ctx _2.v)))
# 610 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 150 "parser.mly"
                                        ( fun ctx   ->  Let(_1,"_",_4 ctx,_6(addname ctx"_")) )
# 622 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 151 "parser.mly"
                                        ( fun ctx   ->  Abs(_1,_2.v,_4 ctx,_6(addname ctx _2.v)))
# 634 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 152 "parser.mly"
                                        ( fun ctx   ->  If(_1,_2 ctx,_4 ctx,_6 ctx)           )
# 646 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 154 "parser.mly"
                                        ( _1                                                    )
# 653 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 155 "parser.mly"
                                        ( fun ctx   ->  App(tmInfo(_1 ctx),_1 ctx,_2 ctx)     )
# 661 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 156 "parser.mly"
                                        ( fun ctx   ->  App(_1,App(_1,Pi(_1),_2 ctx),_3 ctx))
# 670 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 157 "parser.mly"
                                        ( fun ctx   ->  App(_1,App(_1,Sigma(_1),_2 ctx),_3 ctx))
# 679 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 158 "parser.mly"
                                        ( fun ctx   ->  Succ(_1, _2 ctx )                     )
# 687 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 159 "parser.mly"
                                        ( fun ctx   ->  Pred(_1, _2 ctx )                     )
# 695 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 160 "parser.mly"
                                        ( fun ctx   ->  IsZero(_1, _2 ctx)                    )
# 703 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'TermSeq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 162 "parser.mly"
                                        ( _2                                                    )
# 712 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 163 "parser.mly"
                                        ( fun ctx   ->  Var(_1.i,name2index _1.i ctx _1.v,ctxlen ctx) )
# 719 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 164 "parser.mly"
                                        ( fun ctx   ->  Universe(_1,0)                           )
# 726 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 165 "parser.mly"
                                        ( fun ctx   ->  Bool(_1)                               )
# 733 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 166 "parser.mly"
                                        ( fun ctx   ->  Nat(_1)                                )
# 740 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 167 "parser.mly"
                                        ( fun ctx   ->  True(_1)                              )
# 747 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 168 "parser.mly"
                                        ( fun ctx   ->  False(_1)                             )
# 754 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int     Support.Error.withinfo) in
    Obj.repr(
# 169 "parser.mly"
                                        ( fun ctx   ->  let rec f = function
                                                            | 0 -> Zero(_1.i)
                                                            | n -> Succ(_1.i,f(n-1))in f _1.v )
# 763 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 173 "parser.mly"
                                        ( _1                                                    )
# 770 "parser.ml"
               : 'TermSeq))
(* Entry toplevel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry input *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.context -> (Syntax.command list * Syntax.context))
let input (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Syntax.context -> (Syntax.command list * Syntax.context))
