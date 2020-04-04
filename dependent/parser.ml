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
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\005\000\
\009\000\009\000\009\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\012\000\000\000\000\000"

let yylen = "\002\000\
\000\000\003\000\002\000\002\000\002\000\003\000\001\000\003\000\
\001\000\002\000\002\000\000\000\002\000\002\000\002\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\001\000\
\001\000\006\000\006\000\001\000\002\000\003\000\003\000\002\000\
\002\000\002\000\003\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\001\000\000\000\037\000\000\000\000\000\038\000\039\000\
\000\000\000\000\000\000\000\000\000\000\040\000\041\000\000\000\
\000\000\042\000\007\000\000\000\044\000\000\000\009\000\024\000\
\000\000\028\000\000\000\017\000\000\000\000\000\018\000\019\000\
\016\000\000\000\000\000\000\000\036\000\032\000\033\000\034\000\
\000\000\000\000\000\000\010\000\000\000\000\000\011\000\043\000\
\000\000\000\000\029\000\000\000\003\000\004\000\000\000\000\000\
\000\000\000\000\000\000\030\000\031\000\000\000\000\000\000\000\
\000\000\015\000\035\000\008\000\002\000\000\000\005\000\021\000\
\022\000\023\000\000\000\000\000\000\000\006\000\000\000\000\000\
\026\000\027\000"

let yydgoto = "\003\000\
\021\000\027\000\054\000\022\000\023\000\044\000\047\000\035\000\
\024\000\025\000\026\000\049\000"

let yysindex = "\007\000\
\001\000\000\000\000\000\000\000\002\255\002\255\000\000\000\000\
\216\255\216\255\216\255\244\254\197\255\000\000\000\000\226\254\
\008\255\000\000\000\000\197\255\000\000\220\254\000\000\000\000\
\216\255\000\000\015\255\000\000\002\255\002\255\000\000\000\000\
\000\000\002\255\153\255\153\255\000\000\000\000\000\000\000\000\
\009\255\004\255\002\255\000\000\002\255\197\255\000\000\000\000\
\234\254\001\000\000\000\239\254\000\000\000\000\255\254\153\255\
\153\255\242\254\002\255\000\000\000\000\002\255\197\255\030\255\
\030\255\000\000\000\000\000\000\000\000\175\255\000\000\000\000\
\000\000\000\000\030\255\231\254\043\255\000\000\197\255\197\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\255\
\037\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\098\255\000\000\065\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\255\
\031\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\069\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\024\000\000\000\005\000\231\255\000\000\000\000\000\000\100\000\
\243\255\000\000\043\000\000\000"

let yytablesize = 306
let yytable = "\042\000\
\019\000\055\000\059\000\028\000\029\000\030\000\048\000\001\000\
\002\000\041\000\043\000\031\000\032\000\059\000\079\000\052\000\
\004\000\005\000\006\000\050\000\063\000\060\000\061\000\033\000\
\007\000\008\000\009\000\010\000\011\000\012\000\013\000\067\000\
\066\000\014\000\015\000\016\000\017\000\018\000\036\000\074\000\
\045\000\062\000\072\000\073\000\055\000\069\000\036\000\036\000\
\046\000\077\000\034\000\038\000\039\000\040\000\070\000\036\000\
\036\000\059\000\036\000\036\000\080\000\071\000\012\000\020\000\
\045\000\081\000\082\000\051\000\013\000\012\000\020\000\020\000\
\020\000\068\000\078\000\013\000\000\000\053\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\036\000\014\000\020\000\
\020\000\000\000\020\000\020\000\036\000\014\000\000\000\000\000\
\000\000\000\000\000\000\036\000\025\000\025\000\000\000\000\000\
\000\000\036\000\000\000\000\000\020\000\025\000\025\000\025\000\
\025\000\025\000\025\000\025\000\000\000\020\000\000\000\000\000\
\000\000\000\000\020\000\000\000\020\000\025\000\000\000\000\000\
\056\000\057\000\000\000\020\000\000\000\058\000\000\000\000\000\
\000\000\025\000\000\000\000\000\000\000\000\000\064\000\000\000\
\065\000\000\000\000\000\000\000\000\000\000\000\000\000\025\000\
\000\000\025\000\004\000\005\000\006\000\000\000\075\000\000\000\
\025\000\076\000\007\000\008\000\009\000\010\000\011\000\012\000\
\013\000\000\000\000\000\014\000\015\000\000\000\037\000\018\000\
\004\000\005\000\006\000\000\000\059\000\000\000\000\000\000\000\
\007\000\008\000\009\000\010\000\011\000\012\000\013\000\000\000\
\000\000\014\000\015\000\016\000\017\000\018\000\004\000\005\000\
\006\000\020\000\000\000\000\000\000\000\000\000\007\000\008\000\
\009\000\010\000\011\000\012\000\013\000\000\000\000\000\014\000\
\015\000\004\000\037\000\018\000\000\000\000\000\000\000\020\000\
\000\000\007\000\008\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\015\000\000\000\037\000\018\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\005\000\006\000\000\000\000\000\000\000\
\020\000\000\000\007\000\008\000\009\000\010\000\011\000\012\000\
\013\000\000\000\000\000\014\000\015\000\016\000\017\000\018\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\020\000"

let yycheck = "\013\000\
\000\000\027\000\028\001\002\001\003\001\004\001\020\000\001\000\
\002\000\022\001\041\001\010\001\011\001\028\001\040\001\001\001\
\002\001\003\001\004\001\056\001\017\001\035\000\036\000\022\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\054\001\
\046\000\019\001\020\001\021\001\022\001\023\001\002\001\054\001\
\033\001\033\001\056\000\057\000\070\000\063\001\010\001\011\001\
\041\001\063\000\049\001\009\000\010\000\011\000\056\001\019\001\
\020\001\028\001\022\001\023\001\018\001\063\001\056\001\049\001\
\000\000\079\000\080\000\025\000\056\001\063\001\002\001\003\001\
\004\001\050\000\070\000\063\001\255\255\063\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\049\001\056\001\019\001\
\020\001\255\255\022\001\023\001\056\001\063\001\255\255\255\255\
\255\255\255\255\255\255\063\001\003\001\004\001\255\255\255\255\
\255\255\006\000\255\255\255\255\040\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\255\255\049\001\255\255\255\255\
\255\255\255\255\054\001\255\255\056\001\028\001\255\255\255\255\
\029\000\030\000\255\255\063\001\255\255\034\000\255\255\255\255\
\255\255\040\001\255\255\255\255\255\255\255\255\043\000\255\255\
\045\000\255\255\255\255\255\255\255\255\255\255\255\255\054\001\
\255\255\056\001\002\001\003\001\004\001\255\255\059\000\255\255\
\063\001\062\000\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\255\255\019\001\020\001\255\255\022\001\023\001\
\002\001\003\001\004\001\255\255\028\001\255\255\255\255\255\255\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\255\255\
\255\255\019\001\020\001\021\001\022\001\023\001\002\001\003\001\
\004\001\049\001\255\255\255\255\255\255\255\255\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\255\255\255\255\019\001\
\020\001\002\001\022\001\023\001\255\255\255\255\255\255\049\001\
\255\255\010\001\011\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\019\001\020\001\255\255\022\001\023\001\255\255\
\255\255\255\255\255\255\255\255\255\255\049\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\004\001\255\255\255\255\255\255\
\049\001\255\255\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\255\255\019\001\020\001\021\001\022\001\023\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\049\001"

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
# 372 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 104 "parser.mly"
                                        ( let _,ctx' = _1 [] in pr_ctx ctx';
                                          fun ctx   ->  [],ctx'                                 )
# 382 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 106 "parser.mly"
                                        ( fun ctx   ->  [],ctx                                  )
# 390 "parser.ml"
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
# 401 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 112 "parser.mly"
                                        ( fun ctx   ->  let cmd,ctx'   = _1 ctx in [cmd],ctx'   )
# 409 "parser.ml"
               : 'oneREPL))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'oneREPL) in
    Obj.repr(
# 113 "parser.mly"
                                        ( fun ctx   ->  let cmd,ctx'   = _1 ctx in 
                                                        let cmds,ctx'' = _3 ctx' in cmd::cmds,ctx''  )
# 419 "parser.ml"
               : 'oneREPL))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 117 "parser.mly"
                                        ( fun ctx   ->  [],ctx                                  )
# 426 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    Obj.repr(
# 118 "parser.mly"
                                        ( fun ctx   ->  let cmd,ctx  = _1 ctx in 
                                                        let cmds,ctx = _3 ctx in cmd::cmds,ctx  )
# 436 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'TermWrap) in
    Obj.repr(
# 122 "parser.mly"
                                        ( fun ctx   ->  let t = _1 ctx in Eval(tmInfo t,t),ctx  )
# 443 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'TyBinder) in
    Obj.repr(
# 123 "parser.mly"
                                        ( fun ctx   ->  Bind(_1.i,_1.v,_2 ctx),addname ctx _1.v )
# 451 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 124 "parser.mly"
                                        ( fun ctx   ->  Bind(_1.i,_1.v,_2 ctx),addname ctx _1.v )
# 459 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
                                        ( fun ctx   ->  BindTyVar                               )
# 465 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 127 "parser.mly"
                                        ( fun ctx   ->  BindTyAbb(_2 ctx)                       )
# 473 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 129 "parser.mly"
                                        ( fun ctx   ->  BindVar(_2 ctx)                       )
# 481 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 130 "parser.mly"
                                        ( fun ctx   ->  BindAbb(_2 ctx,None)                  )
# 489 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 134 "parser.mly"
                                        ( fun ctx   ->  Var(_1.i,name2index _1.i ctx _1.v,ctxlen ctx))
# 496 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 135 "parser.mly"
                                        ( fun ctx   ->  Univ(_1,0)                           )
# 503 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 136 "parser.mly"
                                        ( fun ctx   ->  Bool(_1)                               )
# 510 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 137 "parser.mly"
                                        ( fun ctx   ->  Nat(_1)                                )
# 517 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 138 "parser.mly"
                                        ( fun ctx   ->  Ap(_2,Ap(_2,Pi(_2),_1 ctx),Lam(_2,"_",_1 ctx,_3 ctx)))
# 526 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 139 "parser.mly"
                                        ( fun ctx   ->  Ap(_1,Ap(_1,Pi(_1),_2 ctx),_3 ctx))
# 535 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 140 "parser.mly"
                                        ( fun ctx   ->  Ap(_1,Ap(_1,Sgm(_1),_2 ctx),_3 ctx))
# 544 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 141 "parser.mly"
                                        ( _2 )
# 553 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 145 "parser.mly"
                                        ( _1                                                    )
# 560 "parser.ml"
               : 'TermWrap))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ApTerm) in
    Obj.repr(
# 147 "parser.mly"
                                       ( _1                                                    )
# 567 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 148 "parser.mly"
                                        ( fun ctx   ->  Lam(_1,_2.v,_4 ctx,_6(addname ctx _2.v)))
# 579 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 149 "parser.mly"
                                        ( fun ctx   ->  If(_1,_2 ctx,_4 ctx,_6 ctx)           )
# 591 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 151 "parser.mly"
                                        ( _1                                                    )
# 598 "parser.ml"
               : 'ApTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ApTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 152 "parser.mly"
                                       ( fun ctx   ->  Ap(tmInfo(_1 ctx),_1 ctx,_2 ctx)     )
# 606 "parser.ml"
               : 'ApTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 153 "parser.mly"
                                        ( fun ctx   ->  Ap(_1,Ap(_1,Pi(_1),_2 ctx),_3 ctx))
# 615 "parser.ml"
               : 'ApTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 154 "parser.mly"
                                        ( fun ctx   ->  Ap(_1,Ap(_1,Sgm(_1),_2 ctx),_3 ctx))
# 624 "parser.ml"
               : 'ApTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 155 "parser.mly"
                                        ( fun ctx   ->  Succ(_1, _2 ctx )                     )
# 632 "parser.ml"
               : 'ApTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 156 "parser.mly"
                                        ( fun ctx   ->  Pred(_1, _2 ctx )                     )
# 640 "parser.ml"
               : 'ApTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 157 "parser.mly"
                                        ( fun ctx   ->  IsZero(_1, _2 ctx)                    )
# 648 "parser.ml"
               : 'ApTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'TermSeq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 159 "parser.mly"
                                        ( _2                                                    )
# 657 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 160 "parser.mly"
                                        ( fun ctx   ->  Var(_1.i,name2index _1.i ctx _1.v,ctxlen ctx) )
# 664 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 161 "parser.mly"
                                        ( fun ctx   ->  Univ(_1,0)                           )
# 671 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 162 "parser.mly"
                                        ( fun ctx   ->  Bool(_1)                               )
# 678 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 163 "parser.mly"
                                        ( fun ctx   ->  Nat(_1)                                )
# 685 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 164 "parser.mly"
                                        ( fun ctx   ->  True(_1)                              )
# 692 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 165 "parser.mly"
                                        ( fun ctx   ->  False(_1)                             )
# 699 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int     Support.Error.withinfo) in
    Obj.repr(
# 166 "parser.mly"
                                        ( fun ctx   ->  let rec f = function
                                                            | 0 -> Zero(_1.i)
                                                            | n -> Succ(_1.i,f(n-1))in f _1.v )
# 708 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 170 "parser.mly"
                                        ( _1                                                    )
# 715 "parser.ml"
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
