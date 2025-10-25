type token =
  | BOOL of (Support.info)
  | LAMBDA of (Support.info)
  | IF of (Support.info)
  | THEN of (Support.info)
  | ELSE of (Support.info)
  | TRUE of (Support.info)
  | FALSE of (Support.info)
  | SUCC of (Support.info)
  | PRED of (Support.info)
  | ISZERO of (Support.info)
  | UCID of (string  Support.withinfo)
  | LCID of (string  Support.withinfo)
  | STRINGV of (string  Support.withinfo)
  | INTV of (int     Support.withinfo)
  | FLOATV of (float   Support.withinfo)
  | APOSTROPHE of (Support.info)
  | DQUOTE of (Support.info)
  | ARROW of (Support.info)
  | LEFTARROW of (Support.info)
  | BARGT of (Support.info)
  | BANG of (Support.info)
  | STAR of (Support.info)
  | SLASH of (Support.info)
  | HASH of (Support.info)
  | BARRCURLY of (Support.info)
  | LCURLYBAR of (Support.info)
  | BARRSQUARE of (Support.info)
  | LSQUAREBAR of (Support.info)
  | COLON of (Support.info)
  | COLONCOLON of (Support.info)
  | COLONEQ of (Support.info)
  | COLONHASH of (Support.info)
  | COMMA of (Support.info)
  | DARROW of (Support.info)
  | DDARROW of (Support.info)
  | DOT of (Support.info)
  | EXISTS of (Support.info)
  | EQ of (Support.info)
  | EQEQ of (Support.info)
  | GT of (Support.info)
  | LT of (Support.info)
  | LCURLY of (Support.info)
  | RCURLY of (Support.info)
  | LPAREN of (Support.info)
  | RPAREN of (Support.info)
  | LSQUARE of (Support.info)
  | RSQUARE of (Support.info)
  | SEMI of (Support.info)
  | DSEMI of (Support.info)
  | TRIANGLE of (Support.info)
  | USCORE of (Support.info)
  | VBAR of (Support.info)
  | NEWLINE of (Support.info)
  | EOF of (Support.info)

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Support
open Syntax
open Format
open Type
open Eval
# 66 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* BOOL *);
  258 (* LAMBDA *);
  259 (* IF *);
  260 (* THEN *);
  261 (* ELSE *);
  262 (* TRUE *);
  263 (* FALSE *);
  264 (* SUCC *);
  265 (* PRED *);
  266 (* ISZERO *);
  267 (* UCID *);
  268 (* LCID *);
  269 (* STRINGV *);
  270 (* INTV *);
  271 (* FLOATV *);
  272 (* APOSTROPHE *);
  273 (* DQUOTE *);
  274 (* ARROW *);
  275 (* LEFTARROW *);
  276 (* BARGT *);
  277 (* BANG *);
  278 (* STAR *);
  279 (* SLASH *);
  280 (* HASH *);
  281 (* BARRCURLY *);
  282 (* LCURLYBAR *);
  283 (* BARRSQUARE *);
  284 (* LSQUAREBAR *);
  285 (* COLON *);
  286 (* COLONCOLON *);
  287 (* COLONEQ *);
  288 (* COLONHASH *);
  289 (* COMMA *);
  290 (* DARROW *);
  291 (* DDARROW *);
  292 (* DOT *);
  293 (* EXISTS *);
  294 (* EQ *);
  295 (* EQEQ *);
  296 (* GT *);
  297 (* LT *);
  298 (* LCURLY *);
  299 (* RCURLY *);
  300 (* LPAREN *);
  301 (* RPAREN *);
  302 (* LSQUARE *);
  303 (* RSQUARE *);
  304 (* SEMI *);
  305 (* DSEMI *);
  306 (* TRIANGLE *);
  307 (* USCORE *);
  308 (* VBAR *);
  309 (* NEWLINE *);
    0 (* EOF *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\003\000\003\000\002\000\002\000\
\004\000\004\000\004\000\006\000\007\000\009\000\009\000\008\000\
\008\000\005\000\005\000\005\000\005\000\010\000\010\000\010\000\
\010\000\010\000\011\000\011\000\011\000\011\000\011\000\000\000\
\000\000"

let yylen = "\002\000\
\000\000\002\000\001\000\003\000\002\000\002\000\001\000\003\000\
\001\000\002\000\002\000\002\000\001\000\003\000\001\000\003\000\
\001\000\001\000\004\000\006\000\006\000\001\000\002\000\002\000\
\002\000\002\000\003\000\001\000\001\000\001\000\001\000\002\000\
\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\029\000\030\000\
\000\000\000\000\000\000\000\000\000\000\031\000\000\000\007\000\
\033\000\000\000\009\000\000\000\022\000\000\000\003\000\000\000\
\028\000\000\000\024\000\025\000\026\000\000\000\011\000\010\000\
\000\000\000\000\023\000\000\000\005\000\006\000\000\000\000\000\
\000\000\015\000\000\000\012\000\013\000\000\000\027\000\008\000\
\004\000\000\000\019\000\000\000\000\000\000\000\000\000\000\000\
\014\000\016\000\020\000\021\000"

let yydgoto = "\003\000\
\004\000\017\000\022\000\018\000\019\000\031\000\044\000\045\000\
\046\000\020\000\021\000"

let yysindex = "\026\000\
\000\000\058\000\000\000\009\255\251\254\034\255\000\000\000\000\
\019\255\019\255\019\255\232\254\232\254\000\000\034\255\000\000\
\000\000\218\254\000\000\019\255\000\000\003\000\000\000\249\254\
\000\000\020\255\000\000\000\000\000\000\001\255\000\000\000\000\
\224\254\058\000\000\000\009\255\000\000\000\000\001\255\034\255\
\034\255\000\000\001\255\000\000\000\000\014\255\000\000\000\000\
\000\000\003\255\000\000\042\255\006\255\001\255\034\255\034\255\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\052\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\014\000\000\000\000\000\000\000\
\000\000\000\000\000\000\030\000\000\000\074\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\023\000\000\000\002\000\250\255\046\000\221\255\006\000\
\000\000\000\000\045\000"

let yytablesize = 374
let yytable = "\026\000\
\017\000\042\000\038\000\050\000\030\000\023\000\024\000\053\000\
\033\000\034\000\005\000\006\000\047\000\028\000\007\000\008\000\
\009\000\010\000\011\000\012\000\013\000\039\000\014\000\041\000\
\007\000\008\000\001\000\002\000\040\000\018\000\025\000\054\000\
\014\000\051\000\052\000\005\000\006\000\049\000\055\000\007\000\
\008\000\009\000\010\000\011\000\043\000\025\000\056\000\014\000\
\059\000\060\000\057\000\032\000\015\000\027\000\028\000\029\000\
\048\000\016\000\032\000\058\000\000\000\000\000\015\000\000\000\
\035\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\002\000\000\000\000\000\000\000\015\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\017\000\017\000\000\000\000\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\000\000\017\000\028\000\
\028\000\000\000\000\000\028\000\028\000\028\000\028\000\028\000\
\028\000\028\000\000\000\028\000\000\000\000\000\000\000\018\000\
\018\000\018\000\018\000\000\000\017\000\018\000\018\000\018\000\
\018\000\000\000\000\000\000\000\017\000\017\000\000\000\000\000\
\017\000\017\000\036\000\037\000\000\000\000\000\000\000\000\000\
\000\000\028\000\000\000\005\000\006\000\028\000\028\000\007\000\
\008\000\009\000\010\000\011\000\012\000\013\000\000\000\014\000\
\000\000\000\000\018\000\002\000\002\000\018\000\018\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\000\000\002\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\015\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000"

let yycheck = "\006\000\
\000\000\001\001\000\000\039\000\029\001\004\000\012\001\043\000\
\015\000\048\001\002\001\003\001\045\001\000\000\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\029\001\014\001\004\001\
\006\001\007\001\001\000\002\000\036\001\000\000\012\001\018\001\
\014\001\040\000\041\000\002\001\003\001\036\000\036\001\006\001\
\007\001\008\001\009\001\010\001\044\001\012\001\005\001\014\001\
\055\000\056\000\045\001\000\000\044\001\009\000\010\000\011\000\
\034\000\000\000\013\000\054\000\255\255\255\255\044\001\255\255\
\020\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\000\000\255\255\255\255\255\255\044\001\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\255\255\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\255\255\014\001\002\001\
\003\001\255\255\255\255\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\255\255\014\001\255\255\255\255\255\255\002\001\
\003\001\004\001\005\001\255\255\036\001\008\001\009\001\010\001\
\011\001\255\255\255\255\255\255\044\001\045\001\255\255\255\255\
\048\001\049\001\048\001\049\001\255\255\255\255\255\255\255\255\
\255\255\044\001\255\255\002\001\003\001\048\001\049\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\255\255\014\001\
\255\255\255\255\045\001\002\001\003\001\048\001\049\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\255\255\014\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\044\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\044\001"

let yynames_const = "\
  "

let yynames_block = "\
  BOOL\000\
  LAMBDA\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  UCID\000\
  LCID\000\
  STRINGV\000\
  INTV\000\
  FLOATV\000\
  APOSTROPHE\000\
  DQUOTE\000\
  ARROW\000\
  LEFTARROW\000\
  BARGT\000\
  BANG\000\
  STAR\000\
  SLASH\000\
  HASH\000\
  BARRCURLY\000\
  LCURLYBAR\000\
  BARRSQUARE\000\
  LSQUAREBAR\000\
  COLON\000\
  COLONCOLON\000\
  COLONEQ\000\
  COLONHASH\000\
  COMMA\000\
  DARROW\000\
  DDARROW\000\
  DOT\000\
  EXISTS\000\
  EQ\000\
  EQEQ\000\
  GT\000\
  LT\000\
  LCURLY\000\
  RCURLY\000\
  LPAREN\000\
  RPAREN\000\
  LSQUARE\000\
  RSQUARE\000\
  SEMI\000\
  DSEMI\000\
  TRIANGLE\000\
  USCORE\000\
  VBAR\000\
  NEWLINE\000\
  EOF\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
                                ( fun ctx   ->  [],ctx  )
# 344 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 60 "parser.mly"
                                ( fun ctx   ->  let blks,ctx    = _1 ctx in 
                                                let blk,ctx     = _2 ctx in 
                                                List.append blks blk, ctx  )
# 354 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Command) in
    Obj.repr(
# 64 "parser.mly"
                                ( fun ctx   ->  let cmd,ctx = _1 ctx in [cmd],ctx )
# 361 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Command) in
    Obj.repr(
# 65 "parser.mly"
                                ( fun ctx   ->  let cmd,ctx = _3 ctx in let cmds,ctx  = _1 ctx in 
                                                List.append cmds [cmd], ctx )
# 371 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 67 "parser.mly"
                                ( let cmds,ctx  = _1 emptycontext in 
                                  let _         = List.iter (print_eval ctx) cmds in 
                                  fun ctx   -> let blk,ctx = _1 ctx in blk,ctx )
# 381 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 70 "parser.mly"
                                ( fun ctx   -> let blk,ctx = _1 ctx in blk,ctx )
# 389 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 76 "parser.mly"
                              ( fun ctx   ->    [],ctx )
# 396 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    Obj.repr(
# 77 "parser.mly"
                              ( fun ctx   ->    let cmd,ctx   = _1 ctx in 
                                                let cmds,ctx  = _3 ctx in 
                                                cmd::cmds,ctx )
# 407 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 109 "parser.mly"
                                    ( fun ctx   ->  Eval(tmInfo (_1 ctx),_1 ctx),ctx )
# 414 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 110 "parser.mly"
                                    ( fun ctx   ->  Bind(_1.i,_1.v,_2 ctx), addname ctx _1.v )
# 422 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 111 "parser.mly"
                                    ( fun ctx   ->  Bind(_1.i,_1.v,_2 ctx), addname ctx _1.v )
# 430 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 113 "parser.mly"
                                    ( fun ctx   ->  VarBind(_2 ctx) )
# 438 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 116 "parser.mly"
                                    ( _1 )
# 445 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 118 "parser.mly"
                                    ( _2 )
# 454 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 119 "parser.mly"
                                    ( fun ctx   ->  TyBool )
# 461 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 121 "parser.mly"
                                    ( fun ctx   ->  TyArr(_1 ctx, _3 ctx) )
# 470 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 122 "parser.mly"
                                    ( _1 )
# 477 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTm) in
    Obj.repr(
# 125 "parser.mly"
                                    ( _1 )
# 484 "parser.ml"
               : 'Tm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string  Support.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 126 "parser.mly"
                                    ( fun ctx   ->  let t2 = _4 (addname ctx _2.v) in 
                                                    TmAbs(_1, _2.v, typeof ctx t2, t2) )
# 495 "parser.ml"
               : 'Tm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 128 "parser.mly"
                                    ( fun ctx   ->  let ctx1 = addname ctx _2.v in 
                                                    TmAbs(_1, _2.v, _4 ctx, _6 ctx1) )
# 508 "parser.ml"
               : 'Tm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Tm) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Tm) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 130 "parser.mly"
                                    ( fun ctx   ->  TmIf(_1, _2 ctx, _4 ctx, _6 ctx) )
# 520 "parser.ml"
               : 'Tm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATm) in
    Obj.repr(
# 132 "parser.mly"
                                    ( _1 )
# 527 "parser.ml"
               : 'AppTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATm) in
    Obj.repr(
# 133 "parser.mly"
                                    ( fun ctx   ->  let e1 = _1 ctx in TmApp(tmInfo e1,e1,_2 ctx) )
# 535 "parser.ml"
               : 'AppTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATm) in
    Obj.repr(
# 134 "parser.mly"
                                    ( fun ctx   ->  TmSucc(_1, _2 ctx ) )
# 543 "parser.ml"
               : 'AppTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATm) in
    Obj.repr(
# 135 "parser.mly"
                                    ( fun ctx   ->  TmPred(_1, _2 ctx ) )
# 551 "parser.ml"
               : 'AppTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATm) in
    Obj.repr(
# 136 "parser.mly"
                                    ( fun ctx   ->  TmIsZero(_1, _2 ctx) )
# 559 "parser.ml"
               : 'AppTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Tm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 138 "parser.mly"
                                    ( _2 )
# 568 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.withinfo) in
    Obj.repr(
# 139 "parser.mly"
                                    ( fun ctx   ->  TmVar(_1.i, name2index _1.i ctx _1.v, ctxlength ctx) )
# 575 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 140 "parser.mly"
                                    ( fun ctx   ->  TmTrue(_1) )
# 582 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 141 "parser.mly"
                                    ( fun ctx   ->  TmFalse(_1) )
# 589 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int     Support.withinfo) in
    Obj.repr(
# 142 "parser.mly"
                                    ( fun ctx   ->  let rec f = function
                                                    | 0 -> TmZero(_1.i)
                                                    | n -> TmSucc(_1.i,f(n-1))      in f _1.v )
# 598 "parser.ml"
               : 'ATm))
(* Entry repl *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry line *)
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
let repl (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.context -> (Syntax.command list * Syntax.context))
let line (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Syntax.context -> (Syntax.command list * Syntax.context))
