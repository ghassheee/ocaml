type token =
  | IN of (Support.Error.info)
  | LET of (Support.Error.info)
  | BOOL of (Support.Error.info)
  | NAT of (Support.Error.info)
  | LAMBDA of (Support.Error.info)
  | IF of (Support.Error.info)
  | THEN of (Support.Error.info)
  | ELSE of (Support.Error.info)
  | TRUE of (Support.Error.info)
  | FALSE of (Support.Error.info)
  | SUCC of (Support.Error.info)
  | PRED of (Support.Error.info)
  | ISZERO of (Support.Error.info)
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
  | HOGE of (Support.Error.info)

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Support.Error
open Support.Pervasive
open Syntax
open Core 
open Format
open Type
open Eval

let pe = print_endline 
# 74 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* IN *);
  258 (* LET *);
  259 (* BOOL *);
  260 (* NAT *);
  261 (* LAMBDA *);
  262 (* IF *);
  263 (* THEN *);
  264 (* ELSE *);
  265 (* TRUE *);
  266 (* FALSE *);
  267 (* SUCC *);
  268 (* PRED *);
  269 (* ISZERO *);
  270 (* UCID *);
  271 (* LCID *);
  272 (* INTV *);
  273 (* FLOATV *);
  274 (* STRINGV *);
  275 (* APOSTROPHE *);
  276 (* DQUOTE *);
  277 (* ARROW *);
  278 (* BANG *);
  279 (* BARGT *);
  280 (* BARRCURLY *);
  281 (* BARRSQUARE *);
  282 (* COLON *);
  283 (* COLONCOLON *);
  284 (* COLONEQ *);
  285 (* COLONHASH *);
  286 (* COMMA *);
  287 (* DARROW *);
  288 (* DDARROW *);
  289 (* DOT *);
    0 (* EOF *);
  290 (* EQ *);
  291 (* EQEQ *);
  292 (* EXISTS *);
  293 (* GT *);
  294 (* HASH *);
  295 (* LCURLY *);
  296 (* LCURLYBAR *);
  297 (* LEFTARROW *);
  298 (* LPAREN *);
  299 (* LSQUARE *);
  300 (* LSQUAREBAR *);
  301 (* LT *);
  302 (* RCURLY *);
  303 (* RPAREN *);
  304 (* RSQUARE *);
  305 (* SEMI *);
  306 (* SLASH *);
  307 (* STAR *);
  308 (* TRIANGLE *);
  309 (* USCORE *);
  310 (* VBAR *);
  311 (* NEWLINE *);
  312 (* DOUBLESEMI *);
  313 (* HOGE *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\003\000\003\000\003\000\003\000\001\000\001\000\
\004\000\004\000\006\000\007\000\009\000\009\000\009\000\008\000\
\008\000\005\000\005\000\005\000\005\000\010\000\010\000\010\000\
\010\000\010\000\011\000\011\000\011\000\011\000\011\000\000\000\
\000\000"

let yylen = "\002\000\
\000\000\002\000\001\000\003\000\002\000\002\000\001\000\003\000\
\001\000\002\000\002\000\001\000\003\000\001\000\001\000\003\000\
\001\000\001\000\006\000\006\000\006\000\001\000\002\000\002\000\
\002\000\002\000\003\000\001\000\001\000\001\000\001\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\029\000\030\000\
\000\000\000\000\000\000\000\000\031\000\007\000\000\000\032\000\
\000\000\009\000\000\000\022\000\000\000\000\000\000\000\028\000\
\000\000\023\000\024\000\025\000\000\000\010\000\000\000\000\000\
\026\000\000\000\003\000\000\000\000\000\000\000\014\000\015\000\
\000\000\011\000\012\000\000\000\027\000\008\000\006\000\000\000\
\005\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\
\000\000\000\000\013\000\016\000\019\000\020\000\021\000"

let yydgoto = "\003\000\
\016\000\021\000\034\000\017\000\018\000\030\000\042\000\043\000\
\044\000\019\000\020\000"

let yysindex = "\004\000\
\062\000\000\000\000\000\244\254\004\255\005\255\000\000\000\000\
\044\255\044\255\044\255\254\254\000\000\000\000\005\255\000\000\
\232\254\000\000\044\255\000\000\033\255\251\254\007\255\000\000\
\030\255\000\000\000\000\000\000\019\255\000\000\249\254\062\000\
\000\000\002\000\000\000\005\255\019\255\005\255\000\000\000\000\
\019\255\000\000\000\000\020\255\000\000\000\000\000\000\033\255\
\000\000\054\255\023\255\049\255\011\255\019\255\000\000\005\255\
\005\255\005\255\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
\000\000\000\000\034\000\000\000\063\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\083\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\032\000\000\000\000\000\239\255\250\255\000\000\227\255\011\000\
\000\000\000\000\017\000"

let yytablesize = 381
let yytable = "\025\000\
\017\000\047\000\022\000\035\000\001\000\002\000\004\000\051\000\
\031\000\005\000\006\000\053\000\028\000\007\000\008\000\009\000\
\010\000\011\000\023\000\024\000\013\000\039\000\040\000\029\000\
\032\000\026\000\027\000\028\000\036\000\050\000\055\000\052\000\
\037\000\018\000\004\000\033\000\038\000\005\000\006\000\045\000\
\054\000\007\000\008\000\009\000\010\000\011\000\015\000\012\000\
\013\000\061\000\062\000\063\000\007\000\008\000\056\000\057\000\
\058\000\059\000\024\000\013\000\041\000\014\000\033\000\046\000\
\060\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\015\000\000\000\000\000\
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
\000\000\000\000\017\000\000\000\000\000\017\000\017\000\000\000\
\000\000\017\000\017\000\017\000\017\000\017\000\028\000\017\000\
\017\000\028\000\028\000\000\000\000\000\028\000\028\000\028\000\
\028\000\028\000\000\000\028\000\028\000\000\000\000\000\000\000\
\000\000\017\000\018\000\018\000\000\000\000\000\018\000\018\000\
\018\000\018\000\017\000\000\000\018\000\018\000\018\000\017\000\
\000\000\017\000\048\000\000\000\000\000\000\000\028\000\000\000\
\000\000\017\000\049\000\000\000\000\000\028\000\000\000\004\000\
\000\000\000\000\005\000\006\000\000\000\028\000\007\000\008\000\
\009\000\010\000\011\000\000\000\012\000\013\000\000\000\000\000\
\018\000\000\000\018\000\000\000\002\000\000\000\000\000\002\000\
\002\000\000\000\018\000\002\000\002\000\002\000\002\000\002\000\
\000\000\002\000\002\000\000\000\000\000\000\000\000\000\015\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\000"

let yycheck = "\006\000\
\000\000\000\000\015\001\021\000\001\000\002\000\002\001\037\000\
\015\000\005\001\006\001\041\000\000\000\009\001\010\001\011\001\
\012\001\013\001\015\001\015\001\016\001\003\001\004\001\026\001\
\049\001\009\000\010\000\011\000\034\001\036\000\048\000\038\000\
\026\001\000\000\002\001\019\000\007\001\005\001\006\001\047\001\
\021\001\009\001\010\001\011\001\012\001\013\001\042\001\015\001\
\016\001\056\000\057\000\058\000\009\001\010\001\001\001\033\001\
\008\001\047\001\015\001\016\001\042\001\000\000\000\000\032\000\
\054\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\042\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\042\001\255\255\255\255\
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
\255\255\255\255\002\001\255\255\255\255\005\001\006\001\255\255\
\255\255\009\001\010\001\011\001\012\001\013\001\002\001\015\001\
\016\001\005\001\006\001\255\255\255\255\009\001\010\001\011\001\
\012\001\013\001\255\255\015\001\016\001\255\255\255\255\255\255\
\255\255\033\001\001\001\002\001\255\255\255\255\005\001\006\001\
\007\001\008\001\042\001\255\255\011\001\012\001\013\001\047\001\
\255\255\049\001\049\001\255\255\255\255\255\255\042\001\255\255\
\255\255\057\001\057\001\255\255\255\255\049\001\255\255\002\001\
\255\255\255\255\005\001\006\001\255\255\057\001\009\001\010\001\
\011\001\012\001\013\001\255\255\015\001\016\001\255\255\255\255\
\047\001\255\255\049\001\255\255\002\001\255\255\255\255\005\001\
\006\001\255\255\057\001\009\001\010\001\011\001\012\001\013\001\
\255\255\015\001\016\001\255\255\255\255\255\255\255\255\042\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\042\001"

let yynames_const = "\
  "

let yynames_block = "\
  IN\000\
  LET\000\
  BOOL\000\
  NAT\000\
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
  HOGE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                                    ( fun ctx   -> [],ctx  )
# 362 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 97 "parser.mly"
                                    ( fun ctx   -> 
                                        let blks,ctx = _1 ctx in 
                                        let blk,ctx = _2 ctx in 
                                        (List.append blks blk, ctx) )
# 373 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Command) in
    Obj.repr(
# 102 "parser.mly"
                                    ( fun ctx   -> let cmd,ctx = _1 ctx in [cmd],ctx )
# 380 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Command) in
    Obj.repr(
# 103 "parser.mly"
                                    ( fun ctx   -> 
                                        let cmd,ctx = _3 ctx in 
                                        let cmds,ctx  = _1 ctx in 
                                        List.append cmds [cmd], ctx )
# 392 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 107 "parser.mly"
                                    ( let cmds,ctx = _1 emptycontext in let _ = List.iter (print_eval ctx) cmds in 
                                      fun ctx   -> let blk,ctx = _1 ctx in blk,ctx )
# 401 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 109 "parser.mly"
                                    ( fun ctx   -> let blk,ctx = _1 ctx in blk,ctx )
# 409 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 114 "parser.mly"
                                    ( fun ctx   -> [],ctx )
# 416 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    Obj.repr(
# 115 "parser.mly"
                                    ( fun ctx   -> 
          let cmd,ctx   = _1 ctx in 
          let cmds,ctx  = _3 ctx in 
          cmd::cmds,ctx )
# 428 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 123 "parser.mly"
                                    ( fun ctx   -> let t = _1 ctx in Eval(tmInfo t,t),ctx )
# 435 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 124 "parser.mly"
                                    ( fun ctx   -> ((Bind(_1.i,_1.v,_2 ctx)), addname ctx _1.v) )
# 443 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 126 "parser.mly"
                                    ( fun ctx   -> VarBind(_2 ctx) )
# 451 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 129 "parser.mly"
                                    ( _1 )
# 458 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 131 "parser.mly"
                                    ( _2 )
# 467 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 132 "parser.mly"
                                    ( fun ctx   -> TyBool )
# 474 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 133 "parser.mly"
                                    ( fun ctx   -> TyNat  )
# 481 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 135 "parser.mly"
                                    ( fun ctx   -> TyArr(_1 ctx, _3 ctx) )
# 490 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 136 "parser.mly"
                                    ( _1 )
# 497 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 139 "parser.mly"
                                    ( _1 )
# 504 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 140 "parser.mly"
                                    ( fun ctx   -> TmLet(_1, _2.v, _4 ctx, _6 (addname ctx _2.v)) )
# 516 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 142 "parser.mly"
        ( pe "PARSER: λx:T.t"; fun ctx -> let ctx1=addname ctx _2.v in TmAbs(_1,_2.v,_4 ctx,_6 ctx1))
# 528 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 143 "parser.mly"
                                    ( fun ctx   -> TmIf(_1, _2 ctx, _4 ctx, _6 ctx) )
# 540 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 145 "parser.mly"
                                    ( _1 )
# 547 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 146 "parser.mly"
                                    ( fun ctx   -> TmSucc(_1, _2 ctx ) )
# 555 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 147 "parser.mly"
                                    ( fun ctx   -> TmPred(_1, _2 ctx ) )
# 563 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 148 "parser.mly"
                                    ( fun ctx   -> TmIsZero(_1, _2 ctx) )
# 571 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 149 "parser.mly"
                                    ( fun ctx   -> let e1=_1 ctx in TmApp(tmInfo e1,e1,_2 ctx) )
# 579 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 151 "parser.mly"
                                    ( pe "PARSER: ( t )"; _2 )
# 588 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 152 "parser.mly"
                                    ( fun ctx   -> TmVar(_1.i, name2index _1.i ctx _1.v, ctxlength ctx) )
# 595 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 153 "parser.mly"
                                    ( fun ctx   -> TmTrue(_1) )
# 602 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 154 "parser.mly"
                                    ( fun ctx   -> TmFalse(_1) )
# 609 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int     Support.Error.withinfo) in
    Obj.repr(
# 155 "parser.mly"
                                    ( fun ctx   -> let rec f = function
              0 -> TmZero(_1.i)
            | n -> pe "succ"; TmSucc(_1.i, f (n-1))
          in f _1.v )
# 619 "parser.ml"
               : 'ATerm))
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
