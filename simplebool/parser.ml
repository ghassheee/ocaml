type token =
  | BOOL of (Support.Error.info)
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
open Format
open Type
open Eval

# 69 "parser.ml"
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
  269 (* INTV *);
  270 (* FLOATV *);
  271 (* STRINGV *);
  272 (* APOSTROPHE *);
  273 (* DQUOTE *);
  274 (* ARROW *);
  275 (* BANG *);
  276 (* BARGT *);
  277 (* BARRCURLY *);
  278 (* BARRSQUARE *);
  279 (* COLON *);
  280 (* COLONCOLON *);
  281 (* COLONEQ *);
  282 (* COLONHASH *);
  283 (* COMMA *);
  284 (* DARROW *);
  285 (* DDARROW *);
  286 (* DOT *);
    0 (* EOF *);
  287 (* EQ *);
  288 (* EQEQ *);
  289 (* EXISTS *);
  290 (* GT *);
  291 (* HASH *);
  292 (* LCURLY *);
  293 (* LCURLYBAR *);
  294 (* LEFTARROW *);
  295 (* LPAREN *);
  296 (* LSQUARE *);
  297 (* LSQUAREBAR *);
  298 (* LT *);
  299 (* RCURLY *);
  300 (* RPAREN *);
  301 (* RSQUARE *);
  302 (* SEMI *);
  303 (* SLASH *);
  304 (* STAR *);
  305 (* TRIANGLE *);
  306 (* USCORE *);
  307 (* VBAR *);
  308 (* NEWLINE *);
  309 (* DOUBLESEMI *);
  310 (* HOGE *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\003\000\003\000\003\000\003\000\001\000\001\000\
\004\000\004\000\006\000\007\000\009\000\009\000\008\000\008\000\
\005\000\005\000\005\000\005\000\010\000\010\000\010\000\010\000\
\010\000\011\000\011\000\011\000\011\000\011\000\000\000\000\000"

let yylen = "\002\000\
\000\000\002\000\001\000\003\000\002\000\002\000\001\000\003\000\
\001\000\002\000\002\000\001\000\003\000\001\000\003\000\001\000\
\001\000\004\000\006\000\006\000\001\000\002\000\002\000\002\000\
\002\000\003\000\001\000\001\000\001\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\001\000\000\000\000\000\000\000\028\000\029\000\000\000\
\000\000\000\000\000\000\030\000\007\000\000\000\031\000\000\000\
\009\000\000\000\021\000\000\000\000\000\027\000\000\000\023\000\
\024\000\025\000\000\000\010\000\000\000\000\000\022\000\000\000\
\003\000\000\000\000\000\000\000\014\000\000\000\011\000\012\000\
\000\000\026\000\008\000\006\000\000\000\005\000\000\000\018\000\
\000\000\000\000\000\000\004\000\000\000\000\000\013\000\015\000\
\019\000\020\000"

let yydgoto = "\003\000\
\015\000\020\000\032\000\016\000\017\000\028\000\039\000\040\000\
\041\000\018\000\019\000"

let yysindex = "\004\000\
\074\000\000\000\000\000\251\254\008\255\000\000\000\000\038\255\
\038\255\038\255\255\254\000\000\000\000\008\255\000\000\233\254\
\000\000\038\255\000\000\026\255\252\254\000\000\020\255\000\000\
\000\000\000\000\002\255\000\000\239\254\074\000\000\000\002\000\
\000\000\002\255\008\255\008\255\000\000\002\255\000\000\000\000\
\022\255\000\000\000\000\000\000\026\255\000\000\012\255\000\000\
\041\255\011\255\002\255\000\000\008\255\008\255\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\000\000\000\043\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\086\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\026\000\000\000\000\000\248\255\251\255\000\000\226\255\006\000\
\000\000\000\000\044\000"

let yytablesize = 381
let yytable = "\023\000\
\016\000\044\000\037\000\047\000\001\000\002\000\021\000\050\000\
\029\000\004\000\005\000\033\000\027\000\006\000\007\000\008\000\
\009\000\010\000\034\000\022\000\012\000\027\000\030\000\036\000\
\017\000\035\000\042\000\004\000\005\000\048\000\049\000\006\000\
\007\000\008\000\009\000\010\000\052\000\011\000\012\000\051\000\
\038\000\053\000\032\000\006\000\007\000\054\000\014\000\057\000\
\058\000\022\000\012\000\024\000\025\000\026\000\055\000\043\000\
\056\000\000\000\000\000\000\000\000\000\031\000\000\000\000\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\000\000\000\000\014\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\
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
\000\000\000\000\016\000\016\000\000\000\000\000\016\000\016\000\
\016\000\016\000\016\000\000\000\016\000\016\000\027\000\027\000\
\000\000\000\000\027\000\027\000\027\000\027\000\027\000\000\000\
\027\000\027\000\017\000\017\000\017\000\017\000\016\000\000\000\
\017\000\017\000\017\000\000\000\000\000\000\000\000\000\016\000\
\000\000\000\000\000\000\000\000\016\000\000\000\016\000\045\000\
\000\000\000\000\000\000\027\000\000\000\000\000\016\000\046\000\
\000\000\000\000\027\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\000\000\000\017\000\000\000\017\000\000\000\
\000\000\000\000\000\000\004\000\005\000\000\000\017\000\006\000\
\007\000\008\000\009\000\010\000\000\000\011\000\012\000\002\000\
\002\000\000\000\000\000\002\000\002\000\002\000\002\000\002\000\
\000\000\002\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\000"

let yycheck = "\005\000\
\000\000\000\000\001\001\034\000\001\000\002\000\012\001\038\000\
\014\000\002\001\003\001\020\000\000\000\006\001\007\001\008\001\
\009\001\010\001\023\001\012\001\013\001\023\001\046\001\004\001\
\000\000\030\001\044\001\002\001\003\001\035\000\036\000\006\001\
\007\001\008\001\009\001\010\001\045\000\012\001\013\001\018\001\
\039\001\030\001\000\000\006\001\007\001\005\001\039\001\053\000\
\054\000\012\001\013\001\008\000\009\000\010\000\044\001\030\000\
\051\000\255\255\255\255\255\255\255\255\018\000\255\255\255\255\
\039\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\000\000\255\255\255\255\039\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
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
\008\001\009\001\010\001\255\255\012\001\013\001\002\001\003\001\
\255\255\255\255\006\001\007\001\008\001\009\001\010\001\255\255\
\012\001\013\001\002\001\003\001\004\001\005\001\030\001\255\255\
\008\001\009\001\010\001\255\255\255\255\255\255\255\255\039\001\
\255\255\255\255\255\255\255\255\044\001\255\255\046\001\046\001\
\255\255\255\255\255\255\039\001\255\255\255\255\054\001\054\001\
\255\255\255\255\046\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\054\001\255\255\044\001\255\255\046\001\255\255\
\255\255\255\255\255\255\002\001\003\001\255\255\054\001\006\001\
\007\001\008\001\009\001\010\001\255\255\012\001\013\001\002\001\
\003\001\255\255\255\255\006\001\007\001\008\001\009\001\010\001\
\255\255\012\001\013\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\039\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\039\001"

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
# 90 "parser.mly"
                              ( fun ctx   -> [],ctx  )
# 349 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 91 "parser.mly"
                              ( fun ctx   -> 
                                let blks,ctx = _1 ctx in let blk,ctx = _2 ctx in (List.append blks blk, ctx) )
# 358 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Command) in
    Obj.repr(
# 94 "parser.mly"
                              ( fun ctx   -> let cmd,ctx = _1 ctx in [cmd],ctx )
# 365 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Command) in
    Obj.repr(
# 95 "parser.mly"
                              ( fun ctx   -> let cmd,ctx = _3 ctx in let cmds,ctx  = _1 ctx in 
                                List.append cmds [cmd], ctx )
# 375 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 97 "parser.mly"
                              ( let cmds,ctx = _1 emptycontext in let _ = List.iter (print_eval ctx) cmds in 
                                fun ctx   -> let blk,ctx = _1 ctx in blk,ctx )
# 384 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 99 "parser.mly"
                              ( fun ctx   -> let blk,ctx = _1 ctx in blk,ctx )
# 392 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 104 "parser.mly"
                              ( fun ctx   -> [],ctx )
# 399 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    Obj.repr(
# 105 "parser.mly"
                              ( fun ctx   -> 
          let cmd,ctx   = _1 ctx in 
          let cmds,ctx  = _3 ctx in 
          cmd::cmds,ctx )
# 411 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 113 "parser.mly"
                                    ( fun ctx   -> let t = _1 ctx in Eval(tmInfo t,t),ctx )
# 418 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 114 "parser.mly"
                                    ( fun ctx   -> ((Bind(_1.i,_1.v,_2 ctx)), addname ctx _1.v) )
# 426 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 116 "parser.mly"
                                    ( fun ctx   -> VarBind(_2 ctx) )
# 434 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 119 "parser.mly"
                                    ( _1 )
# 441 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 121 "parser.mly"
                                    ( _2 )
# 450 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 122 "parser.mly"
                                    ( fun ctx   -> TyBool )
# 457 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 124 "parser.mly"
                                    ( fun ctx   -> TyArr(_1 ctx, _3 ctx) )
# 466 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 125 "parser.mly"
                                    ( _1 )
# 473 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 128 "parser.mly"
                                    ( _1 )
# 480 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 129 "parser.mly"
                                    ( print_endline "hoge";  
        fun ctx -> 
            let t2 = _4 (addname ctx _2.v) in 
            TmAbs(_1, _2.v, typeof ctx t2, t2) )
# 493 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 134 "parser.mly"
    ( print_endline "hoge";  fun ctx -> 
        let ctx1 = addname ctx _2.v in 
        TmAbs(_1, _2.v, _4 ctx, _6 ctx1) )
# 507 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 137 "parser.mly"
                                    ( fun ctx -> TmIf(_1, _2 ctx, _4 ctx, _6 ctx) )
# 519 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 139 "parser.mly"
                                    ( _1 )
# 526 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 140 "parser.mly"
                                    ( print_endline "fuga";fun ctx -> let e1 = _1 ctx in TmApp(tmInfo e1,e1,_2 ctx) )
# 534 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 141 "parser.mly"
                                    ( fun ctx -> TmSucc(_1, _2 ctx ) )
# 542 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 142 "parser.mly"
                                    ( fun ctx -> TmPred(_1, _2 ctx ) )
# 550 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 143 "parser.mly"
                                    ( fun ctx -> TmIsZero(_1, _2 ctx) )
# 558 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 145 "parser.mly"
                                    ( print_endline "term"; _2 )
# 567 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 146 "parser.mly"
                                    ( fun ctx -> TmVar(_1.i, name2index _1.i ctx _1.v, ctxlength ctx) )
# 574 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 147 "parser.mly"
                                    ( fun ctx -> TmTrue(_1) )
# 581 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 148 "parser.mly"
                                    ( fun ctx -> TmFalse(_1) )
# 588 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int     Support.Error.withinfo) in
    Obj.repr(
# 149 "parser.mly"
                                    ( fun ctx -> let rec f = function
              0 -> TmZero(_1.i)
            | n -> print_endline "succ"; TmSucc(_1.i, f (n-1))
          in f _1.v )
# 598 "parser.ml"
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
