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
open Core 
open Format
open Type
open Eval

# 70 "parser.ml"
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
\001\000\002\000\001\000\001\000\003\000\001\000\003\000\001\000\
\001\000\004\000\006\000\006\000\001\000\002\000\002\000\002\000\
\002\000\003\000\001\000\001\000\001\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\001\000\000\000\000\000\000\000\028\000\029\000\000\000\
\000\000\000\000\000\000\030\000\007\000\000\000\031\000\000\000\
\009\000\000\000\021\000\000\000\000\000\027\000\000\000\023\000\
\024\000\025\000\011\000\010\000\000\000\000\000\022\000\000\000\
\003\000\000\000\000\000\000\000\026\000\008\000\006\000\000\000\
\005\000\000\000\018\000\000\000\004\000\000\000\000\000\019\000\
\020\000"

let yydgoto = "\003\000\
\015\000\020\000\032\000\016\000\017\000\028\000\000\000\000\000\
\000\000\018\000\019\000"

let yysindex = "\005\000\
\022\000\000\000\000\000\248\254\008\255\000\000\000\000\037\255\
\037\255\037\255\217\254\000\000\000\000\008\255\000\000\229\254\
\000\000\037\255\000\000\026\255\238\254\000\000\020\255\000\000\
\000\000\000\000\000\000\000\000\237\254\022\000\000\000\002\000\
\000\000\015\255\008\255\008\255\000\000\000\000\000\000\026\255\
\000\000\253\254\000\000\032\255\000\000\008\255\008\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\000\000\040\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\062\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\015\000\000\000\000\000\239\255\251\255\000\000\000\000\000\000\
\000\000\000\000\043\000"

let yytablesize = 357
let yytable = "\023\000\
\027\000\039\000\033\000\021\000\034\000\001\000\002\000\027\000\
\029\000\004\000\005\000\035\000\017\000\006\000\007\000\008\000\
\009\000\010\000\030\000\022\000\012\000\013\000\045\000\036\000\
\037\000\042\000\046\000\004\000\005\000\043\000\044\000\006\000\
\007\000\008\000\009\000\010\000\047\000\011\000\012\000\032\000\
\048\000\049\000\006\000\007\000\038\000\000\000\014\000\000\000\
\022\000\012\000\024\000\025\000\026\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\031\000\002\000\000\000\000\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\014\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\027\000\027\000\000\000\000\000\027\000\027\000\
\027\000\027\000\027\000\000\000\027\000\027\000\017\000\017\000\
\017\000\017\000\000\000\000\000\017\000\017\000\017\000\004\000\
\005\000\000\000\000\000\006\000\007\000\008\000\009\000\010\000\
\000\000\011\000\012\000\000\000\000\000\000\000\000\000\027\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\000\040\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\000\041\000\
\017\000\000\000\017\000\000\000\014\000\000\000\000\000\002\000\
\002\000\000\000\017\000\002\000\002\000\002\000\002\000\002\000\
\000\000\002\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\000"

let yycheck = "\005\000\
\000\000\000\000\020\000\012\001\023\001\001\000\002\000\047\001\
\014\000\002\001\003\001\030\001\000\000\006\001\007\001\008\001\
\009\001\010\001\046\001\012\001\013\001\000\000\040\000\004\001\
\044\001\011\001\030\001\002\001\003\001\035\000\036\000\006\001\
\007\001\008\001\009\001\010\001\005\001\012\001\013\001\000\000\
\046\000\047\000\006\001\007\001\030\000\255\255\039\001\255\255\
\012\001\013\001\008\000\009\000\010\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\018\000\000\000\255\255\255\255\
\039\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\039\001\255\255\255\255\255\255\255\255\
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
\008\001\009\001\010\001\255\255\012\001\013\001\002\001\003\001\
\004\001\005\001\255\255\255\255\008\001\009\001\010\001\002\001\
\003\001\255\255\255\255\006\001\007\001\008\001\009\001\010\001\
\255\255\012\001\013\001\255\255\255\255\255\255\255\255\039\001\
\255\255\255\255\255\255\255\255\255\255\255\255\046\001\046\001\
\255\255\255\255\255\255\255\255\255\255\255\255\054\001\054\001\
\044\001\255\255\046\001\255\255\039\001\255\255\255\255\002\001\
\003\001\255\255\054\001\006\001\007\001\008\001\009\001\010\001\
\255\255\012\001\013\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
# 91 "parser.mly"
                              ( fun ctx   -> [],ctx  )
# 341 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 92 "parser.mly"
                              ( fun ctx   -> 
                                let blks,ctx = _1 ctx in let blk,ctx = _2 ctx in (List.append blks blk, ctx) )
# 350 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Command) in
    Obj.repr(
# 95 "parser.mly"
                              ( fun ctx   -> let cmd,ctx = _1 ctx in [cmd],ctx )
# 357 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Command) in
    Obj.repr(
# 96 "parser.mly"
                              ( fun ctx   -> let cmd,ctx = _3 ctx in let cmds,ctx  = _1 ctx in 
                                List.append cmds [cmd], ctx )
# 367 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 98 "parser.mly"
                              ( let cmds,ctx = _1 emptycontext in let _ = List.iter (print_eval ctx) cmds in 
                                fun ctx   -> let blk,ctx = _1 ctx in blk,ctx )
# 376 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 100 "parser.mly"
                              ( fun ctx   -> let blk,ctx = _1 ctx in blk,ctx )
# 384 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 105 "parser.mly"
                              ( fun ctx   -> [],ctx )
# 391 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    Obj.repr(
# 106 "parser.mly"
                              ( fun ctx   -> 
          let cmd,ctx   = _1 ctx in 
          let cmds,ctx  = _3 ctx in 
          cmd::cmds,ctx )
# 403 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 114 "parser.mly"
                                    ( fun ctx   -> let t = _1 ctx in Eval(tmInfo t,t),ctx )
# 410 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 115 "parser.mly"
                                    ( fun ctx   -> ((Bind(_1.i,_1.v,_2 ctx)), addname ctx _1.v) )
# 418 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 117 "parser.mly"
                                    ( fun ctx   -> NameBind )
# 425 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 120 "parser.mly"
                                    ( _1 )
# 432 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 122 "parser.mly"
                                    ( _2 )
# 441 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 123 "parser.mly"
                                    ( fun ctx   -> TyBool )
# 448 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 125 "parser.mly"
                                    ( fun ctx   -> TyArr(_1 ctx, _3 ctx) )
# 457 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 126 "parser.mly"
                                    ( _1 )
# 464 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 129 "parser.mly"
                                    ( _1 )
# 471 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 130 "parser.mly"
                                    ( print_endline "hoge";  
        fun ctx -> 
            let t2 = _4 (addname ctx _2.v) in 
            TmAbs(_1, _2.v, typeof ctx t2, t2) )
# 484 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 135 "parser.mly"
    ( print_endline "hoge";  fun ctx -> 
        let t2 = _6 (addname ctx _2.v) in TmAbs(_1, _2.v, typeof ctx t2, t2) )
# 497 "parser.ml"
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
# 509 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 139 "parser.mly"
                                    ( _1 )
# 516 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 140 "parser.mly"
                                    ( print_endline "fuga";fun ctx -> let e1 = _1 ctx in TmApp(tmInfo e1,e1,_2 ctx) )
# 524 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 141 "parser.mly"
                                    ( fun ctx -> TmSucc(_1, _2 ctx ) )
# 532 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 142 "parser.mly"
                                    ( fun ctx -> TmPred(_1, _2 ctx ) )
# 540 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 143 "parser.mly"
                                    ( fun ctx -> TmIsZero(_1, _2 ctx) )
# 548 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 145 "parser.mly"
                                    ( print_endline "term"; _2 )
# 557 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 146 "parser.mly"
                                    ( fun ctx -> TmVar(_1.i, name2index _1.i ctx _1.v, ctxlength ctx) )
# 564 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 147 "parser.mly"
                                    ( fun ctx -> TmTrue(_1) )
# 571 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 148 "parser.mly"
                                    ( fun ctx -> TmFalse(_1) )
# 578 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int     Support.Error.withinfo) in
    Obj.repr(
# 149 "parser.mly"
                                    ( fun ctx -> let rec f = function
              0 -> TmZero(_1.i)
            | n -> print_endline "succ"; TmSucc(_1.i, f (n-1))
          in f _1.v )
# 588 "parser.ml"
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
