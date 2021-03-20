type token =
  | BOOL of (Support.info)
  | NAT of (Support.info)
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
  | INTV of (int     Support.withinfo)
  | FLOATV of (float   Support.withinfo)
  | STRINGV of (string  Support.withinfo)
  | APOSTROPHE of (Support.info)
  | DQUOTE of (Support.info)
  | ARROW of (Support.info)
  | LEFTARROW of (Support.info)
  | DARROW of (Support.info)
  | DDARROW of (Support.info)
  | BANG of (Support.info)
  | HASH of (Support.info)
  | STAR of (Support.info)
  | TRIANGLE of (Support.info)
  | LPAREN of (Support.info)
  | RPAREN of (Support.info)
  | LSQUARE of (Support.info)
  | RSQUARE of (Support.info)
  | LCURLY of (Support.info)
  | RCURLY of (Support.info)
  | LCURLYBAR of (Support.info)
  | BARRCURLY of (Support.info)
  | LSQUAREBAR of (Support.info)
  | BARRSQUARE of (Support.info)
  | BARGT of (Support.info)
  | COLON of (Support.info)
  | COLONCOLON of (Support.info)
  | COLONEQ of (Support.info)
  | COLONHASH of (Support.info)
  | SEMI of (Support.info)
  | DSEMI of (Support.info)
  | EQ of (Support.info)
  | EQEQ of (Support.info)
  | GT of (Support.info)
  | LT of (Support.info)
  | DOT of (Support.info)
  | COMMA of (Support.info)
  | USCORE of (Support.info)
  | VBAR of (Support.info)
  | SLASH of (Support.info)
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

let rec addbinders tyT = function 
    | []                    -> tyT
    | (tyX,k) :: rest       -> TyAbs(tyX,k,addbinders tyT rest)

# 71 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* BOOL *);
  258 (* NAT *);
  259 (* LAMBDA *);
  260 (* IF *);
  261 (* THEN *);
  262 (* ELSE *);
  263 (* TRUE *);
  264 (* FALSE *);
  265 (* SUCC *);
  266 (* PRED *);
  267 (* ISZERO *);
  268 (* UCID *);
  269 (* LCID *);
  270 (* INTV *);
  271 (* FLOATV *);
  272 (* STRINGV *);
  273 (* APOSTROPHE *);
  274 (* DQUOTE *);
  275 (* ARROW *);
  276 (* LEFTARROW *);
  277 (* DARROW *);
  278 (* DDARROW *);
  279 (* BANG *);
  280 (* HASH *);
  281 (* STAR *);
  282 (* TRIANGLE *);
  283 (* LPAREN *);
  284 (* RPAREN *);
  285 (* LSQUARE *);
  286 (* RSQUARE *);
  287 (* LCURLY *);
  288 (* RCURLY *);
  289 (* LCURLYBAR *);
  290 (* BARRCURLY *);
  291 (* LSQUAREBAR *);
  292 (* BARRSQUARE *);
  293 (* BARGT *);
  294 (* COLON *);
  295 (* COLONCOLON *);
  296 (* COLONEQ *);
  297 (* COLONHASH *);
  298 (* SEMI *);
  299 (* DSEMI *);
  300 (* EQ *);
  301 (* EQEQ *);
  302 (* GT *);
  303 (* LT *);
  304 (* DOT *);
  305 (* COMMA *);
  306 (* USCORE *);
  307 (* VBAR *);
  308 (* SLASH *);
  309 (* NEWLINE *);
    0 (* EOF *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\003\000\003\000\003\000\003\000\001\000\001\000\
\004\000\004\000\004\000\006\000\007\000\007\000\010\000\010\000\
\011\000\011\000\009\000\012\000\012\000\013\000\013\000\008\000\
\008\000\008\000\014\000\014\000\016\000\016\000\015\000\015\000\
\015\000\015\000\005\000\005\000\005\000\017\000\017\000\017\000\
\017\000\017\000\018\000\018\000\018\000\018\000\018\000\000\000\
\000\000"

let yylen = "\002\000\
\000\000\002\000\001\000\003\000\002\000\002\000\001\000\003\000\
\001\000\002\000\002\000\002\000\002\000\003\000\000\000\003\000\
\000\000\002\000\001\000\001\000\003\000\001\000\003\000\001\000\
\005\000\001\000\003\000\001\000\002\000\001\000\001\000\003\000\
\001\000\001\000\001\000\006\000\006\000\001\000\002\000\002\000\
\002\000\002\000\003\000\001\000\001\000\001\000\001\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\001\000\000\000\000\000\000\000\045\000\046\000\000\000\
\000\000\000\000\000\000\000\000\047\000\000\000\007\000\048\000\
\000\000\009\000\000\000\038\000\000\000\000\000\044\000\000\000\
\039\000\040\000\041\000\000\000\000\000\011\000\000\000\000\000\
\010\000\000\000\000\000\042\000\000\000\003\000\000\000\000\000\
\000\000\000\000\022\000\000\000\013\000\019\000\000\000\000\000\
\033\000\034\000\000\000\031\000\000\000\012\000\024\000\000\000\
\000\000\043\000\008\000\000\000\005\000\006\000\000\000\000\000\
\018\000\016\000\000\000\000\000\014\000\000\000\000\000\000\000\
\029\000\004\000\000\000\000\000\023\000\021\000\000\000\032\000\
\027\000\036\000\037\000\000\000\025\000"

let yydgoto = "\003\000\
\016\000\021\000\037\000\017\000\018\000\033\000\030\000\054\000\
\045\000\031\000\042\000\046\000\047\000\055\000\056\000\057\000\
\019\000\020\000"

let yysindex = "\029\000\
\108\000\000\000\000\000\251\254\070\255\000\000\000\000\014\255\
\014\255\014\255\250\254\230\254\000\000\070\255\000\000\000\000\
\231\254\000\000\014\255\000\000\049\255\241\254\000\000\027\255\
\000\000\000\000\000\000\253\254\255\254\000\000\246\254\017\255\
\000\000\010\255\108\000\000\000\003\000\000\000\017\255\070\255\
\255\254\030\255\000\000\255\254\000\000\000\000\018\255\017\255\
\000\000\000\000\033\255\000\000\017\255\000\000\000\000\029\255\
\013\255\000\000\000\000\049\255\000\000\000\000\254\254\043\255\
\000\000\000\000\022\255\255\254\000\000\253\254\023\255\013\255\
\000\000\000\000\070\255\070\255\000\000\000\000\006\255\000\000\
\000\000\000\000\000\000\017\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\024\255\067\000\000\000\000\000\000\000\000\000\
\000\000\000\000\096\000\000\000\069\000\000\000\000\000\000\000\
\000\000\000\000\000\000\249\254\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\133\000\000\000\000\000\000\000\
\000\000\024\255\000\000\000\000\000\000\000\000\013\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\055\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\037\000\000\000\000\000\239\255\251\255\000\000\000\000\219\255\
\222\255\043\000\016\000\000\000\019\000\017\000\031\000\000\000\
\000\000\056\000"

let yytablesize = 416
let yytable = "\024\000\
\030\000\063\000\062\000\038\000\017\000\028\000\065\000\022\000\
\034\000\067\000\069\000\032\000\020\000\049\000\050\000\071\000\
\035\000\049\000\050\000\051\000\006\000\007\000\039\000\043\000\
\052\000\044\000\023\000\013\000\052\000\001\000\002\000\040\000\
\029\000\048\000\064\000\041\000\017\000\058\000\068\000\053\000\
\014\000\028\000\074\000\053\000\070\000\075\000\085\000\072\000\
\076\000\077\000\080\000\004\000\005\000\084\000\028\000\006\000\
\007\000\008\000\009\000\010\000\011\000\012\000\013\000\025\000\
\026\000\027\000\044\000\015\000\049\000\082\000\083\000\059\000\
\004\000\005\000\036\000\014\000\006\000\007\000\008\000\009\000\
\010\000\017\000\023\000\013\000\066\000\079\000\078\000\073\000\
\081\000\000\000\000\000\000\000\000\000\000\000\000\000\035\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\015\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
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
\000\000\030\000\030\000\030\000\030\000\000\000\000\000\030\000\
\030\000\030\000\030\000\030\000\030\000\030\000\030\000\020\000\
\020\000\000\000\000\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\030\000\030\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\
\020\000\000\000\030\000\030\000\060\000\061\000\000\000\000\000\
\030\000\000\000\000\000\000\000\000\000\000\000\020\000\020\000\
\020\000\028\000\028\000\000\000\020\000\028\000\028\000\028\000\
\028\000\028\000\000\000\028\000\028\000\044\000\044\000\000\000\
\000\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\000\000\028\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\044\000\000\000\000\000\
\028\000\028\000\035\000\035\000\035\000\035\000\028\000\000\000\
\035\000\035\000\035\000\035\000\044\000\044\000\004\000\005\000\
\000\000\000\000\006\000\007\000\008\000\009\000\010\000\011\000\
\012\000\013\000\000\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\014\000\002\000\
\002\000\035\000\035\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000"

let yycheck = "\005\000\
\000\000\039\000\000\000\021\000\012\001\012\001\041\000\013\001\
\014\000\044\000\048\000\038\001\000\000\001\001\002\001\053\000\
\042\001\001\001\002\001\003\001\007\001\008\001\038\001\025\001\
\012\001\027\001\013\001\014\001\012\001\001\000\002\000\005\001\
\039\001\044\001\040\000\039\001\044\001\028\001\021\001\027\001\
\027\001\012\001\060\000\027\001\012\001\048\001\084\000\019\001\
\006\001\028\001\028\001\003\001\004\001\048\001\000\000\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\008\000\
\009\000\010\000\000\000\044\001\000\000\075\000\076\000\035\000\
\003\001\004\001\019\000\027\001\007\001\008\001\009\001\010\001\
\011\001\048\001\013\001\014\001\042\000\070\000\068\000\057\000\
\072\000\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
\027\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
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
\255\255\001\001\002\001\003\001\004\001\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\003\001\
\004\001\255\255\255\255\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\027\001\028\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\027\001\
\028\001\255\255\042\001\043\001\042\001\043\001\255\255\255\255\
\048\001\255\255\255\255\255\255\255\255\255\255\042\001\043\001\
\044\001\003\001\004\001\255\255\048\001\007\001\008\001\009\001\
\010\001\011\001\255\255\013\001\014\001\003\001\004\001\255\255\
\255\255\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\255\255\028\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\027\001\255\255\255\255\
\042\001\043\001\003\001\004\001\005\001\006\001\048\001\255\255\
\009\001\010\001\011\001\012\001\042\001\043\001\003\001\004\001\
\255\255\255\255\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\255\255\028\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\027\001\003\001\
\004\001\042\001\043\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\027\001"

let yynames_const = "\
  "

let yynames_block = "\
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
  LEFTARROW\000\
  DARROW\000\
  DDARROW\000\
  BANG\000\
  HASH\000\
  STAR\000\
  TRIANGLE\000\
  LPAREN\000\
  RPAREN\000\
  LSQUARE\000\
  RSQUARE\000\
  LCURLY\000\
  RCURLY\000\
  LCURLYBAR\000\
  BARRCURLY\000\
  LSQUAREBAR\000\
  BARRSQUARE\000\
  BARGT\000\
  COLON\000\
  COLONCOLON\000\
  COLONEQ\000\
  COLONHASH\000\
  SEMI\000\
  DSEMI\000\
  EQ\000\
  EQEQ\000\
  GT\000\
  LT\000\
  DOT\000\
  COMMA\000\
  USCORE\000\
  VBAR\000\
  SLASH\000\
  NEWLINE\000\
  EOF\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                                    (   fun ctx ->  [],ctx  )
# 374 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 65 "parser.mly"
                                    (   fun ctx ->  let blks,ctx = _1 ctx in     
                                                    let blk,ctx  = _2 ctx in
                                                    (List.append blks blk,ctx) )
# 384 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Command) in
    Obj.repr(
# 70 "parser.mly"
                                    (   fun ctx ->  let cmd,ctx     = _1 ctx in [cmd],ctx )
# 391 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Command) in
    Obj.repr(
# 71 "parser.mly"
                                    (   fun ctx ->  let cmd,ctx     = _3 ctx in   
                                                    let cmds,ctx    = _1 ctx in 
                                                    List.append cmds [cmd], ctx )
# 402 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 74 "parser.mly"
                                    (   let cmds,ctx    = _1 emptyctx in 
                                        let _           = List.iter (pr_eval ctx) cmds in 
                                        fun ctx ->  let blk,ctx     = _1 ctx in blk,ctx )
# 412 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 77 "parser.mly"
                                    (   fun ctx ->  let blk,ctx     = _1 ctx in blk,ctx )
# 420 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 85 "parser.mly"
                                    (   fun ctx ->  [],ctx )
# 427 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    Obj.repr(
# 86 "parser.mly"
                                    (   fun ctx ->  let cmd,ctx'    = _1 ctx  in
                                                    let cmds,ctx''  = _3 ctx' in
                                                    cmd::cmds,ctx'' )
# 438 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 95 "parser.mly"
                                    (   fun ctx ->  let t = _1 ctx in 
                                                    pr"PARSING THE TERM : ";
                                                    pr_tm ctx t;pn();
                                                    pr"UNDER CONTEXT ";
                                                    pr_ctx ctx; 
                                                    Eval(tmInfo t,t),ctx       )
# 450 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 101 "parser.mly"
                                    (   fun ctx ->  pr"BINDING TERM : ";pn();
                                                    Bind(_1.i,_1.v,_2 ctx),addname ctx _1.v     )
# 459 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'TyBinder) in
    Obj.repr(
# 103 "parser.mly"
                                    (   fun ctx ->  pr"BINDING TYPE : ";pn();
                                                    Bind(_1.i,_1.v,_2 ctx),addname ctx _1.v     )
# 468 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Ty) in
    Obj.repr(
# 106 "parser.mly"
                                    (   fun ctx ->  BindTmVar(_2 ctx)                           )
# 476 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Kn) in
    Obj.repr(
# 108 "parser.mly"
                                    (   fun ctx ->  BindTyVar(Kn) )
# 484 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'TyAbbArgs) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Ty) in
    Obj.repr(
# 109 "parser.mly"
                                    (   fun ctx ->  let b,ctx' = _1 [] ctx in 
                                                pr_ctx ctx'; 
                                        BindTyAbb(addbinders (_3 ctx') b,None)                 )
# 495 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
                                    (   fun b ctx ->    b,ctx      )
# 501 "parser.ml"
               : 'TyAbbArgs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string  Support.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'BindKn) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'TyAbbArgs) in
    Obj.repr(
# 114 "parser.mly"
                                    (   fun b ctx -> let ctx'=addname ctx _1.v
                                                     in 
                                                        pr_ctx ctx'; 
                                                        _3 (b @ [(_1.v,_2 ctx)]) ctx'          )
# 513 "parser.ml"
               : 'TyAbbArgs))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
                                    (   fun ctx -> Kn                                           )
# 519 "parser.ml"
               : 'BindKn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Kn) in
    Obj.repr(
# 120 "parser.mly"
                                    (   _2                                                      )
# 527 "parser.ml"
               : 'BindKn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowKn) in
    Obj.repr(
# 124 "parser.mly"
                                    (   _1          )
# 534 "parser.ml"
               : 'Kn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AKn) in
    Obj.repr(
# 127 "parser.mly"
                                    (   _1                                                      )
# 541 "parser.ml"
               : 'ArrowKn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AKn) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AKn) in
    Obj.repr(
# 128 "parser.mly"
                                    (   fun ctx -> KnArr(_1 ctx, _3 ctx)                        )
# 550 "parser.ml"
               : 'ArrowKn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 130 "parser.mly"
                                    (   fun ctx -> Kn                                           )
# 557 "parser.ml"
               : 'AKn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Kn) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 131 "parser.mly"
                                    (   _2                                                      )
# 566 "parser.ml"
               : 'AKn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowTy) in
    Obj.repr(
# 135 "parser.mly"
                                    (   _1                                                      )
# 573 "parser.ml"
               : 'Ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string  Support.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'BindKn) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Ty) in
    Obj.repr(
# 136 "parser.mly"
                                    (   fun ctx -> let ctx' = addname ctx _2.v in 
                                                    TyAbs(_2.v, _3 ctx, _5 ctx')    )
# 585 "parser.ml"
               : 'Ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowTy) in
    Obj.repr(
# 138 "parser.mly"
                                    (   _1                                          )
# 592 "parser.ml"
               : 'Ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ATy) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowTy) in
    Obj.repr(
# 140 "parser.mly"
                                    (   fun ctx -> TyArr(_1 ctx, _3 ctx)                        )
# 601 "parser.ml"
               : 'ArrowTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTy) in
    Obj.repr(
# 141 "parser.mly"
                                    (   _1                                                      )
# 608 "parser.ml"
               : 'ArrowTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTy) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATy) in
    Obj.repr(
# 143 "parser.mly"
                                    (   fun ctx -> TyApp(_1 ctx,_2 ctx)                         )
# 616 "parser.ml"
               : 'AppTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATy) in
    Obj.repr(
# 144 "parser.mly"
                                    (   _1                                                      )
# 623 "parser.ml"
               : 'AppTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.withinfo) in
    Obj.repr(
# 146 "parser.mly"
                                    (   fun ctx ->  pr_ctx ctx;
                                                    let i = name2index _1.i ctx _1.v in 
                                                    pr"var_index i := "; pi i;pn();
                                                    TyVar(i,ctxlen ctx) )
# 633 "parser.ml"
               : 'ATy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 150 "parser.mly"
                                    (   _2                                                      )
# 642 "parser.ml"
               : 'ATy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 151 "parser.mly"
                                    (   fun ctx -> TyBool                                       )
# 649 "parser.ml"
               : 'ATy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 152 "parser.mly"
                                    (   fun ctx -> TyNat                                        )
# 656 "parser.ml"
               : 'ATy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 158 "parser.mly"
                                    (   _1 )
# 663 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 159 "parser.mly"
                                    (   fun ctx -> let ctx1=addname ctx _2.v in 
                                                   let t = TmAbs(_1,_2.v,_4 ctx,_6 ctx1) in 
                                                   t)
# 677 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 162 "parser.mly"
                                    (   fun ctx -> TmIf(_1, _2 ctx, _4 ctx, _6 ctx) )
# 689 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 164 "parser.mly"
                                    (   _1 )
# 696 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 165 "parser.mly"
                                    (   fun ctx -> TmSucc(_1, _2 ctx ) )
# 704 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 166 "parser.mly"
                                    (   fun ctx -> TmPred(_1, _2 ctx ) )
# 712 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 167 "parser.mly"
                                    (   fun ctx -> TmIsZero(_1, _2 ctx) )
# 720 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 168 "parser.mly"
                                    (   fun ctx -> let e1=_1 ctx in TmApp(tmInfo e1,e1,_2 ctx) )
# 728 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 170 "parser.mly"
                                    (   _2 )
# 737 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.withinfo) in
    Obj.repr(
# 171 "parser.mly"
                                    (   fun ctx -> TmVar(_1.i, name2index _1.i ctx _1.v, ctxlen ctx) )
# 744 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 172 "parser.mly"
                                    (   fun ctx -> TmTrue(_1) )
# 751 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 173 "parser.mly"
                                    (   fun ctx -> TmFalse(_1) )
# 758 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int     Support.withinfo) in
    Obj.repr(
# 174 "parser.mly"
                                    (   fun ctx -> let rec f = function
              0 -> TmZero(_1.i)
            | n -> TmSucc(_1.i, f (n-1))
          in f _1.v )
# 768 "parser.ml"
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
