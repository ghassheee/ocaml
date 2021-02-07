type token =
  | SHOWCONTEXT of (Support.info)
  | UNIV of (Support.info)
  | PI of (Support.info)
  | SIGMA of (Support.info)
  | LETREC of (Support.info)
  | FIX of (Support.info)
  | LET of (Support.info)
  | IN of (Support.info)
  | WHERE of (Support.info)
  | BOOL of (Support.info)
  | NAT of (Support.info)
  | SUCC of (Support.info)
  | PRED of (Support.info)
  | ZERO of (Support.info)
  | ISZERO of (Support.info)
  | LAMBDA of (Support.info)
  | IF of (Support.info)
  | THEN of (Support.info)
  | ELSE of (Support.info)
  | TRUE of (Support.info)
  | FALSE of (Support.info)
  | INTV of (int    Support.withinfo)
  | FLOATV of (float  Support.withinfo)
  | UCID of (string Support.withinfo)
  | LCID of (string Support.withinfo)
  | STRINGV of (string Support.withinfo)
  | APOSTROPHE of (Support.info)
  | ARROW of (Support.info)
  | DARROW of (Support.info)
  | DDARROW of (Support.info)
  | BANG of (Support.info)
  | DQUOTE of (Support.info)
  | LTBAR of (Support.info)
  | BARGT of (Support.info)
  | LCURLYBAR of (Support.info)
  | BARRCURLY of (Support.info)
  | BARRSQUARE of (Support.info)
  | LSQUAREBAR of (Support.info)
  | COMMA of (Support.info)
  | DOT of (Support.info)
  | EQ of (Support.info)
  | GT of (Support.info)
  | LT of (Support.info)
  | EQEQ of (Support.info)
  | EXISTS of (Support.info)
  | ALL of (Support.info)
  | HASH of (Support.info)
  | LCURLY of (Support.info)
  | RCURLY of (Support.info)
  | LPAREN of (Support.info)
  | RPAREN of (Support.info)
  | LSQUARE of (Support.info)
  | RSQUARE of (Support.info)
  | LEFTARROW of (Support.info)
  | COLON of (Support.info)
  | COLONCOLON of (Support.info)
  | COLONEQ of (Support.info)
  | COLONHASH of (Support.info)
  | SEMI of (Support.info)
  | DSEMI of (Support.info)
  | SLASH of (Support.info)
  | STAR of (Support.info)
  | TRIANGLE of (Support.info)
  | USCORE of (Support.info)
  | VBAR of (Support.info)
  | NEWLINE of (Support.info)
  | EOF of (Support.info)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Format
open Support
open Syntax
open Type
open Eval
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
  263 (* LET *);
  264 (* IN *);
  265 (* WHERE *);
  266 (* BOOL *);
  267 (* NAT *);
  268 (* SUCC *);
  269 (* PRED *);
  270 (* ZERO *);
  271 (* ISZERO *);
  272 (* LAMBDA *);
  273 (* IF *);
  274 (* THEN *);
  275 (* ELSE *);
  276 (* TRUE *);
  277 (* FALSE *);
  278 (* INTV *);
  279 (* FLOATV *);
  280 (* UCID *);
  281 (* LCID *);
  282 (* STRINGV *);
  283 (* APOSTROPHE *);
  284 (* ARROW *);
  285 (* DARROW *);
  286 (* DDARROW *);
  287 (* BANG *);
  288 (* DQUOTE *);
  289 (* LTBAR *);
  290 (* BARGT *);
  291 (* LCURLYBAR *);
  292 (* BARRCURLY *);
  293 (* BARRSQUARE *);
  294 (* LSQUAREBAR *);
  295 (* COMMA *);
  296 (* DOT *);
  297 (* EQ *);
  298 (* GT *);
  299 (* LT *);
  300 (* EQEQ *);
  301 (* EXISTS *);
  302 (* ALL *);
  303 (* HASH *);
  304 (* LCURLY *);
  305 (* RCURLY *);
  306 (* LPAREN *);
  307 (* RPAREN *);
  308 (* LSQUARE *);
  309 (* RSQUARE *);
  310 (* LEFTARROW *);
  311 (* COLON *);
  312 (* COLONCOLON *);
  313 (* COLONEQ *);
  314 (* COLONHASH *);
  315 (* SEMI *);
  316 (* DSEMI *);
  317 (* SLASH *);
  318 (* STAR *);
  319 (* TRIANGLE *);
  320 (* USCORE *);
  321 (* VBAR *);
  322 (* NEWLINE *);
    0 (* EOF *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\003\000\003\000\001\000\001\000\
\004\000\004\000\004\000\006\000\006\000\007\000\007\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\005\000\
\005\000\005\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\011\000\000\000\000\000"

let yylen = "\002\000\
\000\000\003\000\002\000\002\000\002\000\003\000\001\000\003\000\
\001\000\002\000\002\000\000\000\002\000\002\000\002\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\001\000\
\006\000\006\000\001\000\002\000\003\000\003\000\002\000\002\000\
\002\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\001\000\000\000\036\000\000\000\000\000\037\000\038\000\
\000\000\000\000\000\000\000\000\000\000\039\000\040\000\041\000\
\000\000\000\000\000\000\007\000\043\000\000\000\009\000\000\000\
\027\000\000\000\017\000\000\000\000\000\018\000\019\000\016\000\
\000\000\000\000\000\000\035\000\031\000\032\000\033\000\000\000\
\000\000\000\000\010\000\000\000\000\000\011\000\042\000\000\000\
\000\000\028\000\000\000\003\000\004\000\000\000\000\000\000\000\
\000\000\000\000\029\000\030\000\000\000\000\000\000\000\015\000\
\000\000\034\000\008\000\002\000\000\000\005\000\021\000\022\000\
\023\000\000\000\000\000\000\000\006\000\000\000\000\000\025\000\
\026\000"

let yydgoto = "\003\000\
\021\000\026\000\053\000\022\000\023\000\043\000\046\000\034\000\
\024\000\025\000\048\000"

let yysindex = "\008\000\
\001\000\000\000\000\000\000\000\127\255\127\255\000\000\000\000\
\221\255\221\255\221\255\233\254\204\255\000\000\000\000\000\000\
\222\254\220\254\204\255\000\000\000\000\210\254\000\000\221\255\
\000\000\013\255\000\000\127\255\127\255\000\000\000\000\000\000\
\127\255\153\255\153\255\000\000\000\000\000\000\000\000\219\254\
\014\255\127\255\000\000\204\255\127\255\000\000\000\000\001\255\
\001\000\000\000\235\254\000\000\000\000\208\254\153\255\153\255\
\232\254\127\255\000\000\000\000\127\255\204\255\025\255\000\000\
\025\255\000\000\000\000\000\000\180\255\000\000\000\000\000\000\
\000\000\025\255\236\254\038\255\000\000\204\255\204\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\237\254\034\255\000\000\000\000\000\000\000\000\000\000\094\255\
\000\000\058\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\244\254\000\000\
\247\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\065\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\011\000\000\000\248\255\233\255\243\255\000\000\000\000\090\000\
\000\000\093\000\000\000"

let yytablesize = 307
let yytable = "\041\000\
\020\000\040\000\054\000\058\000\044\000\047\000\042\000\058\000\
\001\000\002\000\069\000\070\000\049\000\051\000\004\000\005\000\
\006\000\061\000\045\000\078\000\059\000\060\000\007\000\008\000\
\009\000\010\000\073\000\011\000\012\000\013\000\064\000\062\000\
\014\000\015\000\016\000\035\000\017\000\018\000\068\000\012\000\
\012\000\071\000\072\000\035\000\035\000\054\000\013\000\013\000\
\076\000\014\000\014\000\066\000\058\000\035\000\035\000\035\000\
\079\000\044\000\035\000\067\000\077\000\000\000\019\000\000\000\
\080\000\081\000\020\000\020\000\020\000\000\000\000\000\000\000\
\052\000\000\000\020\000\020\000\020\000\020\000\000\000\020\000\
\020\000\020\000\000\000\035\000\020\000\020\000\020\000\000\000\
\000\000\020\000\000\000\000\000\035\000\035\000\000\000\035\000\
\024\000\024\000\000\000\000\000\000\000\037\000\038\000\039\000\
\020\000\024\000\024\000\000\000\024\000\024\000\024\000\024\000\
\024\000\000\000\020\000\020\000\050\000\055\000\056\000\000\000\
\000\000\024\000\057\000\020\000\020\000\000\000\000\000\000\000\
\027\000\028\000\029\000\063\000\000\000\024\000\065\000\000\000\
\030\000\031\000\000\000\000\000\000\000\000\000\000\000\000\000\
\024\000\000\000\000\000\074\000\000\000\000\000\075\000\032\000\
\024\000\024\000\004\000\005\000\006\000\000\000\000\000\000\000\
\000\000\000\000\007\000\008\000\009\000\010\000\000\000\011\000\
\012\000\013\000\000\000\000\000\014\000\015\000\016\000\000\000\
\033\000\036\000\000\000\000\000\058\000\004\000\005\000\006\000\
\000\000\000\000\000\000\000\000\000\000\007\000\008\000\009\000\
\010\000\000\000\011\000\012\000\013\000\000\000\000\000\014\000\
\015\000\016\000\019\000\017\000\018\000\004\000\005\000\006\000\
\000\000\000\000\000\000\000\000\000\000\007\000\008\000\009\000\
\010\000\000\000\011\000\012\000\013\000\000\000\004\000\014\000\
\015\000\016\000\000\000\000\000\036\000\019\000\007\000\008\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\015\000\016\000\000\000\000\000\036\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\000\000\000\000\000\
\000\000\000\000\004\000\005\000\006\000\000\000\000\000\000\000\
\000\000\000\000\007\000\008\000\009\000\010\000\019\000\011\000\
\012\000\013\000\000\000\000\000\014\000\015\000\016\000\000\000\
\017\000\018\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000"

let yycheck = "\013\000\
\000\000\025\001\026\000\028\001\041\001\019\000\041\001\028\001\
\001\000\002\000\059\001\060\001\059\001\001\001\002\001\003\001\
\004\001\055\001\055\001\040\001\034\000\035\000\010\001\011\001\
\012\001\013\001\051\001\015\001\016\001\017\001\044\000\018\001\
\020\001\021\001\022\001\002\001\024\001\025\001\060\001\059\001\
\060\001\055\000\056\000\010\001\011\001\069\000\059\001\060\001\
\062\000\059\001\060\001\051\001\028\001\020\001\021\001\022\001\
\019\001\000\000\025\001\049\000\069\000\255\255\050\001\255\255\
\078\000\079\000\002\001\003\001\004\001\255\255\255\255\255\255\
\060\001\255\255\010\001\011\001\012\001\013\001\255\255\015\001\
\016\001\017\001\255\255\050\001\020\001\021\001\022\001\255\255\
\255\255\025\001\255\255\255\255\059\001\060\001\255\255\006\000\
\003\001\004\001\255\255\255\255\255\255\009\000\010\000\011\000\
\040\001\012\001\013\001\255\255\015\001\016\001\017\001\018\001\
\019\001\255\255\050\001\051\001\024\000\028\000\029\000\255\255\
\255\255\028\001\033\000\059\001\060\001\255\255\255\255\255\255\
\002\001\003\001\004\001\042\000\255\255\040\001\045\000\255\255\
\010\001\011\001\255\255\255\255\255\255\255\255\255\255\255\255\
\051\001\255\255\255\255\058\000\255\255\255\255\061\000\025\001\
\059\001\060\001\002\001\003\001\004\001\255\255\255\255\255\255\
\255\255\255\255\010\001\011\001\012\001\013\001\255\255\015\001\
\016\001\017\001\255\255\255\255\020\001\021\001\022\001\255\255\
\050\001\025\001\255\255\255\255\028\001\002\001\003\001\004\001\
\255\255\255\255\255\255\255\255\255\255\010\001\011\001\012\001\
\013\001\255\255\015\001\016\001\017\001\255\255\255\255\020\001\
\021\001\022\001\050\001\024\001\025\001\002\001\003\001\004\001\
\255\255\255\255\255\255\255\255\255\255\010\001\011\001\012\001\
\013\001\255\255\015\001\016\001\017\001\255\255\002\001\020\001\
\021\001\022\001\255\255\255\255\025\001\050\001\010\001\011\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\020\001\021\001\022\001\255\255\255\255\025\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\050\001\255\255\255\255\
\255\255\255\255\002\001\003\001\004\001\255\255\255\255\255\255\
\255\255\255\255\010\001\011\001\012\001\013\001\050\001\015\001\
\016\001\017\001\255\255\255\255\020\001\021\001\022\001\255\255\
\024\001\025\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\050\001"

let yynames_const = "\
  "

let yynames_block = "\
  SHOWCONTEXT\000\
  UNIV\000\
  PI\000\
  SIGMA\000\
  LETREC\000\
  FIX\000\
  LET\000\
  IN\000\
  WHERE\000\
  BOOL\000\
  NAT\000\
  SUCC\000\
  PRED\000\
  ZERO\000\
  ISZERO\000\
  LAMBDA\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  INTV\000\
  FLOATV\000\
  UCID\000\
  LCID\000\
  STRINGV\000\
  APOSTROPHE\000\
  ARROW\000\
  DARROW\000\
  DDARROW\000\
  BANG\000\
  DQUOTE\000\
  LTBAR\000\
  BARGT\000\
  LCURLYBAR\000\
  BARRCURLY\000\
  BARRSQUARE\000\
  LSQUAREBAR\000\
  COMMA\000\
  DOT\000\
  EQ\000\
  GT\000\
  LT\000\
  EQEQ\000\
  EXISTS\000\
  ALL\000\
  HASH\000\
  LCURLY\000\
  RCURLY\000\
  LPAREN\000\
  RPAREN\000\
  LSQUARE\000\
  RSQUARE\000\
  LEFTARROW\000\
  COLON\000\
  COLONCOLON\000\
  COLONEQ\000\
  COLONHASH\000\
  SEMI\000\
  DSEMI\000\
  SLASH\000\
  STAR\000\
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
# 58 "parser.mly"
                                    ( fun ctx   ->  [],[]                                           )
# 378 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 59 "parser.mly"
                                    ( let _,ctx' =  _1 [] in pr_ctx ctx';
                                          fun ctx   ->  [],ctx'                                     )
# 388 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 61 "parser.mly"
                                    ( fun ctx   ->  [],ctx                                          )
# 396 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 62 "parser.mly"
                                    ( let _,ectx =  _1 [] in  
                                      let cmds,_ =  _2 ectx in 
                                      let ectx'  =  process_cmds ectx cmds in 
                                      fun ctx   ->  [],ectx'                                        )
# 407 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Cmd) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 67 "parser.mly"
                                    ( fun ctx   ->  let cmd,ctx'   = _1 ctx in [cmd],ctx'           )
# 415 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Cmd) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 68 "parser.mly"
                                    ( fun ctx   ->  let cmd,ctx'   = _1 ctx in 
                                                        let cmds,ctx'' = _3 ctx' in cmd::cmds,ctx'' )
# 425 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 75 "parser.mly"
                                    ( fun ctx   ->  [],ctx                                          )
# 432 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Cmd) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    Obj.repr(
# 76 "parser.mly"
                                    ( fun ctx   ->  let cmd,ctx  = _1 ctx in 
                                                        let cmds,ctx = _3 ctx in cmd::cmds,ctx      )
# 442 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 83 "parser.mly"
                                    ( fun ctx   ->  let t = _1 ctx in Eval(tmInfo t,t),ctx          )
# 449 "parser.ml"
               : 'Cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'TyBind) in
    Obj.repr(
# 84 "parser.mly"
                                    ( fun ctx   ->  Bind(_1.i,_1.v,_2 ctx),addname ctx _1.v         )
# 457 "parser.ml"
               : 'Cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Bind) in
    Obj.repr(
# 85 "parser.mly"
                                    ( fun ctx   ->  Bind(_1.i,_1.v,_2 ctx),addname ctx _1.v         )
# 465 "parser.ml"
               : 'Cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                                    ( fun ctx   ->  BindTyVar                                       )
# 471 "parser.ml"
               : 'TyBind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Ty) in
    Obj.repr(
# 88 "parser.mly"
                                    ( fun ctx   ->  BindTyAbb(_2 ctx)                               )
# 479 "parser.ml"
               : 'TyBind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Ty) in
    Obj.repr(
# 90 "parser.mly"
                                    ( fun ctx   ->  BindVar(_2 ctx)                                 )
# 487 "parser.ml"
               : 'Bind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 91 "parser.mly"
                                    ( fun ctx   ->  BindAbb(_2 ctx,None)                            )
# 495 "parser.ml"
               : 'Bind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.withinfo) in
    Obj.repr(
# 97 "parser.mly"
                                    ( fun ctx   ->  Var(_1.i,name2index _1.i ctx _1.v,ctxlen ctx)   )
# 502 "parser.ml"
               : 'Ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 98 "parser.mly"
                                    ( fun ctx   ->  Univ(_1,0)                                      )
# 509 "parser.ml"
               : 'Ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 99 "parser.mly"
                                    ( fun ctx   ->  Bool(_1)                                        )
# 516 "parser.ml"
               : 'Ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 100 "parser.mly"
                                    ( fun ctx   ->  Nat(_1)                                         )
# 523 "parser.ml"
               : 'Ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Ty) in
    Obj.repr(
# 101 "parser.mly"
                                    ( fun ctx   ->  Ap(_2,Ap(_2,Pi(_2),_1 ctx),Lam(_2,"_",_1 ctx,_3 ctx)))
# 532 "parser.ml"
               : 'Ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 102 "parser.mly"
                                    ( fun ctx   ->  Ap(_1,Ap(_1,Pi(_1),_2 ctx),_3 ctx)              )
# 541 "parser.ml"
               : 'Ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 103 "parser.mly"
                                    ( fun ctx   ->  Ap(_1,Ap(_1,Sgm(_1),_2 ctx),_3 ctx)             )
# 550 "parser.ml"
               : 'Ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 104 "parser.mly"
                                    ( _2 )
# 559 "parser.ml"
               : 'Ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ApTm) in
    Obj.repr(
# 110 "parser.mly"
                                    ( _1                                                            )
# 566 "parser.ml"
               : 'Tm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 111 "parser.mly"
                                    ( fun ctx   ->  Lam(_1,_2.v,_4 ctx,_6(addname ctx _2.v))        )
# 578 "parser.ml"
               : 'Tm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Tm) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Tm) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 112 "parser.mly"
                                    ( fun ctx   ->  If(_1,_2 ctx,_4 ctx,_6 ctx)                     )
# 590 "parser.ml"
               : 'Tm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATm) in
    Obj.repr(
# 114 "parser.mly"
                                    ( _1                                                            )
# 597 "parser.ml"
               : 'ApTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ApTm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATm) in
    Obj.repr(
# 115 "parser.mly"
                                    ( fun ctx   ->  Ap(tmInfo(_1 ctx),_1 ctx,_2 ctx)                )
# 605 "parser.ml"
               : 'ApTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 116 "parser.mly"
                                    ( fun ctx   ->  Ap(_1,Ap(_1,Pi(_1),_2 ctx),_3 ctx))
# 614 "parser.ml"
               : 'ApTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 117 "parser.mly"
                                    ( fun ctx   ->  Ap(_1,Ap(_1,Sgm(_1),_2 ctx),_3 ctx))
# 623 "parser.ml"
               : 'ApTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATm) in
    Obj.repr(
# 118 "parser.mly"
                                    ( fun ctx   ->  Succ(_1, _2 ctx )                               )
# 631 "parser.ml"
               : 'ApTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATm) in
    Obj.repr(
# 119 "parser.mly"
                                    ( fun ctx   ->  Pred(_1, _2 ctx )                               )
# 639 "parser.ml"
               : 'ApTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATm) in
    Obj.repr(
# 120 "parser.mly"
                                    ( fun ctx   ->  IsZero(_1, _2 ctx)                              )
# 647 "parser.ml"
               : 'ApTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'TmSeq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 122 "parser.mly"
                                    ( _2                                                            )
# 656 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.withinfo) in
    Obj.repr(
# 123 "parser.mly"
                                    ( fun ctx   ->  Var(_1.i,name2index _1.i ctx _1.v,ctxlen ctx)   )
# 663 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 124 "parser.mly"
                                    ( fun ctx   ->  Univ(_1,0)                                      )
# 670 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 125 "parser.mly"
                                    ( fun ctx   ->  Bool(_1)                                        )
# 677 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 126 "parser.mly"
                                    ( fun ctx   ->  Nat(_1)                                         )
# 684 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 127 "parser.mly"
                                    ( fun ctx   ->  True(_1)                                        )
# 691 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 128 "parser.mly"
                                    ( fun ctx   ->  False(_1)                                       )
# 698 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int    Support.withinfo) in
    Obj.repr(
# 129 "parser.mly"
                                    ( fun ctx   ->  let rec f = function
                                                            | 0 -> Zero(_1.i)
                                                            | n -> Succ(_1.i,f(n-1))in f _1.v       )
# 707 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 133 "parser.mly"
                                    ( _1                                                            )
# 714 "parser.ml"
               : 'TmSeq))
(* Entry compiler *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry repl *)
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
let compiler (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.context -> (Syntax.command list * Syntax.context))
let repl (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Syntax.context -> (Syntax.command list * Syntax.context))
