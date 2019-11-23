type token =
  | AS of (Support.Error.info)
  | UNIT of (Support.Error.info)
  | UNITTYPE of (Support.Error.info)
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
# 78 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* AS *);
  258 (* UNIT *);
  259 (* UNITTYPE *);
  260 (* WHERE *);
  261 (* IN *);
  262 (* LET *);
  263 (* BOOL *);
  264 (* NAT *);
  265 (* SUCC *);
  266 (* PRED *);
  267 (* ISZERO *);
  268 (* LAMBDA *);
  269 (* IF *);
  270 (* THEN *);
  271 (* ELSE *);
  272 (* TRUE *);
  273 (* FALSE *);
  274 (* UCID *);
  275 (* LCID *);
  276 (* INTV *);
  277 (* FLOATV *);
  278 (* STRINGV *);
  279 (* APOSTROPHE *);
  280 (* DQUOTE *);
  281 (* ARROW *);
  282 (* BANG *);
  283 (* BARGT *);
  284 (* BARRCURLY *);
  285 (* BARRSQUARE *);
  286 (* COLON *);
  287 (* COLONCOLON *);
  288 (* COLONEQ *);
  289 (* COLONHASH *);
  290 (* COMMA *);
  291 (* DARROW *);
  292 (* DDARROW *);
  293 (* DOT *);
    0 (* EOF *);
  294 (* EQ *);
  295 (* EQEQ *);
  296 (* EXISTS *);
  297 (* GT *);
  298 (* HASH *);
  299 (* LCURLY *);
  300 (* LCURLYBAR *);
  301 (* LEFTARROW *);
  302 (* LPAREN *);
  303 (* LSQUARE *);
  304 (* LSQUAREBAR *);
  305 (* LT *);
  306 (* RCURLY *);
  307 (* RPAREN *);
  308 (* RSQUARE *);
  309 (* SEMI *);
  310 (* SLASH *);
  311 (* STAR *);
  312 (* TRIANGLE *);
  313 (* USCORE *);
  314 (* VBAR *);
  315 (* NEWLINE *);
  316 (* DOUBLESEMI *);
  317 (* HOGE *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\003\000\003\000\003\000\003\000\001\000\001\000\
\004\000\004\000\006\000\007\000\009\000\009\000\009\000\009\000\
\009\000\010\000\010\000\011\000\011\000\012\000\012\000\008\000\
\008\000\005\000\005\000\005\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\014\000\014\000\014\000\014\000\014\000\
\015\000\015\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\017\000\017\000\018\000\018\000\019\000\019\000\000\000\
\000\000"

let yylen = "\002\000\
\000\000\002\000\001\000\003\000\002\000\002\000\001\000\003\000\
\001\000\002\000\002\000\001\000\003\000\001\000\001\000\001\000\
\003\000\000\000\001\000\001\000\003\000\003\000\001\000\003\000\
\001\000\005\000\005\000\001\000\001\000\003\000\003\000\006\000\
\006\000\006\000\006\000\001\000\002\000\002\000\002\000\002\000\
\003\000\001\000\003\000\003\000\001\000\001\000\001\000\001\000\
\001\000\000\000\001\000\001\000\003\000\003\000\001\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\001\000\000\000\046\000\000\000\000\000\000\000\000\000\
\000\000\000\000\047\000\048\000\000\000\049\000\007\000\000\000\
\000\000\056\000\000\000\000\000\000\000\000\000\036\000\000\000\
\000\000\000\000\000\000\045\000\037\000\038\000\039\000\000\000\
\000\000\000\000\010\000\000\000\000\000\000\000\051\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\040\000\000\000\
\000\000\003\000\000\000\000\000\000\000\000\000\016\000\014\000\
\015\000\000\000\000\000\011\000\012\000\000\000\000\000\044\000\
\000\000\043\000\008\000\000\000\000\000\000\000\031\000\041\000\
\006\000\000\000\005\000\000\000\000\000\000\000\000\000\000\000\
\023\000\000\000\019\000\000\000\000\000\000\000\000\000\053\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\017\000\000\000\013\000\024\000\000\000\000\000\000\000\000\000\
\000\000\000\000\022\000\021\000"

let yydgoto = "\003\000\
\018\000\025\000\049\000\019\000\020\000\035\000\081\000\061\000\
\062\000\082\000\083\000\084\000\021\000\022\000\023\000\024\000\
\038\000\039\000\040\000"

let yysindex = "\059\000\
\229\000\000\000\000\000\000\000\248\254\109\255\109\255\109\255\
\255\254\056\255\000\000\000\000\249\254\000\000\000\000\084\255\
\056\255\000\000\208\254\250\254\041\255\109\255\000\000\036\255\
\103\255\002\255\008\255\000\000\000\000\000\000\000\000\027\255\
\104\255\031\255\000\000\032\255\051\255\037\255\000\000\055\255\
\236\254\229\000\072\255\079\255\056\255\089\255\000\000\031\255\
\022\000\000\000\056\255\056\255\031\255\056\255\000\000\000\000\
\000\000\005\255\031\255\000\000\000\000\085\255\056\255\000\000\
\084\255\000\000\000\000\083\255\094\255\051\255\000\000\000\000\
\000\000\103\255\000\000\087\255\101\255\096\255\245\254\110\255\
\000\000\092\255\000\000\105\255\093\255\031\255\051\255\000\000\
\056\255\056\255\000\000\056\255\056\255\056\255\056\255\031\255\
\000\000\005\255\000\000\000\000\051\255\051\255\051\255\051\255\
\051\255\051\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\092\001\000\000\000\000\095\255\
\000\000\000\000\000\000\253\001\183\001\137\001\000\000\001\000\
\143\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\255\231\254\000\000\000\000\097\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\043\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\098\255\000\000\000\000\000\000\054\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\107\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\100\255\000\000\000\000\242\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\199\001\237\001\160\000\213\000\
\019\001\072\001\000\000\000\000"

let yygindex = "\000\000\
\109\000\000\000\000\000\234\255\000\000\000\000\224\255\067\000\
\000\000\000\000\056\000\000\000\246\255\000\000\134\000\129\000\
\000\000\092\000\000\000"

let yytablesize = 857
let yytable = "\033\000\
\042\000\060\000\050\000\095\000\042\000\037\000\041\000\055\000\
\055\000\045\000\026\000\056\000\057\000\045\000\045\000\072\000\
\046\000\032\000\045\000\054\000\078\000\073\000\034\000\080\000\
\055\000\046\000\085\000\043\000\045\000\045\000\066\000\045\000\
\045\000\055\000\070\000\054\000\048\000\056\000\057\000\051\000\
\076\000\077\000\045\000\079\000\044\000\052\000\045\000\058\000\
\027\000\045\000\059\000\091\000\087\000\025\000\037\000\045\000\
\053\000\004\000\045\000\001\000\002\000\005\000\045\000\107\000\
\006\000\007\000\008\000\009\000\010\000\063\000\045\000\011\000\
\012\000\058\000\028\000\014\000\059\000\046\000\101\000\102\000\
\045\000\103\000\104\000\105\000\106\000\004\000\064\000\046\000\
\065\000\005\000\068\000\092\000\006\000\007\000\008\000\009\000\
\010\000\069\000\016\000\011\000\012\000\017\000\036\000\014\000\
\004\000\093\000\030\000\071\000\005\000\086\000\004\000\006\000\
\007\000\008\000\009\000\010\000\045\000\054\000\011\000\012\000\
\089\000\013\000\014\000\046\000\011\000\012\000\016\000\028\000\
\014\000\017\000\045\000\090\000\094\000\045\000\029\000\030\000\
\031\000\046\000\098\000\096\000\046\000\097\000\057\000\099\000\
\050\000\016\000\052\000\018\000\017\000\020\000\067\000\016\000\
\100\000\108\000\017\000\047\000\088\000\000\000\000\000\032\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\033\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\042\000\000\000\042\000\042\000\042\000\000\000\
\000\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\034\000\042\000\042\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\042\000\000\000\
\000\000\000\000\042\000\000\000\000\000\042\000\000\000\000\000\
\000\000\000\000\000\000\042\000\000\000\000\000\042\000\000\000\
\000\000\000\000\042\000\042\000\000\000\042\000\000\000\025\000\
\000\000\025\000\025\000\025\000\000\000\042\000\025\000\025\000\
\025\000\025\000\025\000\025\000\025\000\025\000\025\000\035\000\
\025\000\025\000\074\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\075\000\025\000\000\000\000\000\000\000\025\000\
\000\000\000\000\025\000\045\000\000\000\000\000\000\000\000\000\
\025\000\000\000\000\000\025\000\000\000\000\000\000\000\025\000\
\025\000\000\000\025\000\000\000\030\000\000\000\030\000\030\000\
\030\000\000\000\025\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\030\000\030\000\000\000\030\000\030\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\029\000\000\000\000\000\000\000\030\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\030\000\000\000\000\000\
\030\000\000\000\000\000\000\000\030\000\030\000\000\000\030\000\
\000\000\032\000\000\000\032\000\032\000\032\000\000\000\030\000\
\032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
\032\000\000\000\032\000\032\000\000\000\000\000\028\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\032\000\000\000\000\000\000\000\000\000\026\000\000\000\
\000\000\000\000\032\000\000\000\000\000\032\000\000\000\000\000\
\000\000\032\000\032\000\000\000\032\000\000\000\033\000\000\000\
\033\000\033\000\033\000\000\000\032\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\004\000\033\000\
\033\000\000\000\005\000\000\000\027\000\006\000\007\000\008\000\
\009\000\010\000\000\000\000\000\011\000\012\000\033\000\013\000\
\014\000\000\000\000\000\000\000\009\000\000\000\000\000\033\000\
\000\000\000\000\033\000\000\000\000\000\000\000\033\000\033\000\
\000\000\033\000\000\000\000\000\000\000\000\000\000\000\016\000\
\000\000\033\000\017\000\000\000\034\000\000\000\034\000\034\000\
\034\000\000\000\000\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\034\000\034\000\000\000\034\000\034\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\034\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\000\000\000\000\000\
\034\000\000\000\000\000\000\000\034\000\034\000\000\000\034\000\
\000\000\035\000\000\000\035\000\035\000\035\000\000\000\034\000\
\035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
\035\000\000\000\035\000\035\000\045\000\045\000\000\000\045\000\
\000\000\045\000\000\000\000\000\045\000\045\000\045\000\045\000\
\045\000\035\000\000\000\045\000\045\000\000\000\045\000\045\000\
\000\000\000\000\035\000\000\000\000\000\035\000\000\000\000\000\
\000\000\035\000\035\000\000\000\035\000\045\000\000\000\000\000\
\045\000\000\000\000\000\000\000\035\000\000\000\045\000\000\000\
\000\000\045\000\000\000\000\000\029\000\029\000\029\000\000\000\
\045\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
\045\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\029\000\000\000\
\000\000\000\000\029\000\000\000\000\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\028\000\000\000\029\000\029\000\028\000\029\000\000\000\028\000\
\028\000\028\000\028\000\028\000\000\000\029\000\028\000\028\000\
\026\000\028\000\028\000\000\000\026\000\000\000\000\000\026\000\
\026\000\026\000\026\000\026\000\000\000\000\000\026\000\026\000\
\028\000\026\000\026\000\000\000\000\000\000\000\000\000\000\000\
\000\000\028\000\000\000\000\000\028\000\000\000\000\000\000\000\
\026\000\000\000\000\000\028\000\000\000\000\000\027\000\000\000\
\000\000\026\000\027\000\028\000\026\000\027\000\027\000\027\000\
\027\000\027\000\000\000\026\000\027\000\027\000\009\000\027\000\
\027\000\000\000\009\000\026\000\000\000\009\000\009\000\009\000\
\009\000\009\000\000\000\000\000\009\000\009\000\027\000\009\000\
\009\000\000\000\000\000\000\000\000\000\000\000\000\000\027\000\
\000\000\000\000\027\000\000\000\000\000\000\000\000\000\000\000\
\000\000\027\000\000\000\000\000\000\000\000\000\000\000\009\000\
\000\000\027\000\009\000\000\000\002\000\000\000\000\000\000\000\
\002\000\009\000\000\000\002\000\002\000\002\000\002\000\002\000\
\000\000\009\000\002\000\002\000\000\000\002\000\002\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\
\002\000"

let yycheck = "\010\000\
\000\000\034\000\025\000\015\001\053\001\016\000\017\000\003\001\
\034\001\030\001\019\001\007\001\008\001\001\001\002\001\048\000\
\037\001\019\001\030\001\034\001\053\000\000\000\030\001\019\001\
\050\001\037\001\059\000\034\001\016\001\017\001\051\001\019\001\
\020\001\003\001\045\000\050\001\001\001\007\001\008\001\038\001\
\051\000\052\000\030\001\054\000\004\001\038\001\034\001\043\001\
\057\001\037\001\046\001\074\000\063\000\000\000\065\000\043\001\
\030\001\002\001\046\001\001\000\002\000\006\001\050\001\096\000\
\009\001\010\001\011\001\012\001\013\001\038\001\030\001\016\001\
\017\001\043\001\019\001\020\001\046\001\037\001\089\000\090\000\
\030\001\092\000\093\000\094\000\095\000\002\001\050\001\037\001\
\034\001\006\001\019\001\005\001\009\001\010\001\011\001\012\001\
\013\001\019\001\043\001\016\001\017\001\046\001\019\001\020\001\
\002\001\005\001\000\000\019\001\006\001\025\001\002\001\009\001\
\010\001\011\001\012\001\013\001\030\001\014\001\016\001\017\001\
\038\001\019\001\020\001\037\001\016\001\017\001\043\001\019\001\
\020\001\046\001\030\001\038\001\037\001\030\001\006\000\007\000\
\008\000\037\001\034\001\030\001\037\001\050\001\000\000\051\001\
\050\001\043\001\050\001\050\001\046\001\050\001\042\000\043\001\
\086\000\098\000\046\001\022\000\065\000\255\255\255\255\000\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\004\001\005\001\006\001\255\255\
\255\255\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\000\000\019\001\020\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\030\001\255\255\
\255\255\255\255\034\001\255\255\255\255\037\001\255\255\255\255\
\255\255\255\255\255\255\043\001\255\255\255\255\046\001\255\255\
\255\255\255\255\050\001\051\001\255\255\053\001\255\255\002\001\
\255\255\004\001\005\001\006\001\255\255\061\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\000\000\
\019\001\020\001\053\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\061\001\030\001\255\255\255\255\255\255\034\001\
\255\255\255\255\037\001\000\000\255\255\255\255\255\255\255\255\
\043\001\255\255\255\255\046\001\255\255\255\255\255\255\050\001\
\051\001\255\255\053\001\255\255\002\001\255\255\004\001\005\001\
\006\001\255\255\061\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\255\255\019\001\020\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\034\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\043\001\255\255\255\255\
\046\001\255\255\255\255\255\255\050\001\051\001\255\255\053\001\
\255\255\002\001\255\255\004\001\005\001\006\001\255\255\061\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\255\255\019\001\020\001\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\034\001\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\043\001\255\255\255\255\046\001\255\255\255\255\
\255\255\050\001\051\001\255\255\053\001\255\255\002\001\255\255\
\004\001\005\001\006\001\255\255\061\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\002\001\019\001\
\020\001\255\255\006\001\255\255\000\000\009\001\010\001\011\001\
\012\001\013\001\255\255\255\255\016\001\017\001\034\001\019\001\
\020\001\255\255\255\255\255\255\000\000\255\255\255\255\043\001\
\255\255\255\255\046\001\255\255\255\255\255\255\050\001\051\001\
\255\255\053\001\255\255\255\255\255\255\255\255\255\255\043\001\
\255\255\061\001\046\001\255\255\002\001\255\255\004\001\005\001\
\006\001\255\255\255\255\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\255\255\019\001\020\001\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\034\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\043\001\255\255\255\255\
\046\001\255\255\255\255\255\255\050\001\051\001\255\255\053\001\
\255\255\002\001\255\255\004\001\005\001\006\001\255\255\061\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\255\255\019\001\020\001\001\001\002\001\255\255\004\001\
\255\255\006\001\255\255\255\255\009\001\010\001\011\001\012\001\
\013\001\034\001\255\255\016\001\017\001\255\255\019\001\020\001\
\255\255\255\255\043\001\255\255\255\255\046\001\255\255\255\255\
\255\255\050\001\051\001\255\255\053\001\034\001\255\255\255\255\
\037\001\255\255\255\255\255\255\061\001\255\255\043\001\255\255\
\255\255\046\001\255\255\255\255\004\001\005\001\006\001\255\255\
\053\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\061\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\030\001\255\255\
\255\255\255\255\034\001\255\255\255\255\037\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\002\001\255\255\050\001\051\001\006\001\053\001\255\255\009\001\
\010\001\011\001\012\001\013\001\255\255\061\001\016\001\017\001\
\002\001\019\001\020\001\255\255\006\001\255\255\255\255\009\001\
\010\001\011\001\012\001\013\001\255\255\255\255\016\001\017\001\
\034\001\019\001\020\001\255\255\255\255\255\255\255\255\255\255\
\255\255\043\001\255\255\255\255\046\001\255\255\255\255\255\255\
\034\001\255\255\255\255\053\001\255\255\255\255\002\001\255\255\
\255\255\043\001\006\001\061\001\046\001\009\001\010\001\011\001\
\012\001\013\001\255\255\053\001\016\001\017\001\002\001\019\001\
\020\001\255\255\006\001\061\001\255\255\009\001\010\001\011\001\
\012\001\013\001\255\255\255\255\016\001\017\001\034\001\019\001\
\020\001\255\255\255\255\255\255\255\255\255\255\255\255\043\001\
\255\255\255\255\046\001\255\255\255\255\255\255\255\255\255\255\
\255\255\053\001\255\255\255\255\255\255\255\255\255\255\043\001\
\255\255\061\001\046\001\255\255\002\001\255\255\255\255\255\255\
\006\001\053\001\255\255\009\001\010\001\011\001\012\001\013\001\
\255\255\061\001\016\001\017\001\255\255\019\001\020\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\043\001\255\255\255\255\
\046\001"

let yynames_const = "\
  "

let yynames_block = "\
  AS\000\
  UNIT\000\
  UNITTYPE\000\
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
  HOGE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
                                    ( fun ctx   -> [],ctx  )
# 520 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 104 "parser.mly"
                                    ( fun ctx   -> 
                                        let blks,ctx    = _1 ctx in 
                                        let blk,ctx     = _2 ctx in 
                                        (List.append blks blk, ctx) )
# 531 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Command) in
    Obj.repr(
# 109 "parser.mly"
                                    ( fun ctx   -> let cmd,ctx = _1 ctx in [cmd],ctx )
# 538 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Command) in
    Obj.repr(
# 110 "parser.mly"
                                    ( fun ctx   -> 
                                        let cmd,ctx = _3 ctx in 
                                        let cmds,ctx  = _1 ctx in 
                                        List.append cmds [cmd], ctx )
# 550 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 114 "parser.mly"
                                    ( let cmds,ctx = _1 emptycontext in let _ = List.iter (print_eval ctx) cmds in 
                                      fun ctx   -> let blk,ctx = _1 ctx in blk,ctx )
# 559 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 116 "parser.mly"
                                    ( fun ctx   -> let blk,ctx = _1 ctx in blk,ctx )
# 567 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 121 "parser.mly"
                                    ( fun ctx   -> [],ctx )
# 574 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    Obj.repr(
# 122 "parser.mly"
                                    ( fun ctx   -> 
          let cmd,ctx   = _1 ctx in 
          let cmds,ctx  = _3 ctx in 
          cmd::cmds,ctx )
# 586 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'TermWrap) in
    Obj.repr(
# 130 "parser.mly"
                                        ( fun ctx   -> let t = _1 ctx in Eval(tmInfo t,t),ctx )
# 593 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 131 "parser.mly"
                                        ( fun ctx   -> ((Bind(_1.i,_1.v,_2 ctx)), addname ctx _1.v) )
# 601 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 133 "parser.mly"
                                        ( fun ctx   -> VarBind(_2 ctx) )
# 609 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 136 "parser.mly"
                                        ( _1 )
# 616 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 138 "parser.mly"
                                        ( _2 )
# 625 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 139 "parser.mly"
                                        ( fun ctx   -> TyBool )
# 632 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 140 "parser.mly"
                                        ( fun ctx   -> TyNat  )
# 639 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 141 "parser.mly"
                                        ( fun ctx   -> TyUnit )
# 646 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'TyFields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 142 "parser.mly"
                                        ( fun ctx   -> TyRecord(_2 ctx 1) )
# 655 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "parser.mly"
                                        ( fun ctx   -> fun i -> [] )
# 661 "parser.ml"
               : 'TyFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NETyFields) in
    Obj.repr(
# 145 "parser.mly"
                                        ( _1 )
# 668 "parser.ml"
               : 'TyFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'TyField) in
    Obj.repr(
# 147 "parser.mly"
                                        ( fun ctx   -> fun i -> [_1 ctx i] )
# 675 "parser.ml"
               : 'NETyFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'TyField) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NETyFields) in
    Obj.repr(
# 148 "parser.mly"
                                        ( fun ctx   -> fun i -> (_1 ctx i)::(_3 ctx (i+1)) )
# 684 "parser.ml"
               : 'NETyFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 150 "parser.mly"
                                        ( fun ctx   -> fun i -> (_1.v, _3 ctx) )
# 693 "parser.ml"
               : 'TyField))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 151 "parser.mly"
                                        ( fun ctx   -> fun i -> (string_of_int i, _1 ctx) )
# 700 "parser.ml"
               : 'TyField))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 154 "parser.mly"
                                        ( fun ctx   -> TyArr(_1 ctx, _3 ctx) )
# 709 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 155 "parser.mly"
                                        ( _1 )
# 716 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'TermWrap) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 158 "parser.mly"
                                        ( fun ctx   -> TmLet(_2, _3.v, _5 ctx, _1 (addname ctx _3.v)) )
# 727 "parser.ml"
               : 'TermWrap))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 159 "parser.mly"
                                        ( fun ctx   -> TmLet(_2, _3.v, _5 ctx, _1 (addname ctx _3.v)) )
# 738 "parser.ml"
               : 'TermWrap))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 160 "parser.mly"
                                        ( _1 )
# 745 "parser.ml"
               : 'TermWrap))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 162 "parser.mly"
                                        ( _1 )
# 752 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 163 "parser.mly"
                                        ( fun ctx   -> TmLet(_2, "_", _3 ctx, _1 ctx) )
# 761 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 164 "parser.mly"
                                        ( fun ctx   -> TmProj(_2, _1 ctx, _3.v) )
# 770 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 165 "parser.mly"
                                        ( fun ctx -> TmLet(_1,_2.v,_4 ctx,_6(addname ctx _2.v)) )
# 782 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 166 "parser.mly"
                                        ( fun ctx -> TmLet(_1, "_", _4 ctx, _6 ctx) )
# 794 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 167 "parser.mly"
                                        ( fun ctx -> TmAbs(_1,_2.v,_4 ctx,_6 (addname ctx _2.v)))
# 806 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 168 "parser.mly"
                                        ( fun ctx   -> TmIf(_1, _2 ctx, _4 ctx, _6 ctx) )
# 818 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AscribeTerm) in
    Obj.repr(
# 170 "parser.mly"
                                        ( _1 )
# 825 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 171 "parser.mly"
                                        ( fun ctx   -> TmSucc(_1, _2 ctx ) )
# 833 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 172 "parser.mly"
                                        ( fun ctx   -> TmPred(_1, _2 ctx ) )
# 841 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 173 "parser.mly"
                                        ( fun ctx   -> TmIsZero(_1, _2 ctx) )
# 849 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AscribeTerm) in
    Obj.repr(
# 174 "parser.mly"
                                        ( fun ctx   -> let e1=_1 ctx in TmApp(tmInfo e1,e1,_2 ctx) )
# 857 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ATerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 176 "parser.mly"
                                        ( fun ctx   -> TmAscribe(_2, _1 ctx, _3 ctx) )
# 866 "parser.ml"
               : 'AscribeTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 177 "parser.mly"
                                        ( _1 )
# 873 "parser.ml"
               : 'AscribeTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 179 "parser.mly"
                                        ( _2 )
# 882 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 180 "parser.mly"
                                        ( fun ctx   -> TmRecord(_1, _2 ctx 1))
# 891 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 181 "parser.mly"
                                        ( fun ctx   -> TmVar(_1.i, name2index _1.i ctx _1.v, ctxlen ctx) )
# 898 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 182 "parser.mly"
                                        ( fun ctx   -> TmUnit(_1) )
# 905 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 183 "parser.mly"
                                        ( fun ctx   -> TmTrue(_1) )
# 912 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 184 "parser.mly"
                                        ( fun ctx   -> TmFalse(_1) )
# 919 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int     Support.Error.withinfo) in
    Obj.repr(
# 185 "parser.mly"
                                        ( fun ctx   ->  let rec f = function
                                                        | 0 -> TmZero(_1.i)
                                                        | n -> TmSucc(_1.i, f(n-1))
                                                        in f _1.v )
# 929 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 190 "parser.mly"
                                        ( fun ctx   -> fun i -> [] )
# 935 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 191 "parser.mly"
                                        ( _1 )
# 942 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Field) in
    Obj.repr(
# 194 "parser.mly"
                                        ( fun ctx   -> fun i -> [ _1 ctx i ] )
# 949 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Field) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 195 "parser.mly"
                                        ( fun ctx   -> fun i -> (_1 ctx i)::(_3 ctx(i+1)) )
# 958 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 198 "parser.mly"
                                        ( fun ctx   -> fun i -> (_1.v, _3 ctx) )
# 967 "parser.ml"
               : 'Field))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 199 "parser.mly"
                                        ( fun ctx   -> fun i -> (string_of_int i, _1 ctx) )
# 974 "parser.ml"
               : 'Field))
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
