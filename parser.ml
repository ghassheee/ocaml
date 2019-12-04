type token =
  | STRING of (Support.Error.info)
  | FLOAT of (Support.Error.info)
  | TIMESFLOAT of (Support.Error.info)
  | CASE of (Support.Error.info)
  | OF of (Support.Error.info)
  | TAG of (Support.Error.info)
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
# 82 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* STRING *);
  258 (* FLOAT *);
  259 (* TIMESFLOAT *);
  260 (* CASE *);
  261 (* OF *);
  262 (* TAG *);
  263 (* AS *);
  264 (* UNIT *);
  265 (* UNITTYPE *);
  266 (* WHERE *);
  267 (* IN *);
  268 (* LET *);
  269 (* BOOL *);
  270 (* NAT *);
  271 (* SUCC *);
  272 (* PRED *);
  273 (* ISZERO *);
  274 (* LAMBDA *);
  275 (* IF *);
  276 (* THEN *);
  277 (* ELSE *);
  278 (* TRUE *);
  279 (* FALSE *);
  280 (* UCID *);
  281 (* LCID *);
  282 (* INTV *);
  283 (* FLOATV *);
  284 (* STRINGV *);
  285 (* APOSTROPHE *);
  286 (* DQUOTE *);
  287 (* ARROW *);
  288 (* BANG *);
  289 (* BARGT *);
  290 (* BARRCURLY *);
  291 (* BARRSQUARE *);
  292 (* COLON *);
  293 (* COLONCOLON *);
  294 (* COLONEQ *);
  295 (* COLONHASH *);
  296 (* COMMA *);
  297 (* DARROW *);
  298 (* DDARROW *);
  299 (* DOT *);
    0 (* EOF *);
  300 (* EQ *);
  301 (* EQEQ *);
  302 (* EXISTS *);
  303 (* GT *);
  304 (* HASH *);
  305 (* LCURLY *);
  306 (* LCURLYBAR *);
  307 (* LEFTARROW *);
  308 (* LPAREN *);
  309 (* LSQUARE *);
  310 (* LSQUAREBAR *);
  311 (* LT *);
  312 (* RCURLY *);
  313 (* RPAREN *);
  314 (* RSQUARE *);
  315 (* SEMI *);
  316 (* SLASH *);
  317 (* STAR *);
  318 (* TRIANGLE *);
  319 (* USCORE *);
  320 (* VBAR *);
  321 (* NEWLINE *);
  322 (* DOUBLESEMI *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\003\000\003\000\001\000\001\000\004\000\
\004\000\004\000\006\000\006\000\007\000\007\000\008\000\010\000\
\010\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\012\000\012\000\013\000\013\000\014\000\014\000\
\005\000\005\000\005\000\015\000\015\000\016\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\017\000\017\000\
\017\000\017\000\017\000\017\000\018\000\018\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\020\000\020\000\021\000\021\000\022\000\022\000\000\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\002\000\003\000\001\000\003\000\001\000\
\002\000\002\000\000\000\002\000\002\000\002\000\001\000\003\000\
\001\000\001\000\003\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\000\000\001\000\001\000\003\000\003\000\001\000\
\005\000\005\000\001\000\001\000\003\000\007\000\001\000\004\000\
\003\000\003\000\006\000\006\000\006\000\006\000\001\000\003\000\
\002\000\002\000\002\000\002\000\003\000\001\000\003\000\003\000\
\007\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\000\000\001\000\001\000\003\000\003\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\001\000\000\000\000\000\061\000\000\000\000\000\000\000\
\000\000\000\000\000\000\062\000\063\000\000\000\000\000\064\000\
\060\000\059\000\006\000\000\000\000\000\000\000\071\000\000\000\
\000\000\000\000\000\000\047\000\000\000\000\000\058\000\000\000\
\000\000\000\000\049\000\050\000\051\000\000\000\000\000\000\000\
\009\000\000\000\000\000\010\000\000\000\000\000\000\000\066\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\052\000\000\000\000\000\000\000\002\000\003\000\000\000\000\000\
\000\000\000\000\000\000\000\000\021\000\020\000\024\000\022\000\
\023\000\018\000\000\000\000\000\000\000\012\000\015\000\000\000\
\013\000\000\000\000\000\056\000\000\000\055\000\000\000\007\000\
\000\000\000\000\000\000\042\000\048\000\053\000\000\000\004\000\
\000\000\040\000\000\000\000\000\000\000\000\000\000\000\000\000\
\032\000\000\000\028\000\000\000\000\000\000\000\000\000\000\000\
\068\000\000\000\000\000\000\000\005\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\025\000\000\000\019\000\026\000\
\016\000\000\000\000\000\000\000\000\000\037\000\000\000\000\000\
\000\000\000\000\031\000\030\000\000\000\000\000\057\000\000\000\
\000\000\000\000"

let yydgoto = "\003\000\
\023\000\030\000\062\000\024\000\025\000\041\000\044\000\105\000\
\026\000\079\000\080\000\106\000\107\000\108\000\098\000\099\000\
\027\000\028\000\029\000\047\000\048\000\049\000"

let yysindex = "\150\000\
\001\000\000\000\000\000\069\001\000\000\246\254\112\255\112\255\
\112\255\021\255\069\001\000\000\000\000\235\254\233\254\000\000\
\000\000\000\000\000\000\111\001\069\001\032\255\000\000\019\255\
\048\255\001\255\112\255\000\000\073\255\043\000\000\000\007\255\
\042\255\050\255\000\000\000\000\000\000\055\255\254\254\238\255\
\000\000\238\255\069\001\000\000\064\255\248\254\049\255\000\000\
\070\255\057\255\087\255\001\000\098\255\111\255\069\001\121\255\
\000\000\146\255\112\255\238\255\000\000\000\000\228\254\099\255\
\069\001\069\001\238\255\069\001\000\000\000\000\000\000\000\000\
\000\000\000\000\046\255\238\255\046\255\000\000\000\000\124\255\
\000\000\248\254\069\001\000\000\111\001\000\000\069\001\000\000\
\113\255\115\255\248\254\000\000\000\000\000\000\027\001\000\000\
\131\255\000\000\096\255\056\255\061\255\120\255\245\254\130\255\
\000\000\114\255\000\000\128\255\116\255\125\255\238\255\248\254\
\000\000\079\255\069\001\069\001\000\000\132\255\099\255\069\001\
\069\001\069\001\069\001\238\255\000\000\046\255\000\000\000\000\
\000\000\168\255\248\254\248\254\157\255\000\000\248\254\248\254\
\248\254\248\254\000\000\000\000\238\255\136\255\000\000\148\255\
\127\001\112\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\037\255\177\255\000\000\
\000\000\000\000\000\000\134\255\000\000\000\000\000\000\000\000\
\043\255\018\255\201\255\000\000\079\000\191\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\241\000\240\254\000\000\000\000\
\137\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\079\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\138\255\000\000\145\255\000\000\000\000\122\255\
\000\000\062\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\137\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\119\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\236\254\000\000\000\000\000\000\029\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\023\255\047\255\000\000\000\000\159\000\177\000\
\181\000\199\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\009\255"

let yygindex = "\000\000\
\143\000\000\000\101\000\235\255\000\000\000\000\000\000\222\255\
\252\255\086\000\000\000\121\000\075\000\000\000\088\000\000\000\
\063\000\234\255\251\255\000\000\124\000\000\000"

let yytablesize = 694
let yytable = "\032\000\
\019\000\035\000\036\000\037\000\057\000\078\000\039\000\081\000\
\063\000\123\000\054\000\064\000\042\000\038\000\033\000\046\000\
\050\000\068\000\038\000\038\000\043\000\058\000\040\000\070\000\
\055\000\094\000\029\000\055\000\038\000\038\000\095\000\056\000\
\102\000\055\000\056\000\029\000\055\000\096\000\082\000\070\000\
\056\000\109\000\055\000\056\000\038\000\038\000\069\000\070\000\
\038\000\056\000\091\000\038\000\034\000\093\000\071\000\038\000\
\051\000\035\000\072\000\073\000\100\000\101\000\033\000\103\000\
\038\000\038\000\120\000\038\000\069\000\074\000\104\000\121\000\
\038\000\063\000\038\000\059\000\035\000\052\000\112\000\060\000\
\046\000\033\000\114\000\035\000\069\000\065\000\034\000\053\000\
\033\000\139\000\067\000\055\000\055\000\066\000\075\000\011\000\
\055\000\076\000\056\000\056\000\077\000\008\000\011\000\056\000\
\084\000\034\000\143\000\083\000\008\000\085\000\131\000\132\000\
\034\000\086\000\055\000\135\000\136\000\137\000\138\000\005\000\
\014\000\056\000\089\000\057\000\017\000\130\000\017\000\014\000\
\017\000\017\000\087\000\017\000\017\000\012\000\013\000\090\000\
\031\000\016\000\017\000\018\000\058\000\017\000\017\000\017\000\
\017\000\092\000\017\000\017\000\017\000\017\000\001\000\002\000\
\060\000\097\000\111\000\118\000\115\000\017\000\116\000\119\000\
\020\000\017\000\122\000\021\000\017\000\124\000\022\000\126\000\
\017\000\125\000\017\000\128\000\127\000\017\000\141\000\133\000\
\017\000\017\000\017\000\058\000\017\000\142\000\144\000\058\000\
\058\000\017\000\058\000\017\000\145\000\065\000\072\000\027\000\
\067\000\027\000\088\000\117\000\129\000\110\000\058\000\058\000\
\140\000\058\000\058\000\058\000\058\000\039\000\134\000\146\000\
\113\000\000\000\039\000\039\000\000\000\000\000\000\000\000\000\
\058\000\000\000\000\000\058\000\039\000\039\000\000\000\000\000\
\000\000\058\000\000\000\000\000\058\000\000\000\000\000\058\000\
\000\000\000\000\000\000\058\000\039\000\000\000\069\000\070\000\
\039\000\000\000\058\000\039\000\000\000\000\000\071\000\039\000\
\000\000\000\000\072\000\073\000\000\000\000\000\000\000\000\000\
\039\000\039\000\000\000\039\000\004\000\074\000\000\000\000\000\
\005\000\000\000\039\000\000\000\006\000\000\000\000\000\007\000\
\008\000\009\000\010\000\011\000\000\000\000\000\012\000\013\000\
\014\000\015\000\016\000\017\000\018\000\000\000\075\000\000\000\
\000\000\076\000\000\000\000\000\077\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\
\000\000\020\000\005\000\000\000\021\000\000\000\006\000\022\000\
\000\000\007\000\008\000\009\000\010\000\011\000\000\000\000\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\054\000\000\000\000\000\054\000\000\000\
\054\000\054\000\000\000\020\000\000\000\000\000\021\000\000\000\
\000\000\022\000\054\000\054\000\054\000\054\000\000\000\054\000\
\054\000\054\000\054\000\000\000\061\000\000\000\000\000\000\000\
\000\000\000\000\054\000\000\000\000\000\000\000\054\000\000\000\
\000\000\054\000\000\000\036\000\000\000\054\000\000\000\054\000\
\036\000\036\000\054\000\000\000\000\000\054\000\054\000\054\000\
\000\000\054\000\036\000\036\000\000\000\041\000\054\000\000\000\
\054\000\000\000\041\000\041\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\000\000\000\041\000\041\000\036\000\000\000\
\000\000\036\000\000\000\043\000\000\000\036\000\000\000\000\000\
\043\000\043\000\000\000\000\000\000\000\000\000\036\000\036\000\
\041\000\036\000\043\000\043\000\000\000\044\000\000\000\041\000\
\036\000\045\000\044\000\044\000\000\000\000\000\045\000\045\000\
\041\000\041\000\000\000\041\000\044\000\044\000\043\000\000\000\
\045\000\045\000\041\000\046\000\000\000\043\000\000\000\000\000\
\046\000\046\000\000\000\000\000\000\000\000\000\043\000\043\000\
\044\000\043\000\046\000\046\000\045\000\000\000\000\000\044\000\
\043\000\000\000\000\000\045\000\000\000\000\000\000\000\000\000\
\044\000\044\000\000\000\044\000\045\000\045\000\046\000\045\000\
\000\000\000\000\044\000\058\000\000\000\046\000\045\000\058\000\
\058\000\000\000\000\000\000\000\000\000\000\000\046\000\046\000\
\000\000\046\000\000\000\000\000\000\000\000\000\058\000\058\000\
\046\000\058\000\058\000\058\000\058\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\058\000\000\000\000\000\000\000\
\058\000\000\000\000\000\058\000\000\000\000\000\004\000\000\000\
\000\000\058\000\005\000\000\000\058\000\000\000\006\000\058\000\
\058\000\007\000\008\000\009\000\010\000\011\000\000\000\000\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\004\000\000\000\000\000\020\000\005\000\000\000\021\000\000\000\
\006\000\022\000\000\000\007\000\008\000\009\000\010\000\011\000\
\000\000\000\000\012\000\013\000\000\000\031\000\016\000\017\000\
\018\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\020\000\005\000\000\000\
\021\000\000\000\006\000\022\000\000\000\007\000\008\000\009\000\
\010\000\011\000\000\000\000\000\012\000\013\000\005\000\045\000\
\016\000\017\000\018\000\000\000\000\000\007\000\008\000\009\000\
\000\000\000\000\000\000\000\000\012\000\013\000\000\000\031\000\
\016\000\017\000\018\000\000\000\000\000\000\000\000\000\020\000\
\000\000\000\000\021\000\000\000\000\000\022\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\
\000\000\000\000\021\000\000\000\000\000\022\000"

let yycheck = "\004\000\
\000\000\007\000\008\000\009\000\027\000\040\000\011\000\042\000\
\030\000\021\001\010\001\005\001\036\001\005\001\025\001\020\000\
\021\000\020\001\010\001\011\001\044\001\027\000\044\001\040\001\
\036\001\060\000\047\001\036\001\020\001\021\001\059\001\043\001\
\067\000\036\001\043\001\056\001\036\001\066\001\043\000\056\001\
\043\001\076\000\036\001\043\001\036\001\025\001\001\001\002\001\
\040\001\043\001\055\000\043\001\063\001\059\000\009\001\047\001\
\025\001\040\001\013\001\014\001\065\000\066\000\040\001\068\000\
\056\001\057\001\011\001\059\001\040\001\024\001\025\001\011\001\
\064\001\095\000\066\001\003\001\059\001\059\001\083\000\007\001\
\085\000\059\001\087\000\066\001\056\001\044\001\040\001\040\001\
\066\001\124\000\036\001\036\001\036\001\044\001\049\001\059\001\
\036\001\052\001\043\001\043\001\055\001\059\001\066\001\043\001\
\056\001\059\001\141\000\044\001\066\001\040\001\115\000\116\000\
\066\001\057\001\036\001\120\000\121\000\122\000\123\000\008\001\
\059\001\043\001\025\001\146\000\003\001\047\001\005\001\066\001\
\007\001\008\001\044\001\010\001\011\001\022\001\023\001\025\001\
\025\001\026\001\027\001\028\001\146\000\020\001\021\001\022\001\
\023\001\025\001\025\001\026\001\027\001\028\001\001\000\002\000\
\007\001\055\001\031\001\025\001\044\001\036\001\044\001\064\001\
\049\001\040\001\043\001\052\001\043\001\036\001\055\001\040\001\
\047\001\056\001\049\001\047\001\057\001\052\001\007\001\044\001\
\055\001\056\001\057\001\003\001\059\001\025\001\047\001\007\001\
\008\001\064\001\010\001\066\001\041\001\056\001\000\000\047\001\
\056\001\056\001\052\000\095\000\111\000\077\000\022\001\023\001\
\126\000\025\001\026\001\027\001\028\001\005\001\119\000\145\000\
\085\000\255\255\010\001\011\001\255\255\255\255\255\255\255\255\
\040\001\255\255\255\255\043\001\020\001\021\001\255\255\255\255\
\255\255\049\001\255\255\255\255\052\001\255\255\255\255\055\001\
\255\255\255\255\255\255\059\001\036\001\255\255\001\001\002\001\
\040\001\255\255\066\001\043\001\255\255\255\255\009\001\047\001\
\255\255\255\255\013\001\014\001\255\255\255\255\255\255\255\255\
\056\001\057\001\255\255\059\001\004\001\024\001\255\255\255\255\
\008\001\255\255\066\001\255\255\012\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\255\255\049\001\255\255\
\255\255\052\001\255\255\255\255\055\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\004\001\255\255\
\255\255\049\001\008\001\255\255\052\001\255\255\012\001\055\001\
\255\255\015\001\016\001\017\001\018\001\019\001\255\255\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\005\001\255\255\255\255\008\001\255\255\
\010\001\011\001\255\255\049\001\255\255\255\255\052\001\255\255\
\255\255\055\001\020\001\021\001\022\001\023\001\255\255\025\001\
\026\001\027\001\028\001\255\255\066\001\255\255\255\255\255\255\
\255\255\255\255\036\001\255\255\255\255\255\255\040\001\255\255\
\255\255\043\001\255\255\005\001\255\255\047\001\255\255\049\001\
\010\001\011\001\052\001\255\255\255\255\055\001\056\001\057\001\
\255\255\059\001\020\001\021\001\255\255\005\001\064\001\255\255\
\066\001\255\255\010\001\011\001\255\255\255\255\255\255\255\255\
\255\255\255\255\036\001\255\255\020\001\021\001\040\001\255\255\
\255\255\043\001\255\255\005\001\255\255\047\001\255\255\255\255\
\010\001\011\001\255\255\255\255\255\255\255\255\056\001\057\001\
\040\001\059\001\020\001\021\001\255\255\005\001\255\255\047\001\
\066\001\005\001\010\001\011\001\255\255\255\255\010\001\011\001\
\056\001\057\001\255\255\059\001\020\001\021\001\040\001\255\255\
\020\001\021\001\066\001\005\001\255\255\047\001\255\255\255\255\
\010\001\011\001\255\255\255\255\255\255\255\255\056\001\057\001\
\040\001\059\001\020\001\021\001\040\001\255\255\255\255\047\001\
\066\001\255\255\255\255\047\001\255\255\255\255\255\255\255\255\
\056\001\057\001\255\255\059\001\056\001\057\001\040\001\059\001\
\255\255\255\255\066\001\003\001\255\255\047\001\066\001\007\001\
\008\001\255\255\255\255\255\255\255\255\255\255\056\001\057\001\
\255\255\059\001\255\255\255\255\255\255\255\255\022\001\023\001\
\066\001\025\001\026\001\027\001\028\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\036\001\255\255\255\255\255\255\
\040\001\255\255\255\255\043\001\255\255\255\255\004\001\255\255\
\255\255\049\001\008\001\255\255\052\001\255\255\012\001\055\001\
\056\001\015\001\016\001\017\001\018\001\019\001\255\255\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\004\001\255\255\255\255\049\001\008\001\255\255\052\001\255\255\
\012\001\055\001\255\255\015\001\016\001\017\001\018\001\019\001\
\255\255\255\255\022\001\023\001\255\255\025\001\026\001\027\001\
\028\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\004\001\255\255\255\255\049\001\008\001\255\255\
\052\001\255\255\012\001\055\001\255\255\015\001\016\001\017\001\
\018\001\019\001\255\255\255\255\022\001\023\001\008\001\025\001\
\026\001\027\001\028\001\255\255\255\255\015\001\016\001\017\001\
\255\255\255\255\255\255\255\255\022\001\023\001\255\255\025\001\
\026\001\027\001\028\001\255\255\255\255\255\255\255\255\049\001\
\255\255\255\255\052\001\255\255\255\255\055\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\049\001\
\255\255\255\255\052\001\255\255\255\255\055\001"

let yynames_const = "\
  "

let yynames_block = "\
  STRING\000\
  FLOAT\000\
  TIMESFLOAT\000\
  CASE\000\
  OF\000\
  TAG\000\
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
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
                                        ( fun ctx   ->  [],ctx                                  )
# 509 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 109 "parser.mly"
                                        ( fun ctx   ->  [],ctx                                  )
# 517 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'oneREPL) in
    Obj.repr(
# 110 "parser.mly"
                                        ( let cmds,ctx = _2[]in process_commands emptyctx cmds; 
                                          fun ctx   ->  [],ctx                                  )
# 526 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 113 "parser.mly"
                                        ( fun ctx   ->  let cmd,ctx = _1 ctx in [cmd],ctx )
# 534 "parser.ml"
               : 'oneREPL))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'oneREPL) in
    Obj.repr(
# 114 "parser.mly"
                                        ( fun ctx   ->  let cmd,ctx = _1 ctx in 
                                                        let cmds,ctx = _3 ctx in cmd::cmds,ctx  )
# 544 "parser.ml"
               : 'oneREPL))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 118 "parser.mly"
                                        ( fun ctx   ->  [],ctx                                  )
# 551 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    Obj.repr(
# 119 "parser.mly"
                                        ( fun ctx   ->  let cmd,ctx  = _1 ctx in 
                                                        let cmds,ctx = _3 ctx in cmd::cmds,ctx  )
# 561 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'TermWrap) in
    Obj.repr(
# 123 "parser.mly"
                                        ( fun ctx   ->  let t = _1 ctx in Eval(tmInfo t,t),ctx  )
# 568 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'TyBinder) in
    Obj.repr(
# 124 "parser.mly"
                                        ( fun ctx   ->  Bind(_1.i,_1.v,_2 ctx),addname ctx _1.v )
# 576 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 125 "parser.mly"
                                        ( fun ctx   ->  Bind(_1.i,_1.v,_2 ctx),addname ctx _1.v )
# 584 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
                                        ( fun ctx   ->  BindTyVar                               )
# 590 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 128 "parser.mly"
                                        ( fun ctx   ->  BindTyAbb(_2 ctx)                       )
# 598 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 130 "parser.mly"
                                        ( fun ctx   ->  BindTmVar(_2 ctx)                         )
# 606 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 131 "parser.mly"
                                        ( fun ctx   ->  BindTmAbb(_2 ctx,None)                  )
# 614 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 134 "parser.mly"
                                        ( _1                                                    )
# 621 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 136 "parser.mly"
                                        ( fun ctx   ->  TyArr(_1 ctx, _3 ctx)                   )
# 630 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 137 "parser.mly"
                                        ( _1                                                    )
# 637 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 139 "parser.mly"
                                        ( fun ctx   ->  if isnamebound ctx _1.v then 
                                                        TyVar(name2index _1.i ctx _1.v, ctxlen ctx) else 
                                                            error _1.i"Type Not Found"            )
# 646 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 142 "parser.mly"
                                        ( _2                                                    )
# 655 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 143 "parser.mly"
                                        ( fun ctx   ->  TyFloat                                 )
# 662 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 144 "parser.mly"
                                        ( fun ctx   ->  TyString                                )
# 669 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 145 "parser.mly"
                                        ( fun ctx   ->  TyBool                                  )
# 676 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 146 "parser.mly"
                                        ( fun ctx   ->  TyNat                                   )
# 683 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 147 "parser.mly"
                                        ( fun ctx   ->  TyUnit                                  )
# 690 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'TyFields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 148 "parser.mly"
                                        ( fun ctx   ->  TyRecord(_2 ctx 1)                      )
# 699 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'TyFields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 149 "parser.mly"
                                        ( fun ctx   ->  TyVariant(_2 ctx 1)                     )
# 708 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    Obj.repr(
# 151 "parser.mly"
                                        ( fun ctx   ->  fun i -> []                             )
# 714 "parser.ml"
               : 'TyFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NETyFields) in
    Obj.repr(
# 152 "parser.mly"
                                        ( _1                                                    )
# 721 "parser.ml"
               : 'TyFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'TyField) in
    Obj.repr(
# 154 "parser.mly"
                                        ( fun ctx   ->  fun i -> [_1 ctx i]                     )
# 728 "parser.ml"
               : 'NETyFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'TyField) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NETyFields) in
    Obj.repr(
# 155 "parser.mly"
                                        ( fun ctx   ->  fun i -> (_1 ctx i)::(_3 ctx (i+1))     )
# 737 "parser.ml"
               : 'NETyFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 157 "parser.mly"
                                        ( fun ctx   ->  fun i -> (_1.v, _3 ctx)                 )
# 746 "parser.ml"
               : 'TyField))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 158 "parser.mly"
                                        ( fun ctx   ->  fun i -> (string_of_int i, _1 ctx)      )
# 753 "parser.ml"
               : 'TyField))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'TermWrap) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 161 "parser.mly"
                                        ( fun ctx   ->  TmLet(_2,_3.v,_5 ctx,_1(addname ctx _3.v)) )
# 764 "parser.ml"
               : 'TermWrap))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 162 "parser.mly"
                                        ( fun ctx   ->  TmLet(_2,_3.v,_5 ctx,_1(addname ctx _3.v)) )
# 775 "parser.ml"
               : 'TermWrap))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 163 "parser.mly"
                                        ( _1                                                    )
# 782 "parser.ml"
               : 'TermWrap))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Case) in
    Obj.repr(
# 165 "parser.mly"
                                        ( fun ctx   ->  [_1 ctx]                                )
# 789 "parser.ml"
               : 'Cases))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Case) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Cases) in
    Obj.repr(
# 166 "parser.mly"
                                        ( fun ctx   ->  (_1 ctx)::(_3 ctx)                      )
# 798 "parser.ml"
               : 'Cases))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string  Support.Error.withinfo) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 168 "parser.mly"
                                        ( fun ctx   ->  (_2.v,(_4.v,_7(addname ctx _4.v)))      )
# 811 "parser.ml"
               : 'Case))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 170 "parser.mly"
                                        ( _1                                                    )
# 818 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Cases) in
    Obj.repr(
# 171 "parser.mly"
                                        ( fun ctx   ->  TmCase(_1,_2 ctx,_4 ctx)                )
# 828 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 172 "parser.mly"
                                        ( fun ctx   ->  TmLet(_2, "_", _3 ctx, _1 ctx)          )
# 837 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 173 "parser.mly"
                                        ( fun ctx   ->  TmProj(_2, _1 ctx, _3.v)                )
# 846 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 174 "parser.mly"
                                        ( fun ctx   ->  TmLet(_1,_2.v,_4 ctx,_6(addname ctx _2.v)))
# 858 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 175 "parser.mly"
                                        ( fun ctx   ->  TmLet(_1,"_",_4 ctx,_6(addname ctx"_")) )
# 870 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 176 "parser.mly"
                                        ( fun ctx   ->  TmAbs(_1,_2.v,_4 ctx,_6(addname ctx _2.v)))
# 882 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 177 "parser.mly"
                                        ( fun ctx   ->  TmIf(_1,_2 ctx,_4 ctx,_6 ctx)           )
# 894 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AscribeTerm) in
    Obj.repr(
# 179 "parser.mly"
                                        ( _1                                                    )
# 901 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ATerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 180 "parser.mly"
                                        ( fun ctx   ->  TmTimesfloat(_2,_1 ctx,_3 ctx)          )
# 910 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 181 "parser.mly"
                                        ( fun ctx   ->  TmSucc(_1, _2 ctx )                     )
# 918 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 182 "parser.mly"
                                        ( fun ctx   ->  TmPred(_1, _2 ctx )                     )
# 926 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 183 "parser.mly"
                                        ( fun ctx   ->  TmIsZero(_1, _2 ctx)                    )
# 934 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AscribeTerm) in
    Obj.repr(
# 184 "parser.mly"
                                        ( fun ctx   ->  let t=_1 ctx in TmApp(tmInfo t,t,_2 ctx))
# 942 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ATerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 186 "parser.mly"
                                        ( fun ctx   ->  TmAscribe(_2,_1 ctx,_3 ctx)             )
# 951 "parser.ml"
               : 'AscribeTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 187 "parser.mly"
                                        ( _1                                                    )
# 958 "parser.ml"
               : 'AscribeTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 189 "parser.mly"
                                        ( _2                                                    )
# 967 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 190 "parser.mly"
                                        ( fun ctx   ->  TmRecord(_1,_2 ctx 1)                   )
# 976 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 191 "parser.mly"
                                        ( fun ctx   ->  TmTag(_1,_2.v,_4 ctx,_7 ctx)            )
# 989 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 192 "parser.mly"
                                        ( fun ctx   ->  TmVar(_1.i,name2index _1.i ctx _1.v,ctxlen ctx) )
# 996 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 193 "parser.mly"
                                        ( fun ctx   ->  TmString(_1.i,_1.v)                     )
# 1003 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float   Support.Error.withinfo) in
    Obj.repr(
# 194 "parser.mly"
                                        ( fun ctx   ->  TmFloat(_1.i,_1.v)                      )
# 1010 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 195 "parser.mly"
                                        ( fun ctx   ->  TmUnit(_1)                              )
# 1017 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 196 "parser.mly"
                                        ( fun ctx   ->  TmTrue(_1)                              )
# 1024 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 197 "parser.mly"
                                        ( fun ctx   ->  TmFalse(_1)                             )
# 1031 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int     Support.Error.withinfo) in
    Obj.repr(
# 198 "parser.mly"
                                        ( fun ctx   ->  let rec f = function
                                                            | 0 -> TmZero(_1.i)
                                                            | n -> TmSucc(_1.i,f(n-1))in f _1.v )
# 1040 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 202 "parser.mly"
                                        ( fun ctx   ->  fun i -> []                             )
# 1046 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 203 "parser.mly"
                                        ( _1                                                    )
# 1053 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Field) in
    Obj.repr(
# 205 "parser.mly"
                                        ( fun ctx   -> fun i -> [ _1 ctx i ]                    )
# 1060 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Field) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 206 "parser.mly"
                                        ( fun ctx   -> fun i -> (_1 ctx i)::(_3 ctx(i+1))       )
# 1069 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 208 "parser.mly"
                                        ( fun ctx   -> fun i -> (_1.v, _3 ctx)                  )
# 1078 "parser.ml"
               : 'Field))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 209 "parser.mly"
                                        ( fun ctx   -> fun i -> (string_of_int i, _1 ctx)       )
# 1085 "parser.ml"
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
