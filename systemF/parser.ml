type token =
  | LOAD of (string  Support.Error.withinfo)
  | SHOWCONTEXT of (Support.Error.info)
  | TOP of (Support.Error.info)
  | REF of (Support.Error.info)
  | REFTYPE of (Support.Error.info)
  | LIST of (Support.Error.info)
  | TAIL of (Support.Error.info)
  | HEAD of (Support.Error.info)
  | ISNIL of (Support.Error.info)
  | CONS of (Support.Error.info)
  | NIL of (Support.Error.info)
  | LETREC of (Support.Error.info)
  | FIX of (Support.Error.info)
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
  | LAM of (Support.Error.info)
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
open Interpreter 

let pe = print_endline 
# 96 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* LOAD *);
  258 (* SHOWCONTEXT *);
  259 (* TOP *);
  260 (* REF *);
  261 (* REFTYPE *);
  262 (* LIST *);
  263 (* TAIL *);
  264 (* HEAD *);
  265 (* ISNIL *);
  266 (* CONS *);
  267 (* NIL *);
  268 (* LETREC *);
  269 (* FIX *);
  270 (* STRING *);
  271 (* FLOAT *);
  272 (* TIMESFLOAT *);
  273 (* CASE *);
  274 (* OF *);
  275 (* TAG *);
  276 (* AS *);
  277 (* UNIT *);
  278 (* UNITTYPE *);
  279 (* WHERE *);
  280 (* IN *);
  281 (* LET *);
  282 (* BOOL *);
  283 (* NAT *);
  284 (* SUCC *);
  285 (* PRED *);
  286 (* ISZERO *);
  287 (* LAM *);
  288 (* IF *);
  289 (* THEN *);
  290 (* ELSE *);
  291 (* TRUE *);
  292 (* FALSE *);
  293 (* UCID *);
  294 (* LCID *);
  295 (* INTV *);
  296 (* FLOATV *);
  297 (* STRINGV *);
  298 (* APOSTROPHE *);
  299 (* DQUOTE *);
  300 (* ARROW *);
  301 (* BANG *);
  302 (* BARGT *);
  303 (* BARRCURLY *);
  304 (* BARRSQUARE *);
  305 (* COLON *);
  306 (* COLONCOLON *);
  307 (* COLONEQ *);
  308 (* COLONHASH *);
  309 (* COMMA *);
  310 (* DARROW *);
  311 (* DDARROW *);
  312 (* DOT *);
    0 (* EOF *);
  313 (* EQ *);
  314 (* EQEQ *);
  315 (* EXISTS *);
  316 (* GT *);
  317 (* HASH *);
  318 (* LCURLY *);
  319 (* LCURLYBAR *);
  320 (* LEFTARROW *);
  321 (* LPAREN *);
  322 (* LSQUARE *);
  323 (* LSQUAREBAR *);
  324 (* LT *);
  325 (* RCURLY *);
  326 (* RPAREN *);
  327 (* RSQUARE *);
  328 (* SEMI *);
  329 (* SLASH *);
  330 (* STAR *);
  331 (* TRIANGLE *);
  332 (* USCORE *);
  333 (* VBAR *);
  334 (* NEWLINE *);
  335 (* DOUBLESEMI *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\002\000\003\000\003\000\001\000\
\001\000\004\000\004\000\004\000\006\000\006\000\007\000\007\000\
\008\000\010\000\010\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\013\000\012\000\012\000\
\014\000\014\000\015\000\015\000\005\000\005\000\005\000\009\000\
\009\000\009\000\009\000\009\000\009\000\016\000\016\000\016\000\
\016\000\016\000\016\000\016\000\017\000\017\000\017\000\018\000\
\018\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\019\000\019\000\020\000\020\000\021\000\021\000\022\000\022\000\
\023\000\023\000\000\000\000\000"

let yylen = "\002\000\
\000\000\002\000\003\000\002\000\002\000\002\000\003\000\001\000\
\003\000\001\000\002\000\002\000\000\000\002\000\002\000\002\000\
\001\000\003\000\001\000\001\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\000\000\001\000\
\001\000\003\000\003\000\001\000\005\000\005\000\001\000\001\000\
\006\000\006\000\006\000\006\000\008\000\001\000\003\000\002\000\
\002\000\002\000\002\000\002\000\003\000\003\000\001\000\003\000\
\001\000\003\000\003\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\000\000\001\000\001\000\003\000\
\003\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\001\000\000\000\000\000\000\000\063\000\000\000\000\000\
\000\000\000\000\000\000\000\000\064\000\065\000\000\000\000\000\
\066\000\062\000\061\000\008\000\000\000\000\000\075\000\000\000\
\000\000\000\000\000\000\000\000\055\000\000\000\000\000\000\000\
\060\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\000\000\000\000\000\012\000\000\000\074\000\
\000\000\070\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\000\000\000\004\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\022\000\024\000\
\023\000\027\000\025\000\026\000\020\000\000\000\000\000\000\000\
\014\000\017\000\000\000\015\000\016\000\000\000\059\000\000\000\
\000\000\058\000\009\000\000\000\000\000\000\000\053\000\054\000\
\056\000\003\000\000\000\006\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\000\000\000\032\000\000\000\000\000\000\000\
\000\000\073\000\072\000\068\000\000\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\000\000\000\021\000\
\029\000\018\000\037\000\038\000\000\000\041\000\042\000\043\000\
\044\000\035\000\034\000\000\000\045\000"

let yydgoto = "\003\000\
\023\000\031\000\064\000\024\000\025\000\043\000\046\000\107\000\
\026\000\082\000\083\000\108\000\000\000\109\000\110\000\027\000\
\028\000\029\000\030\000\053\000\049\000\050\000\051\000"

let yysindex = "\006\000\
\001\000\000\000\000\000\245\254\203\255\000\000\224\254\203\255\
\203\255\203\255\249\254\162\001\000\000\000\000\248\254\229\254\
\000\000\000\000\000\000\000\000\200\001\162\001\000\000\230\254\
\253\254\028\255\203\255\243\254\000\000\032\255\058\255\004\255\
\000\000\254\254\000\255\005\255\254\254\254\254\254\254\012\255\
\030\255\099\255\000\000\099\255\162\001\000\000\007\255\000\000\
\255\254\000\000\013\255\251\254\011\255\001\000\031\255\042\255\
\254\254\203\255\002\255\099\255\000\000\003\255\000\000\000\000\
\195\254\099\255\162\001\162\001\099\255\162\001\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\255\099\255\010\255\
\000\000\000\000\040\255\000\000\000\000\162\001\000\000\200\001\
\162\001\000\000\000\000\034\255\035\255\254\254\000\000\000\000\
\000\000\000\000\124\001\000\000\043\255\077\255\079\255\048\255\
\073\255\066\255\000\000\047\255\000\000\064\255\049\255\071\255\
\099\255\000\000\000\000\000\000\162\001\162\001\000\000\162\001\
\162\001\162\001\162\001\162\001\099\255\000\000\010\255\000\000\
\000\000\000\000\000\000\000\000\094\255\000\000\000\000\000\000\
\000\000\000\000\000\000\162\001\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\205\254\211\255\
\000\000\000\000\000\000\000\000\055\255\000\000\000\000\000\000\
\219\254\222\254\150\255\034\000\000\000\156\255\132\000\000\000\
\000\000\084\000\000\000\000\000\134\000\184\000\234\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\228\001\000\000\
\000\000\000\000\065\255\063\255\000\000\000\000\000\000\000\000\
\028\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\069\255\000\000\075\255\
\000\000\000\000\106\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\078\001\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\210\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\089\000\000\000\049\000\230\255\000\000\000\000\000\000\216\255\
\244\255\036\000\000\000\070\000\000\000\024\000\000\000\000\000\
\007\000\000\000\000\000\063\000\000\000\065\000\000\000"

let yytablesize = 809
let yytable = "\041\000\
\020\000\081\000\058\000\084\000\065\000\035\000\001\000\002\000\
\048\000\052\000\099\000\034\000\071\000\033\000\037\000\038\000\
\039\000\100\000\039\000\097\000\013\000\044\000\033\000\072\000\
\073\000\101\000\032\000\013\000\104\000\045\000\040\000\074\000\
\085\000\057\000\010\000\075\000\076\000\039\000\111\000\095\000\
\096\000\010\000\059\000\036\000\039\000\054\000\077\000\106\000\
\042\000\055\000\056\000\060\000\066\000\059\000\102\000\103\000\
\067\000\105\000\061\000\062\000\069\000\068\000\070\000\086\000\
\094\000\088\000\089\000\087\000\092\000\004\000\005\000\078\000\
\065\000\114\000\079\000\048\000\052\000\080\000\006\000\093\000\
\090\000\098\000\007\000\113\000\138\000\008\000\009\000\010\000\
\011\000\012\000\117\000\118\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\120\000\121\000\071\000\122\000\123\000\
\131\000\132\000\124\000\133\000\134\000\135\000\136\000\137\000\
\072\000\073\000\125\000\126\000\127\000\140\000\128\000\021\000\
\074\000\019\000\022\000\069\000\075\000\076\000\019\000\141\000\
\019\000\019\000\129\000\076\000\067\000\071\000\031\000\077\000\
\063\000\031\000\019\000\019\000\019\000\019\000\091\000\019\000\
\019\000\019\000\019\000\119\000\130\000\112\000\139\000\116\000\
\115\000\000\000\000\000\000\000\000\000\000\000\019\000\000\000\
\078\000\019\000\019\000\079\000\000\000\019\000\080\000\019\000\
\000\000\000\000\019\000\057\000\040\000\040\000\019\000\019\000\
\057\000\019\000\057\000\057\000\000\000\000\000\040\000\040\000\
\019\000\000\000\000\000\000\000\057\000\057\000\057\000\057\000\
\000\000\057\000\057\000\057\000\057\000\000\000\000\000\000\000\
\000\000\000\000\040\000\000\000\000\000\000\000\000\000\000\000\
\057\000\000\000\000\000\057\000\000\000\000\000\000\000\000\000\
\000\000\057\000\040\000\040\000\057\000\040\000\000\000\006\000\
\057\000\057\000\060\000\057\000\040\000\000\000\060\000\060\000\
\000\000\060\000\057\000\000\000\000\000\013\000\014\000\000\000\
\033\000\017\000\018\000\019\000\000\000\060\000\060\000\000\000\
\060\000\060\000\060\000\060\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\060\000\
\021\000\000\000\060\000\022\000\004\000\005\000\000\000\000\000\
\060\000\000\000\000\000\060\000\000\000\006\000\000\000\000\000\
\000\000\007\000\060\000\000\000\008\000\009\000\010\000\011\000\
\012\000\060\000\000\000\013\000\014\000\015\000\016\000\017\000\
\018\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\046\000\000\000\
\046\000\046\000\000\000\000\000\000\000\000\000\021\000\000\000\
\000\000\022\000\046\000\046\000\046\000\046\000\000\000\046\000\
\046\000\046\000\046\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\046\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\000\
\000\000\000\000\046\000\000\000\000\000\000\000\046\000\046\000\
\048\000\046\000\048\000\048\000\000\000\000\000\000\000\000\000\
\046\000\000\000\000\000\000\000\048\000\048\000\048\000\048\000\
\000\000\048\000\048\000\048\000\048\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\048\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\048\000\000\000\000\000\048\000\000\000\000\000\000\000\
\048\000\048\000\049\000\048\000\049\000\049\000\000\000\000\000\
\000\000\000\000\048\000\000\000\000\000\000\000\049\000\049\000\
\049\000\049\000\000\000\049\000\049\000\049\000\049\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\049\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\049\000\000\000\000\000\049\000\000\000\
\000\000\000\000\049\000\049\000\050\000\049\000\050\000\050\000\
\000\000\000\000\000\000\000\000\049\000\000\000\000\000\000\000\
\050\000\050\000\050\000\050\000\000\000\050\000\050\000\050\000\
\050\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\050\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\050\000\000\000\000\000\
\050\000\000\000\000\000\000\000\050\000\050\000\051\000\050\000\
\051\000\051\000\000\000\000\000\000\000\000\000\050\000\000\000\
\000\000\000\000\051\000\051\000\051\000\051\000\000\000\051\000\
\051\000\051\000\051\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\051\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\051\000\
\000\000\000\000\051\000\000\000\000\000\000\000\051\000\051\000\
\052\000\051\000\052\000\052\000\000\000\000\000\000\000\000\000\
\051\000\000\000\000\000\000\000\052\000\052\000\052\000\052\000\
\000\000\052\000\052\000\052\000\052\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\052\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\052\000\000\000\000\000\052\000\000\000\000\000\000\000\
\052\000\052\000\047\000\052\000\047\000\047\000\000\000\000\000\
\000\000\000\000\052\000\000\000\000\000\000\000\047\000\047\000\
\047\000\047\000\000\000\047\000\047\000\047\000\047\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\047\000\000\000\000\000\000\000\000\000\004\000\
\005\000\000\000\000\000\047\000\000\000\000\000\047\000\000\000\
\006\000\000\000\047\000\047\000\007\000\047\000\000\000\008\000\
\009\000\010\000\011\000\012\000\047\000\000\000\013\000\014\000\
\015\000\016\000\017\000\018\000\019\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\005\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
\000\000\021\000\007\000\000\000\022\000\008\000\009\000\010\000\
\011\000\012\000\000\000\000\000\013\000\014\000\000\000\033\000\
\017\000\018\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\005\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\006\000\000\000\000\000\021\000\
\007\000\000\000\022\000\008\000\009\000\010\000\011\000\012\000\
\000\000\000\000\013\000\014\000\000\000\047\000\017\000\018\000\
\019\000\000\000\000\000\060\000\000\000\000\000\000\000\060\000\
\060\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\021\000\060\000\060\000\
\022\000\060\000\060\000\060\000\060\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\060\000\000\000\000\000\060\000\000\000\000\000\000\000\000\000\
\000\000\060\000\000\000\000\000\060\000\000\000\000\000\000\000\
\060\000"

let yycheck = "\012\000\
\000\000\042\000\016\001\044\000\031\000\038\001\001\000\002\000\
\021\000\022\000\072\001\005\000\003\001\060\001\008\000\009\000\
\010\000\079\001\053\001\060\000\072\001\049\001\069\001\014\001\
\015\001\066\000\038\001\079\001\069\000\057\001\038\001\022\001\
\045\000\027\000\072\001\026\001\027\001\072\001\079\000\038\001\
\039\001\079\001\056\001\076\001\079\001\072\001\037\001\038\001\
\057\001\053\001\023\001\020\001\049\001\056\001\067\000\068\000\
\057\001\070\000\001\001\002\001\049\001\057\001\033\001\057\001\
\058\000\053\001\072\001\069\001\038\001\012\001\013\001\062\001\
\099\000\086\000\065\001\088\000\089\000\068\001\021\001\038\001\
\070\001\079\001\025\001\044\001\125\000\028\001\029\001\030\001\
\031\001\032\001\057\001\057\001\035\001\036\001\037\001\038\001\
\039\001\040\001\041\001\057\001\024\001\003\001\024\001\056\001\
\117\000\118\000\034\001\120\000\121\000\122\000\123\000\124\000\
\014\001\015\001\049\001\069\001\053\001\024\001\070\001\062\001\
\022\001\016\001\065\001\069\001\026\001\027\001\021\001\140\000\
\023\001\024\001\060\001\000\000\070\001\069\001\060\001\037\001\
\079\001\069\001\033\001\034\001\035\001\036\001\054\000\038\001\
\039\001\040\001\041\001\099\000\113\000\080\000\127\000\089\000\
\088\000\255\255\255\255\255\255\255\255\255\255\053\001\255\255\
\062\001\056\001\057\001\065\001\255\255\060\001\068\001\062\001\
\255\255\255\255\065\001\016\001\023\001\024\001\069\001\070\001\
\021\001\072\001\023\001\024\001\255\255\255\255\033\001\034\001\
\079\001\255\255\255\255\255\255\033\001\034\001\035\001\036\001\
\255\255\038\001\039\001\040\001\041\001\255\255\255\255\255\255\
\255\255\255\255\053\001\255\255\255\255\255\255\255\255\255\255\
\053\001\255\255\255\255\056\001\255\255\255\255\255\255\255\255\
\255\255\062\001\069\001\070\001\065\001\072\001\255\255\021\001\
\069\001\070\001\016\001\072\001\079\001\255\255\020\001\021\001\
\255\255\023\001\079\001\255\255\255\255\035\001\036\001\255\255\
\038\001\039\001\040\001\041\001\255\255\035\001\036\001\255\255\
\038\001\039\001\040\001\041\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\053\001\
\062\001\255\255\056\001\065\001\012\001\013\001\255\255\255\255\
\062\001\255\255\255\255\065\001\255\255\021\001\255\255\255\255\
\255\255\025\001\072\001\255\255\028\001\029\001\030\001\031\001\
\032\001\079\001\255\255\035\001\036\001\037\001\038\001\039\001\
\040\001\041\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\021\001\255\255\
\023\001\024\001\255\255\255\255\255\255\255\255\062\001\255\255\
\255\255\065\001\033\001\034\001\035\001\036\001\255\255\038\001\
\039\001\040\001\041\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\053\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\062\001\
\255\255\255\255\065\001\255\255\255\255\255\255\069\001\070\001\
\021\001\072\001\023\001\024\001\255\255\255\255\255\255\255\255\
\079\001\255\255\255\255\255\255\033\001\034\001\035\001\036\001\
\255\255\038\001\039\001\040\001\041\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\053\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\062\001\255\255\255\255\065\001\255\255\255\255\255\255\
\069\001\070\001\021\001\072\001\023\001\024\001\255\255\255\255\
\255\255\255\255\079\001\255\255\255\255\255\255\033\001\034\001\
\035\001\036\001\255\255\038\001\039\001\040\001\041\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\053\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\062\001\255\255\255\255\065\001\255\255\
\255\255\255\255\069\001\070\001\021\001\072\001\023\001\024\001\
\255\255\255\255\255\255\255\255\079\001\255\255\255\255\255\255\
\033\001\034\001\035\001\036\001\255\255\038\001\039\001\040\001\
\041\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\053\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\062\001\255\255\255\255\
\065\001\255\255\255\255\255\255\069\001\070\001\021\001\072\001\
\023\001\024\001\255\255\255\255\255\255\255\255\079\001\255\255\
\255\255\255\255\033\001\034\001\035\001\036\001\255\255\038\001\
\039\001\040\001\041\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\053\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\062\001\
\255\255\255\255\065\001\255\255\255\255\255\255\069\001\070\001\
\021\001\072\001\023\001\024\001\255\255\255\255\255\255\255\255\
\079\001\255\255\255\255\255\255\033\001\034\001\035\001\036\001\
\255\255\038\001\039\001\040\001\041\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\053\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\062\001\255\255\255\255\065\001\255\255\255\255\255\255\
\069\001\070\001\021\001\072\001\023\001\024\001\255\255\255\255\
\255\255\255\255\079\001\255\255\255\255\255\255\033\001\034\001\
\035\001\036\001\255\255\038\001\039\001\040\001\041\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\053\001\255\255\255\255\255\255\255\255\012\001\
\013\001\255\255\255\255\062\001\255\255\255\255\065\001\255\255\
\021\001\255\255\069\001\070\001\025\001\072\001\255\255\028\001\
\029\001\030\001\031\001\032\001\079\001\255\255\035\001\036\001\
\037\001\038\001\039\001\040\001\041\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\012\001\013\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\021\001\255\255\
\255\255\062\001\025\001\255\255\065\001\028\001\029\001\030\001\
\031\001\032\001\255\255\255\255\035\001\036\001\255\255\038\001\
\039\001\040\001\041\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\012\001\013\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\021\001\255\255\255\255\062\001\
\025\001\255\255\065\001\028\001\029\001\030\001\031\001\032\001\
\255\255\255\255\035\001\036\001\255\255\038\001\039\001\040\001\
\041\001\255\255\255\255\016\001\255\255\255\255\255\255\020\001\
\021\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\062\001\035\001\036\001\
\065\001\038\001\039\001\040\001\041\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\053\001\255\255\255\255\056\001\255\255\255\255\255\255\255\255\
\255\255\062\001\255\255\255\255\065\001\255\255\255\255\255\255\
\069\001"

let yynames_const = "\
  "

let yynames_block = "\
  LOAD\000\
  SHOWCONTEXT\000\
  TOP\000\
  REF\000\
  REFTYPE\000\
  LIST\000\
  TAIL\000\
  HEAD\000\
  ISNIL\000\
  CONS\000\
  NIL\000\
  LETREC\000\
  FIX\000\
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
  LAM\000\
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
# 126 "parser.mly"
                                    ( fun _ _   ->  [],emptyctx,emptystore                  )
# 578 "parser.ml"
               : Syntax.context -> Eval.store -> (Syntax.command list * Syntax.context * Eval.store)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> Eval.store -> (Syntax.command list * Syntax.context * Eval.store)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 127 "parser.mly"
                                    ( let file   = _2.v in 
                                      fun ctx s ->  [],ctx,s                                )
# 587 "parser.ml"
               : Syntax.context -> Eval.store -> (Syntax.command list * Syntax.context * Eval.store)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.context -> Eval.store -> (Syntax.command list * Syntax.context * Eval.store)) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 129 "parser.mly"
                                    ( let _,ctx',s' = _1 [] emptystore in pr_ctx ctx';
                                      fun _ _  ->  [],ctx',s'                               )
# 597 "parser.ml"
               : Syntax.context -> Eval.store -> (Syntax.command list * Syntax.context * Eval.store)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> Eval.store -> (Syntax.command list * Syntax.context * Eval.store)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 131 "parser.mly"
                                    ( fun ctx s ->  [],ctx,s                                )
# 605 "parser.ml"
               : Syntax.context -> Eval.store -> (Syntax.command list * Syntax.context * Eval.store)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.context -> Eval.store -> (Syntax.command list * Syntax.context * Eval.store)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'oneREPL) in
    Obj.repr(
# 132 "parser.mly"
                                    ( let _,ev_ctx,s    = _1 [] emptystore in   
                                      let cmds,_      = _2 ev_ctx in 
                                      let ev_ctx',s'   = process_commands ev_ctx s cmds in 
                                      fun _ _  -> [],ev_ctx',s'  )
# 616 "parser.ml"
               : Syntax.context -> Eval.store -> (Syntax.command list * Syntax.context * Eval.store)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 137 "parser.mly"
                                    ( fun ctx ->  let cmd,ctx'    = _1 ctx in [cmd],ctx'  )
# 624 "parser.ml"
               : 'oneREPL))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'oneREPL) in
    Obj.repr(
# 138 "parser.mly"
                                    ( fun ctx ->  let cmd,ctx'    = _1 ctx in 
                                                  let cmds,ctx''  = _3 ctx' in cmd::cmds,ctx''  )
# 634 "parser.ml"
               : 'oneREPL))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 145 "parser.mly"
                                    ( fun ctx ->  [],ctx                                  )
# 641 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    Obj.repr(
# 146 "parser.mly"
                                    ( fun ctx ->  let cmd,ctx  = _1 ctx in 
                                                  let cmds,ctx = _3 ctx in cmd::cmds,ctx  )
# 651 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'TmWrap) in
    Obj.repr(
# 152 "parser.mly"
                                    ( fun ctx ->  let t = _1 ctx in Eval(tmInfo t,t),ctx  )
# 658 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'TyBinder) in
    Obj.repr(
# 153 "parser.mly"
                                    ( fun ctx ->  Bind(_1.i,_1.v,_2 ctx),addname ctx _1.v )
# 666 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 154 "parser.mly"
                                    ( fun ctx ->  Bind(_1.i,_1.v,_2 ctx),addname ctx _1.v )
# 674 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    Obj.repr(
# 156 "parser.mly"
                                    ( fun ctx ->  BindTyVar                               )
# 680 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Ty) in
    Obj.repr(
# 157 "parser.mly"
                                  ( fun ctx ->  BindTyAbb(_2 ctx)                       )
# 688 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Ty) in
    Obj.repr(
# 159 "parser.mly"
                                  ( fun ctx ->  BindTmVar(_2 ctx)                         )
# 696 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 160 "parser.mly"
                                    ( fun ctx ->  BindTmAbb(_2 ctx,None)                  )
# 704 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowTy) in
    Obj.repr(
# 165 "parser.mly"
                                  ( _1                                                  )
# 711 "parser.ml"
               : 'Ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ATy) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowTy) in
    Obj.repr(
# 167 "parser.mly"
                                ( fun ctx ->  TyArr(_1 ctx, _3 ctx)                   )
# 720 "parser.ml"
               : 'ArrowTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATy) in
    Obj.repr(
# 168 "parser.mly"
                                  ( _1                                                  )
# 727 "parser.ml"
               : 'ArrowTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 170 "parser.mly"
                                    ( fun ctx ->  if isbound ctx _1.v 
                                        then  TyVar(name2index _1.i ctx _1.v, ctxlen ctx) 
                                        else  TyId(_1.v)                                  )
# 736 "parser.ml"
               : 'ATy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 173 "parser.mly"
                                  ( _2                                                  )
# 745 "parser.ml"
               : 'ATy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 174 "parser.mly"
                                    ( fun ctx ->  TyTop                                   )
# 752 "parser.ml"
               : 'ATy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 175 "parser.mly"
                                    ( fun ctx ->  TyFloat                                 )
# 759 "parser.ml"
               : 'ATy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 176 "parser.mly"
                                    ( fun ctx ->  TyString                                )
# 766 "parser.ml"
               : 'ATy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 177 "parser.mly"
                                    ( fun ctx ->  TyBool                                  )
# 773 "parser.ml"
               : 'ATy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 178 "parser.mly"
                                    ( fun ctx ->  TyNat                                   )
# 780 "parser.ml"
               : 'ATy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 179 "parser.mly"
                                    ( fun ctx ->  TyUnit                                  )
# 787 "parser.ml"
               : 'ATy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'TyFields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 180 "parser.mly"
                                    ( fun ctx ->  TyRecord(_2 ctx 1)                      )
# 796 "parser.ml"
               : 'ATy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'TyFields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 181 "parser.mly"
                                    ( fun ctx ->  TyVariant(_2 ctx 1)                     )
# 805 "parser.ml"
               : 'ATy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 183 "parser.mly"
                                  ( fun ctx ->  TyList(_2 ctx)                          )
# 814 "parser.ml"
               : 'LTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 185 "parser.mly"
                                  ( fun ctx ->  fun i -> []                             )
# 820 "parser.ml"
               : 'TyFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NETyFields) in
    Obj.repr(
# 186 "parser.mly"
                                    ( _1                                                  )
# 827 "parser.ml"
               : 'TyFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'TyField) in
    Obj.repr(
# 188 "parser.mly"
                                    ( fun ctx ->  fun i -> [_1 ctx i]                     )
# 834 "parser.ml"
               : 'NETyFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'TyField) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NETyFields) in
    Obj.repr(
# 189 "parser.mly"
                                    ( fun ctx ->  fun i -> (_1 ctx i)::(_3 ctx (i+1))     )
# 843 "parser.ml"
               : 'NETyFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Ty) in
    Obj.repr(
# 191 "parser.mly"
                                  ( fun ctx ->  fun i -> (_1.v, _3 ctx)                 )
# 852 "parser.ml"
               : 'TyField))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Ty) in
    Obj.repr(
# 192 "parser.mly"
                                  ( fun ctx ->  fun i -> (string_of_int i, _1 ctx)      )
# 859 "parser.ml"
               : 'TyField))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'TmWrap) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 199 "parser.mly"
                                    ( fun ctx ->  TmLet(_2,_3.v,_5 ctx,_1(addname ctx _3.v)) )
# 870 "parser.ml"
               : 'TmWrap))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Tm) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 200 "parser.mly"
                                    ( fun ctx ->  TmLet(_2,_3.v,_5 ctx,_1(addname ctx _3.v)) )
# 881 "parser.ml"
               : 'TmWrap))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 201 "parser.mly"
                                    ( _1                                                  )
# 888 "parser.ml"
               : 'TmWrap))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTm) in
    Obj.repr(
# 203 "parser.mly"
                                    ( _1                                                  )
# 895 "parser.ml"
               : 'Tm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Tm) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 204 "parser.mly"
                                    ( fun ctx ->  TmLet(_1,_2.v,_4 ctx,_6(addname ctx _2.v)))
# 907 "parser.ml"
               : 'Tm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Tm) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 205 "parser.mly"
                                    ( fun ctx ->  TmLet(_1,"_",_4 ctx,_6(addname ctx"_")) )
# 919 "parser.ml"
               : 'Tm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 207 "parser.mly"
                                    ( fun ctx ->  TmAbs(_1,_2.v,_4 ctx,_6(addname ctx _2.v)))
# 931 "parser.ml"
               : 'Tm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Tm) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Tm) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 208 "parser.mly"
                                    ( fun ctx ->  TmIf(_1,_2 ctx,_4 ctx,_6 ctx)           )
# 943 "parser.ml"
               : 'Tm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string  Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'Ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'Tm) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 210 "parser.mly"
                                    ( fun ctx ->  let ctx' = addname ctx _2.v in 
                                                  TmLet(_1,_2.v,TmFix(_1,TmAbs(_1,_2.v,_4 ctx,_6 ctx')),_8 ctx'))
# 958 "parser.ml"
               : 'Tm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PathTm) in
    Obj.repr(
# 213 "parser.mly"
                                    ( _1                                                  )
# 965 "parser.ml"
               : 'AppTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PathTm) in
    Obj.repr(
# 214 "parser.mly"
                                    ( fun ctx ->  TmTimesfloat(_2,_1 ctx,_3 ctx)          )
# 974 "parser.ml"
               : 'AppTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTm) in
    Obj.repr(
# 215 "parser.mly"
                                    ( fun ctx ->  TmFix(_1, _2 ctx )                      )
# 982 "parser.ml"
               : 'AppTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTm) in
    Obj.repr(
# 216 "parser.mly"
                                    ( fun ctx ->  TmSucc(_1, _2 ctx )                     )
# 990 "parser.ml"
               : 'AppTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTm) in
    Obj.repr(
# 217 "parser.mly"
                                    ( fun ctx ->  TmPred(_1, _2 ctx )                     )
# 998 "parser.ml"
               : 'AppTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTm) in
    Obj.repr(
# 218 "parser.mly"
                                    ( fun ctx ->  TmIsZero(_1, _2 ctx)                    )
# 1006 "parser.ml"
               : 'AppTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTm) in
    Obj.repr(
# 219 "parser.mly"
                                    ( fun ctx ->  let t=_1 ctx in TmApp(tmInfo t,t,_2 ctx))
# 1014 "parser.ml"
               : 'AppTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 221 "parser.mly"
                                    ( fun ctx ->  TmProj(_2, _1 ctx, _3.v)                )
# 1023 "parser.ml"
               : 'PathTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int     Support.Error.withinfo) in
    Obj.repr(
# 222 "parser.mly"
                                    ( fun ctx ->  TmProj(_2, _1 ctx, soi _3.v)            )
# 1032 "parser.ml"
               : 'PathTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AscribeTm) in
    Obj.repr(
# 223 "parser.mly"
                                    ( _1                                                  )
# 1039 "parser.ml"
               : 'PathTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ATm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Ty) in
    Obj.repr(
# 225 "parser.mly"
                                  ( fun ctx ->  TmAscribe(_2,_1 ctx,_3 ctx)             )
# 1048 "parser.ml"
               : 'AscribeTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATm) in
    Obj.repr(
# 226 "parser.mly"
                                    ( _1                                                  )
# 1055 "parser.ml"
               : 'AscribeTm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'TmSeq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 228 "parser.mly"
                                    ( _2                                                  )
# 1064 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 229 "parser.mly"
                                    ( fun ctx ->  TmRecord(_1,_2 ctx 1)                   )
# 1073 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 230 "parser.mly"
                                    ( fun ctx ->  TmVar(_1.i,name2index _1.i ctx _1.v,ctxlen ctx))
# 1080 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string  Support.Error.withinfo) in
    Obj.repr(
# 231 "parser.mly"
                                    ( fun ctx ->  TmString(_1.i,_1.v)                     )
# 1087 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float   Support.Error.withinfo) in
    Obj.repr(
# 232 "parser.mly"
                                    ( fun ctx ->  TmFloat(_1.i,_1.v)                      )
# 1094 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 233 "parser.mly"
                                    ( fun ctx ->  TmUnit(_1)                              )
# 1101 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 234 "parser.mly"
                                    ( fun ctx ->  TmTrue(_1)                              )
# 1108 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 235 "parser.mly"
                                    ( fun ctx ->  TmFalse(_1)                             )
# 1115 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int     Support.Error.withinfo) in
    Obj.repr(
# 236 "parser.mly"
                                    ( fun ctx ->  let rec f = function
                                                      | 0 -> TmZero(_1.i)
                                                      | n -> TmSucc(_1.i,f(n-1))in f _1.v )
# 1124 "parser.ml"
               : 'ATm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 240 "parser.mly"
                                    ( _1                                                  )
# 1131 "parser.ml"
               : 'TmSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Tm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'TmSeq) in
    Obj.repr(
# 241 "parser.mly"
                                    ( fun ctx ->  TmApp(_2,TmAbs(_2,"_",TyUnit,_3(addname ctx"_")),_1 ctx) )
# 1140 "parser.ml"
               : 'TmSeq))
; (fun __caml_parser_env ->
    Obj.repr(
# 244 "parser.mly"
                                    ( fun ctx ->  fun i -> []                             )
# 1146 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 245 "parser.mly"
                                    ( _1                                                  )
# 1153 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Field) in
    Obj.repr(
# 247 "parser.mly"
                                    ( fun ctx -> fun i -> [ _1 ctx i ]                    )
# 1160 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Field) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 248 "parser.mly"
                                    ( fun ctx -> fun i -> (_1 ctx i)::(_3 ctx(i+1))       )
# 1169 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string  Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 250 "parser.mly"
                                    ( fun ctx -> fun i -> (_1.v, _3 ctx)                  )
# 1178 "parser.ml"
               : 'Field))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Tm) in
    Obj.repr(
# 251 "parser.mly"
                                    ( fun ctx -> fun i -> (string_of_int i, _1 ctx)       )
# 1185 "parser.ml"
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
   (Parsing.yyparse yytables 2 lexfun lexbuf : Syntax.context -> Eval.store -> (Syntax.command list * Syntax.context * Eval.store))
