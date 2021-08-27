type token =
  | NOT of (Support.info)
  | AND of (Support.info)
  | OR of (Support.info)
  | ARROW of (Support.info)
  | BOT of (Support.info)
  | TOP of (Support.info)
  | SEMI of (Support.info)
  | DSEMI of (Support.info)
  | LPAREN of (Support.info)
  | RPAREN of (Support.info)
  | UCID of (string Support.withinfo)
  | LCID of (string Support.withinfo)
  | STRINGV of (string Support.withinfo)
  | EOF of (Support.info)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Printf
    open Support
    open Syntax 
# 24 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* NOT *);
  258 (* AND *);
  259 (* OR *);
  260 (* ARROW *);
  261 (* BOT *);
  262 (* TOP *);
  263 (* SEMI *);
  264 (* DSEMI *);
  265 (* LPAREN *);
  266 (* RPAREN *);
  267 (* UCID *);
  268 (* LCID *);
  269 (* STRINGV *);
    0 (* EOF *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\003\000\003\000\004\000\004\000\004\000\
\005\000\005\000\005\000\005\000\005\000\005\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\001\000\003\000\003\000\001\000\
\003\000\002\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\013\000\014\000\000\000\011\000\012\000\
\001\000\015\000\000\000\000\000\000\000\008\000\010\000\000\000\
\000\000\000\000\000\000\000\000\009\000\002\000\000\000\000\000\
\000\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\013\000\014\000"

let yysindex = "\005\000\
\001\000\000\000\002\255\000\000\000\000\002\255\000\000\000\000\
\000\000\000\000\021\255\005\255\022\255\000\000\000\000\000\255\
\001\000\002\255\002\255\002\255\000\000\000\000\005\255\022\255\
\022\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\255\011\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\251\254\012\255\
\013\255"

let yygindex = "\000\000\
\012\000\000\000\250\255\007\000\028\000"

let yytablesize = 269
let yytable = "\016\000\
\009\000\004\000\003\000\018\000\004\000\001\000\004\000\005\000\
\018\000\021\000\006\000\023\000\007\000\008\000\005\000\006\000\
\007\000\005\000\006\000\007\000\005\000\006\000\007\000\019\000\
\020\000\024\000\025\000\017\000\022\000\003\000\015\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\000\000\000\000\000\000\004\000\005\000\000\000\
\000\000\006\000\000\000\007\000\008\000"

let yycheck = "\006\000\
\000\000\007\001\001\001\004\001\010\001\001\000\005\001\006\001\
\004\001\010\001\009\001\018\000\011\001\012\001\004\001\004\001\
\004\001\007\001\007\001\007\001\010\001\010\001\010\001\002\001\
\003\001\019\000\020\000\007\001\017\000\007\001\003\000\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\255\255\255\255\005\001\006\001\255\255\
\255\255\009\001\255\255\011\001\012\001"

let yynames_const = "\
  "

let yynames_block = "\
  NOT\000\
  AND\000\
  OR\000\
  ARROW\000\
  BOT\000\
  TOP\000\
  SEMI\000\
  DSEMI\000\
  LPAREN\000\
  RPAREN\000\
  UCID\000\
  LCID\000\
  STRINGV\000\
  EOF\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 29 "parser.mly"
                                ( fun ctx   ->  [],ctx )
# 177 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.context -> (Syntax.command list * Syntax.context)) in
    Obj.repr(
# 30 "parser.mly"
                                ( fun ctx   ->  let cmd,ctx     = _1 ctx in 
                                                let cmds,ctx    = _3 ctx in 
                                                cmd::cmds,ctx   )
# 188 "parser.ml"
               : Syntax.context -> (Syntax.command list * Syntax.context)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Prop) in
    Obj.repr(
# 35 "parser.mly"
                                ( fun ctx -> let p = _1 ctx in Inversion(propInfo p, p),ctx )
# 195 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Prop) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Prop) in
    Obj.repr(
# 38 "parser.mly"
                                ( fun ctx -> PArr(_2, _1 ctx, _3 ctx)                           )
# 204 "parser.ml"
               : 'Prop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppProp) in
    Obj.repr(
# 39 "parser.mly"
              ( _1 )
# 211 "parser.ml"
               : 'Prop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AppProp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AppProp) in
    Obj.repr(
# 41 "parser.mly"
                                ( fun ctx -> PAnd(_2, _1 ctx, _3 ctx)                           )
# 220 "parser.ml"
               : 'AppProp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AppProp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AppProp) in
    Obj.repr(
# 42 "parser.mly"
                                ( fun ctx ->  POr(_2, _1 ctx, _3 ctx)                           )
# 229 "parser.ml"
               : 'AppProp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AProp) in
    Obj.repr(
# 43 "parser.mly"
                                ( _1                                                            )
# 236 "parser.ml"
               : 'AppProp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Prop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 45 "parser.mly"
                                ( _2 )
# 245 "parser.ml"
               : 'AProp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AProp) in
    Obj.repr(
# 46 "parser.mly"
                                ( fun ctx -> PArr(_1, _2 ctx, PBot(_1))                         )
# 253 "parser.ml"
               : 'AProp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.withinfo) in
    Obj.repr(
# 47 "parser.mly"
                                ( fun ctx ->  
                                  if isbound _1.v ctx
                                      then PVar(_1.i, name2index _1.i ctx _1.v, ctxlen ctx)
                                      else PId(_1.i, _1.v) )
# 263 "parser.ml"
               : 'AProp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.withinfo) in
    Obj.repr(
# 51 "parser.mly"
                                ( fun ctx -> PVar(_1.i, name2index _1.i (addname ctx _1.v) _1.v, ctxlen ctx)   )
# 270 "parser.ml"
               : 'AProp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 52 "parser.mly"
                                ( fun ctx -> PBot(_1)                                           )
# 277 "parser.ml"
               : 'AProp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.info) in
    Obj.repr(
# 53 "parser.mly"
                                ( fun ctx -> PTop(_1)                                           )
# 284 "parser.ml"
               : 'AProp))
(* Entry toplevel *)
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
