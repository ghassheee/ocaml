type token =
  | LOAD of (string  Support.Error.withinfo)
  | SHOWCONTEXT of (Support.Error.info)
  | ALL of (Support.Error.info)
  | SOME of (Support.Error.info)
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
  | BARRCUR of (Support.Error.info)
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
  | LCUR of (Support.Error.info)
  | LCURBAR of (Support.Error.info)
  | LEFTARROW of (Support.Error.info)
  | LPAREN of (Support.Error.info)
  | LSQUARE of (Support.Error.info)
  | LSQUAREBAR of (Support.Error.info)
  | LT of (Support.Error.info)
  | RCUR of (Support.Error.info)
  | RPAREN of (Support.Error.info)
  | RSQUARE of (Support.Error.info)
  | SEMI of (Support.Error.info)
  | SLASH of (Support.Error.info)
  | STAR of (Support.Error.info)
  | TRIANGLE of (Support.Error.info)
  | USCORE of (Support.Error.info)
  | VBAR of (Support.Error.info)
  | NEWLINE of (Support.Error.info)
  | DSEMI of (Support.Error.info)

val toplevel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.context -> (Syntax.command list * Syntax.context)
val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.context -> Eval.store -> (Syntax.command list * Syntax.context * Eval.store)
