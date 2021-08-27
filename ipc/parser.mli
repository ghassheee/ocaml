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

val toplevel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.context -> (Syntax.command list * Syntax.context)
