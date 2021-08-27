type token =
  | NEWLINE
  | EOF
  | LPAREN
  | RPAREN
  | EQ
  | PLUS
  | MINUS
  | MUL
  | AND
  | OR
  | NUM of (int)
  | VAR of (string)
  | A
  | D
  | M
  | MD
  | AM
  | AD
  | AMD
  | AT
  | ONE
  | ZERO
  | BANG
  | COLON
  | SEMI
  | JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.commands * Support.tbl
