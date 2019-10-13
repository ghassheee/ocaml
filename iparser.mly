/*  Yacc grammar for the parser. */

%{
open Support.Error
open Support.Pervasive
open Syntax
open Core 
open Format
open Evaluator

%}

/* All token has info type 
 * So the declaration of a token is;
 *  X:  %token IF
 *  O:  %token <info> IF 
 * and sometime, -- in the case of identifiers and 
 * constant values -- more info is provided. */

/* Keyword tokens */
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> SUCC
%token <Support.Error.info> PRED
%token <Support.Error.info> ISZERO

/* Identifier and constant value tokens */
%token <string  Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string  Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int     Support.Error.withinfo> INTV
%token <float   Support.Error.withinfo> FLOATV
%token <string  Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI        /* semicolon */ 
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR
%token <Support.Error.info> NEWLINE
%token <Support.Error.info> DOUBLESEMI
%token <Support.Error.info> HOGE
/* The returned type of a toplevel is Syntax.command list. */
%start toplevel

%type <Syntax.command list> toplevel
%%

toplevel :  /* Left Recursion */                
                       { [] } 
  | toplevel block     { List.append $1 $2 } 

block :     /* Left Recursion */                    
    Command                 { let _ = print_eval $1 in [$1] }             
  | block SEMI Command      { let _ = print_eval $3 in List.append $1 [$3] }  
  | block HOGE              { $1 }  
  | block EOF               { $1 } 

Command :       /* A top-level command */ 
  | Term                            { let t     = $1 in Eval(tmInfo t,t) }
Term :
    AppTerm                         { $1 }
  | IF Term THEN Term ELSE Term     { TmIf($1, $2, $4, $6) }
AppTerm :
    ATerm                           { $1 }
  | SUCC ATerm                      { TmSucc($1, $2) }
  | PRED ATerm                      { TmPred($1, $2) }
  | ISZERO ATerm                    { TmIsZero($1, $2) }
ATerm :         /* Atomic terms are ones that never require extra parentheses */
    LPAREN Term RPAREN              { $2 } 
  | TRUE                            { TmTrue($1) }
  | FALSE                           { TmFalse($1) }
  | INTV                            { let rec f n = match n with
              0 -> TmZero($1.i)
            | n -> TmSucc($1.i, f (n-1))
          in f $1.v }


/* Right Recursion */
/* 
block :
    EOF                     { [] }
  | Command HOGE            { let _ = print_eval $1 in [$1] } 
  | Command SEMI block      { let cmd = $1 in let cmds = $3 in let _ = print_eval cmd;flush stdout in  cmd::cmds } 
*/

