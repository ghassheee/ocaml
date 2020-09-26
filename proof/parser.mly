/*  Yacc grammar for the parser. */

%{
open Support
open Syntax
open Format
open Type
open Eval

%}

/* All token has info type 
 * So the declaration of a token is;
 *  X:  %token IF
 *  O:  %token <info> IF 
 * and sometime, -- in the case of identifiers and 
 * constant values -- more info is provided. */

/* Keyword tokens */
%token <Support.info> BOOL

%token <Support.info> LAMBDA
%token <Support.info> IF
%token <Support.info> THEN
%token <Support.info> ELSE
%token <Support.info> TRUE
%token <Support.info> FALSE
%token <Support.info> SUCC
%token <Support.info> PRED
%token <Support.info> ISZERO

/* Identifier and constant value tokens */
%token <string  Support.withinfo> UCID  /* uppercase-initial */
%token <string  Support.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int     Support.withinfo> INTV
%token <float   Support.withinfo> FLOATV
%token <string  Support.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.info> APOSTROPHE
%token <Support.info> DQUOTE
%token <Support.info> ARROW
%token <Support.info> BANG
%token <Support.info> BARGT
%token <Support.info> BARRCURLY
%token <Support.info> BARRSQUARE
%token <Support.info> COLON
%token <Support.info> COLONCOLON
%token <Support.info> COLONEQ
%token <Support.info> COLONHASH
%token <Support.info> COMMA
%token <Support.info> DARROW
%token <Support.info> DDARROW
%token <Support.info> DOT
%token <Support.info> EOF
%token <Support.info> EQ
%token <Support.info> EQEQ
%token <Support.info> EXISTS
%token <Support.info> GT
%token <Support.info> HASH
%token <Support.info> LCURLY
%token <Support.info> LCURLYBAR
%token <Support.info> LEFTARROW
%token <Support.info> LPAREN
%token <Support.info> LSQUARE
%token <Support.info> LSQUAREBAR
%token <Support.info> LT
%token <Support.info> RCURLY
%token <Support.info> RPAREN
%token <Support.info> RSQUARE
%token <Support.info> SEMI        /* semicolon */ 
%token <Support.info> SLASH
%token <Support.info> STAR
%token <Support.info> TRIANGLE
%token <Support.info> USCORE
%token <Support.info> VBAR
%token <Support.info> NEWLINE
%token <Support.info> DOUBLESEMI
%token <Support.info> HOGE
/* The returned type of a toplevel is Syntax.command list. */
%start toplevel
%start input 
%type <Syntax.context -> (Syntax.command list * Syntax.context)> input 
%type <Syntax.context -> (Syntax.command list * Syntax.context)> toplevel

%%
/* Begin Interpreter */ 
input : /* Left Recursion */
    |                         { fun ctx   -> [],ctx  }
    | input block             { fun ctx   -> 
                                let blks,ctx = $1 ctx in let blk,ctx = $2 ctx in (List.append blks blk, ctx) } 
block :     /* Left Recursion */                    
    | Command                 { fun ctx   -> let cmd,ctx = $1 ctx in [cmd],ctx }             
    | block SEMI Command      { fun ctx   -> let cmd,ctx = $3 ctx in let cmds,ctx  = $1 ctx in 
                                List.append cmds [cmd], ctx }  
    | block HOGE              { let cmds,ctx = $1 emptycontext in let _ = List.iter (print_eval ctx) cmds in 
                                fun ctx   -> let blk,ctx = $1 ctx in blk,ctx }  
    | block EOF               { fun ctx   -> let blk,ctx = $1 ctx in blk,ctx } 
/* End Interpreter */

/* Begin Compiler */
toplevel :  /* Right Recursion */                
    | EOF                     { fun ctx   -> [],ctx } 
    | Command SEMI toplevel   { fun ctx   -> 
          let cmd,ctx   = $1 ctx in 
          let cmds,ctx  = $3 ctx in 
          cmd::cmds,ctx } 
/* End Compliler */ 

/* Modules both for Interpreter and for Compiler */ 
Command :       /* A top-level command */ 
    | Term                          { fun ctx   -> let t = $1 ctx in Eval(tmInfo t,t),ctx }
    | LCID Binder                   { fun ctx   -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) } 
Binder  : 
    | COLON Type                    { fun ctx   -> VarBind($2 ctx) } 

Type : 
    | ArrowType                     { $1 } 
AType : 
    | LPAREN Type RPAREN            { $2 } 
    | BOOL                          { fun ctx   -> TyBool } 
ArrowType :
    | AType ARROW ArrowType         { fun ctx   -> TyArr($1 ctx, $3 ctx) }
    | AType                         { $1 } 

Term :
    | AppTerm                       { $1 }
    | LAMBDA LCID DOT Term          { print_endline "hoge";  
        fun ctx -> 
            let t2 = $4 (addname ctx $2.v) in 
            TmAbs($1, $2.v, typeof ctx t2, t2) }
    | LAMBDA LCID COLON Type DOT Term
    { print_endline "hoge";  fun ctx -> 
        let ctx1 = addname ctx $2.v in 
        TmAbs($1, $2.v, $4 ctx, $6 ctx1) }
    | IF Term THEN Term ELSE Term   { fun ctx -> TmIf($1, $2 ctx, $4 ctx, $6 ctx) }
AppTerm :
    | ATerm                         { $1 }
    | AppTerm ATerm                 { print_endline "fuga";fun ctx -> let e1 = $1 ctx in TmApp(tmInfo e1,e1,$2 ctx) }
    | SUCC ATerm                    { fun ctx -> TmSucc($1, $2 ctx ) }
    | PRED ATerm                    { fun ctx -> TmPred($1, $2 ctx ) }
    | ISZERO ATerm                  { fun ctx -> TmIsZero($1, $2 ctx) }
ATerm :         /* Atomic terms are ones that never require extra parentheses */
    | LPAREN Term RPAREN            { print_endline "term"; $2 } 
    | LCID                          { fun ctx -> TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) } 
    | TRUE                          { fun ctx -> TmTrue($1) }
    | FALSE                         { fun ctx -> TmFalse($1) }
    | INTV                          { fun ctx -> let rec f = function
              0 -> TmZero($1.i)
            | n -> print_endline "succ"; TmSucc($1.i, f (n-1))
          in f $1.v }


