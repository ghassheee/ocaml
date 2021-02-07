/*  Yacc grammar for the parser. */

%{
open Support
open Syntax
open Format
open Type
open Eval
%}


/* Keyword tokens */
%token <Support.info>   BOOL
                        LAMBDA
                        IF
                        THEN
                        ELSE
                        TRUE
                        FALSE
                        SUCC
                        PRED
                        ISZERO

/* Identifier and constant value tokens */
%token <string  Support.withinfo>   UCID  /* uppercase */
                                    LCID  /* lowercase */
                                    STRINGV
%token <int     Support.withinfo>   INTV
%token <float   Support.withinfo>   FLOATV

/* Symbolic tokens */
%token <Support.info> APOSTROPHE
 DQUOTE
 ARROW LEFTARROW 
 BARGT 
 BANG STAR SLASH HASH 
 BARRCURLY LCURLYBAR
 BARRSQUARE LSQUAREBAR
 COLON COLONCOLON COLONEQ COLONHASH COMMA
 DARROW DDARROW DOT
 EXISTS 
 EQ EQEQ GT LT 
 LCURLY RCURLY
 LPAREN RPAREN
 LSQUARE RSQUARE
 SEMI DSEMI
 TRIANGLE
 USCORE VBAR
 NEWLINE EOF 


%start  repl  line
%type <Syntax.context -> (Syntax.command list * Syntax.context)>  repl  line

%%

/***** REPL *****/ 
repl :     /* Left Recursion */
    |                           { fun ctx   ->  [],ctx  }
    | repl block                { fun ctx   ->  let blks,ctx    = $1 ctx in 
                                                let blk,ctx     = $2 ctx in 
                                                List.append blks blk, ctx  } 
block :     /* Left Recursion */                    
    | Command                   { fun ctx   ->  let cmd,ctx = $1 ctx in [cmd],ctx }             
    | block SEMI Command        { fun ctx   ->  let cmd,ctx = $3 ctx in let cmds,ctx  = $1 ctx in 
                                                List.append cmds [cmd], ctx }  
    | block DSEMI               { let cmds,ctx  = $1 emptycontext in 
                                  let _         = List.iter (print_eval ctx) cmds in 
                                  fun ctx   -> let blk,ctx = $1 ctx in blk,ctx }  
    | block EOF                 { fun ctx   -> let blk,ctx = $1 ctx in blk,ctx } 



/***** Compiler *****/
line :  /* Right Recursion */                
    | EOF                     { fun ctx   ->    [],ctx } 
    | Command SEMI line       { fun ctx   ->    let cmd,ctx   = $1 ctx in 
                                                let cmds,ctx  = $3 ctx in 
                                                cmd::cmds,ctx } 

/*
Thm : 
    | Lemma LCID COLON Type         { } 
Proof : 
    | Proof DOT Tactics QED DOT     { } 
Tactics : 
    | Tactic DOT Tactics            { }

Tactic : 
    | AppTac
AppTac :
    | ATac 
    | AppTac ATac 
ATac : 
    | APPLY Type 
    | AUTO
    | ASSUMP 
    | EXACT Tm 
    | INTRO LCID
    | INTROS Vars

Vars : 
    | LCID Vars 
*/ 


/*************  Modules both for Interpreter and for Compiler ***************************/ 
Command :       /* A top-level command */ 
    | Tm                            { fun ctx   ->  Eval(tmInfo ($1 ctx),$1 ctx),ctx }
    | LCID Binder                   { fun ctx   ->  Bind($1.i,$1.v,$2 ctx), addname ctx $1.v } 
    | UCID Binder                   { fun ctx   ->  Bind($1.i,$1.v,$2 ctx), addname ctx $1.v } 
Binder  : 
    | COLON Type                    { fun ctx   ->  VarBind($2 ctx) } 

Type : 
    | ArrowType                     { $1 } 
AType : 
    | LPAREN Type RPAREN            { $2 } 
    | BOOL                          { fun ctx   ->  TyBool } 
ArrowType :
    | AType ARROW ArrowType         { fun ctx   ->  TyArr($1 ctx, $3 ctx) }
    | AType                         { $1 } 

Tm :
    | AppTm                         { $1 }
    | LAMBDA LCID DOT Tm            { fun ctx   ->  let t2 = $4 (addname ctx $2.v) in 
                                                    TmAbs($1, $2.v, typeof ctx t2, t2) }
    | LAMBDA LCID COLON Type DOT Tm { fun ctx   ->  let ctx1 = addname ctx $2.v in 
                                                    TmAbs($1, $2.v, $4 ctx, $6 ctx1) }
    | IF Tm THEN Tm ELSE Tm         { fun ctx   ->  TmIf($1, $2 ctx, $4 ctx, $6 ctx) }
AppTm :
    | ATm                           { $1 }
    | AppTm ATm                     { fun ctx   ->  let e1 = $1 ctx in TmApp(tmInfo e1,e1,$2 ctx) }
    | SUCC ATm                      { fun ctx   ->  TmSucc($1, $2 ctx ) }
    | PRED ATm                      { fun ctx   ->  TmPred($1, $2 ctx ) }
    | ISZERO ATm                    { fun ctx   ->  TmIsZero($1, $2 ctx) }
ATm :         /* Atomic terms are ones that never require extra parentheses */
    | LPAREN Tm RPAREN              { $2 } 
    | LCID                          { fun ctx   ->  TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) } 
    | TRUE                          { fun ctx   ->  TmTrue($1) }
    | FALSE                         { fun ctx   ->  TmFalse($1) }
    | INTV                          { fun ctx   ->  let rec f = function
                                                    | 0 -> TmZero($1.i)
                                                    | n -> TmSucc($1.i,f(n-1))      in f $1.v }


