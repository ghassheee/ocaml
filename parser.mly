/*  Yacc grammar for the parser. */

%{
open Format
open Support.Error
open Support.Pervasive
open Syntax
open Type
open Eval

let pe = print_endline 
%}


/* REPL Methods */ 

%token <Support.Error.info> SHOWCONTEXT


/* Keyword tokens */
%token <Support.Error.info> LETREC 
%token <Support.Error.info> FIX 

%token <Support.Error.info> WHERE
%token <Support.Error.info> IN
%token <Support.Error.info> LET

%token <Support.Error.info> BOOL
%token <Support.Error.info> NAT

%token <Support.Error.info> SUCC
%token <Support.Error.info> PRED
%token <Support.Error.info> ISZERO

%token <Support.Error.info> LAMBDA
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE

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
/* The returned type of a toplevel is Syntax.command list. */
%start toplevel
%start input 
%type <Syntax.context -> (Syntax.command list * Syntax.context)> input 
%type <Syntax.context -> (Syntax.command list * Syntax.context)> toplevel

%%
/************   REPL   ***************************************************************************/
input :   /* Left Recursion */
    |                                   { fun ctx   ->  [],[]                                   }
    | input SHOWCONTEXT DOUBLESEMI      { let _,ctx' = $1 [] in pr_ctx ctx';
                                          fun ctx   ->  [],ctx'                                 }  
    | input DOUBLESEMI                  { fun ctx   ->  [],ctx                                  } 
    | input oneREPL                     { let _,ev_ctx  = $1 [] in  
                                          let cmds,_    = $2 ev_ctx in 
                                          let ev_ctx'   = process_commands ev_ctx cmds in 
                                          fun ctx   ->  [],ev_ctx'                              } 
oneREPL : 
    | Command DOUBLESEMI                { fun ctx   ->  let cmd,ctx'   = $1 ctx in [cmd],ctx'   } 
    | Command SEMI oneREPL              { fun ctx   ->  let cmd,ctx'   = $1 ctx in 
                                                        let cmds,ctx'' = $3 ctx' in cmd::cmds,ctx''  }
/************  COMPILER  *************************************************************************/
toplevel : /* Right Recursion */                
    | EOF                               { fun ctx   ->  [],ctx                                  } 
    | Command SEMI toplevel             { fun ctx   ->  let cmd,ctx  = $1 ctx in 
                                                        let cmds,ctx = $3 ctx in cmd::cmds,ctx  } 
/************   COMMAND  *************************************************************************/
Command     :   
    | TermWrap                          { fun ctx   ->  let t = $1 ctx in Eval(tmInfo t,t),ctx  }
    | UCID TyBinder                     { fun ctx   ->  Bind($1.i,$1.v,$2 ctx),addname ctx $1.v } 
    | LCID Binder                       { fun ctx   ->  Bind($1.i,$1.v,$2 ctx),addname ctx $1.v } 
TyBinder    :
    |                                   { fun ctx   ->  BindTyVar                               }
    | EQ Type                           { fun ctx   ->  BindTyAbb($2 ctx)                       } 
Binder      : 
    | COLON Type                        { fun ctx   ->  BindTmVar($2 ctx)                       } 
    | EQ Term                           { fun ctx   ->  BindTmAbb($2 ctx,None)                  } 
/************    TYPE    *************************************************************************/

Type :
    | UNIV                              { fun ctx   ->  Universe($1,0)                           }
    | BOOL                              { fun ctx   ->  TmBool($1)                               }
    | NAT                               { fun ctx   ->  TmNat($1)                                }
    | PI Term Term                      { fun ctx   ->  TmApp($1,TmApp($1,TmPi($1),$2 ctx),$3 ctx)}
    | SIGMA Term Term                   { fun ctx   ->  TmApp($1,TmApp($1,TmSigma($1),$2 ctx),$3 ctx)}

/************    TERM    *************************************************************************/
TermWrap    :
    | TermWrap COMMA LCID EQ Term       { fun ctx   ->  TmLet($2,$3.v,$5 ctx,$1(addname ctx $3.v)) }
    | Term     WHERE LCID EQ Term       { fun ctx   ->  TmLet($2,$3.v,$5 ctx,$1(addname ctx $3.v)) }
    | Term                              { $1                                                    } 
Term        :
    | AppTerm                           { $1                                                    }
    | Term DOT LCID                     { fun ctx   ->  TmProj($2, $1 ctx, $3.v)                }
    | LET LCID EQ Term IN Term          { fun ctx   ->  TmLet($1,$2.v,$4 ctx,$6(addname ctx $2.v))}
    | LET USCORE EQ Term IN Term        { fun ctx   ->  TmLet($1,"_",$4 ctx,$6(addname ctx"_")) }
    | LAMBDA LCID COLON Type DOT Term   { fun ctx   ->  TmAbs($1,$2.v,$4 ctx,$6(addname ctx $2.v))}
    | IF Term THEN Term ELSE Term       { fun ctx   ->  TmIf($1,$2 ctx,$4 ctx,$6 ctx)           }
    | LETREC LCID COLON Type EQ Term IN Term
                                        { fun ctx   ->  let ctx' = addname ctx $2.v in 
                                                        TmLet($1,$2.v,TmFix($1,TmAbs($1,$2.v,$4 ctx,$6 ctx')),$8 ctx')}
AppTerm     :
    | ATerm                             { $1                                                    }
    | FIX ATerm                         { fun ctx   ->  TmFix($1, $2 ctx )                      }
    | SUCC ATerm                        { fun ctx   ->  TmSucc($1, $2 ctx )                     }
    | PRED ATerm                        { fun ctx   ->  TmPred($1, $2 ctx )                     }
    | ISZERO ATerm                      { fun ctx   ->  TmIsZero($1, $2 ctx)                    }
ATerm       :                               /* Atomic terms never require extra parentheses */
    | LPAREN TermSeq RPAREN             { $2                                                    }
    | LCID                              { fun ctx   ->  TmVar($1.i,name2index $1.i ctx $1.v,ctxlen ctx) } 
    | UNIT                              { fun ctx   ->  TmUnit($1)                              }
    | TRUE                              { fun ctx   ->  TmTrue($1)                              }
    | FALSE                             { fun ctx   ->  TmFalse($1)                             }
    | INTV                              { fun ctx   ->  let rec f = function
                                                            | 0 -> TmZero($1.i)
                                                            | n -> TmSucc($1.i,f(n-1))in f $1.v }
TermSeq     : 
    | Term                              { $1                                                    } 
    | Term SEMI TermSeq                 { fun ctx   ->  TmApp($2,TmAbs($2,"_",TyUnit,$3(addname ctx"_")),$1 ctx) } 

