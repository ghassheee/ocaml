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
%token <Support.Error.info> UNIV
%token <Support.Error.info> PI
%token <Support.Error.info> SIGMA


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
    | COLON Type                        { fun ctx   ->  BindVar($2 ctx)                       } 
    | EQ Term                           { fun ctx   ->  BindAbb($2 ctx,None)                  } 
/************    TYPE    *************************************************************************/

Type :
    | LCID                              { fun ctx   ->  Var($1.i,name2index $1.i ctx $1.v,ctxlen ctx)}
    | UNIV                              { fun ctx   ->  Universe($1,0)                           }
    | BOOL                              { fun ctx   ->  Bool($1)                               }
    | NAT                               { fun ctx   ->  Nat($1)                                }
    | Type ARROW Type                   { fun ctx   ->  App($2,App($2,Pi($2),$1 ctx),Abs($2,"_",$1 ctx,$3 ctx))}
    | PI Type Term                      { fun ctx   ->  App($1,App($1,Pi($1),$2 ctx),$3 ctx)}
    | SIGMA Type Term                   { fun ctx   ->  App($1,App($1,Sigma($1),$2 ctx),$3 ctx)}
    | LPAREN Type RPAREN                { $2 } 

/************    TERM    *************************************************************************/
TermWrap    :
    | Term                              { $1                                                    } 
Term        :
    | AppTerm                           { $1                                                    }
    | LAMBDA LCID COLON Type DOT Term   { fun ctx   ->  Abs($1,$2.v,$4 ctx,$6(addname ctx $2.v))}
    | IF Term THEN Term ELSE Term       { fun ctx   ->  If($1,$2 ctx,$4 ctx,$6 ctx)           }
AppTerm     :
    | ATerm                             { $1                                                    }
    | AppTerm ATerm                     { fun ctx   ->  App(tmInfo($1 ctx),$1 ctx,$2 ctx)     }
    | PI Type Term                      { fun ctx   ->  App($1,App($1,Pi($1),$2 ctx),$3 ctx)}
    | SIGMA Type Term                   { fun ctx   ->  App($1,App($1,Sigma($1),$2 ctx),$3 ctx)}
    | SUCC ATerm                        { fun ctx   ->  Succ($1, $2 ctx )                     }
    | PRED ATerm                        { fun ctx   ->  Pred($1, $2 ctx )                     }
    | ISZERO ATerm                      { fun ctx   ->  IsZero($1, $2 ctx)                    }
ATerm       :                               /* Atomic terms never require extra parentheses */
    | LPAREN TermSeq RPAREN             { $2                                                    }
    | LCID                              { fun ctx   ->  Var($1.i,name2index $1.i ctx $1.v,ctxlen ctx) } 
    | UNIV                              { fun ctx   ->  Universe($1,0)                           }
    | BOOL                              { fun ctx   ->  Bool($1)                               }
    | NAT                               { fun ctx   ->  Nat($1)                                }
    | TRUE                              { fun ctx   ->  True($1)                              }
    | FALSE                             { fun ctx   ->  False($1)                             }
    | INTV                              { fun ctx   ->  let rec f = function
                                                            | 0 -> Zero($1.i)
                                                            | n -> Succ($1.i,f(n-1))in f $1.v }
TermSeq     : 
    | Term                              { $1                                                    } 

