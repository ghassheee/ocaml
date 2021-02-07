%{
open Format
open Support
open Syntax
open Type
open Eval
%}

/* REPL Methods */ 
%token <Support.info>   SHOWCONTEXT

/* Keywords */
%token <Support.info>   UNIV PI SIGMA 
                        LETREC FIX 
                        LET IN WHERE
                        BOOL NAT
                        SUCC PRED ZERO ISZERO 
                        LAMBDA
                        IF THEN ELSE TRUE FALSE 

/* Identifiers and Constants */
%token <int    Support.withinfo>    INTV
%token <float  Support.withinfo>    FLOATV
%token <string Support.withinfo>    UCID    /* uppercase */ 
                                    LCID    /* lowercase */ 
                                    STRINGV

/* Symbols */
%token <Support.info>   APOSTROPHE
                        ARROW DARROW DDARROW
                        BANG
                        DQUOTE
                        LTBAR BARGT 
                        LCURLYBAR BARRCURLY BARRSQUARE LSQUAREBAR
                        COMMA DOT
                        EQ GT LT EQEQ
                        EXISTS ALL 
                        HASH
                        LCURLY RCURLY  
                        LPAREN RPAREN 
                        LSQUARE RSQUARE 
                        LEFTARROW
                        COLON COLONCOLON COLONEQ COLONHASH
                        SEMI DSEMI
                        SLASH STAR TRIANGLE
                        USCORE VBAR
                        NEWLINE EOF 

%start compiler
%start repl 
%type <Syntax.context -> (Syntax.command list * Syntax.context)> repl 
%type <Syntax.context -> (Syntax.command list * Syntax.context)> compiler

%%
/************   REPL   ****************/

repl :   /* Left Recursion */
    |                               { fun ctx   ->  [],[]                                           }
    | repl SHOWCONTEXT DSEMI        { let _,ctx' =  $1 [] in pr_ctx ctx';
                                          fun ctx   ->  [],ctx'                                     }  
    | repl DSEMI                    { fun ctx   ->  [],ctx                                          } 
    | repl line                     { let _,ectx =  $1 [] in  
                                      let cmds,_ =  $2 ectx in 
                                      let ectx'  =  process_cmds ectx cmds in 
                                      fun ctx   ->  [],ectx'                                        } 
line : 
    | Cmd DSEMI                     { fun ctx   ->  let cmd,ctx'   = $1 ctx in [cmd],ctx'           } 
    | Cmd SEMI line                 { fun ctx   ->  let cmd,ctx'   = $1 ctx in 
                                                        let cmds,ctx'' = $3 ctx' in cmd::cmds,ctx'' }


/************  COMPILER  **************/

compiler : /* Right Recursion */                
    | EOF                           { fun ctx   ->  [],ctx                                          } 
    | Cmd SEMI compiler             { fun ctx   ->  let cmd,ctx  = $1 ctx in 
                                                        let cmds,ctx = $3 ctx in cmd::cmds,ctx      } 


/************   COMMAND  **************/

Cmd         :   
    | Tm                            { fun ctx   ->  let t = $1 ctx in Eval(tmInfo t,t),ctx          }
    | UCID TyBind                   { fun ctx   ->  Bind($1.i,$1.v,$2 ctx),addname ctx $1.v         } 
    | LCID Bind                     { fun ctx   ->  Bind($1.i,$1.v,$2 ctx),addname ctx $1.v         } 
TyBind      :
    |                               { fun ctx   ->  BindTyVar                                       }
    | EQ Ty                         { fun ctx   ->  BindTyAbb($2 ctx)                               } 
Bind        : 
    | COLON Ty                      { fun ctx   ->  BindVar($2 ctx)                                 } 
    | EQ Tm                         { fun ctx   ->  BindAbb($2 ctx,None)                            } 


/************    TYPE    **************/

Ty :
    | LCID                          { fun ctx   ->  Var($1.i,name2index $1.i ctx $1.v,ctxlen ctx)   }
    | UNIV                          { fun ctx   ->  Univ($1,0)                                      }
    | BOOL                          { fun ctx   ->  Bool($1)                                        }
    | NAT                           { fun ctx   ->  Nat($1)                                         }
    | Ty ARROW Ty                   { fun ctx   ->  Ap($2,Ap($2,Pi($2),$1 ctx),Lam($2,"_",$1 ctx,$3 ctx))}
    | PI Ty Tm                      { fun ctx   ->  Ap($1,Ap($1,Pi($1),$2 ctx),$3 ctx)              }
    | SIGMA Ty Tm                   { fun ctx   ->  Ap($1,Ap($1,Sgm($1),$2 ctx),$3 ctx)             }
    | LPAREN Ty RPAREN              { $2 } 


/************    TERM    **************/

Tm        :
    | ApTm                          { $1                                                            }
    | LAMBDA LCID COLON Ty DOT Tm   { fun ctx   ->  Lam($1,$2.v,$4 ctx,$6(addname ctx $2.v))        }
    | IF Tm THEN Tm ELSE Tm         { fun ctx   ->  If($1,$2 ctx,$4 ctx,$6 ctx)                     }
ApTm     :
    | ATm                           { $1                                                            }
    | ApTm ATm                      { fun ctx   ->  Ap(tmInfo($1 ctx),$1 ctx,$2 ctx)                }
    | PI Ty Tm                      { fun ctx   ->  Ap($1,Ap($1,Pi($1),$2 ctx),$3 ctx)}
    | SIGMA Ty Tm                   { fun ctx   ->  Ap($1,Ap($1,Sgm($1),$2 ctx),$3 ctx)}
    | SUCC ATm                      { fun ctx   ->  Succ($1, $2 ctx )                               }
    | PRED ATm                      { fun ctx   ->  Pred($1, $2 ctx )                               }
    | ISZERO ATm                    { fun ctx   ->  IsZero($1, $2 ctx)                              }
ATm       :          
    | LPAREN TmSeq RPAREN           { $2                                                            }
    | LCID                          { fun ctx   ->  Var($1.i,name2index $1.i ctx $1.v,ctxlen ctx)   }
    | UNIV                          { fun ctx   ->  Univ($1,0)                                      }
    | BOOL                          { fun ctx   ->  Bool($1)                                        }
    | NAT                           { fun ctx   ->  Nat($1)                                         }
    | TRUE                          { fun ctx   ->  True($1)                                        }
    | FALSE                         { fun ctx   ->  False($1)                                       }
    | INTV                          { fun ctx   ->  let rec f = function
                                                            | 0 -> Zero($1.i)
                                                            | n -> Succ($1.i,f(n-1))in f $1.v       }
TmSeq     : 
    | Tm                            { $1                                                            } 

