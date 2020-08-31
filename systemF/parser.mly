/*  Yacc grammar for the parser. */

%{
open Support
open Syntax
open Print 
open Type
open Eval
open Interpreter 

%}


/* REPL Methods */ 
%token <string  Support.withinfo> LOAD
%token <Support.info> SHOWCONTEXT

/* Keyword tokens */

%token <Support.info> NIL 
%token <Support.info> ALL
%token <Support.info> SOME
%token <Support.info> TOP 
%token <Support.info> REF 
%token <Support.info> REFTYPE
%token <Support.info> LETREC 
%token <Support.info> FIX 
%token <Support.info> STRING
%token <Support.info> FLOAT
%token <Support.info> TIMESFLOAT
%token <Support.info> CASE
%token <Support.info> OF
%token <Support.info> TAG
%token <Support.info> AS
%token <Support.info> UNIT
%token <Support.info> UNITTYPE
%token <Support.info> WHERE
%token <Support.info> IN
%token <Support.info> LET
%token <Support.info> BOOL
%token <Support.info> NAT
%token <Support.info> SUCC
%token <Support.info> PRED
%token <Support.info> ISZERO
%token <Support.info> LAM
%token <Support.info> IF
%token <Support.info> THEN
%token <Support.info> ELSE
%token <Support.info> TRUE
%token <Support.info> FALSE

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
%token <Support.info> BARRCUR
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
%token <Support.info> LCUR
%token <Support.info> LCURBAR
%token <Support.info> LEFTARROW
%token <Support.info> LPAREN
%token <Support.info> LSQUARE
%token <Support.info> LSQUAREBAR
%token <Support.info> LT
%token <Support.info> RCUR
%token <Support.info> RPAREN
%token <Support.info> RSQUARE
%token <Support.info> SEMI        /* semicolon */ 
%token <Support.info> SLASH
%token <Support.info> STAR
%token <Support.info> TRIANGLE
%token <Support.info> USCORE
%token <Support.info> VBAR
%token <Support.info> NEWLINE
%token <Support.info> DSEMI

/* The returned type of a toplevel is Syntax.command list. */
%start toplevel
%start input 
%type <Syntax.context -> Eval.store -> (Syntax.command list * Syntax.context * Eval.store)> input 
%type <Syntax.context -> (Syntax.command list * Syntax.context)> toplevel

%%


/************   REPL   ****************/

input :   /* Left Recursion */
    |                                   { fun _ _   ->  [],emptyctx,emptystore                      }
    | input SHOWCONTEXT DSEMI           { let _,ctx',s'     = $1 [] emptystore in pr_ctx ctx';
                                          fun _ _   ->  [],ctx',s'                                  }  
    | input DSEMI                       { fun ctx s ->  [],ctx,s                                    } 
    | input oneREPL                     { let _,ev_ctx,s    = $1 [] emptystore in   
                                          let cmds,_        = $2 ev_ctx in 
                                          let ev_ctx',s'    = process_commands ev_ctx s cmds in 
                                          fun _ _   ->  [],ev_ctx',s'                               } 
oneREPL : 
    | Command DSEMI                     { fun ctx ->  let cmd,ctx'    = $1 ctx in [cmd],ctx'        } 
    | Command SEMI oneREPL              { fun ctx ->  let cmd,ctx'    = $1 ctx in 
                                                      let cmds,ctx''  = $3 ctx' in cmd::cmds,ctx''  }


/************  COMPILER  **************/

toplevel : /* Right Recursion */                
    | EOF                               { fun ctx ->  [],ctx                                        } 
    | Command SEMI toplevel             { fun ctx ->  let cmd,ctx  = $1 ctx in 
                                                      let cmds,ctx = $3 ctx in cmd::cmds,ctx        } 


/************   COMMAND  **************/

Command     :   
    | TmWrap                            { fun ctx ->  let t = $1 ctx in Eval(tmInfo t,t),ctx        }
    | UCID TyBinder                     { fun ctx ->  Bind($1.i,$1.v,$2 ctx),addname ctx $1.v       } 
    | LCID Binder                       { fun ctx ->  Bind($1.i,$1.v,$2 ctx),addname ctx $1.v       } 
TyBinder    :
    |                                   { fun ctx ->  BindTyVar                                     }
    | EQ Ty                             { fun ctx ->  BindTyAbb($2 ctx)                             } 
Binder      : 
    | COLON Ty                          { fun ctx ->  BindTmVar($2 ctx)                             }
    | EQ Tm                             { fun ctx ->  BindTmAbb($2 ctx,None)                        } 


/************    TYPE    **************/

Ty        : 
    | ArrTy                             { $1                                                        } 
    | ALL UCID DOT Ty                   { fun ctx ->  TyAll($2.v,$4(addname ctx $2.v))              } 
    | SOME UCID DOT Ty                  { fun ctx ->  TySome($2.v,$4(addname ctx $2.v))             }
ArrTy     :
    | ATy ARROW ArrTy                   { fun ctx ->  TyArr($1 ctx, $3 ctx)                         }
    | ATy                               { $1                                                        } 
ATy       : 
    | UCID                              { fun ctx ->  if isbound ctx $1.v 
                                            then  TyVar(name2index $1.i ctx $1.v, ctxlen ctx)       
                                            else  TyId($1.v)                                        } 
    | LPAREN Ty RPAREN                  { $2                                                        } 
    | TOP                               { fun ctx ->  TyTop                                         } 
    | FLOAT                             { fun ctx ->  TyFloat                                       }
    | STRING                            { fun ctx ->  TyString                                      } 
    | BOOL                              { fun ctx ->  TyBool                                        } 
    | NAT                               { fun ctx ->  TyNat                                         }
    | UNITTYPE                          { fun ctx ->  TyUnit                                        } 
    | LCUR TyFlds RCUR                  { fun ctx ->  TyRecord($2 ctx 1)                            }


TyFlds    :
    | /* Empty Ty */                    { fun ctx ->  fun i -> []                                   } 
    | NETyFlds                          { $1                                                        }
NETyFlds  :
    | TyFld                             { fun ctx ->  fun i -> [$1 ctx i]                           }
    | TyFld COMMA NETyFlds              { fun ctx ->  fun i -> ($1 ctx i)::($3 ctx (i+1))           }
TyFld     : 
    | LCID COLON Ty                     { fun ctx ->  fun i -> ($1.v, $3 ctx)                       } 
    | Ty                                { fun ctx ->  fun i -> (string_of_int i, $1 ctx)            } 




/************    TERM    ************/ 
TmWrap    :
    | TmWrap COMMA LCID EQ Tm           { fun ctx ->  TmLet($2,$3.v,$5 ctx,$1(addname ctx $3.v))    }
    | Tm     WHERE LCID EQ Tm           { fun ctx ->  TmLet($2,$3.v,$5 ctx,$1(addname ctx $3.v))    }
    | Tm                                { $1                                                        } 
Tm        :
    | AppTm                             { $1                                                        }
    | LET LCID EQ Tm IN Tm              { fun ctx ->  TmLet($1,$2.v,$4 ctx,$6(addname ctx $2.v))    }
    | LET USCORE EQ Tm IN Tm            { fun ctx ->  TmLet($1,"_",$4 ctx,$6(addname ctx"_"))       }
    | LAM LCID COLON Ty DOT Tm          { fun ctx ->  TmAbs($1,$2.v,$4 ctx,$6(addname ctx $2.v))    }
    | LAM UCID DOT Tm                   { fun ctx ->  TmTAbs($1,$2.v,$4(addname ctx $2.v))          } 
    | LET LCUR UCID COMMA LCID RCUR EQ Tm IN Tm
                                        { fun ctx ->  let ctx' = addname(addname ctx $3.v)$5.v in 
                                                      TmUnpack($1,$3.v,$5.v,$8 ctx,$10 ctx')        } 
    | IF Tm THEN Tm ELSE Tm             { fun ctx ->  TmIf($1,$2 ctx,$4 ctx,$6 ctx)                 }
    | LETREC LCID COLON Ty EQ Tm IN Tm  { fun ctx ->  let ctx' = addname ctx $2.v in 
                                                      TmLet($1,$2.v,
                                                        TmFix($1,TmAbs($1,$2.v,$4 ctx,$6 ctx')),
                                                        $8 ctx')                                    }
AppTm     :
    | PathTm                            { $1                                                        }
    | PathTm TIMESFLOAT PathTm          { fun ctx ->  TmTimesfloat($2,$1 ctx,$3 ctx)                } 
    | FIX     PathTm                    { fun ctx ->  TmFix($1, $2 ctx )                            }
    | SUCC    PathTm                    { fun ctx ->  TmSucc($1, $2 ctx )                           }
    | PRED    PathTm                    { fun ctx ->  TmPred($1, $2 ctx )                           }
    | ISZERO  PathTm                    { fun ctx ->  TmIsZero($1, $2 ctx)                          }
    | AppTm   PathTm                    { fun ctx ->  TmApp(tmInfo ($1 ctx),$1 ctx,$2 ctx)          }
    | AppTm LSQUARE Ty RSQUARE          { fun ctx ->  TmTApp(tmInfo ($1 ctx),$1 ctx,$3 ctx)         }
PathTm    : 
    | PathTm DOT LCID                   { fun ctx ->  TmProj($2, $1 ctx, $3.v)                      }
    | PathTm DOT INTV                   { fun ctx ->  TmProj($2, $1 ctx, soi $3.v)                  }
    | AscribeTm                         { $1                                                        } 
AscribeTm : 
    | ATm AS Ty                         { fun ctx ->  TmAscribe($2,$1 ctx,$3 ctx)                   }
    | ATm                               { $1                                                        }
ATm       :                       
    | LPAREN TmSeq RPAREN               { $2                                                        }
    | LCUR STAR Ty COMMA Tm RCUR AS Ty  { fun ctx ->  TmPack($1,$3 ctx,$5 ctx,$8 ctx)               } 
    | LCUR Flds RCUR                    { fun ctx ->  TmRecord($1,$2 ctx 1)                         }
    | LCID                              { fun ctx ->  TmVar($1.i,name2index $1.i ctx $1.v,ctxlen ctx)}
    | STRINGV                           { fun ctx ->  TmString($1.i,$1.v)                           }
    | FLOATV                            { fun ctx ->  TmFloat($1.i,$1.v)                            }
    | UNIT                              { fun ctx ->  TmUnit($1)                                    }
    | TRUE                              { fun ctx ->  TmTrue($1)                                    }
    | FALSE                             { fun ctx ->  TmFalse($1)                                   }
    | INTV                              { fun ctx ->  let rec f = function
                                                          | 0 -> TmZero($1.i)
                                                          | n -> TmSucc($1.i,f(n-1))in f $1.v       }
TmSeq     : 
    | Tm                                { $1                                                        } 
    | Tm SEMI TmSeq                     { fun ctx ->  TmApp($2,TmAbs($2,"_",TyUnit,$3(addname ctx"_")),$1 ctx) } 

Flds      : 
    | /* empty */                       { fun ctx -> fun i -> []                                    }
    | NEFlds                            { $1                                                        }
NEFlds    : 
    | Fld                               { fun ctx -> fun i -> [ $1 ctx i ]                          }
    | Fld COMMA NEFlds                  { fun ctx -> fun i -> ($1 ctx i)::($3 ctx(i+1))             }
Fld       : 
    | LCID EQ Tm                        { fun ctx -> fun i -> ($1.v, $3 ctx)                        }
    | Tm                                { fun ctx -> fun i -> (string_of_int i, $1 ctx)             }
