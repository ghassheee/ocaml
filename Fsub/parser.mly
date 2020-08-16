/*  Yacc grammar for the parser. */

%{
open Format
open Support.Error
open Support.Pervasive
open Syntax
open Print 
open Type
open Eval
open Interpreter 

let pe = print_endline 
%}


/* REPL Methods */ 
%token <string  Support.Error.withinfo> LOAD
%token <Support.Error.info> SHOWCONTEXT

/* Keyword tokens */

%token <Support.Error.info> SUB

%token <Support.Error.info> ALL
%token <Support.Error.info> SOME

%token <Support.Error.info> TOP 

%token <Support.Error.info> REF 
%token <Support.Error.info> REFTYPE

%token <Support.Error.info> LIST
%token <Support.Error.info> TAIL
%token <Support.Error.info> HEAD
%token <Support.Error.info> ISNIL
%token <Support.Error.info> CONS
%token <Support.Error.info> NIL

%token <Support.Error.info> LETREC 
%token <Support.Error.info> FIX 

%token <Support.Error.info> STRING
%token <Support.Error.info> FLOAT
%token <Support.Error.info> TIMESFLOAT

%token <Support.Error.info> CASE
%token <Support.Error.info> OF
%token <Support.Error.info> TAG

%token <Support.Error.info> AS

%token <Support.Error.info> UNIT
%token <Support.Error.info> UNITTYPE

%token <Support.Error.info> WHERE
%token <Support.Error.info> IN
%token <Support.Error.info> LET

%token <Support.Error.info> BOOL
%token <Support.Error.info> NAT

%token <Support.Error.info> SUCC
%token <Support.Error.info> PRED
%token <Support.Error.info> ISZERO

%token <Support.Error.info> LAM
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
%token <Support.Error.info> BARRCUR
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
%token <Support.Error.info> LCUR
%token <Support.Error.info> LCURBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCUR
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI        /* semicolon */ 
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR
%token <Support.Error.info> NEWLINE
%token <Support.Error.info> DSEMI
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
    | OTy                               { fun ctx ->  BindTyVar($1 ctx)                             }
    | EQ Ty                             { fun ctx ->  BindTyAbb($2 ctx)                             } 
Binder      : 
    | COLON Ty                          { fun ctx ->  BindTmVar($2 ctx)                             }
    | EQ Tm                             { fun ctx ->  BindTmAbb($2 ctx,None)                        } 

OTy         :
    |                                   { fun ctx ->  TyTop                                         } 
    | SUB Ty                            { fun ctx ->  $2 ctx                                        } 



/************    TYPE    **************/

Ty        : 
    | ArrTy                             { $1                                                        } 
    | ALL  UCID OTy DOT Ty              { fun ctx ->  TyAll ($2.v,$3 ctx, $5(addname ctx $2.v))     } 
    | SOME UCID OTy DOT Ty              { fun ctx ->  TySome($2.v,$3 ctx, $5(addname ctx $2.v))     }
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
    | LAM UCID OTy DOT Tm               { fun ctx ->  TmTAbs($1,$2.v,$3 ctx,$5(addname ctx $2.v))   } 
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
