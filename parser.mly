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

/* All token has info type 
 * So the declaration of a token is;
 *      %token <info> IF 
 * and sometime, -- in the case of identifiers and 
 * constant values -- more info is provided. */

/* Keyword tokens */
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
    |                                   { fun ctx   ->  [],ctx                                  }
    | input DOUBLESEMI                  { fun ctx   ->  [],ctx                                  } 
    | input oneREPL                     { let cmds,ctx = $2[]in process_commands emptyctx cmds; 
                                          fun ctx   ->  [],ctx                                  } 
oneREPL : 
    | Command DOUBLESEMI                { fun ctx   ->  let cmd,ctx = $1 ctx in [cmd],ctx } 
    | Command SEMI oneREPL              { fun ctx   ->  let cmd,ctx = $1 ctx in 
                                                        let cmds,ctx = $3 ctx in cmd::cmds,ctx  }
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
    | COLON Type                        { fun ctx   ->  BindTmVar($2 ctx)                         } 
    | EQ Term                           { fun ctx   ->  BindTmAbb($2 ctx,None)                  } 
/************    TYPE    *************************************************************************/
Type        : 
    | ArrowType                         { $1                                                    } 
ArrowType   :
    | AType ARROW ArrowType             { fun ctx   ->  TyArr($1 ctx, $3 ctx)                   }
    | AType                             { $1                                                    } 
AType       : 
    | UCID                              { fun ctx   ->  if isnamebound ctx $1.v then 
                                                        TyVar(name2index $1.i ctx $1.v, ctxlen ctx) else 
                                                            error $1.i"Type Not Found"            } 
    | LPAREN Type RPAREN                { $2                                                    } 
    | FLOAT                             { fun ctx   ->  TyFloat                                 }
    | STRING                            { fun ctx   ->  TyString                                } 
    | BOOL                              { fun ctx   ->  TyBool                                  } 
    | NAT                               { fun ctx   ->  TyNat                                   }
    | UNITTYPE                          { fun ctx   ->  TyUnit                                  } 
    | LCURLY TyFields RCURLY            { fun ctx   ->  TyRecord($2 ctx 1)                      }
    | LT TyFields GT                    { fun ctx   ->  TyVariant($2 ctx 1)                     } 
TyFields    :
    | /* Empty Type */                  { fun ctx   ->  fun i -> []                             } 
    | NETyFields                        { $1                                                    }
NETyFields  :
    | TyField                           { fun ctx   ->  fun i -> [$1 ctx i]                     }
    | TyField COMMA NETyFields          { fun ctx   ->  fun i -> ($1 ctx i)::($3 ctx (i+1))     }
TyField     : 
    | LCID COLON Type                   { fun ctx   ->  fun i -> ($1.v, $3 ctx)                 } 
    | Type                              { fun ctx   ->  fun i -> (string_of_int i, $1 ctx)      } 
/************    TERM    *************************************************************************/
TermWrap    :
    | TermWrap COMMA LCID EQ Term       { fun ctx   ->  TmLet($2,$3.v,$5 ctx,$1(addname ctx $3.v)) }
    | Term     WHERE LCID EQ Term       { fun ctx   ->  TmLet($2,$3.v,$5 ctx,$1(addname ctx $3.v)) }
    | Term                              { $1                                                    } 
Cases       :
    | Case                              { fun ctx   ->  [$1 ctx]                                }
    | Case VBAR Cases                   { fun ctx   ->  ($1 ctx)::($3 ctx)                      }
Case        :
    | LT LCID EQ LCID GT DARROW AppTerm { fun ctx   ->  ($2.v,($4.v,$7(addname ctx $4.v)))      } 
Term        :
    | AppTerm                           { $1                                                    }
    | CASE Term OF Cases                { fun ctx   ->  TmCase($1,$2 ctx,$4 ctx)                }
    | Term COLON Term                   { fun ctx   ->  TmLet($2, "_", $3 ctx, $1 ctx)          } 
    | Term DOT LCID                     { fun ctx   ->  TmProj($2, $1 ctx, $3.v)                }
    | LET LCID EQ Term IN Term          { fun ctx   ->  TmLet($1,$2.v,$4 ctx,$6(addname ctx $2.v))}
    | LET USCORE EQ Term IN Term        { fun ctx   ->  TmLet($1,"_",$4 ctx,$6(addname ctx"_")) }
    | LAMBDA LCID COLON Type DOT Term   { fun ctx   ->  TmAbs($1,$2.v,$4 ctx,$6(addname ctx $2.v))}
    | IF Term THEN Term ELSE Term       { fun ctx   ->  TmIf($1,$2 ctx,$4 ctx,$6 ctx)           }
AppTerm     :
    | AscribeTerm                       { $1                                                    }
    | ATerm TIMESFLOAT ATerm            { fun ctx   ->  TmTimesfloat($2,$1 ctx,$3 ctx)          } 
    | SUCC ATerm                        { fun ctx   ->  TmSucc($1, $2 ctx )                     }
    | PRED ATerm                        { fun ctx   ->  TmPred($1, $2 ctx )                     }
    | ISZERO ATerm                      { fun ctx   ->  TmIsZero($1, $2 ctx)                    }
    | AppTerm AscribeTerm               { fun ctx   ->  let t=$1 ctx in TmApp(tmInfo t,t,$2 ctx)}
AscribeTerm : 
    | ATerm AS Type                     { fun ctx   ->  TmAscribe($2,$1 ctx,$3 ctx)             }
    | ATerm                             { $1                                                    }
ATerm       :                               /* Atomic terms never require extra parentheses */
    | LPAREN Term RPAREN                { $2                                                    }
    | LCURLY Fields RCURLY              { fun ctx   ->  TmRecord($1,$2 ctx 1)                   }
    | LT LCID EQ Term GT AS Type        { fun ctx   ->  TmTag($1,$2.v,$4 ctx,$7 ctx)            } 
    | LCID                              { fun ctx   ->  TmVar($1.i,name2index $1.i ctx $1.v,ctxlen ctx) } 
    | STRINGV                           { fun ctx   ->  TmString($1.i,$1.v)                     }
    | FLOATV                            { fun ctx   ->  TmFloat($1.i,$1.v)                      }
    | UNIT                              { fun ctx   ->  TmUnit($1)                              }
    | TRUE                              { fun ctx   ->  TmTrue($1)                              }
    | FALSE                             { fun ctx   ->  TmFalse($1)                             }
    | INTV                              { fun ctx   ->  let rec f = function
                                                            | 0 -> TmZero($1.i)
                                                            | n -> TmSucc($1.i,f(n-1))in f $1.v }
Fields      : 
    | /* empty */                       { fun ctx   ->  fun i -> []                             }
    | NEFields                          { $1                                                    }
NEFields    : 
    | Field                             { fun ctx   -> fun i -> [ $1 ctx i ]                    }
    | Field COMMA NEFields              { fun ctx   -> fun i -> ($1 ctx i)::($3 ctx(i+1))       }
Field       : 
    | LCID EQ Term                      { fun ctx   -> fun i -> ($1.v, $3 ctx)                  }
    | Term                              { fun ctx   -> fun i -> (string_of_int i, $1 ctx)       }
