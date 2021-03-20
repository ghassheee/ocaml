/*  Yacc grammar for the parser. */

%{
open Support
open Syntax
open Format
open Type
open Eval

let rec addbinders tyT = function 
    | []                    -> tyT
    | (tyX,k) :: rest       -> TyAbs(tyX,k,addbinders tyT rest)

%}

/* All token has info type 
 * So the declaration of a token is;
 *  X:  %token IF
 *  O:  %token <info> IF 
 * and sometime, -- in the case of identifiers and 
 * constant values -- more info is provided. */

/* Keyword tokens */
%token <Support.info> BOOL
%token <Support.info> NAT
%token <Support.info> LAMBDA 
%token <Support.info> IF THEN ELSE 
%token <Support.info> TRUE FALSE 
%token <Support.info> SUCC PRED ISZERO

/* Identifier and constant value tokens */
%token <string  Support.withinfo> UCID  /* uppercase-initial */
%token <string  Support.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int     Support.withinfo> INTV
%token <float   Support.withinfo> FLOATV
%token <string  Support.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.info> APOSTROPHE DQUOTE                         /* ' ''         */
%token <Support.info> ARROW LEFTARROW DARROW DDARROW            /* -> <- => ==> */ 
%token <Support.info> BANG HASH STAR TRIANGLE                   /* ! # * Δ      */          
%token <Support.info> LPAREN RPAREN LSQUARE RSQUARE             /* ( ) [ ]      */ 
%token <Support.info> LCURLY RCURLY                             /* { }          */
%token <Support.info> LCURLYBAR BARRCURLY LSQUAREBAR BARRSQUARE /* {| |} [| |]  */ 
%token <Support.info> BARGT                                     /* |>           */ 
%token <Support.info> COLON COLONCOLON COLONEQ COLONHASH        /* : :: := :#   */ 
%token <Support.info> SEMI DSEMI                                /* ; ;;         */ 
%token <Support.info> EQ EQEQ GT LT                             /* = == > <     */ 
%token <Support.info> DOT COMMA USCORE VBAR SLASH               /* . , _ | /    */
%token <Support.info> NEWLINE EOF 


/* The returned type of a toplevel is Syntax.command list. */
%start toplevel
%start input 
%type <Syntax.context -> (Syntax.command list * Syntax.context)> input 
%type <Syntax.context -> (Syntax.command list * Syntax.context)> toplevel

%%

/* ---------------------------------------------------------------------------- */
/* Begin Interpreter */ 
input : /* Left Recursion */
    |                               {   fun ctx ->  [],ctx  }
    | input block                   {   fun ctx ->  let blks,ctx = $1 ctx in     
                                                    let blk,ctx  = $2 ctx in
                                                    (List.append blks blk,ctx) }

block :     /* Left Recursion */                    
    | Command                       {   fun ctx ->  let cmd,ctx     = $1 ctx in [cmd],ctx }
    | block SEMI Command            {   fun ctx ->  let cmd,ctx     = $3 ctx in   
                                                    let cmds,ctx    = $1 ctx in 
                                                    List.append cmds [cmd], ctx }
    | block DSEMI                   {   let cmds,ctx    = $1 emptyctx in 
                                        let _           = List.iter (pr_eval ctx) cmds in 
                                        fun ctx ->  let blk,ctx     = $1 ctx in blk,ctx }  
    | block EOF                     {   fun ctx ->  let blk,ctx     = $1 ctx in blk,ctx } 
/* End Interpreter */



/* ---------------------------------------------------------------------------- */
/* Begin Compiler */
toplevel :  /* Right Recursion */                
    | EOF                           {   fun ctx ->  [],ctx } 
    | Command SEMI toplevel         {   fun ctx ->  let cmd,ctx'    = $1 ctx  in
                                                    let cmds,ctx''  = $3 ctx' in
                                                    cmd::cmds,ctx'' }          



/* ---------------------------------------------------------------------------- */
/* Modules for both Repl and Compiler */ 
Command     :        
    | Term                          {   fun ctx ->  let t = $1 ctx in 
                                                    pr"PARSING THE TERM : ";
                                                    pr_tm ctx t;pn();
                                                    pr"UNDER CONTEXT ";
                                                    pr_ctx ctx; 
                                                    Eval(tmInfo t,t),ctx       }
    | LCID Binder                   {   fun ctx ->  pr"BINDING TERM : ";pn();
                                                    Bind($1.i,$1.v,$2 ctx),addname ctx $1.v     }
    | UCID TyBinder                 {   fun ctx ->  pr"BINDING TYPE : ";pn();
                                                    Bind($1.i,$1.v,$2 ctx),addname ctx $1.v     }
Binder      : 
    | COLON Ty                      {   fun ctx ->  BindTmVar($2 ctx)                           }
TyBinder    : 
    | COLONCOLON Kn                 {   fun ctx ->  BindTyVar(Kn) } 
    | TyAbbArgs EQ Ty               {   fun ctx ->  let b,ctx' = $1 [] ctx in 
                                                pr_ctx ctx'; 
                                        BindTyAbb(addbinders ($3 ctx') b,None)                 }
TyAbbArgs   :
    |                               {   fun b ctx ->    b,ctx      } 
    | UCID BindKn TyAbbArgs         {   fun b ctx -> let ctx'=addname ctx $1.v
                                                     in 
                                                        pr_ctx ctx'; 
                                                        $3 (b @ [($1.v,$2 ctx)]) ctx'          }
BindKn      : 
    |                               {   fun ctx -> Kn                                           }
    | COLONCOLON Kn                 {   $2                                                      }

/* ---------------------------------------------------------------------------- */
Kn        : 
    | ArrowKn                       {   $1          }

ArrowKn   :
    | AKn                           {   $1                                                      }
    | AKn DARROW AKn                {   fun ctx -> KnArr($1 ctx, $3 ctx)                        }
AKn       : 
    | STAR                          {   fun ctx -> Kn                                           }
    | LPAREN Kn RPAREN              {   $2                                                      }

/* ---------------------------------------------------------------------------- */
Ty        : 
    | ArrowTy                       {   $1                                                      }
    | LAMBDA UCID BindKn DOT Ty     {   fun ctx -> let ctx' = addname ctx $2.v in 
                                                    TyAbs($2.v, $3 ctx, $5 ctx')    }
    | ArrowTy                       {   $1                                          } 
ArrowTy   :
    | ATy ARROW ArrowTy             {   fun ctx -> TyArr($1 ctx, $3 ctx)                        }
    | AppTy                         {   $1                                                      }
AppTy     : 
    | AppTy ATy                     {   fun ctx -> TyApp($1 ctx,$2 ctx)                         }
    | ATy                           {   $1                                                      }
ATy       : 
    | UCID                          {   fun ctx ->  pr_ctx ctx;
                                                    let i = name2index $1.i ctx $1.v in 
                                                    pr"var_index i := "; pi i;pn();
                                                    TyVar(i,ctxlen ctx) }
    | LPAREN Ty RPAREN              {   $2                                                      }
    | BOOL                          {   fun ctx -> TyBool                                       }
    | NAT                           {   fun ctx -> TyNat                                        }



/* ---------------------------------------------------------------------------- */
Term        :
    | AppTerm                       {   $1 }
    | LAMBDA LCID COLON Ty DOT Term {   fun ctx -> let ctx1=addname ctx $2.v in 
                                                   let t = TmAbs($1,$2.v,$4 ctx,$6 ctx1) in 
                                                   t}
    | IF Term THEN Term ELSE Term   {   fun ctx -> TmIf($1, $2 ctx, $4 ctx, $6 ctx) }
AppTerm     :
    | ATerm                         {   $1 }
    | SUCC ATerm                    {   fun ctx -> TmSucc($1, $2 ctx ) }
    | PRED ATerm                    {   fun ctx -> TmPred($1, $2 ctx ) }
    | ISZERO ATerm                  {   fun ctx -> TmIsZero($1, $2 ctx) }
    | AppTerm ATerm                 {   fun ctx -> let e1=$1 ctx in TmApp(tmInfo e1,e1,$2 ctx) }
ATerm       :         
    | LPAREN Term RPAREN            {   $2 } 
    | LCID                          {   fun ctx -> TmVar($1.i, name2index $1.i ctx $1.v, ctxlen ctx) } 
    | TRUE                          {   fun ctx -> TmTrue($1) }
    | FALSE                         {   fun ctx -> TmFalse($1) }
    | INTV                          {   fun ctx -> let rec f = function
              0 -> TmZero($1.i)
            | n -> TmSucc($1.i, f (n-1))
          in f $1.v }


