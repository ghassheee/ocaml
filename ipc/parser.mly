%{
    open Printf
    open Support
    open Syntax 
%}
%token <Support.info> NOT

%token <Support.info> AND
%token <Support.info> OR
%token <Support.info> ARROW
%token <Support.info> BOT
%token <Support.info> TOP

%token <Support.info> SEMI 
%token <Support.info> DSEMI
%token <Support.info> LPAREN
%token <Support.info> RPAREN

%token <string Support.withinfo> UCID
%token <string Support.withinfo> LCID
%token <string Support.withinfo> STRINGV
%token <Support.info> EOF 

%start toplevel 
%type <Syntax.context -> (Syntax.command list * Syntax.context)> toplevel 

%%
toplevel : /* Right Recursion */
    | EOF                       { fun ctx   ->  [],ctx }
    | Command SEMI toplevel     { fun ctx   ->  let cmd,ctx     = $1 ctx in 
                                                let cmds,ctx    = $3 ctx in 
                                                cmd::cmds,ctx   }

Command : 
    | Prop                      { fun ctx -> let p = $1 ctx in Inversion(propInfo p, p),ctx } 

Prop :
    | Prop ARROW Prop           { fun ctx -> PArr($2, $1 ctx, $3 ctx)                           }
    | AppProp { $1 } 
AppProp : 
    | AppProp AND AppProp       { fun ctx -> PAnd($2, $1 ctx, $3 ctx)                           }
    | AppProp OR AppProp        { fun ctx ->  POr($2, $1 ctx, $3 ctx)                           }
    | AProp                     { $1                                                            } 
AProp : 
    | LPAREN Prop RPAREN        { $2 } 
    | NOT AProp                 { fun ctx -> PArr($1, $2 ctx, PBot($1))                         } 
    | UCID                      { fun ctx ->  
                                  if isbound $1.v ctx
                                      then PVar($1.i, name2index $1.i ctx $1.v, ctxlen ctx)
                                      else PId($1.i, $1.v) } 
    | LCID                      { fun ctx -> PVar($1.i, name2index $1.i (addname ctx $1.v) $1.v, ctxlen ctx)   } 
    | BOT                       { fun ctx -> PBot($1)                                           } 
    | TOP                       { fun ctx -> PTop($1)                                           } 


