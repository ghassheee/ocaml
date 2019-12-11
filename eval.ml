open Support.Pervasive
open Support.Error
open Syntax
open Type

exception NoRuleApplies

let rec eval1 ctx  t = let p str = pr str;pr_tm ctx t;pn() in match t with  
    | App(_,App(_,Pi(_),t1),t2)             ->  p"E-PI          : "; raise NoRuleApplies 
    | App(_,Abs(_,x,t,u),v)when isval ctx v ->  p"E-APPABS      : "; tmSubstTop v u 
    | App(fi,v,t)when isval ctx v           ->  p"E-APP1        : "; App(fi,v,eval1 ctx t) 
    | App(fi,t1,t2)                         ->  p"E-APP2        : "; App(fi,eval1 ctx t1,t2) 
    | If(_,True(_),t2,t3)                   ->  p"E-IFTRUE      : "; t2
    | If(_,False(_),t2,t3)                  ->  p"E-IFFLASE     : "; t3
    | If(fi,t1,t2,t3)                       ->  p"E-IF          : "; If(fi,eval1 ctx t1, t2, t3)
    | Succ(fi,t1)                           ->  p"E-SUCC        : "; Succ(fi, eval1 ctx t1)
    | Pred(_,Zero(_))                       ->  p"E-PREDZRO     : "; Zero(dummyinfo)
    | Pred(_,Succ(_,n))when isnum ctx n     ->  p"E-PREDSUC     : "; n
    | Pred(fi,t1)                           ->  p"E-PRED        : "; Pred(fi, eval1 ctx t1)
    | IsZero(_,Zero(_))                     ->  p"E-ISZROZRO    : "; True(dummyinfo)
    | IsZero(_,Succ(_,n))when isnum ctx n   ->  p"E-ISZROSUC    : "; False(dummyinfo)
    | IsZero(fi,t1)                         ->  p"E-ISZRO       : "; IsZero(fi, eval1 ctx t1)
    | _                                     ->  raise NoRuleApplies

let rec eval ctx t =
    try eval ctx (eval1 ctx t) 
    with NoRuleApplies -> t


(*------------ Binding ------------*)

let evalbind ctx            = function
    | BindAbb(t,tyT)            ->  BindAbb(eval ctx t,tyT) 
    | bind                      ->  bind

let checkbind fi ctx        = function 
    | BindAbb(t,None)           ->  BindAbb(t, Some(typeof ctx t))
    | BindAbb(t,Some(tyT))      ->  if tyeqv ctx(typeof ctx t)tyT then BindAbb(t,Some(tyT))else error fi"TyAbbErr"
    | bind                      ->  bind  

let rec process_command ctx = function 
    | Eval(fi,t)                ->
            pr_tm ctx t;pn();
            pe"----------------------------------------------------";
            let tyT = typeof ctx t in
            pe"----------------   TYPE CHECKED !   ----------------";
            let t' = eval ctx t in
            pe"----------------   EVAL FINISHED !  ----------------"; 
            pr_ATerm true ctx t'; pb 1 2; pr ": "; pr_tm ctx tyT; pn();pn(); ctx
    | Bind(fi,x,bind)           ->  
            let bind' = checkbind fi ctx bind in 
            let bind'' = evalbind ctx bind' in 
            pr x;pr" ";prbindty ctx bind'';pn();addbind ctx x bind'' 

let rec process_commands ctx = function 
    | []                        ->  ctx 
    | cmd::cmds                 ->  oobox0;
                                    let ctx' = process_command ctx cmd in 
                                    Format.print_flush();
                                    process_commands ctx' cmds 

