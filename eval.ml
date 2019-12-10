open Support.Pervasive
open Support.Error
open Syntax
open Type

exception NoRuleApplies
exception EvalFloatFailure


let rec eval1 ctx  t = let p str = pr str;pr_tm ctx t;pn() in match t with  
    | TmLet(fi,x,v1,t2)when isval ctx v1->  p"E-LETV        : "; tmSubstTop v1 t2 
    | TmLet(fi,x,t1,t2)                 ->  p"E-LET         : "; TmLet(fi,x,eval1 ctx t1,t2)
    | TmApp(fi,TmApp(_,TmPi(_),t1),t2) ->  p"E-PI          : "; raise NoRuleApplies 
    | TmApp(fi,TmAbs(_,x,tyT11,t12),v2)  (*  (λx:T11.t12) v2  ⇒  [x ↦ v2] t12    E-AppAbs  *)
        when isval ctx v2               ->  p"E-APPABS      : "; tmSubstTop v2 t12 
     (*                     t2 → t2'                            *
      *         ------------------------------- E-App2          *
      *                 v1 t2 → v1 t2'                          *) 
    | TmApp(fi,v1,t2)                                          
        when isval ctx v1               ->  p"E-APP1        : "; TmApp(fi,v1,eval1 ctx t2) 
    | TmApp(fi,t1,t2)                   ->  p"E-APP2        : "; TmApp(fi,eval1 ctx t1,t2) 
    | TmIf(_,TmTrue(_),t2,t3)           ->  p"E-IFTRUE      : "; t2
    | TmIf(_,TmFalse(_),t2,t3)          ->  p"E-IFFLASE     : "; t3
    | TmIf(fi,t1,t2,t3)                 ->  p"E-IF          : "; TmIf(fi,eval1 ctx t1, t2, t3)
    | TmSucc(fi,t1)                     ->  p"E-SUCC        : "; TmSucc(fi, eval1 ctx t1)
    | TmPred(_,TmZero(_))               ->  p"E-PREDZRO     : "; TmZero(dummyinfo)
    | TmPred(_,TmSucc(_,nv1)) 
        when isnum ctx nv1              ->  p"E-PREDSUC     : "; nv1
    | TmPred(fi,t1)                     ->  p"E-PRED        : "; TmPred(fi, eval1 ctx t1)
    | TmIsZero(_,TmZero(_))             ->  p"E-ISZROZRO    : "; TmTrue(dummyinfo)
    | TmIsZero(_,TmSucc(_,nv1)) 
        when isnum ctx nv1              ->  p"E-ISZROSUC    : "; TmFalse(dummyinfo)
    | TmIsZero(fi,t1)                   ->  p"E-ISZRO       : "; TmIsZero(fi, eval1 ctx t1)
    | _                                 ->  raise NoRuleApplies

let rec eval ctx t =
    try eval ctx (eval1 ctx t) 
    with NoRuleApplies -> t


(*------------ Binding ------------*)

let evalbind ctx            = function
    | BindTmAbb(t,tyT)          ->  BindTmAbb(eval ctx t,tyT) 
    | bind                      ->  bind

let checkbind fi ctx        = function 
    | BindTmAbb(t,None)         ->  BindTmAbb(t, Some(typeof ctx t))
    | BindTmAbb(t,Some(tyT))    ->  if (=)(typeof ctx t)tyT then BindTmAbb(t,Some(tyT))else error fi"TyAbbErr"
    | bind                      ->  bind  

let rec process_command ctx = function 
    | Eval(fi,t)                ->
            pn();
            pe"----------------------------------------------------";
            let tyT = typeof ctx t in
            pe"----------------   TYPE CHECKED !   ----------------";
            let t' = eval ctx t in
            pe"----------------   EVAL FINISHED !  ----------------"; 
            pr_ATerm true ctx t'; pb 1 2; pr ": "; pr_ty ctx tyT; pn();pn(); ctx
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



