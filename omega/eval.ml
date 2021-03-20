open Format
open Support
open Syntax
open Arg 
open Type

exception NoRuleApplies

let rec isnum ctx = function 
    | TmZero(_)                     -> true
    | TmSucc(_,t1)                  -> isnum ctx t1
    | _                             -> false

let rec isval ctx = function 
    | TmAbs(_,_,_,_)                -> true
    | TmTrue(_)                     -> true
    | TmFalse(_)                    -> true
    | t when isnum ctx t            -> true
    | _                             -> false

let rec eval1 ctx = function 
    | TmApp(_,TmAbs(_,x,_,t),v)when isval ctx v ->  tmSubstTop v t 
    | TmApp(fi,v1,t2) when isval ctx v1         ->  TmApp(fi,v1,eval1 ctx t2) 
    | TmApp(fi,t1,t2)                           ->  TmApp(fi,eval1 ctx t1,t2) 
    | TmIf(_,TmTrue(_),t2,t3)                   ->  t2
    | TmIf(_,TmFalse(_),t2,t3)                  ->  t3
    | TmIf(fi,t1,t2,t3)                         ->  let t1' = eval1 ctx t1 in TmIf(fi,t1',t2,t3)
    | TmSucc(fi,t1)                             ->  let t1' = eval1 ctx t1 in TmSucc(fi,t1')
    | TmPred(_,TmZero(_))                       ->  TmZero(dummy)
    | TmPred(_,TmSucc(_,n))when isnum ctx n     ->  n
    | TmPred(fi,t1)                             ->  TmPred(fi, eval1 ctx t1)
    | TmIsZero(_,TmZero(_))                     ->  TmTrue(dummy)
    | TmIsZero(_,TmSucc(_,n))when isnum ctx n   ->  TmFalse(dummy)
    | TmIsZero(fi,t1)                           ->  let t1' = eval1 ctx t1 in TmIsZero(fi, t1')
    | _                                         ->  raise NoRuleApplies

let rec eval ctx t          = try eval ctx (eval1 ctx t) with    
    | NoRuleApplies             -> t

let rec process_command ctx = function 
    | Eval(fi,t)                ->  
            pn();
            pr_tm ctx t; pn();
            pr_ctx ctx; 
            pe"-----------------------------------------------------";
            let tyT = typeof ctx t in           
            pe"-----------------  TYPE CHECKED !  ------------------";
            let t'  = eval ctx t in                                
            pe"------------------   EVALUATED !  -------------------";
            pr_tm ctx t';pb 1 2;pr" : ";pr_ty ctx tyT;pn();pn();
            ctx
    | Bind(fi,x,bind)           ->  
                                    let ctx' = addbind ctx x bind in 
                                    pr"FOUND NEW BINDING  "; 
                                    pr(x);prbindty ctx' bind; pn(); 
                                    pr"BINDED CONTEXT ";
                                    pr_ctx ctx'; 
                                    ctx'

let rec process_commands ctx = function 
    | []                        ->  ctx
    | cmd::cmds                 ->  open_hvbox 0;
                                    let ctx' = process_command ctx cmd in 
                                    process_commands ctx' cmds




let pr_eval ctx cmd         = 
    open_hvbox 0; 
    let _ = process_command ctx cmd in 
    print_flush ()




