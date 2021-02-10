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
    | TmApp(_,TmAbs(_,x,_,t),v)when isval ctx v ->  termSubstTop v t 
    | TmApp(fi,v1,t2) when isval ctx v1         ->  TmApp(fi,v1,eval1 ctx t2) 
    | TmApp(fi,t1,t2)                           ->  TmApp(fi,eval1 ctx t1,t2) 
    | TmIf(_,TmTrue(_),t2,t3)                   ->  t2
    | TmIf(_,TmFalse(_),t2,t3)                  ->  t3
    | TmIf(fi,t1,t2,t3)                         ->  let t1' = eval1 ctx t1 in TmIf(fi,t1',t2,t3)
    | TmSucc(fi,t1)                             ->  let t1' = eval1 ctx t1 in TmSucc(fi,t1')
    | TmPred(_,TmZero(_))                       ->  TmZero(dummyinfo)
    | TmPred(_,TmSucc(_,n))when isnum ctx n     ->  n
    | TmPred(fi,t1)                             ->  TmPred(fi, eval1 ctx t1)
    | TmIsZero(_,TmZero(_))                     ->  TmTrue(dummyinfo)
    | TmIsZero(_,TmSucc(_,n))when isnum ctx n   ->  TmFalse(dummyinfo)
    | TmIsZero(fi,t1)                           ->  let t1' = eval1 ctx t1 in TmIsZero(fi, t1')
    | _                                         ->  raise NoRuleApplies

let rec eval ctx t          = try eval ctx (eval1 ctx t) with    
    | NoRuleApplies             -> t

let rec process_command ctx = function 
    | Eval(fi,t)                ->  let tyT = typeof ctx t in 
                                    prATerm true ctx (eval ctx t);
                                    pb 1 2;pr ": ";pr_ty tyT;pn(); 
                                    ctx
    | Bind(fi,x,bind)           ->  pr("Now, "^x^" is a variable: "); 
                                    prbindty ctx bind; pn(); 
                                    addbind ctx x bind 


let pr_eval ctx cmd         = 
    open_hvbox 0; 
    let _ = process_command ctx cmd in 
    print_flush ()




