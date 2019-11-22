open Format
open Core
open Support.Pervasive
open Support.Error
open Syntax
open Arg 
open Type

exception NoRuleApplies


let rec eval1 ctx = function 
    | TmAscribe(fi,v,tyT) 
        when isval ctx v                ->  v
    | TmAscribe(fi,t,tyT)               ->  TmAscribe(fi,eval1 ctx t,tyT) 
    | TmLet(fi,x,t1,t2)                 ->  TmApp(fi,TmAbs(fi,x,typeof ctx t1,t2),t1)  

    (*  (λx:T11.t12) v2     ⇒   [x ↦ v2] t12    E-AppAbs        *)
    | TmApp(fi,TmAbs(_,x,tyT11,t12),v2)                        
        when isval ctx v2               ->  termSubstTop v2 t12 
     (*                     t2 → t2'                            *
      *         ------------------------------- E-App2          *
      *                 v1 t2 → v1 t2'                          *) 
    | TmApp(fi,v1,t2)                                          
        when isval ctx v1               ->  TmApp(fi,v1,eval1 ctx t2) 
    | TmApp(fi,t1,t2)                   ->  TmApp(fi,eval1 ctx t1,t2)               (*      E-App1          *)
    | TmIf(_,TmTrue(_),t2,t3)           ->  t2
    | TmIf(_,TmFalse(_),t2,t3)          ->  t3
    | TmIf(fi,t1,t2,t3)                 ->  let t1' = eval1 ctx t1 in TmIf(fi, t1', t2, t3)
    | TmSucc(fi,t1)                     ->  let t1' = eval1 ctx t1 in TmSucc(fi, t1')
    | TmPred(_,TmZero(_))               ->  TmZero(dummyinfo)
    | TmPred(_,TmSucc(_,nv1)) 
        when isnum ctx nv1              ->  nv1
    | TmPred(fi,t1)                     ->  TmPred(fi, eval1 ctx t1)
    | TmIsZero(_,TmZero(_))             ->  TmTrue(dummyinfo)
    | TmIsZero(_,TmSucc(_,nv1)) 
        when isnum ctx nv1              ->  TmFalse(dummyinfo)
    | TmIsZero(fi,t1)                   ->  let t1' = eval1 ctx t1 in TmIsZero(fi, t1')
    | TmRecord(fi,fields)               ->  let rec ev_flds = function              (*      E-Rcd           *)
        | []                                -> raise NoRuleApplies                  (*                      *)
        | (l,v)::rest when isval ctx v      -> (l,v)::(ev_flds rest)                (*                      *)
        | (l,t)::rest                       -> ev_flds ((l,eval1 ctx t)::rest) in   (*                      *)
                                            TmRecord(fi,ev_flds fields)             (*                      *)
    | TmProj(fi,(TmRecord(f,flds)as v),l)                                           (*      E-ProjRcd       *)
        when isval ctx v                ->  (try List.assoc l flds with Not_found -> raise NoRuleApplies)
    | TmProj(fi,t,l)                    ->  TmProj(fi,eval1 ctx t,l)                (*      E-Proj          *)
    | _                                 ->  raise NoRuleApplies

let rec eval ctx t =
    try eval ctx (eval1 ctx t) 
    with NoRuleApplies -> t



let rec process_command ctx = function 
    | Eval(fi,t)                ->  
            let tyT = typeof ctx t in
            let _ = print_endline "TYPE CHECKED !" in  
            pr_ATerm true ctx (eval ctx t); 
            print_break 1 2; pr ": "; pr_ty tyT; force_newline(); ctx
    | Bind(fi,x,bind)           ->  pr ("Now, "^x^ " is a variable: "); prbindingty ctx bind; force_newline(); addbinding ctx x bind 


let print_eval ctx cmd      = 
    open_hvbox 0; 
    process_command ctx cmd; 
    print_flush ()




