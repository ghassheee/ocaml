open Format
open Core
open Support.Pervasive
open Support.Error
open Syntax
open Arg 
open Type

exception NoRuleApplies


let rec eval1 ctx  t = match t with  
    | TmAscribe(fi,v,tyT) 
        when isval ctx v                ->  v
    | TmAscribe(fi,t1,tyT)              ->  TmAscribe(fi,eval1 ctx t1,tyT) 
    | TmLet(fi,x,v1,t2)when isval ctx v1->  pr"E-LETV        : ";pr_tm ctx t;pn(); termSubstTop v1 t2 
    | TmLet(fi,x,t1,t2)                 ->  pr"E-LET         : ";pr_tm ctx t;pn(); TmLet(fi,x,eval1 ctx t1,t2)
(*  | TmLet(fi,x,t1,t2)                 ->  pr"E-LET(SUGAR)  : ";pr_tm ctx t;pn(); 
                                            TmApp(fi,TmAbs(fi,x,typeof ctx t1,t2),t1)  *)
    | TmApp(fi,TmAbs(_,x,tyT11,t12),v2)  (*  (λx:T11.t12) v2  ⇒  [x ↦ v2] t12    E-AppAbs  *)
        when isval ctx v2               ->  pr"E-APPABS      : ";pr_tm ctx t;pn(); termSubstTop v2 t12 
     (*                     t2 → t2'                            *
      *         ------------------------------- E-App2          *
      *                 v1 t2 → v1 t2'                          *) 
    | TmApp(fi,v1,t2)                                          
        when isval ctx v1               ->  pr"E-APP1        : ";pr_tm ctx t;pn(); TmApp(fi,v1,eval1 ctx t2) 
    | TmApp(fi,t1,t2)                   ->  pr"E-APP2        : ";pr_tm ctx t;pn(); TmApp(fi,eval1 ctx t1,t2) 
    | TmIf(_,TmTrue(_),t2,t3)           ->  pr"E-IFTRUE      : ";pr_tm ctx t;pn(); t2
    | TmIf(_,TmFalse(_),t2,t3)          ->  pr"E-IFFLASE     : ";pr_tm ctx t;pn(); t3
    | TmIf(fi,t1,t2,t3)                 ->  pr"E-IF          : ";pr_tm ctx t;pn(); TmIf(fi,eval1 ctx t1, t2, t3)
    | TmSucc(fi,t1)                     ->  pr"E-SUCC        : ";pr_tm ctx t;pn(); TmSucc(fi, eval1 ctx t1)
    | TmPred(_,TmZero(_))               ->  pr"E-PREDZRO     : ";pr_tm ctx t;pn(); TmZero(dummyinfo)
    | TmPred(_,TmSucc(_,nv1)) 
        when isnum ctx nv1              ->  pr"E-PREDSUC     : ";pr_tm ctx t;pn(); nv1
    | TmPred(fi,t1)                     ->  pr"E-PRED        : ";pr_tm ctx t;pn(); TmPred(fi, eval1 ctx t1)
    | TmIsZero(_,TmZero(_))             ->  pr"E-ISZROZRO    : ";pr_tm ctx t;pn(); TmTrue(dummyinfo)
    | TmIsZero(_,TmSucc(_,nv1)) 
        when isnum ctx nv1              ->  pr"E-ISZROSUC    : ";pr_tm ctx t;pn(); TmFalse(dummyinfo)
    | TmIsZero(fi,t1)                   ->  pr"E-ISZRO       : ";pr_tm ctx t;pn(); TmIsZero(fi, eval1 ctx t1)
    | TmRecord(fi,fields)               ->  pr"E-RCD         : ";pr_tm ctx t;pn(); let rec ev_flds = function
        | []                                -> raise NoRuleApplies                  (*                      *)
        | (l,v)::rest when isval ctx v      -> (l,v)::(ev_flds rest)                (*                      *)
        | (l,t)::rest                       -> ev_flds ((l,eval1 ctx t)::rest) in   (*                      *)
                                            TmRecord(fi,ev_flds fields)             (*                      *)
    | TmProj(fi,(TmRecord(f,flds)as v),l)                                           (*      E-ProjRcd       *)
        when isval ctx v                ->  pr"E-PROJRCD     : ";pr_tm ctx t;pn(); 
                                            (try List.assoc l flds with Not_found -> raise NoRuleApplies)
    | TmProj(fi,t1,l)                   ->  pr"E-PROJ        : ";pr_tm ctx t;pn(); TmProj(fi,eval1 ctx t1,l)        
    | _                                 ->  raise NoRuleApplies

let rec eval ctx t =
    try eval ctx (eval1 ctx t) 
    with NoRuleApplies -> t



let rec process_command ctx = function 
    | Eval(fi,t)                ->
            pn();pr_tm ctx t;pn();
            pe"----------------------------------------------------";
            let tyT = typeof ctx t in
            pe"----------------   TYPE CHECKED !   ----------------";
            let t' = eval ctx t in
            pe"----------------   EVAL FINISHED !  ----------------"; 
            pr_ATerm true ctx t'; print_break 1 2; pr ": "; pr_ty tyT; pn();pn(); ctx
    | Bind(fi,x,bind)           ->  pr ("Now, "^x^ " is a variable: "); prbindingty ctx bind; force_newline(); addbinding ctx x bind 


let print_eval ctx cmd      = 
    open_hvbox 0; 
    process_command ctx cmd; 
    print_flush ()




