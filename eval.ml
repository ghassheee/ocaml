open Support.Pervasive
open Support.Error
open Syntax
open Type

exception NoRuleApplies
exception EvalFloatFailure

let rec evalF1 = function
    | TmFloat(_,a)          -> a 
    | TmTimesfloat(_,a,b)   -> evalF1 a *. evalF1 b
    | _                     -> raise EvalFloatFailure

let rec eval1 ctx  t = match t with  
    | TmTag(fi,l,t1,tyT1)               ->  pr"E-TAG         : ";pr_tm ctx t;pn(); TmTag(fi,l,eval1 ctx t1,tyT1)
    | TmVar(fi,n,_)                     ->  pr"E-VAR         : ";pr_tm ctx t;pn(); (match getbind fi ctx n with
        | BindTmAbb(t,_)                    -> t
        | _                                 -> raise NoRuleApplies) 
    | TmCase(fi,TmTag(_,l,v,_),cases) when isval ctx v  ->  
                                            pr"E-CASETAGV    : ";pr_tm ctx t;pn();
                                            (try let (x,t)= List.assoc l cases in  tmSubstTop v t
                                            with Not_found -> raise NoRuleApplies)
    | TmCase(fi,t0,cases)               ->  pr"E-CASE        : ";pr_tm ctx t;pn(); TmCase(fi,eval1 ctx t0,cases) 
    | TmAscribe(fi,v,_)when isval ctx v ->  pr"E-ASCRIBEVAR  : ";pr_tm ctx t;pn(); v
    | TmAscribe(fi,t1,tyT)              ->  pr"E-ASCRIBE     : ";pr_tm ctx t;pn(); TmAscribe(fi,eval1 ctx t1,tyT) 
    | TmLet(fi,x,v1,t2)when isval ctx v1->  pr"E-LETV        : ";pr_tm ctx t;pn(); tmSubstTop v1 t2 
    | TmLet(fi,x,t1,t2)                 ->  pr"E-LET         : ";pr_tm ctx t;pn(); TmLet(fi,x,eval1 ctx t1,t2)
(*  | TmLet(fi,x,t1,t2)                 ->  pr"E-LET(SUGAR)  : ";pr_tm ctx t;pn(); 
                                            TmApp(fi,TmAbs(fi,x,typeof ctx t1,t2),t1)  *)
    | TmApp(fi,TmAbs(_,x,tyT11,t12),v2)  (*  (λx:T11.t12) v2  ⇒  [x ↦ v2] t12    E-AppAbs  *)
        when isval ctx v2               ->  pr"E-APPABS      : ";pr_tm ctx t;pn(); tmSubstTop v2 t12 
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
    | TmTimesfloat(fi,t1,t2) as tm      ->  pr"E-TIMESFLOAT  : ";pr_tm ctx t;pn(); TmFloat(fi,evalF1 tm)  
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


(*------------ Binding ------------*)

let evalbind ctx            = function
    | BindTmAbb(t,tyT)          ->  BindTmAbb(eval ctx t,tyT) 
    | bind                      ->  bind

let checkbind fi ctx        = function 
    | BindTmAbb(t,None)         ->  BindTmAbb(t, Some(typeof ctx t))
    | BindTmAbb(t,Some(tyT))    ->  if tyeqv ctx(typeof ctx t)tyT then BindTmAbb(t,Some(tyT))else error fi"TyAbbErr"
    | bind                      ->  bind  

let rec process_command ctx = function 
    | Eval(fi,t)                ->
            pn();pr_tm ctx t;pn();
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
    | []                        -> () 
    | cmd::cmds                 -> oobox0;let ctx' = process_command ctx cmd in Format.print_flush();process_commands ctx' cmds 



