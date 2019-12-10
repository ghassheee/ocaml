open Support.Error
open Support.Pervasive
open Syntax

exception NoRuleApplies


(* ----------------------------------  
let istyabb ctx i           = match getbind dummyinfo ctx i with 
    | BindTyAbb(_)                  -> true
    | _                             -> false

let gettyabb ctx i          = match getbind dummyinfo ctx i with 
    | BindTyAbb(tyT)                -> tyT
    | _                             -> raise NoRuleApplies 

let rec computety ctx tyT   = match tyT with 
    | TyVar(i,_)when istyabb ctx i  -> gettyabb ctx i 
    | _                             -> raise NoRuleApplies

let rec simplifyty ctx tyT  = try simplifyty ctx(computety ctx tyT) with NoRuleApplies -> tyT
*) 

(*
let rec tyeqv ctx tyS tyT   = 
    let tyS = simplifyty ctx tyS in 
    let tyT = simplifyty ctx tyT in 
    match (tyS,tyT) with 
    | (TyVar(i,_),_) when istyabb ctx i -> tyeqv ctx(gettyabb ctx i)tyT
    | (_,TyVar(i,_)) when istyabb ctx i -> tyeqv ctx tyS(gettyabb ctx i)
    | TyVar(i,_),TyVar(j,_)             -> i=j
    | TyArr(tyS1,tyS2),TyArr(tyT1,tyT2) -> tyeqv ctx tyS1 tyT1 && tyeqv ctx tyS2 tyT2
    | TyRecord(flds1),TyRecord(flds2)   ->  List.length flds1 = List.length flds2 &&
                                            List.for_all(fun(li2,tyTi2)-> 
                                                try let tyTi1 = List.assoc li2 flds1 in tyeqv ctx tyTi1 tyTi2 
                                                with Not_found -> false) flds2
    | TyVariant(flds1),TyVariant(flds2) ->  List.length flds1 = List.length flds2 &&
                                            List.for_all(fun(li2,tyTi2)->
                                                try let tyTi1 = List.assoc li2 flds1 in tyeqv ctx tyTi1 tyTi2
                                                with Not_found -> false) flds2 
    | (tyS,tyT)                         -> tyS = tyT 
*) 
(* ----------- TYPING --------------- *) 

let tyeqv tyT tyS = match (tyS,tyT) with 
    | Universe(_,i),Universe(_,j) -> i = j
    | TmBool(_),TmBool(_)   -> true
    | TmNat(_),TmNat(_)     -> true 
    | TmApp(_,TmApp(_,TmPi(_),tyS1),tyS2),TmApp(_,TmApp(_,TmPi(_),tyT1),tyT2) -> tyS1 = tyT1 && tyS2 = tyT2 


let rec typeof ctx   t      = let p str = pr str;pr"(∣Γ∣=";pi(ctxlen ctx);pr") ";pr_tm ctx t;pn() in match t with
    | TmApp(fi,TmApp(_,TmPi(_),t1),t2) -> 
                                    p "T-PI          : "; Universe(fi,0) 
    | TmBool(fi)                ->  p "T-BOOL        : "; Universe(fi,0) 
    | TmNat(fi)                 ->  p "T-NAT         : "; Universe(fi,0) 
    | TmVar(fi,i,_)             ->  p "T-VAR         : "; getTypeFromContext fi ctx i  
    | TmLet(fi,x,t1,t2)         ->  p "T-LET         : "; tyShift(-1)(typeof(addbind ctx x(BindTmVar(typeof ctx t1)))t2)
    | TmAbs(fi,x,tyT1,t2)       ->  p "T-ABS         : ";
            let ctx'    = addbind ctx x (BindTmVar(tyT1)) in   
            let tyT2    = typeof ctx' t2 in                    
            TmApp(fi,TmApp(fi,TmPi(fi),tyT1),tyShift(-1)tyT2)                        
    | TmApp(fi,t1,t2)           ->  p "T-APP         : ";     
            let tyT1 = typeof ctx t1 in                         (*   Γ |- t1 : T2→T12 ∧ Γ ∣- t2 : T2        *)
            let tyT2 = typeof ctx t2 in                         (*   ------------------------------- T-App  *)
            (match tyT1 with                     (*         Γ ∣- t1 t2 : T12                 *)   
                | TmApp(_,TmApp(_,TmPi(_),tyT11),tyT12)  -> if (=) tyT2 tyT11 then tyT12 else error fi "type mismatch" 
                | _                     -> error fi "arrow type expected" )
    | TmTrue(fi)                ->  p "T-TRUE        : "; TmBool(fi)
    | TmFalse(fi)               ->  p "T-FALSE       : "; TmBool(fi)
    | TmZero(fi)                ->  p "T-ZERO        : "; TmNat(fi)
    | TmSucc(fi,t)              ->  p "T-SUCC        : ";
                                    if (=)(typeof ctx t)(TmNat(fi)) then TmNat(fi) else error fi "succ expects 𝐍"  
    | TmPred(fi,t)              ->  p "T-PRED        : ";
                                    if (=)(typeof ctx t)(TmNat(fi)) then TmNat(fi) else error fi "succ expects 𝐍"  
    | TmIsZero(fi,t)            ->  p "T-ISZERO      : ";
                                    if (=)(typeof ctx t)(TmNat(fi)) then TmBool(fi) else error fi "iszero expects 𝐍"
    | TmIf(fi,t1,t2,t3)         ->  p "T-IF          : ";
        if (tyeqv)(typeof ctx t1)(TmBool(fi))
            then    let tyT2 = typeof ctx t2 in
                    if (tyeqv)tyT2(typeof ctx t3) then tyT2 else error fi "The result types of if-stmt mismatch" 
            else error fi "if-condition expects a boolean" 

(* CAUTION typeof is used unneededly *)
(* -------------------------------------------------- *) 
(* Bind Print *)    
let prbindty ctx = function
    | BindName                  -> ()
    | BindTmVar(tyT)            -> pr": "; pr_ty ctx tyT 
    | BindTyVar                 -> () 
    | BindTyAbb(tyT)            -> pr"= "; pr_ty ctx tyT
    | BindTmAbb(t,Some(tyT))    -> pr"= "; pr_tm ctx t; pr" : ";pr_ty ctx tyT
    | BindTmAbb(t,None)         -> pr"= "; pr_tm ctx t; pr" : ";pr_ty ctx(typeof ctx t) 

