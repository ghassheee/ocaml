open Format
open Kind
open Support
open Syntax
open Arg 

exception NoRuleApplies
let e = error 

(* ----------- TYPING --------------- *) 

let rec typeof ctx   t      = 
    pr_ctx ctx;
    pr"TYPEOF: ";pr_tm ctx t;pn();  match t with
    | TmVar(fi,i,_)             ->  gettype fi ctx i
    | TmAbs(fi,x,tyX,t)         ->  isproper fi ctx tyX;
                                    let ctx' = addbind ctx x (BindTmVar(tyX)) in 
                                    let tyT = typeof ctx' t in 
                                    TyArr(tyX,tyShift(-1)tyT)  
    | TmApp(fi,t1,t2)           ->  let tyT1 = typeof ctx t1 in
                                    let tyT2 = typeof ctx t2 in
                                    (match simplifyty ctx tyT1 with 
        | TyArr(tyT11,tyT12)        ->  if tyeqv ctx tyT2 tyT11 
                                            then tyT12
                                            else e fi"TmApp: type mismatch"   
        | _                         ->  e fi "TmApp: arrow type expected" )
    | TmTrue(fi)                ->  TyBool
    | TmFalse(fi)               ->  TyBool
    | TmZero(fi)                ->  TyNat
    | TmSucc(i,t)               ->  if(=)(typeof ctx t)TyNat then TyNat else e i"succ expects 𝐍"
    | TmPred(i,t)               ->  if(=)(typeof ctx t)TyNat then TyNat else e i"pred expects 𝐍" 
    | TmIsZero(fi,t)            ->  if(=)(typeof ctx t)TyNat then TyBool 
                                    else e fi "iszero expects 𝐍" 
    | TmIf(fi,t1,t2,t3)         ->  if (=)(typeof ctx t1)TyBool 
                                        then    let tyT2 = typeof ctx t2 in
                                                if (=) tyT2 (typeof ctx t3) then tyT2 
                                                else e fi "resulting type of if-stmt mismatch" 
                                        else    e fi "if-condition expects a boolean" 


(* ---- *) 




