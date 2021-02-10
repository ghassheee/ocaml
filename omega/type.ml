open Format
open Support
open Syntax
open Arg 

exception NoRuleApplies
let e = error 

(* ----------- TYPING --------------- *) 

let rec typeof ctx   t      = pr"TYPEOF: ";pr_tm ctx t;pn();  match t with
    | TmVar(fi,i,_)             -> gettype fi ctx i 
    | TmAbs(fi,x,tyT1,t2)       -> let ctx' = addbind ctx x (BindTmVar(tyT1)) in 
                                   let tyT2 = typeof ctx' t2 in 
                                   TyArr(tyT1,tyT2)  
    | TmApp(fi,t1,t2)           -> let tyT1 = typeof ctx t1 in
                                   let tyT2 = typeof ctx t2 in
                                   (match tyT1 with 
        | TyArr(tyT11,tyT12)        ->  let e = e fi"TmApp: type mismatch" in 
                                        if(=)tyT2 tyT11 then tyT12 else e  
        | _                         ->  e fi "TmApp: arrow type expected" )
    | TmTrue(fi)                -> TyBool
    | TmFalse(fi)               -> TyBool
    | TmZero(fi)                -> TyNat
    | TmSucc(fi,t)              ->  let e = e fi "succ expects 𝐍" in  
                                    if (=)(typeof ctx t) TyNat then TyNat  else e 
    | TmPred(fi,t)              ->  let e = e fi "succ expects 𝐍" in  
                                    if (=)(typeof ctx t) TyNat then TyNat  else e 
    | TmIsZero(fi,t)            ->  let e = e fi "iszero expects 𝐍" in 
                                    if (=)(typeof ctx t) TyNat then TyBool else e 
    | TmIf(fi,t1,t2,t3)         ->  if (=)(typeof ctx t1)TyBool 
                                        then    let tyT2 = typeof ctx t2 in
                                                if (=) tyT2 (typeof ctx t3) then tyT2 
                                                else e fi "resulting type of if-stmt mismatch" 
                                        else    e fi "if-condition expects a boolean" 


(* ---- *) 

let prbindty ctx = function
    | BindName                  -> ()
    | BindTmVar(tyT)            -> pr ": "; pr_ty tyT 




