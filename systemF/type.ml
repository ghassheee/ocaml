open Support.Error
open Support.Pervasive
open Syntax
open Subtype 
open Print



(* ----------- TYPING --------------- *) 

let rec typeof ctx   t      = 
    let p str = pr str;pr": (∣Γ∣=";pi(ctxlen ctx);pr") ";pr_tm ctx t;pn() in 
    match t with
    | TmTAbs(fi,tyX,t)          ->  p"T-TABS        "; 
                                    let ctx' = addbind ctx tyX BindTyVar in 
                                    TyAll(tyX,typeof ctx' t) 
    | TmPack(fi,tyT,t,tySome)   ->  p"T-PACK        "; ( match tySome with
        | TySome(tyX,tyT')          ->  let tyU = typeof ctx t in 
                                        let tyU' = tySubstTop tyT tyT' in 
                                        if tyeqv ctx tyU tyU' then tySome
                                        else error fi "Instance does not match Existential Type" 
        | _                         ->  error fi "existential type expected" )
    | TmUnpack(fi,tyX,x,some,t) ->  p"T-UNPACKPACK  "; ( match some with 
        |   TmPack(_,tyInstance,tmInstance,_) 
                                    -> let tySome = typeof ctx some in ( match simplifyty ctx tySome with
            |   TySome(tyY,tyT')        ->  let ctx' = addbind (addbind ctx tyX BindTyVar) x (BindTmVar tyT') in
                                            let ty = tyShift(-2)(typeof ctx' t) in
                                            ty 
            |   _                       ->  error fi "existential type expected")
        |   _                       ->  error fi "existential type set expected" ) 
    | TmTApp(fi,t,tyT)          ->  p"T-TAPP        ";
                                    let tyT1 = typeof ctx t in (match simplifyty ctx tyT1 with 
        | TyAll(tyX,tyT')           -> tySubstTop tyT tyT'
        | _                         -> error fi "∀-type expected" )   
    | TmFix(fi,t1)              ->  p"T-FIX         "; (match typeof ctx t1 with 
        | TyArr(tyS,tyT)            ->  if subtype ctx tyT tyS 
                                                then tyT 
                                                else error fi"fix can take 'x' whose type: A -> A" 
        | _                         -> error fi"fix can only take x whose type is A -> A"  )
    | TmFloat(fi,f)             ->  p"T-FLOAT       "; TyFloat
    | TmTimesfloat(fi,t1,t2)    ->  p"T-TIMESFLOAT  "; 
                                    if tyeqv ctx TyFloat(typeof ctx t1)
                                        && tyeqv ctx TyFloat(typeof ctx t2) 
                                        then TyFloat 
                                        else error fi"TypeMismatch: (*.) requires Floats"  
    | TmString(fi,_)            ->  p"T-STRING      ";  TyString
    | TmVar(fi,i,_)             ->  p"T-VAR         ";  let tyT = getTypeFromContext fi ctx i  in tyT
    | TmLet(fi,x,t1,t2)         ->  p"T-LET         ";  
                                    tyShift(-1)(typeof(addbind ctx x(BindTmVar(typeof ctx t1)))t2)
    | TmAbs(fi,x,tyT1,t2)       ->  p"T-ABS         ";  
                                    let ctx'    = addbind ctx x (BindTmVar(tyT1)) in  
                                    let tyT2    = typeof ctx' t2 in                  
                                    TyArr(tyT1,tyShift(-1)tyT2)                     
    | TmApp(fi,t1,t2)           ->  p"T-APP         ";  
                                    let tyT1 = typeof ctx t1 in        
                                    let tyT2 = typeof ctx t2 in (match simplifyty ctx tyT1 with  
        | TyArr(tyT11,tyT12)        ->  if subtype ctx tyT2 tyT11 
                                            then tyT12 
                                            else error fi "type mismatch" 
        | _                         ->  error fi "arrow type expected" )
    | TmRecord(fi,flds)         ->  p"T-RCD         ";  
                                    TyRecord(List.map(fun(l,t)->(l,typeof ctx t))flds)
    | TmProj(fi,t,l)            ->  p"T-PROJ        ";  (match typeof ctx t with 
        | TyRecord(tyflds)          ->  (try List.assoc l tyflds 
                                        with Not_found -> error fi("label "^l^" not found")) 
        | _                         ->  error fi "Record Type Expected" ) 
    | TmUnit(fi)                ->  p"T-UNIT        ";  TyUnit
    | TmTrue(fi)                ->  p"T-TRUE        ";  TyBool
    | TmFalse(fi)               ->  p"T-FALSE       ";  TyBool
    | TmZero(fi)                ->  p"T-ZERO        ";  TyNat
    | TmSucc(fi,t)              ->  p"T-SUCC        ";
                                    if tyeqv ctx(typeof ctx t)TyNat 
                                        then TyNat 
                                        else error fi "succ expects 𝐍"  
    | TmPred(fi,t)              ->  p"T-PRED        ";
                                    if tyeqv ctx(typeof ctx t)TyNat 
                                        then TyNat 
                                        else error fi "succ expects 𝐍"  
    | TmIsZero(fi,t)            ->  p"T-ISZERO      ";
                                    if tyeqv ctx(typeof ctx t)TyNat 
                                        then TyBool 
                                        else error fi "iszero expects 𝐍"
    | TmIf(fi,t1,t2,t3)         ->  p"T-IF          ";
                                    if (tyeqv ctx)(typeof ctx t1) TyBool 
                                        then    join ctx (typeof ctx t2) (typeof ctx t3)
                                        else    error fi "if-condition expects a boolean" 
    | TmAscribe(fi,t,tyT)       ->  p"T-ASCRIBE     ";
                                    if (subtype ctx)(typeof ctx t)tyT 
                                        then tyT 
                                        else error fi "Type Ascription Mismatch"       



(* CAUTION typeof is used unneededly *)
(* -------------------------------------------------- *) 
(* Bind Print *)    
let prbindty ctx = function
    | BindName                  -> ()
    | BindTmVar(tyT)            -> pr": "; pr_ty ctx tyT 
    | BindTyVar                 -> () 
    | BindTyAbb(tyT)            -> pr"= "; pr_ty ctx tyT
    | BindTmAbb(t,Some(tyT))    -> pr"= "; pr_tm ctx t
    | BindTmAbb(t,None)         -> pr"= "; pr_tm ctx t 

