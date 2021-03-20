
open Support 
open Syntax 

exception NoRuleApplies 


let rec getkind fi ctx i            = match getbind fi ctx i with 
    | BindTyVar(k)                  ->  k 
    | BindTyAbb(_,Some(k))          ->  k
    | BindTyAbb(tyT,None)           ->  kindof ctx tyT  
    | _                             ->  error fi("getkind: Wrong kind bind"^index2name fi ctx i)

and kindof ctx ty           = match ty with 
    (* type operators *) 
    | TyVar(i,_)                    ->  getkind dummy ctx i  
    | TyAbs(tyX,k,ty)               ->  let ctx' = addbind ctx tyX (BindTyVar k) in 
                                        KnArr(k,kindof ctx' ty)
    | TyApp(tyT1,tyT2)              ->  let k1 = kindof ctx tyT1 in
                                        let k2 = kindof ctx tyT2 in 
                                        (match k1 with 
        | KnArr(k11,k12)                ->  if (=)k11 k2 
                                                then k12 
                                                else error dummy "parameter kind mismatch"
        | _                             ->  error dummy "arrow kind expected" )
    (* proper types *) 
    | TyArr(tyT1,tyT2)              ->  let k1 = kindof ctx tyT1 in 
                                        let k2 = kindof ctx tyT2 in 
                                        if (=)k1 Kn && (=)k2 Kn 
                                            then Kn 
                                            else error dummy "Kind * expected" 
    | TyBool                        ->  Kn
    | TyNat                         ->  Kn 


let isproper fi ctx tyT         =  let k = kindof ctx tyT in 
                                    if k = Kn then () else error fi "Kind * expected" 



(* ------- directed equivalence -------- *)

let istyabb ctx i               = match getbind dummy ctx i with
    | BindTyAbb(tyT,_)              -> true
    | _                             -> false;; 

let gettyabb ctx i              = match getbind dummy ctx i with 
    | BindTyAbb(tyT,_)              -> pr"(";pi i;pr",";pr_ty ctx tyT;pr")"; tyT
    | _                             -> raise NoRuleApplies

let rec computety ctx           = function 
    | TyVar(i,_)when istyabb ctx i  -> gettyabb ctx i 
    | TyApp(TyAbs(_,_,tyT),tyT2)    -> tySubstTop tyT2 tyT
    | _                             -> raise NoRuleApplies

let rec simplifyty ctx          = function 
    | TyApp(tyT1,tyT2)              ->  let tyT = TyApp(simplifyty ctx tyT1,tyT2) in 
                                        (try let tyT' = computety ctx tyT in 
                                             simplifyty ctx tyT' with 
                                            | NoRuleApplies -> tyT)
    | tyT                           ->  (try let tyT' = computety ctx tyT in  
                                             simplifyty ctx tyT' with 
                                            | NoRuleApplies -> tyT)


let rec tyeqv ctx tyS tyT       = 
    pr"tyeqv "; 
    pr_ty ctx tyS;pr" ";pr_ty ctx tyT;pn();
    let tyS' = simplifyty ctx tyS in 
    let tyT' = simplifyty ctx tyT in 
    pr"simplified tyeqv ";
    pr_ty ctx tyS';pr" ";pr_ty ctx tyT';pn();
    match (tyS',tyT') with 
    (* nonproper types *)
    | TyVar(i,_),_ when istyabb ctx i   ->  tyeqv ctx(gettyabb ctx i) tyT' 
    | _,TyVar(j,_) when istyabb ctx j   ->  tyeqv ctx tyS'(gettyabb ctx j) 
    | TyVar(i,_),TyVar(j,_)             ->  i=j
    | TyApp(t1,t2),TyApp(u1,u2)         ->  tyeqv ctx t1 u1 && tyeqv ctx t2 u2
    | TyAbs(x,k,t),TyAbs(y,l,u)         ->  let ctx' = addname ctx x in 
                                             (=)k l && tyeqv ctx' t u
    (* proper types *) 
    | TyArr(t1,t2),TyArr(u1,u2)         ->  tyeqv ctx t1 u1 && tyeqv ctx t2 u2
    | TyBool,TyBool                     ->  true
    | TyNat,TyNat                       ->  true
    | _                                 ->  false 




