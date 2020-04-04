open Support.Error
open Support.Pervasive
open Syntax

exception NoRuleAplies 

(* Bind Print *)    
let prbindty ctx = function
    | BindName                  -> ()
    | BindVar(tyT)              -> pr": "; pr_tm ctx tyT 
    | BindTyVar                 -> () 
    | BindTyAbb(tyT)            -> pr"= "; pr_tm ctx tyT
    | BindAbb(t,Some(tyT))      -> pr"= "; pr_tm ctx t; pr" : ";pr_tm ctx tyT
    | BindAbb(t,None)           -> pr"= "; pr_tm ctx t 

(* ----------- TYPING --------------- *) 

let rec tyeqv ctx tyT tyS = match (tyS,tyT) with 
    | Univ(_,i),Univ(_,j)       -> i = j
    | Bool(_),Bool(_)                   -> true
    | Nat(_),Nat(_)                     -> true 
    | If(_,a,b,c),If(_,x,y,z)           -> tyeqv ctx a x && tyeqv ctx b y && tyeqv ctx c z 
    | Ap(_,Ap(_,Pi(_),a),Lam(_,_,_,b)),Ap(_,Ap(_,Pi(_),c),Lam(_,_,_,d)) 
                                        -> tyeqv ctx a c && tyeqv ctx b d  
    | Var(_,i,_),Var(_,j,_)             -> i = j
    | tyA,tyB                           -> (=) tyA tyB 

and typeof ctx   t      = 
    let p str = pr str;pr"(∣Γ∣=";pi(ctxlen ctx);pr") ";pr_tm ctx t;pn() in match t with
    | Univ(fi,i)                            ->  p "D-UNIVERSE    : ";   Univ(fi,i+1) 
    | Ap(fi,Ap(_,Pi(_),tyA),Lam(_,a,_,tyB))   ->  p "D-PI          : ";   
        let univ1 = typeof ctx tyA in 
        let ctx' = addbind ctx a (BindVar(tyA)) in
        let univ2 = typeof ctx' tyB in 
        if tyeqv ctx univ1 univ2 
            then let Univ(_,i)=univ1 in Univ(fi,i) 
            else err fi "Pi type takes two type belonging the same universe."
    | Lam(fi,a,tyA,b)                           ->  p "D-ABS         : "; 
        let ctx'    = addbind ctx a (BindVar(tyA)) in 
        let tyB     = typeof ctx' b in 
        Ap(fi,Ap(fi,Pi(fi),tyA),(Lam(fi,a,tyA,tyB)))  
    | Ap(fi,t1,t2)                             ->  pr"[";pn();p "D-APP         : ";   
        let tyT1 = typeof ctx t1 in
        let tyT2 = typeof ctx t2 in (match tyT1 with           
        | Ap(_,Ap(_,Pi(_),tyA),Lam(_,_,_,tyB))-> 
                if tyeqv ctx tyT2 tyA then let tyT = tmSubstTop t1 tyB in pr"]";pn();tyT else err fi"D-APP Fail" 
        | _                                     ->  err fi "Π-type expected" )
    | Bool(fi)                  ->  p "T-BOOL        : "; Univ(fi,0) 
    | Nat(fi)                   ->  p "T-NAT         : "; Univ(fi,0) 
    | Var(fi,i,_)               ->  p "T-VAR         : "; getTypeFromContext fi ctx i
    | True(fi)                  ->  p "T-TRUE        : "; Bool(fi)
    | False(fi)                 ->  p "T-FALSE       : "; Bool(fi)
    | Zero(fi)                  ->  p "T-ZERO        : "; Nat(fi)
    | Succ(fi,t)                ->  p "T-SUCC        : ";
                                    if tyeqv ctx(typeof ctx t)(Nat(fi))then Nat(fi)else err fi"succ expects 𝐍"  
    | Pred(fi,t)                ->  p "T-PRED        : ";
                                    if tyeqv ctx(typeof ctx t)(Nat(fi))then Nat(fi)else err fi"succ expects 𝐍"  
    | IsZero(fi,t)              ->  p "T-ISZERO      : ";
                                    if tyeqv ctx(typeof ctx t)(Nat(fi))then Bool(fi)else err fi"iszero expects 𝐍"
    | If(fi,True(_),t,_)        ->  p "T-IFTRUE      : "; typeof ctx t
    | If(fi,False(_),_,t)       ->  p "T-IFFALSE     : "; typeof ctx t
    | If(fi,t1,t2,t3)           ->  p "T-IF          : ";
        if tyeqv ctx(typeof ctx t1)(Bool(fi))
            then    let tyT2 = typeof ctx t2 in
                    if tyeqv ctx tyT2(typeof ctx t3) 
                        then tyT2 
                        else If(fi,t1,typeof ctx t2,typeof ctx t3) 
            else err fi "if-condition expects a boolean" 

