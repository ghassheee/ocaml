open Support.Error
open Support.Pervasive
open Syntax

exception NoRuleApplies 

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
    | Universe(_,i),Universe(_,j)       -> i = j
    | Bool(_),Bool(_)                   -> true
    | Nat(_),Nat(_)                     -> true 
    | If(_,a,b,c),If(_,x,y,z)           -> tyeqv ctx a x && tyeqv ctx b y && tyeqv ctx c z 
    | App(_,App(_,Pi(_),tyA),Abs(_,_,_,tyB)),App(_,App(_,Pi(_),tyC),Abs(_,_,_,tyD)) 
                                        -> tyeqv ctx tyA tyC && tyeqv ctx tyB tyD 
    | Var(_,i,_),Var(_,j,_)             -> i = j
    | tyA,tyB                           -> (=) tyA tyB 

and typeof ctx   t      = let p str = pr str;pr"(∣Γ∣=";pi(ctxlen ctx);pr") ";pr_tm ctx t;pn() in match t with
    | Universe(fi,i)            ->  p "T-UNIVERSE    : ";   Universe(fi,i+1) 
    | App(fi,App(_,Pi(_),_),_)  ->  p "T-PI          : ";   Universe(fi,0) 
    | Bool(fi)                  ->  p "T-BOOL        : ";   Universe(fi,0) 
    | Nat(fi)                   ->  p "T-NAT         : ";   Universe(fi,0) 
    | Var(fi,i,_)               ->  p "T-VAR         : ";   getTypeFromContext fi ctx i
    | Abs(fi,a,tyA,b)           ->  p "T-ABS         : ";   
        pr "addbinding "; pr a; prbindty ctx (BindVar(tyA));pn();
        let ctx'    = addbind ctx a (BindVar(tyA)) in 
        let tyB     = typeof ctx' b in 
        App(fi,App(fi,Pi(fi),tyA),(Abs(fi,a,tyA,tyB)))  
    | App(fi,t1,t2)             ->  p "T-APP         : ";   let tyT1 = typeof ctx t1 in
                                                            let tyT2 = typeof ctx t2 in (match tyT1 with           
        | App(_,App(_,Pi(_),tyA),Abs(_,_,_,tyB)) ->  if tyeqv ctx tyT2 tyA then tyB else error fi "type mismatch" 
        | _                                 ->  error fi "arrow type expected" )
    | True(fi)                  ->  p "T-TRUE        : ";     Bool(fi)
    | False(fi)                 ->  p "T-FALSE       : ";     Bool(fi)
    | Zero(fi)                  ->  p "T-ZERO        : ";     Nat(fi)
    | Succ(fi,t)                ->  p "T-SUCC        : ";
                                    if tyeqv ctx(typeof ctx t)(Nat(fi))then Nat(fi) else error fi "succ expects 𝐍"  
    | Pred(fi,t)                ->  p "T-PRED        : ";
                                    if tyeqv ctx(typeof ctx t)(Nat(fi))then Nat(fi) else error fi "succ expects 𝐍"  
    | IsZero(fi,t)              ->  p "T-ISZERO      : ";
                                    if tyeqv ctx(typeof ctx t)(Nat(fi))then Bool(fi) else error fi "iszero expects 𝐍"
    | If(fi,True(_),t,_)        ->  p "T-IFTRUE      : "; typeof ctx t
    | If(fi,False(_),_,t)       ->  p "T-IFFALSE     : "; typeof ctx t
    | If(fi,t1,t2,t3)           ->  p "T-IF          : ";
        if tyeqv ctx(typeof ctx t1)(Bool(fi))
            then    let tyT2 = typeof ctx t2 in
                    if tyeqv ctx tyT2(typeof ctx t3) 
                        then tyT2 
                        else If(fi,t1,typeof ctx t2,typeof ctx t3) 
            else error fi "if-condition expects a boolean" 

