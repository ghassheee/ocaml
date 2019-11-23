open Format
open Core
open Support.Pervasive
open Support.Error
open Syntax
open Arg 

exception NoRuleApplies


(* ----------- TYPING --------------- *) 
let rec tyeqv ctx tyT tyS   = true

let rec typeof ctx   t      = match t with
    | TmVar(fi,i,_)             ->  pr "T-VAR         : ";pr_tm ctx t;pn(); (*x:T ∈ Γ ⇒  Γ ∣- x : T   T-Var  *)  
                                    getTypeFromContext fi ctx i  
    | TmLet(fi,x,t1,t2)         ->  pr "T-LET         : ";pr_tm ctx t;pn();
                                    typeof ctx(TmApp(fi,TmAbs(fi,x,typeof ctx t1,t2),t1))
    | TmAbs(fi,x,tyT1,t2)       ->  pr "T-ABS         : ";pr_tm ctx t;pn();
            let ctx'    = addbinding ctx x (VarBind(tyT1)) in   (*       Γ,x:T1 ∣- t2 : T2          *)  
            let tyT2    = typeof ctx' t2 in                     (*     --------------------- T-Abs  *)
            TyArr(tyT1,tyT2)                                    (*     Γ ∣- λx:T1.t2 : T1→T2        *)
    | TmApp(fi,t1,t2)           ->  pr "T-APP         : ";pr_tm ctx t;pn ();     
            let tyT1 = typeof ctx t1 in                 (*   Γ |- t1 : T2→T12 ∧ Γ ∣- t2 : T2        *)
            let tyT2 = typeof ctx t2 in                 (*   ------------------------------- T-App  *)
            (match tyT1 with                            (*         Γ ∣- t1 t2 : T12                 *)   
                | TyArr(tyT11,tyT12)    -> if (=) tyT2 tyT11 then tyT12 else error fi "type mismatch" 
                | _                     -> error fi "arrow type expected" )
    | TmRecord(fi,flds)         ->  pr "T-RCD         : ";pr_tm ctx t;pn();
                                    TyRecord(List.map (fun(l,t)->(l,typeof ctx t)) flds)
    | TmProj(fi,t,l)            ->  pr "T-PROJ        : ";pr_tm ctx t;pn(); (match typeof ctx t with 
        | TyRecord(tyflds)          ->  (try List.assoc l tyflds 
                                        with Not_found -> error fi ("label "^l^" not found")) 
        | _                         ->  error fi "Record Type Expected" ) 
    | TmUnit(fi)                ->  pr "T-UNIT        : ";pr_tm ctx t;pn(); TyUnit
    | TmTrue(fi)                ->  pr "T-TRUE        : ";pr_tm ctx t;pn(); TyBool
    | TmFalse(fi)               ->  pr "T-FALSE       : ";pr_tm ctx t;pn(); TyBool
    | TmZero(fi)                ->  pr "T-ZERO        : ";pr_tm ctx t;pn(); TyNat
    | TmSucc(fi,t)              ->  pr "T-SUCC        : ";pr_tm ctx t;pn();
                                    if (=) (typeof ctx t) TyNat then TyNat else error fi "succ expects 𝐍"  
    | TmPred(fi,t)              ->  pr "T-PRED        : ";pr_tm ctx t;pn();
                                    if (=) (typeof ctx t) TyNat then TyNat else error fi "succ expects 𝐍"  
    | TmIsZero(fi,t)            ->  pr "T-ISZERO      : ";pr_tm ctx t;pn();
                                    if (=) (typeof ctx t) TyNat then TyBool else error fi "iszero expects 𝐍"
    | TmIf(fi,t1,t2,t3)         ->  pr "T-IF          : ";pr_tm ctx t;pn();
        if (=) (typeof ctx t1) TyBool 
            then    let tyT2 = typeof ctx t2 in
                    if (=)tyT2(typeof ctx t3) then tyT2 else error fi "The result types of if-stmt mismatch" 
            else error fi "if-condition expects a boolean" 
    | TmAscribe(fi,t,tyT)       ->  pr "T-ASCRIBE     : ";pr_tm ctx t;pn();
                                    if (=)(typeof ctx t)tyT then tyT else error fi "Type Ascription Mismatch"       


(* ---- *) 

let prbindingty ctx = function
    | NameBind                  -> ()
    | VarBind(tyT)              -> pr ": "; pr_ty tyT 




