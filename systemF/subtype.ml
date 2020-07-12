open Support.Error
open Support.Pervasive
open Syntax
open Print

exception NoRuleApplies



(* ---------------------------------- *) 
let istyabb ctx i           = match getbind dummy ctx i with 
    | BindTyAbb(_)                  -> true
    | _                             -> false

let gettyabb ctx i          = match getbind dummy ctx i with 
    | BindTyAbb(tyT)                -> tyT
    | _                             -> raise NoRuleApplies 

let rec computety ctx tyT   = match tyT with 
    | TyVar(i,_)when istyabb ctx i  -> gettyabb ctx i 
    | _                             -> raise NoRuleApplies

let rec simplifyty ctx tyT  = try simplifyty ctx(computety ctx tyT) with NoRuleApplies -> tyT

(* ------- TYPE EQUIVALENCE -------- *)

let rec tyeqv ctx tyS tyT   = 
    let tyS = simplifyty ctx tyS in 
    let tyT = simplifyty ctx tyT in 
    match (tyS,tyT) with 
    | (TyVar(i,_),_) when istyabb ctx i ->  tyeqv ctx(gettyabb ctx i)tyT
    | (_,TyVar(i,_)) when istyabb ctx i ->  tyeqv ctx tyS(gettyabb ctx i)
    | TyVar(i,_),TyVar(j,_)             ->  i=j
    | TyArr(tyS1,tyS2),TyArr(tyT1,tyT2) ->  tyeqv ctx tyS1 tyT1 && tyeqv ctx tyS2 tyT2
    | TyRecord(flds1),TyRecord(flds2)   ->  List.length flds1 = List.length flds2 &&
                                            List.for_all(fun(li2,tyTi2) -> try 
                                            let tyTi1 = List.assoc li2 flds1 in 
                                            tyeqv ctx tyTi1 tyTi2
                                            with Not_found -> false) flds2
    | (tyS,tyT)                         ->  tyS = tyT 



(* --------- SUBTYPING -------------- *)

let rec subtype ctx tyS tyT     = 
    tyeqv ctx tyS tyT || 
    let tyS = simplifyty ctx tyS in
    let tyT = simplifyty ctx tyT in match (tyS,tyT) with 
    | _,TyTop                           ->  true
    | TyArr(s1,s2),TyArr(t1,t2)         ->  subtype ctx t1 s1 && subtype ctx s2 t2 
    | TyRecord(fS),TyRecord(fT)         ->  List.for_all ( fun(li,tyTi) -> 
                                                try let tySi = List.assoc li fS in subtype ctx tySi tyTi
                                                with Not_found -> false ) fT 
    | _,_                               ->  false 

(* ---------- JOIN & MEET ---------- *)

let rec join ctx tyS tyT        =
    pr"JOINING TYPES    : "; pr_ty ctx tyS; pr" & "; pr_ty ctx tyT;pn();
    if subtype ctx tyS tyT then tyT else
    if subtype ctx tyT tyS then tyS else 
    let tyS = simplifyty ctx tyS in 
    let tyT = simplifyty ctx tyT in 
    match(tyS,tyT) with 
    | TyRecord(fS),TyRecord(fT)         ->  let lSs = List.map fst fS in 
                                            let lTs = List.map fst fT in 
                                            let commonls = List.find_all (fun l -> List.mem l lTs) lSs in 
                                            let f li = (li,join ctx(List.assoc li fS)(List.assoc li fT)) in 
                                            let commonflds = List.map f commonls in 
                                            TyRecord(commonflds)
    | TyArr(tyS1,tyS2),TyArr(tyT1,tyT2) ->  (try     TyArr(meet ctx tyS1 tyT1, join ctx tyS2 tyT2) 
                                            with Not_found  -> TyTop)
    | _                                 ->  TyTop

and meet ctx tyS tyT                = 
    if subtype ctx tyS tyT then tyS else 
    if subtype ctx tyT tyS then tyT else
    let tyS = simplifyty ctx tyS in 
    let tyT = simplifyty ctx tyT in 
    match(tyS,tyT) with 
    | TyRecord(fS),TyRecord(fT)         ->  let lSs = List.map fst fS in 
                                            let lTs = List.map fst fT in 
                                            let nomem l x = not (List.mem x l) in 
                                            let allLabels = List.append lSs (List.find_all(nomem lSs)lTs) in
                                            let f li =  
                                                if List.mem li allLabels 
                                                    then li, meet ctx (List.assoc li fS)(List.assoc li fT) 
                                                    else if List.mem li lSs 
                                                        then li, List.assoc li fS
                                                        else li, List.assoc li fT in 
                                            let allflds = List.map f allLabels in 
                                            TyRecord(allflds) 
    | TyArr(tyS1,tyS2),TyArr(tyT1,tyT2) ->  TyArr(join ctx tyS1 tyT1, meet ctx tyS2 tyT2) 
    | _                                 ->  pr"hoge";raise Not_found  



