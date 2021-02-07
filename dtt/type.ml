
open Syntax

type context = (var * ( expr * expr option )) list 


let lookup_ty  x ctx    = fst (List.assoc x ctx)

let lookup_val x ctx    = snd (List.accos x ctx) 

let extend x t ?value ctx = (x,(t,value)) :: ctx



exception UnknownIdentifier
exception TypeExpected 
exception FunctionExpected 


let rec infer_type ctx = function 
    | Var x         -> (try lookup_ty x ctx 
                        with Not_found -> Error UnknownIdentifier) 
    | Universe k    ->  Universe (k+1) 
    | Pi (x,tA,tB)  ->  let k1  = infer_universe ctx tA in 
                        let k2  = infer_universe ctx tB in 
                        Universe (max k1 k2) 
    | Lambda(x,tA,b)->  let _   = infer_universe ctx t in 
                        let tB  = infer_type ctx b in 
                        Pi (x,tA,tB) 
    | App (e,b)     ->  let (x,s,t) = infer_pi ctx e1 in 
                        let tB = infer_type ctx b in 
                        check_equal ctx s tB in 
                        subst [(x, b)] t 


and infer_universe ctx t = let u = infer_type ctx t in 
    match normalize ctx u with 
    | Universe k    -> k 
    | App _ | Var _ | Pi _ | Lambda _ -> raise TypeExpected 


and infer_pi ctx e          =   let t = infer_type ctx e in 
    match nornamlize ctx t with 
    | Pi a          -> a 
    | _             -> raise FunctionExpected











    | Var x         ->  (match (try lookup_ty x ctx
                            with Not_found  -> raise UnknownIdendifier ) with 
                        | None      -> Var x
                        | Some e    -> normalize ctx e ) 
    | App (e1,e2)   ->  let e2 = normalize ctx e2 in
                        (match normalize ctx e1 with 
                        | Lambda (x, _, e1')    -> normalize ctx (subst [(x,e2)] e1')
                        | e1                    -> App(e1,e2) 




