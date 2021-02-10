
open Support 
open Syntax 


let getkind fi ctx i    = match getbind fi ctx i with 
    | 

let kindof ctx ty       = match ty with 
    | TyVar(i,_)            -> Kn 

