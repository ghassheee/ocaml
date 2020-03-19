open Syntax
open Support.Error

let find = List.find 

let rec alpha env = function 
    | TmUnit(fi) -> TmUnit(fi)

