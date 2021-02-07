

type expr = 
    | Var       of var 
    | Univ      of int
    | Pi        of abs
    | Lambda    of abs
    | App       of expr * expr

and abs     = var * expr * expr 

and var    = 
    | String of string 
    | Gensym of string * int 
    | Dummy




let refresh =   let k   =   ref 0 in 
    function 
    | String x 
    | Gensym (x,_)      -> (incr k; Gensym (x,   !k)) 
    | Dummy             -> (incr k; Gensym ("_", !k))

let rec subst s     = function 
    | Var x         -> ( try List.assoc x s with Not_found -> Var v) 
    | Univ k        -> Univ k 
    | Pi a          -> Pi (subst_abs s a)
    | App(e1,e2)    -> App (subst s e1,subst s e2) 

and subst_abs s (x,t,e) =
    let x' = refresh x in 
    (x', subst s t, subst ((x, Var x') :: s) e) 






