let rec elem x l = match (x,l) with
    | _,[]                      ->  false 
    | PId(_,s),PId(_,t)::rest   ->  if s = t then true else elem x rest
    | PId(_,s),_::rest          ->  elem x rest
    | _                         ->  List.mem x l

(*
type lj_tree    = ( nj * sequent ) tree
type lj_ptree   = ( nj * sequent with_proof ) tree
type nj_ptree   = ( nj * sequent with_proof ) tree
type nj_tree    = ( nj * sequent ) tree
*)

type nj =
    | NJ_X      
    | NJ_Top_I  
    | NJ_Bot_E 
    | NJ_Arr_I 
    | NJ_Arr_E 
    | NJ_And_I 
    | NJ_And_E1
    | NJ_And_E2
    | NJ_Or_I1 
    | NJ_Or_I2 
    | NJ_Or_E  

(*
let rec eq a b = match a,b with 
    | PVar(_,i,_),PVar(_,j,_)       -> i=j
    | PId(_,s),PId(_,t)             -> s=t
    | PBot(_),PBot(_)               -> true
    | PTop(_),PTop(_)               -> true
    | PArr(_,a1,a2),PArr(_,b1,b2)   -> eq a1 b1 && eq a2 b2
    | PAnd(_,a1,a2),PAnd(_,b1,b2)   -> eq a1 b1 && eq a2 b2
    | POr (_,a1,a2),POr (_,b1,b2)   -> eq a1 b1 && eq a2 b2
    | _,_ -> false 
*)
