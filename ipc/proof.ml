(* 
    +------------------------+
    | 1. LJ Tree Inversion   | 
    +------------------------+
               v 
    +------------------------+
    | 2. Adding Proof        | 
    +------------------------+
               v 
    +------------------------+
    | 3. Proof Conversion    | 
    +------------------------+
               v 
    +------------------------+
    | 4. NJ Tree Construct   |
    +------------------------+
               v 
    +------------------------+
    | 5. Hiding Proof        | 
    +------------------------+

*)

open Support
open Syntax 

exception NoRule
exception InvalidContext

type 'a tree        = 
    | Leaf 
    | Node of 'a * 'a tree list 

type sequent            = context * prop 
type 'seq with_proof    = 'seq * proof 

type lj = 
    | LJ_AX        
    | LJ_Arr_P_L  
    | LJ_Arr_Or_L 
    | LJ_Arr_And_L
    | LJ_Arr_Top_L
    | LJ_Arr_Arr_L
    | LJ_Arr_R    
    | LJ_And_L    
    | LJ_And_R     
    | LJ_Or_L
    | LJ_Or_R1
    | LJ_Or_R2
    | LJ_Bot_L
    | LJ_Top_R
    | LJ_Id

let pr_lj = function
    | LJ_AX         -> "AX"  
    | LJ_Arr_P_L    -> "→pL" 
    | LJ_Arr_Or_L   -> "→∨L"
    | LJ_Arr_And_L  -> "→∧L"
    | LJ_Arr_Top_L  -> "→⊤L"
    | LJ_Arr_Arr_L  -> "→→L"
    | LJ_Arr_R      -> "→R"
    | LJ_And_L      -> "∧L"
    | LJ_And_R      -> "∧R"
    | LJ_Or_L       -> "∨L"
    | LJ_Or_R1      -> "∨R1"
    | LJ_Or_R2      -> "∨R2"
    | LJ_Bot_L      -> "⊥L"
    | LJ_Top_R      -> "⊤R"


(*------ Printing ------*) 
let rec pr_seq  = function
    | [],p      -> pr"⊢ ";pr_Prop p
    | [x],p     -> pr_Prop x;pr" " ;pr_seq ([],p)
    | x::xs,p   -> pr_Prop x;pr", ";pr_seq (xs,p)

let rec pr_lj_tree prefix = function
    | Leaf                  -> pr""
    | Node(a,trees)         -> let l,s = a in 
        pr prefix;pr"[";pr(pr_lj l);pr"]    ";pr_seq s;pn();
        (List.fold_left(fun x y->x;y)(pr"")(List.map(pr_lj_tree(prefix^"    "))trees));;


(*----- Permutations -----*)

let ins_all_positions x l = 
    let rec aux prev acc = function
        | []            -> (prev @ [x]) ::  List.rev acc 
        | hd::tl as l   -> aux (prev @ [hd]) ((prev @ [x] @ l) :: acc) tl 
    in aux [][] l
let rec permutations = function 
    | []            -> []
    | x::[]         -> [x]::[]
    | x::xs         -> List.fold_left(fun acc p -> acc @ ins_all_positions x p) [] (permutations xs) 


(*---- 1. LJ Tree Construction ----*) 

let rec ljTree seq = let ctx,p = seq in
    let rec try_perm =  function
        | []            ->  raise NoRule  
        | x::rest       -> (try    ljTreeL(x,p) 
                            with    NoRule  -> (try     ljTreeR(x,p) 
                                                with    NoRule      -> try_perm rest)) 
    in try_perm (permutations ctx) 

and ljTreeR seq = let ctx,p = seq in match (ctx,p) with 
    | _,PArr(fi,a,b)                    ->  Node((LJ_Arr_R,    seq),[ljTree(a::ctx,b)])
    | _,PAnd(fi,a,b)                    ->  Node((LJ_And_R,    seq),[ljTree(ctx,a); ljTree(ctx,b)])
    | _,POr (fi,a,b)  ->  (try              Node((LJ_Or_R1,    seq),[ljTree(ctx,a)])
                            with NoRule ->  Node((LJ_Or_R2,    seq),[ljTree(ctx,b)]))
    | _,PTop(fi)                        ->  Node((LJ_Top_R,    seq),[Leaf])
    | q::ctx,p     when eq p q          ->  Node((LJ_AX,       seq),[Leaf]) 
    | _,_                               ->  raise NoRule  

and ljTreeL seq = let ctx,p = seq in match seq with 
    | PArr(fi,q,b)::q'::ctx',_ when atom q && eq q q'  
                                        ->  Node((LJ_Arr_P_L,  seq),[ljTree(b::q::ctx',p)])
    | PArr(fi,PAnd(_,a1,a2),b)::ctx',_  ->  Node((LJ_Arr_And_L,seq),[ljTree(PArr(fi,a1,PArr(fi,a2,b))::ctx',p)])
    | PArr(fi,PArr(_,a1,a2),b)::ctx',_  ->  Node((LJ_Arr_Arr_L,seq),[ljTree(PArr(fi,a2,b)::a1::ctx',a2);ljTree(b::ctx',p)])
    | PArr(fi,POr (_,a1,a2),b)::ctx',_  ->  Node((LJ_Arr_Or_L, seq),[ljTree(PArr(fi,a2,b)::PArr(fi,a1,b)::ctx',p)])
    | PArr(fi,PTop(_),b)::ctx',_        ->  Node((LJ_Arr_Top_L,seq),[ljTree(b::ctx',p)])
    | PAnd(fi,a,b)::ctx',_              ->  Node((LJ_And_L,    seq),[ljTree(b::a::ctx',p)])
    | POr (fi,a,b)::ctx',_              ->  Node((LJ_Or_L,     seq),[ljTree(a::ctx',p); ljTree(b::ctx',p)])
    | PBot(fi)::ctx',_                  ->  Node((LJ_Bot_L,    seq),[Leaf]) 
    | _,_                               ->  raise NoRule 


(*----- 2. LJ Proof Addition ----*)

type lj_p = 
    | T_VAR
    | T_ABSLEFT
    | T_CASEABS
    | T_UNCURRY
    | T_ABSUNIT
    | T_SUBSTABS
    | T_PAIRLEFT
    | T_CASE
    | T_BOT
    | T_ABS
    | T_PAIR
    | T_INL
    | T_INR
    | T_UNIT

let pr_ljp = function
    | T_VAR             -> pr"T-VAR"
    | T_ABSLEFT         -> pr"T-ABSL"
    | T_CASEABS         -> pr"T-CASEABS"
    | T_UNCURRY         -> pr"T-UNCURRY"
    | T_ABSUNIT         -> pr"T-ABSUNIT"
    | T_SUBSTABS        -> pr"T-SUBSTABS"
    | T_PAIRLEFT        -> pr"T-PAIRLEFT"
    | T_CASE            -> pr"T-CASE"
    | T_BOT             -> pr"T-BOT"
    | T_ABS             -> pr"T-ABS"
    | T_PAIR            -> pr"T-PAIR"
    | T_INL             -> pr"T-INL" 
    | T_INR             -> pr"T-INR" 
    | T_UNIT            -> pr"T-UNIT"

let rec pr_pseq = function 
    | [],(t,p)          -> pr"⊢ ";pr_Proof t;pr" : ";pr_Prop p
    | [(x,px)],p        -> pr_Proof x;pr":";pr_Prop px;pr" " ;pr_pseq([],p)
    | (x,px)::rest,p    -> pr_Proof x;pr":";pr_Prop px;pr", ";pr_pseq(rest,p)

let rec pr_lj_ptree prefix = function
    | Leaf                  -> pr""
    | Node(a,trees)         -> let l,s = a in 
        pr prefix;pr"[";pr_ljp l;pr"]    ";pr_pseq s;pn();
        (List.fold_left(fun x y->x;y)(pr"")(List.map(pr_lj_ptree(prefix^"    "))trees));;


let add_proof_var prop = Var(prop), prop 

(*
exception NonVarProofAdd

let add_proof_var = function
    | PId(_,_) as prop      ->  Var(prop),prop 
    | PTop(_) as prop       ->  Var(prop),prop 
    | PBot(_) as prop       -> Var(prop), prop 
    | _                     ->  raise NonVarProofAdd
*)


let rec add_proof_var_ctx = function 
    | []                    -> [] 
    | prop::rest            -> add_proof_var prop :: add_proof_var_ctx rest 

let add_proof_var_seq (ctx,p) = add_proof_var_ctx ctx, add_proof_var p 


let rec find prop = function 
    | []                    -> None 
    | (proof,prop')::rest   -> if eq prop prop' then Some proof else find prop rest 
(*
let rec typeof proof = function 
    | []                    -> None 
    | (proof',prop)::rest   -> if equiv proof proof' then Some prop else typeof proof rest 
*) 

exception CannotGetType

let rec typeof = function 
    | Var(p)                -> p 
    | Abs(x,t)              -> PArr(dummy,typeof x, typeof t) 
    | App(Any(_),_)         -> PBot(dummy) 
    | App(f,a)              -> let PArr(_,tyA,tyB) = typeof f in tyB
    | Pair(a,b)             -> PAnd(dummy,typeof a, typeof b) 
    | Fst(p)                -> let PAnd(_,tyA,_) = typeof p in tyA 
    | Snd(p)                -> let PAnd(_,_,tyB) = typeof p in tyB 
    | Inl(a)                -> raise CannotGetType
    | Inr(b)                -> raise CannotGetType 
    | Case(m,ac,bc)         -> let PArr(_,tyA,tyC) = typeof ac in tyC
    | Unit                  -> PTop(dummy) 

let rec make_subset_of_pctx pctx = function 
    | []                ->  []
    | prop::ctx         ->  match find prop pctx with 
                            | Some(proof)  -> (proof,prop)::(make_subset_of_pctx pctx ctx) 
                            | None          -> raise NoRule  

exception MatchFailure 

let rec add_proof       = function 
    (* Leaf (Terminal Node) *) 
    | Node((LJ_AX       ,seq),[Leaf])       ->  Node((T_VAR,     add_proof_var_seq seq),[Leaf])
    | Node((LJ_Top_R    ,seq),[Leaf])       ->  Node((T_VAR,     add_proof_var_seq seq),[Leaf])
    | Node((LJ_Bot_L    ,seq),[Leaf])       ->  let bot::ctx,p = seq in (* extract Bot, P *)  
                                                let x = Var(bot) in 
                                                let pseq = (x,bot)::(add_proof_var_ctx ctx), (App(Any(p),x), p) in 
                                                Node((T_BOT, pseq),[Leaf])
    (* Non Leaf (Non Terminal Node) *) 
    | Node((LJ_Arr_P_L  ,seq),[node_seq])   ->  let node_pseq = add_proof node_seq in 
                                                let tyPB::tyP::ctx,tyG = seq in 
                                                let PArr(_,tyP',tyB) = tyPB in 
                                                let Node((_,(pctx,(g,tyG'))),_) = node_pseq in
                                                if not (eq tyG tyG') then raise MatchFailure else 
                                                let Some(p) = find tyP pctx in 
                                                let Some(b) = find tyB pctx in  
                                                let pctx' = make_subset_of_pctx pctx ctx in 
                                                let tyPB = PArr(dummy,tyP,tyB) in 
                                                let f = Var(tyPB) in 
                                                let pseq = (f,tyPB)::(p,tyP)::pctx',(subst b (App(f,p)) g, tyG) in 
                                                Node((T_ABSLEFT, pseq),[node_pseq])
    | Node((LJ_Arr_Or_L,seq),[node_seq])    ->  let node_pseq = add_proof node_seq in 
                                                let tyAB_C::ctx,tyG = seq in 
                                                let PArr(_,POr(_,tyA,tyB),tyC) = tyAB_C in 
                                                let Node((_,(pctx,(g,tyG))),_) = node_pseq in 
                                                let Some(ac) = find (PArr(dummy,tyA,tyC)) pctx in 
                                                let Some(bc) = find (PArr(dummy,tyB,tyC)) pctx in 
                                                let a,b = Var(tyA),Var(tyB) in 
                                                let pctx' = make_subset_of_pctx pctx ctx in 
                                                let x = Var(tyAB_C) in  
                                                let subst_ac = subst ac (Abs(a,App(x,Inl(a)))) in 
                                                let subst_bc = subst bc (Abs(b,App(x,Inr(b)))) in 
                                                let pseq=(x,tyAB_C)::pctx',(subst_ac (subst_bc g),tyG) in  
                                                Node((T_CASEABS, pseq),[node_pseq])
    | Node((LJ_Arr_And_L,seq),[node_seq])   ->  let node_pseq = add_proof node_seq in
                                                let tyAB_C::ctx,tyG = seq in 
                                                let PArr(_,PAnd(_,tyA,tyB),tyC) = tyAB_C in (* extract tyA tyB *)  
                                                let Node((_,(pctx,(g,tyG'))),_) = node_pseq in (* extract previous pcontext *)
                                                let Some(abc)=find(PArr(dummy,tyA,PArr(dummy,tyB,tyC)))pctx in  (* extract \a.\b.c *) 
                                                let a,b = Var(tyA),Var(tyB) in 
                                                let x = Var(tyAB_C) in 
                                                let pctx' = make_subset_of_pctx pctx ctx in
                                                let abc' = Abs(a,Abs(b,App(x,Pair(a,b)))) in 
                                                let pseq = (x,tyAB_C)::pctx',(subst abc abc' g,tyG) in 
                                                Node((T_UNCURRY, pseq),[node_pseq])
    | Node((LJ_Arr_Top_L,seq),[node_seq])   ->  let node_pseq = add_proof node_seq in 
                                                let tyTB::ctx,tyG = seq in 
                                                let PArr(_,tyT,tyB) = tyTB in 
                                                let Node((_,(pctx,(g,tyG'))),_) = node_pseq in 
                                                let Some(b) = find tyB pctx in 
                                                let pctx' = make_subset_of_pctx pctx ctx in 
                                                let x = Var(tyTB) in 
                                                let pseq = (x,tyTB)::pctx',(subst b (App(x,Unit)) g,tyG) in
                                                Node((T_ABSUNIT, pseq),[node_pseq])
    | Node((LJ_Arr_Arr_L,seq),[n1;n2])      ->  let pn1 = add_proof n1 in
                                                let pn2 = add_proof n2 in 
                                                let tyABC::ctx,tyG = seq in 
                                                let PArr(_,PArr(_,tyA,tyB),tyC) = tyABC in
                                                let tyBC = PArr(dummy,tyB,tyC) in 
                                                let Node((_,(pctx1,_)),_) = pn1 in 
                                                let Node((_,(pctx2,_)),_) = pn2 in 
                                                let Some(a)     = find tyA pctx1 in 
                                                let Some(f)     = find tyBC pctx1 in
                                                let Some(c)     = find tyC pctx2 in 
                                                let Node((_,(_,(b,tyB'))),_) = pn1 in 
                                                if not (eq tyB tyB') then raise MatchFailure else 
                                                let Node((_,(_,(g,tyG'))),_) = pn2 in 
                                                if not (eq tyG tyG') then raise MatchFailure else 
                                                let x  = Var(tyABC) in 
                                                let b' = subst f (Abs(b,App(x,Abs(a,b)))) b in 
                                                let pctx = make_subset_of_pctx (pctx1@pctx2) ctx in 
                                                let pseq = (x,tyABC)::pctx,(subst c (App(x,Abs(a,b'))) g ,tyG) in
                                                Node((T_SUBSTABS, pseq),[pn1;pn2])
    | Node((LJ_And_L,seq),[node_seq])       ->  let node_pseq = add_proof node_seq in 
                                                let tyAB::ctx,tyG = seq in 
                                                let PAnd(_,tyA,tyB) = tyAB in           (* extract type A and type B *) 
                                                let Node((_,(pctx,(g,tyG'))),_) = node_pseq in (* extract previous context *)
                                                let Some(a) = find tyA pctx in   (* extract variable a of A *)
                                                let Some(b) = find tyB pctx in   (* extract variable b of B *) 
                                                let pctx' = make_subset_of_pctx pctx ctx in 
                                                let p = Var(tyAB) in 
                                                let pseq = (p,tyAB)::pctx',(( subst a (Fst(p)) (subst b (Snd(p)) g )),tyG) in  
                                                (* let pseq = (Pair(a,b),tyAB)::pctx',(g,tyG) in *) 
                                                Node((T_PAIRLEFT, pseq),[node_pseq])
    | Node((LJ_Or_L,seq),[n1;n2])           ->  let pn1 = add_proof n1 in 
                                                let pn2 = add_proof n2 in 
                                                let Node((_,(pctx1,(g,tyG))),_) = pn1 in 
                                                let Node((_,(pctx2,(g',tyG'))),_) = pn2 in 
                                                if not(eq tyG tyG') then raise InvalidContext else
                                                let tyA_B::ctx,tyG = seq in 
                                                let POr(_,tyA,tyB) = tyA_B in 
                                                let Some(a) = find tyA pctx1 in 
                                                let Some(b) = find tyB pctx2 in 
                                                let m = Var(PId(dummy,"m")) in 
                                                let result = Case(m,Abs(a,g),Abs(b,g')),tyG in 
                                                let pctx = make_subset_of_pctx (pctx1@pctx2) ctx in 
                                                let pseq = (m,tyA_B)::pctx,result in 
                                                Node((T_CASE, pseq),[pn1;pn2])
    | Node((LJ_Arr_R,seq),[node_seq])       ->  let node_pseq = add_proof node_seq in 
                                                let ctx,tyAB = seq in 
                                                let PArr(_,tyA,tyB) = tyAB in 
                                                let Node((_,(pctx,(b,tyB))),_) = node_pseq in 
                                                (* let Some(a) = find tyA pctx in  *)
                                                let a = Var(tyA) in 
                                                let pctx' = make_subset_of_pctx pctx ctx in 
                                                let pseq = pctx',(Abs(a,b),tyAB) in 
                                                Node((T_ABS, pseq),[node_pseq])
    | Node((LJ_And_R,seq),[n1;n2])          ->  let pn1 = add_proof n1 in 
                                                let Node((_,(pctx,(a,tyA))),_) = pn1 in 
                                                let pn2 = add_proof n2 in 
                                                let Node((_,(pctx,(b,tyB))),_) = pn2 in
                                                let pseq = pctx,(Pair(a,b),PAnd(dummy,tyA,tyB)) in 
                                                Node((T_PAIR, pseq),[pn1;pn2])
    | Node((LJ_Or_R1,seq),[node_seq])       ->  let node_pseq = add_proof node_seq in 
                                                let Node((_,(pctx,(a,tyA))),_) = node_pseq in 
                                                let _,POr(_,_,tyB) = seq in 
                                                let pseq = pctx,(Inl(a),POr(dummy,tyA,tyB)) in 
                                                Node((T_INL, pseq),[node_pseq])
    | Node((LJ_Or_R2,seq),[node_seq])       ->  let node_pseq = add_proof node_seq in 
                                                let Node((_,(pctx,(b,tyB))),_) = node_pseq in 
                                                let _,POr(_,tyA,_) = seq in 
                                                let pseq = pctx,(Inr(b),POr(dummy,tyA,tyB)) in 
                                                Node((T_INR, pseq),[node_pseq])
    | Node((LJ_Top_R,seq),_)                ->  let ctx,p = seq in 
                                                let pseq = add_proof_var_ctx ctx,(Unit,p) in 
                                                Node((T_UNIT, pseq),[Leaf])
    | _                                     ->  Leaf
                                                    

(* ---- 3. Convert Lambda Term  ---- *) 



(* ---- 4. NJ Tree Construction ---- *) 

type nj_p   = TVAR
            | TABS
            | TAPP
            | TPAIR
            | TPAIR1
            | TPAIR2
            | TINL
            | TINR
            | TCASE
            | TBOT
            | TTOP

let pr_njp  = function 
    | TVAR              -> pr"TVAR"      
    | TABS              -> pr"TABS"    
    | TAPP              -> pr"TAPP"     
    | TPAIR             -> pr"TPAIR"    
    | TPAIR1            -> pr"TPAIR1"    
    | TPAIR2            -> pr"TPAIR2"    
    | TINL              -> pr"TINL"    
    | TINR              -> pr"TINR"    
    | TCASE             -> pr"TCASE"    
    | TBOT              -> pr"TBOT"     
    | TTOP              -> pr"TTOP" 


let rec pr_nj_ptree prefix = function
    | Leaf                  -> pr""
    | Node(a,trees)         -> let l,s = a in 
        pr prefix;pr"[";pr_njp l;pr"]    ";pr_pseq s;pn();
        (List.fold_left(fun x y->x;y)(pr"")(List.map(pr_nj_ptree(prefix^"    "))trees));;


let rec njTree pseq = let pctx,(proof,prop) = pseq in match proof with 
    | Var(p)            ->  Node((TVAR,pseq),[Leaf])
    | Abs(x,t)          ->  let PArr(_,tyX,tyT) = prop in 
                            let pctx' = (x,tyX) :: pctx in 
                            let proof' = t in 
                            let prop' = tyT in 
                            let pseq' = pctx',(proof',prop') in 
                            Node((TABS,pseq),[njTree pseq'])
    | App(Any(tyA),m)   ->  let pseq' = pctx,(m,PBot(dummy)) in 
                            Node((TAPP,pseq),[njTree pseq'])
    | App(Var(tyAB),a)  ->  let PArr(_,tyA,tyB) = tyAB in 
                            let pseq1 = pctx,(Var(tyAB),tyAB) in 
                            let pseq2 = pctx,(a,tyA) in 
                            Node((TAPP,pseq),[njTree pseq1; njTree pseq2])
    | App(t1,t2)        ->  let tyT2 = typeof t2 in 
                            let pseq1 = pctx,(t1,PArr(dummy,tyT2,prop)) in 
                            let pseq2 = pctx,(t2,tyT2) in 
                            Node((TAPP,pseq),[njTree pseq1; njTree pseq2])
    | App(t1,t2)        ->  let tyT1 = typeof t1 in 
                            let PArr(_,tyT2,_) = tyT1 in 
                            let pseq1 = pctx,(t1,tyT1) in 
                            let pseq2 = pctx,(t2,tyT2) in 
                            Node((TAPP,pseq),[njTree pseq1; njTree pseq2])
    | Pair(a,b)         ->  let PAnd(_,tyA,tyB) = prop in 
                            let pseq1 = pctx,(a,tyA) in 
                            let pseq2 = pctx,(b,tyB) in 
                            Node((TPAIR,pseq),[njTree pseq1; njTree pseq2]) 
    | Fst(p)            ->  let tyP = typeof p in 
                            let pseq' = pctx,(p,tyP) in 
                            Node((TPAIR1,pseq),[njTree pseq']) 
    | Snd(p)            ->  let tyP = typeof p in 
                            let pseq' = pctx,(p,tyP) in 
                            Node((TPAIR2,pseq),[njTree pseq']) 
    | Inl(p)            ->  let POr(_,tyA,_) = prop in 
                            let pseq' = pctx,(p,tyA) in 
                            Node((TINL,pseq),[njTree pseq']) 
    | Inr(p)            ->  let POr(_,_,tyB) = prop in 
                            let pseq' = pctx,(p,tyB) in 
                            Node((TINR,pseq),[njTree pseq']) 
    | Case(m,ac,bc)     ->  let Abs(a,c1) = ac in 
                            let Abs(b,c2) = bc in 
                            let Var(tyA),Var(tyB) = a,b in 
                            let pctx1 = (a,tyA) :: pctx in 
                            let pctx2 = (b,tyB) :: pctx in
                            let tyA_B = POr(dummy,tyA,tyB) in 
                            let pseq' = pctx,(m,tyA_B) in 
                            let pseq1 = pctx1,(c1,prop) in 
                            let pseq2 = pctx2,(c2,prop) in 
                            Node((TCASE,pseq),[njTree pseq'; njTree pseq1; njTree pseq2]) 
    | Unit              ->  Node((TTOP,pseq),[Leaf])
                            


(* ---- 5. Removing Proof       ---- *)

let rec remove_proof_pctx = function 
    | []    -> []
    | (proof,prop)::rest -> prop :: remove_proof_pctx rest

let remove_proof_pseq = function 
    | pctx,(proof,prop)  -> remove_proof_pctx pctx, prop 

let rec remove_proof = function 
    | Leaf          -> Leaf 
    | Node((label,pseq),node_pseq) -> Node((label, remove_proof_pseq pseq),List.map remove_proof node_pseq)


let rec pr_nj_tree prefix = function
    | Leaf                  -> pr""
    | Node(a,trees)         -> let l,s = a in 
        pr prefix;pr"[";pr_njp l;pr"]    ";pr_seq s;pn();
        (List.fold_left(fun x y->x;y)(pr"")(List.map(pr_nj_tree(prefix^"    "))trees));;



let process_cmd ctx = function 
  | Inversion(fi,p) ->  pr_Prop p;pn();
                        pr"----------------------------------";pn();
                        let lj = ljTreeR([],p) in 
                        pr_lj_tree "" lj; 
                        pn();pn();pn();
                        let plj = add_proof lj in 
                        pr_lj_ptree "" plj;
                        pn();pn();pn();
                        let Node((_,pseq),_) = plj in
                        let pctx,(proof,prop) = pseq in 
                        pr"Now reconstruct proof tree from: \n";
                        pr_Proof proof;
                        pn();pn();pn();
                        let pnj = njTree pseq in 
                        pr_nj_ptree "" pnj; 
                        pn();pn();pn();
                        let nj = remove_proof pnj in 
                        pr_nj_tree "" nj;
                        pn();pn();pn();
                        ctx  

let rec process_cmds ctx = function 
    | []                ->  ctx
    | cmd :: cmds       ->  let ctx' = process_cmd ctx cmd in 
                            Format.print_flush(); process_cmds ctx' cmds ;;

