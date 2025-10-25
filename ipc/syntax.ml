open Support


type prop   = 
    | PId   of info * string
    | PVar  of info * int  * int  
    | PAnd  of info * prop * prop
    | POr   of info * prop * prop
    | PArr  of info * prop * prop 
    | PBot  of info 
    | PTop  of info 

let rec eq a b = match a,b with 
    | PVar(_,i,_),PVar(_,j,_)       -> i=j
    | PId(_,s),PId(_,t)             -> s=t
    | PBot(_),PBot(_)               -> true
    | PTop(_),PTop(_)               -> true
    | PArr(_,a1,a2),PArr(_,b1,b2)   -> eq a1 b1 && eq a2 b2
    | PAnd(_,a1,a2),PAnd(_,b1,b2)   -> eq a1 b1 && eq a2 b2
    | POr (_,a1,a2),POr (_,b1,b2)   -> eq a1 b1 && eq a2 b2
    | _,_                           -> false 

type proof  =
    | Var       of prop  
    | Any       of prop 
    | Unit       
    | Abs       of proof * proof 
    | App       of proof * proof 
    | Pair      of proof * proof 
    | Inr       of proof
    | Inl       of proof
    | Case      of proof * proof * proof 
    | Uncurry   of proof 
    | Fst       of proof 
    | Snd       of proof 


let rec isboundvar str = function 
    | Var(PId(_,s))     -> str = s 
    | Abs(a,b)          -> isboundvar str a || isboundvar str b 
    | App(a,b)          -> isboundvar str a || isboundvar str b 
    | Pair(a,b)         -> isboundvar str a || isboundvar str b 
    | Inl(a)            -> isboundvar str a 
    | Inr(b)            -> isboundvar str b
    | Case(m,f,g)       -> isboundvar str m || isboundvar str f || isboundvar str g 
    | Uncurry(f)        -> isboundvar str f 
    | _                 -> false 

let rec freshvar str term   =   if isboundvar str term 
                                    then freshvar (str ^ "'") term 
                                    else Var(PId(dummy,str))


let atom = function
    | PVar(_)
    | PId(_)
    | PBot(_)
    | PTop(_)   -> true
    | _         -> false 

type bind   = 
    | BindName              (* for parser *)  
    | BindProp of prop      

type context = (string * bind) list

type command = 
    | Inversion     of info * prop 
    | Bind          of info * string * bind 

(* Context Management *)
let emptyctx                    =   []
let ctxlen ctx                  =   List.length ctx
let addbind ctx x bind          =   (x,bind) :: ctx
let addname ctx x               =  addbind ctx x BindName
let rec isbound x               =   function 
    | []                                -> false
    | (y,_) :: ctx                      -> if y=x then true else isbound x ctx ;;
let findvar ctx x               =  if isbound x ctx 
                                        then ctx 
                                        else addname ctx x ;;
let rec pickfresh ctx x         =   if isbound x ctx 
                                        then pickfresh ctx (x^"'") 
                                        else ((x,BindName)::ctx), x;;

let index2name fi ctx n         =   try let xn,_ = List.nth ctx n in xn 
                                    with Failure _ -> err fi "Variable Lookup Failure";;
let rec name2index fi ctx xn    =   match ctx with
    | []                            ->  err fi ("Identifier "^xn^" is unbound")
    | (x,_) :: rest                 ->  if x=xn then 0 else 1+(name2index fi rest xn);;

(* Shifting *)

let rec shift d c = function
    | PVar(fi,i,n)                  ->  if i < c then PVar(fi,i,n+d) else PVar(fi,i+d,n+d)
    | POr (fi,a,b)                   -> POr (fi,shift d c a,shift d c b)
    | PAnd(fi,a,b)                   -> PAnd(fi,shift d c a,shift d c b)
    | PArr(fi,a,b)                   -> PArr(fi,shift d c a,shift d c b)
    | p -> p


(* Substitution *)

let rec equiv a b = match (a,b) with 
    | Var(p),Var(q)         -> eq p q 
    | Any(p),Any(q)         -> eq p q 
    | _                     -> false 


let rec subst a t = function 
    | Var(prop)             -> if equiv a (Var(prop)) then t else Var(prop)     
    | Any(prop)             -> if equiv a (Any(prop)) then t else Any(prop) 
    | Unit                  -> Unit 
    | Abs(x,p)              -> Abs(x, subst a t p) 
    | App(p1,p2)            -> App(subst a t p1, subst a t p2) 
    | Pair(p1,p2)           -> Pair(subst a t p1, subst a t p2) 
    | Inr(p)                -> Inr(subst a t p) 
    | Inl(p)                -> Inl(subst a t p)
    | Case(m,p1,p2)         -> Case(subst a t m, subst a t p1, subst a t p2) 
    | Uncurry(p)            -> Uncurry(subst a t p) 
    | Fst(p)                -> Fst(subst a t p) 
    | Snd(p)                -> Snd(subst a t p) 


(* Extracting File Info *) 
let propInfo = function
    | PAnd(fi,_,_)                  ->  fi 
    | POr(fi,_,_)                   ->  fi
    | PArr(fi,_,_)                  ->  fi
    | PBot(fi)                      ->  fi
    | PTop(fi)                      ->  fi 
    | PVar(fi,_,_)                  ->  fi 
    | PId(fi,_)                     ->  fi 

(* Bind *)
let rec getbind fi ctx i        =   try let _,bind = List.nth ctx i in (* bindshift(i+1) *) bind
                                    with Failure _ -> err fi "no such bind";;

(* IsValue *) 

(* Printing *) 
(*
let obox0 ()    = open_hvbox 0;;
let oobox0()    = open_hovbox 0;;
let obox()      = open_hvbox 2;;
let cbox()      = close_box();;
let br()        = print_break 0 0 ;;
let ps()        = print_space();;
let psbr outer  = if outer then ps() else br();;
   *)
let small       = function
    | PVar(_,_,_)           -> true
    | PBot(_)               -> true
    | PTop(_)               -> true
    | _                     -> false;;

(* Prop Print *)

let rec pr_Prop = function 
  | PArr(fi,a,PBot(_))  -> pr"¬";pr_Prop a;
  | PArr(fi,a,b)        -> pr"(";pr_Prop a; pr"→"; pr_Prop b;pr")"
  | p                   -> pr_AppProp p

and pr_AppProp = function
  | PAnd(fi,a,b)        -> pr"(";pr_Prop a; pr"⋀"; pr_Prop b;pr")"
  | POr (fi,a,b)        -> pr"(";pr_Prop a; pr"⋁"; pr_Prop b;pr")"
  | p                   -> pr_AProp p 

and pr_AProp = function 
    | PVar(fi,i,n)      -> pr ("X" ^ string_of_int i)
                            (*if n=(ctxlen ctx) then pr(index2name fi ctx i) else err fi "ctx error"*)
    | PId(fi,s)         -> pr s 
    | PBot(fi)          -> pr"⊥" (* type "Ctrl-v u22A5" in Vim *)
    | PTop(fi)          -> pr"⊤" (* type "Ctrl-v u22A4" in Vim *);;

(*
let rec pr_Prop ctx = function 
    | PAnd(fi,a,b)      -> "(" ^ pr_Prop ctx a ^ " ⋀ " ^ pr_Prop ctx b ^ ")" ;
    | POr (fi,a,b)      -> "(" ^ pr_Prop ctx a ^ " ⋁ " ^ pr_Prop ctx b ^ ")" ; 
    | PArr(fi,a,PBot(_))-> "¬" ^ pr_Prop ctx a ;
    | PArr(fi,a,b)      -> "(" ^ pr_Prop ctx a ^ " → " ^ pr_Prop ctx b ^ ")" ;
    | p                 -> pr_AProp ctx p

and pr_AProp ctx    = function 
    | PVar(fi,i,n)      -> "X" ^ string_of_int i 
    | PId(fi,s)         -> s 
    | PBot(fi)          -> "⊥" (* type "Ctrl-v u22A5" in Vim *)
    | PTop(fi)          -> "⊤" (* type "Ctrl-v u22A4" in Vim *);;
*)

let rec pr_Proof = function
    | Var(PId(_,s))         ->  pr (String.lowercase_ascii s)
    | Var(PVar(_,i,_))      ->  pr "X";pr (string_of_int i) 
    | Var(p)                ->  pr "x";pr_Prop p
    | Any(p)                ->  pr "∅(";pr_Prop p;pr ")"
    | Unit                  ->  pr "*"
    | Abs(x,t)              ->  pr "λ"; pr_Proof x; pr"."; pr_Proof t
    | App(t1,t2)            ->  pr"(";pr_Proof t1;ps();pr_Proof t2;pr")"
    | Pair(a,b)             ->  pr"("; pr_Proof a;pr",";pr_Proof b;pr")"
    | Inr(t)                ->  pr"inr("; pr_Proof t;pr")"
    | Inl(t)                ->  pr"inl("; pr_Proof t;pr")"
    | Case(m,a,b)           ->  pr"case("; pr_Proof m;pr",";pr_Proof a;pr",";pr_Proof b;pr")"
    | Uncurry(t)            ->  pr"uncurry(";pr_Proof t;pr")"
    | Fst(t)                ->  pr"fst("; pr_Proof t; pr")"
    | Snd(t)                ->  pr"snd("; pr_Proof t; pr")"

let rec pr_ctx = function 
    | [] -> ()
    | a::rest -> pr_Prop a ;pr","; pr_ctx rest 
