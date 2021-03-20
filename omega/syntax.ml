open Format
open Support
open Printf 

(* -------------------------------------------------- *) 
(* Datatypes *)

type kind   = 
    | Kn 
    | KnArr of kind * kind 

type ty     =
    (* Omega *) 
    | TyVar     of int * int
    | TyAbs     of string * kind * ty 
    | TyApp     of ty * ty 
    (* Lambda *) 
    | TyArr     of ty * ty
    | TyBool
    | TyNat 
;;

type term =
    (* Lambda *) 
    | TmVar     of info * int * int 
    | TmAbs     of info * string * ty * term 
    | TmApp     of info * term * term 
    (* Arith *) 
    | TmTrue    of info
    | TmFalse   of info
    | TmIf      of info * term * term * term
    | TmZero    of info
    | TmSucc    of info * term
    | TmPred    of info * term
    | TmIsZero  of info * term
;;

type binding = 
    | BindName
    | BindTmVar of ty
    | BindTyVar of kind 
    | BindTmAbb of term * (ty option) 
    | BindTyAbb of ty * (kind option) 
;;

type context = 
    (string * binding) list 
;;

type command =
    | Eval of info * term
    | Bind of info * string * binding 
;;


let e = error 
(* -------------------------------------------------- *) 
(* Context Management *) 

let emptyctx                    =   []
let ctxlen          ctx         =   List.length ctx
let addbind         ctx x bind  =   (x,bind) :: ctx 
let addname         ctx x       =   addbind ctx x BindName 
let rec isnamebound ctx x       =   match ctx with 
    | []                                -> false
    | (y,_) :: rest                     -> if y=x then true else isnamebound rest x;;
let rec pickfreshname ctx x     =   if isnamebound ctx x 
                                        then pickfreshname ctx (x^"'")
                                        else (x,BindName)::ctx, x
let index2name fi ctx x         =   try let (xn,_) = List.nth ctx x in xn with 
    | Failure _                         -> e fi "variable lookup failure";;
let rec name2index fi ctx x     =   match ctx with 
    | []                                -> e fi ("Identifier " ^ x ^ " is unbound")
    | (y,_) :: rest                     -> if y=x then 0 else 1 + (name2index fi rest x) ;;


(* -------------------------------------------------- *)
(* Type Shifting *) 
(* tymap change the cutoff `c` i.e. if we go inside of lambda `c++` *) 
(* if we encounter variable we shfit it by d since `onvar := shift by d` *) 

let rec tymap onVar c    = 
    let rec walk c  = function 
    | TyVar(k,n)                -> onVar c k n 
    | TyAbs(tyX,k,tyT)          -> TyAbs(tyX,k,walk(c+1)tyT) 
    | TyApp(tyT1,tyT2)          -> TyApp(walk c tyT1, walk c tyT2) 
    (* proper types *) 
    | TyArr(tyT1,tyT2)          -> TyArr(walk c tyT1, walk c tyT2) 
    | TyBool                    -> TyBool
    | TyNat                     -> TyNat in 
    walk c

let tyShiftOnVar d          = fun c k n -> if k>=c then TyVar(k+d,n+d) else TyVar(k,n+d) 
let tyShiftAbove d          = tymap (tyShiftOnVar d) 
let tyShift d               = tyShiftAbove d 0 

(* -------------------------------------------------- *)
(* Type Substitution *) 

let tySubstOnVar j tyS tyT  = fun c k n -> if k=j+c then tyShift c tyS else TyVar(k,n) 
let tySubst      j tyS tyT  = tymap(tySubstOnVar j tyS tyT)0 tyT
let tySubstTop     tyS tyT  = tyShift(-1)(tySubst 0(tyShift 1 tyS)tyT)



(* -------------------------------------------------- *) 
(* Term Shifting *)

let rec tmmap onVar onTy c   = 
    let rec walk c = function 
    | TmVar(fi,k,n)             -> onVar fi c k n
    | TmAbs(fi,x,tyT,t2)        -> TmAbs(fi,x,onTy c tyT,walk (c+1)t2)
    | TmApp(fi,t1,t2)           -> TmApp(fi, walk c t1, walk c t2) 
    | TmIf(fi,t1,t2,t3)         -> TmIf(fi,walk c t1, walk c t2, walk c t3) 
    | TmSucc(fi,t)              -> TmSucc(fi, walk c t) 
    | TmPred(fi,t)              -> TmPred(fi, walk c t) 
    | TmIsZero(fi,t)            -> TmIsZero(fi, walk c t)
    | x                         -> x
    in walk c 

let tmShiftOnVar d        = fun fi c k n->if k>=c then TmVar(fi,k+d,n+d) else TmVar(fi,k,n+d)
let tmShiftAbove d        = tmmap(tmShiftOnVar d)(fun j tyT->tyT) 
let tmShift d             = if d>=0 then pe("SHIFT: "^(string_of_int d));tmShiftAbove d 0 

(* -------------------------------------------------- *) 
(* Term Substitution *) 
let tmSubstOnVar j s t    = fun fi c k n ->if k=j+c then tmShift c s else TmVar(fi,k,n) 
let tmSubst j s t         = tmmap (tmSubstOnVar j s t)(fun j ty->ty)0 t
let tmSubstTop s t        = pe"SUBSTITUTE: "; tmShift(-1)(tmSubst 0(tmShift 1 s)t) 

(* -------------------------------------------------- *)
(* Type Substitution on Term *) 
let idOnVar                 = fun fi c k n -> TmVar(fi,k,n)
let rec tytmSubst j tyS t   = tmmap idOnVar(fun j -> tySubst j tyS) j t 
let tytmSubstTop tyS t      = tmShift(-1)(tytmSubst 0 (tyShift 1 tyS)t)


(* -------------------------------------------------- *) 
(* Bind Shifting *)

let bindShift d             = function 
    | BindName                  -> BindName
    | BindTyVar(k)              -> BindTyVar(k)
    | BindTyAbb(tyT,k)          -> BindTyAbb(tyShift d tyT,k)
    | BindTmVar(tyT)            -> BindTmVar(tyShift d tyT)
    | BindTmAbb(t,None)         -> BindTmAbb(tmShift d t,None)
    | BindTmAbb(t,Some(tyT))    -> BindTmAbb(tmShift d t,Some(tyShift d tyT))

(* -------------------------------------------------- *)
let err_getbind                 =   sprintf"getbind: VarLookupFail: offset:%d,ctx size:%d"
let rec getbind fi ctx n        =   try 
    let (_,bind) = List.nth ctx n in 
    bindShift (n+1) bind with 
        | Failure _                     -> e fi(err_getbind n(List.length ctx)) ;; 

let gettype fi ctx n            =   match getbind fi ctx n with 
    | BindTmVar(tyT)                    -> tyT
    | BindTmAbb(_,Some(tyT))            -> tyT
    | BindTmAbb(_,None)                 -> e fi("gettype: NoType for "^(index2name fi ctx n))
    | _                                 -> e fi("gettype: Wrong bind "^(index2name fi ctx n))


(* -------------------------------------------------- *) 
(* Extracting file info *)
let tmInfo  = function 
    | TmVar(fi,_,_)         -> fi
    | TmAbs(fi,_,_,_)       -> fi
    | TmApp(fi,_,_)         -> fi 
    | TmTrue(fi)            -> fi
    | TmFalse(fi)           -> fi
    | TmIf(fi,_,_,_)        -> fi
    | TmZero(fi)            -> fi
    | TmSucc(fi,_)          -> fi
    | TmPred(fi,_)          -> fi
    | TmIsZero(fi,_)        -> fi 

(* -------------------------------------------------- *) 
(* Printing *)
let obox0() = open_hvbox 0
let obox()  = open_hvbox 2
let cbox()  = close_box()
let break() = print_break 0 0

let small   = function
    | TmVar(_,_,_)              -> true
    | _                         -> false 
;;

(* -------------------------------------------------- *) 
let rec pr_kn = function 
    | Kn            -> pr"*";
    | KnArr(k1,k2)  -> pr_kn k1;pr"=>";pr_kn k2 
(* -------------------------------------------------- *) 
(* Type Print *) 
let rec prTy outer ctx    = function
    | TyAbs(tyX,k,tyT)          ->  let (ctx',tyX') = pickfreshname ctx tyX in            
                                    pr"(λ";pr tyX; pr_kn k; pr".";
                                    prTy outer ctx' tyT; pr")"        
    | tyT                       ->  prArrTy outer ctx tyT
and prArrTy outer ctx       = function
    | TyArr(tyT1,tyT2)          ->           prATy outer ctx tyT1;
                                    pr " → "; 
                                    prATy outer ctx tyT2         
    | tyT                       ->  prAppTy outer ctx tyT
and prAppTy outer ctx       = function 
    | TyApp(tyT1,tyT2)          ->  pr"(";prTy outer ctx tyT1;pr" "; prTy outer ctx tyT2;pr")"
    | tyT                       ->  prATy outer ctx tyT
and prATy outer ctx         = function
    | TyVar(k,n)                ->  pr (index2name dummy ctx k)
    | TyBool                    ->  pr "𝐁" 
    | TyNat                     ->  pr "𝐍"
    | tyT                       ->  pr "("; prTy outer ctx tyT; pr ")"
;;

let pr_ty ctx tyT           = prTy true ctx tyT

(* -------------------------------------------------- *) 
(* Term Print *)
let rec prTm outer ctx  = function 
    | TmAbs(fi,x,tyT1,t2)            ->  let (ctx',x') = pickfreshname ctx x in obox();
        pr "λ"; pr x'; pr ":"; prTy false ctx tyT1; pr "."; 
        if small t2&&not outer then break() else ps();
        prTm outer ctx' t2;
        cbox()
    | TmIf(fi, t1, t2, t3)      ->  obox0();
        pr "if "  ; prTm false ctx t1; ps();
        pr "then "; prTm false ctx t2; ps();
        pr "else "; prTm false ctx t3;
        cbox()
    | t                         -> prAppTm outer ctx t
and prAppTm outer ctx   = function 
    | TmApp(fi, t1, t2)         ->  obox0(); prAppTm false ctx t1; ps(); 
                                    prATm false ctx t2; cbox();
    | TmPred(_,t1)              ->  pr "pred ";     prATm false ctx t1
    | TmIsZero(_,t1)            ->  pr "iszero ";   prATm false ctx t1
    | t                         ->  prATm outer ctx t

and prATm outer ctx     = function 
    | TmVar(fi,x,n)             -> if ctxlen ctx = n 
        then pr (index2name fi ctx x)
        else pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n) ^ " in {" 
                ^ (List.fold_left (fun s(x,_)-> s^" "^x) "" ctx) ^ " }]") 
    | TmTrue(_)                 ->  pr "true"
    | TmFalse(_)                ->  pr "false"
    | TmZero(fi)                ->  pr "0"
    | TmSucc(_,t1)              ->  let rec f n = function 
        | TmZero(_)                 -> pr (string_of_int n)
        | TmSucc(_,s)               -> f (n+1) s
        | _                         -> (pr "(succ "; prATm false ctx t1; pr ")")
    in f 1 t1
    | t                         ->  pr "("; prTm outer ctx t; pr ")"

let pr_tm ctx t = prTm true ctx t 


let prbindty ctx = function
    | BindTmVar(tyT)            ->  pr " : " ; prTy false ctx tyT 
    | BindTyVar(k)              ->  pr " :: "; pr_kn k 
    | BindTyAbb(tyT,Some(k))    ->  pr " := "; prTy false ctx tyT; pr_kn k 
    | BindTyAbb(tyT,None)       ->  pr " := "; prTy false ctx tyT          
    | _                         ->  ()

let rec pr_ctx ctx = 
    let rec loop ctx = function   
    | [] -> ()
    | (t,b)::rest -> pr"(";pr t;prbindty ctx b;pr")";loop ctx rest in
    pr"Γ ≔{ ";loop ctx ctx; pr" }";pn() 
