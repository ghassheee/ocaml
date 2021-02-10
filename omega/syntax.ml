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
    | TyId      of string 
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


(* -------------------------------------------------- *) 
(* Context Management *) 

let emptycontext                = []
let ctxlength       ctx         = List.length ctx
let addbind         ctx x bind  = (x,bind) :: ctx 
let addname         ctx x       = addbind ctx x BindName 
let rec isnamebound ctx x       = match ctx with 
    | []                            -> false
    | (y,_) :: rest                 -> if y=x then true else isnamebound rest x;;
let rec pickfreshname ctx x     = if isnamebound ctx x 
                                    then pickfreshname ctx (x^"'")
                                    else ((x,BindName)::ctx), x;;
let index2name fi ctx x         = try let (xn,_) = List.nth ctx x in xn with 
    | Failure _                  -> error fi "variable lookup failure";;
let rec name2index fi ctx x     = match ctx with 
    | []                            -> error fi ("Identifier " ^ x ^ " is unbound")
    | (y,_) :: rest                 -> if y=x then 0 else 1 + (name2index fi rest x) ;;

let rec getbind fi n            = function
    | ctx                           -> try     let (_,bind)    = List.nth ctx n in bind with 
        | Failure _ -> let e=sprintf "getbind: Variable lookup failure: offset:%d,ctx size:%d" in
                       error fi (e n(List.length ctx));;

let gettype fi ctx n            = match getbind fi n ctx with 
    | BindTmVar(tyT)                -> tyT
    | _                             -> error fi("gettype: Wrong binding"^(index2name fi ctx n))


(* -------------------------------------------------- *) 
(* Shifting *)

let rec walk funOnVar c   = let f = funOnVar in function 
    | TmVar(fi,x,n)             -> funOnVar fi c x n
    | TmAbs(fi,x,tyT,t2)        -> TmAbs(fi,x,tyT,walk f(c+1)t2)
    | TmApp(fi,t1,t2)           -> TmApp(fi, walk f c t1, walk f c t2) 
    | TmIf(fi,t1,t2,t3)         -> TmIf(fi,walk f c t1, walk f c t2, walk f c t3) 
    | TmSucc(fi,t)              -> TmSucc(fi, walk f c t) 
    | TmPred(fi,t)              -> TmPred(fi, walk f c t) 
    | TmIsZero(fi,t)            -> TmIsZero(fi, walk f c t)
    | x                         -> x

let termShiftOnVar d        = fun fi c x n ->   if x>=c then TmVar(fi,x+d,n+d) else TmVar(fi,x,n+d)
let termShiftAbove d        = walk (termShiftOnVar d)
let termShift d             = if d>=0 then print_endline ("SHIFT: "^(string_of_int d));termShiftAbove d 0 

(* -------------------------------------------------- *) 
(* Substitution *) 
let termSubstOnVar j s t    = fun fi c x n ->   if x=j+c then termShift c s else TmVar(fi, x, n) 
let termSubst j s t         = walk (termSubstOnVar j s t) 0 t
let termSubstTop s t        = print_endline "SUBSTITUTE: "; termShift (-1) (termSubst 0 (termShift 1 s) t) 

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
(* Type Print *) 
let rec prType outer      = function
    | tyT                       ->  prArrowType outer tyT
and prArrowType outer     = function
    | TyArr(tyT1,tyT2)          ->  obox0(); prAType false tyT1;
                                    if outer then pr " "; pr "→"; if outer then ps () else break();
                                    prAType outer tyT2; cbox()
    | tyT                       ->  prAType outer tyT
and prAType outer         = function
    | TyBool                    ->  pr "𝐁" 
    | TyNat                     ->  pr "𝐍"
    | tyT                       ->  pr "("; prType outer tyT; pr ")"
;;

let pr_ty tyT                 = prType true tyT

(* -------------------------------------------------- *) 
(* Term Print *)
let rec prTerm outer ctx  = function 
    | TmAbs(fi,x,tyT1,t2)            ->  let (ctx',x') = pickfreshname ctx x in obox();
        pr "λ"; pr x'; pr ":"; prType false tyT1; pr "."; 
        if (small t2) && not outer then break() else ps();
        prTerm outer ctx' t2;
        cbox()
    | TmIf(fi, t1, t2, t3)      ->  obox0();
        pr "if "  ; prTerm false ctx t1; ps();
        pr "then "; prTerm false ctx t2; ps();
        pr "else "; prTerm false ctx t3;
        cbox()
    | t                         -> prAppTerm outer ctx t
and prAppTerm outer ctx   = function 
    | TmApp(fi, t1, t2)         ->  obox0(); prAppTerm false ctx t1; ps(); 
                                    prATerm false ctx t2; cbox();
    | TmPred(_,t1)              ->  pr "pred ";     prATerm false ctx t1
    | TmIsZero(_,t1)            ->  pr "iszero ";   prATerm false ctx t1
    | t                         ->  prATerm outer ctx t

and prATerm outer ctx     = function 
    | TmVar(fi,x,n)             -> if ctxlength ctx = n 
        then pr (index2name fi ctx x)
        else pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n) ^ " in {" 
                ^ (List.fold_left (fun s(x,_)-> s^" "^x) "" ctx) ^ " }]") 
    | TmTrue(_)                 ->  pr "true"
    | TmFalse(_)                ->  pr "false"
    | TmZero(fi)                ->  pr "0"
    | TmSucc(_,t1)              ->  let rec f n = function 
        | TmZero(_)                 -> pr (string_of_int n)
        | TmSucc(_,s)               -> f (n+1) s
        | _                         -> (pr "(succ "; prATerm false ctx t1; pr ")")
    in f 1 t1
    | t                         ->  pr "("; prTerm outer ctx t; pr ")"

let pr_tm t = prTerm true t 


