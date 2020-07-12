open Format
open Support.Error
open Support.Pervasive

let soi = string_of_int
(* -------------------------------------------------- *) 
(* Datatypes *)

type ty     =
    | TyVar     of int * int 
    | TyId      of string 
    | TyTop
    | TyRecord  of (string * ty) list 
    | TyArr     of ty * ty
    | TyList    of ty 
    | TyFloat 
    | TyString  
    | TyUnit
    | TyBool
    | TyNat 
    | TyAll     of string * ty
    | TySome    of string * ty 
;;

type term =
    (* Quantifier *) 
    | TmPack        of info * ty * term * ty 
    | TmUnpack      of info * string * string * term * term 
    | TmTAbs        of info * string * term
    | TmTApp        of info * term * ty 
    (* Fix *)
    | TmFix         of info * term 
    (* Float / String  *) 
    | TmString      of info * string 
    | TmFloat       of info * float
    | TmTimesfloat  of info * term * term 
    (* Record *)
    | TmProj        of info * term * string  
    | TmRecord      of info * (string * term) list 
    (* Ascription *) 
    | TmAscribe     of info * term * ty
    (* Unit *)
    | TmUnit        of info 
    (* Let  *)
    | TmLet         of info * string * term * term
    (* Lambda *) 
    | TmVar         of info * int * int 
    | TmAbs         of info * string * ty * term 
    | TmApp         of info * term * term 
    (* Bool *) 
    | TmTrue        of info
    | TmFalse       of info
    | TmIf          of info * term * term * term
    (* Arith *) 
    | TmZero        of info
    | TmSucc        of info * term
    | TmPred        of info * term
    | TmIsZero      of info * term
;;

type bind = 
    | BindName
    | BindTyVar
    | BindTmVar     of ty
    | BindTyAbb     of ty 
    | BindTmAbb     of term * (ty option) 
;;

type context =      (string * bind) list 

type command =
    | Eval      of info * term
    | Bind      of info * string * bind 
    | SomeBind  of info * string * string * term 
;;


(* -------------------------------------------------- *) 
(* Context Management *) 

let emptyctx                    =   []
let ctxlen      ctx             =   List.length ctx
let addbind     ctx x bind      =   (x,bind) :: ctx 
let addname     ctx x           =   addbind ctx x BindName 

let rec isbound ctx x           =   match ctx with 
    | []                                -> false
    | (y,_) :: rest                     -> if y=x then true else isbound rest x;;

let rec index2name fi ctx n     =   try let (xn,_) = List.nth ctx n in xn 
                                    with Failure _ -> error fi "Variable Lookup Failure"

let rec name2index fi ctx x     =   match ctx with 
    | []                            -> error fi("Unbound Identifier "^x)
    | (y,_)::rest                   -> if x=y then 0 else 1+(name2index fi rest x)

(* -------------------------------------------------- *) 
(* Shifting *)

let rec tyWalk onVar c          = let f = onVar in function 
    | TyVar(x,n)                -> onVar c x n
    | TyRecord(fieldtys)        -> TyRecord(List.map (fun(l,tyT)->(l,tyWalk f c tyT)) fieldtys) 
    | TyArr(tyT1,tyT2)          -> TyArr(tyWalk f c tyT1,tyWalk f c tyT2) 
    | TyAll(tyX,tyT)            -> TyAll(tyX, tyWalk f(c+1)tyT)
    | TySome(tyX,tyT)           -> TySome(tyX, tyWalk f(c+1)tyT)
    | tyT                       -> tyT

let rec tmWalk onVar onTy c   = let (f,g) = (onVar,onTy) in function 
    | TmVar(fi,x,n)             -> onVar fi c x n
    | TmLet(fi,x,t1,t2)         -> TmLet(fi,x,tmWalk f g c t1, tmWalk f g(c+1)t2) 
    | TmAbs(fi,x,tyT,t2)        -> TmAbs(fi,x,g c tyT,tmWalk f g(c+1)t2)
    | TmApp(fi,t1,t2)           -> TmApp(fi,tmWalk f g c t1, tmWalk f g c t2) 
    | TmIf(fi,t1,t2,t3)         -> TmIf(fi,tmWalk f g c t1, tmWalk f g c t2, tmWalk f g c t3) 
    | TmSucc(fi,t)              -> TmSucc(fi,tmWalk f g c t) 
    | TmPred(fi,t)              -> TmPred(fi,tmWalk f g c t) 
    | TmIsZero(fi,t)            -> TmIsZero(fi,tmWalk f g c t)
    | TmAscribe(fi,t,tyT)       -> TmAscribe(fi,tmWalk f g c t,g c tyT) 
    | TmRecord(fi,tl)           -> TmRecord(fi,List.map(fun(l,t)->(l,tmWalk f g c t))tl)  
    | TmProj(fi,t,i)            -> TmProj(fi,tmWalk f g c t,i) 
    | TmFix(fi,t)               -> TmFix(fi,tmWalk f g c t)
    | TmTAbs(fi,tyX,t)          -> TmTAbs(fi,tyX,tmWalk f g c t) 
    | TmTApp(fi,t,tyT)          -> TmTApp(fi,tmWalk f g c t,g c tyT) 
    | TmPack(fi,tyT,t,tyT')     -> TmPack(fi,g c tyT,tmWalk f g c t,g c tyT') 
    | TmUnpack(fi,tyX,x,t,t2)   -> TmUnpack(fi,tyX,x,tmWalk f g c t,tmWalk f g(c+2)t2)  
    | x                         -> x

let tyShiftOnVar d          = fun c x n     ->  if x>=c then TyVar(x+d,n+d)    else TyVar(x,n+d) 
let tyShiftAbove d          = tyWalk (tyShiftOnVar d) 
let tyShift d               = tyShiftAbove d 0    

let tmShiftOnVar d          = fun fi c x n  ->  if x>=c then TmVar(fi,x+d,n+d) else TmVar(fi,x,n+d)
let tmShiftAbove d          = tmWalk (tmShiftOnVar d) (tyShiftAbove d) 
let tmShift d               = tmShiftAbove d 0 

let bindshift d             = function 
    | BindTyAbb(tyT)            ->  BindTyAbb(tyShift d tyT) 
    | BindTmVar(tyT)            ->  BindTmVar(tyShift d tyT) 
    | BindTmAbb(t,tyT_opt)      ->  (let tyT_opt' = match tyT_opt with 
                                        | None      -> None
                                        | Some(tyT) -> Some(tyShift d tyT) in 
                                    BindTmAbb(tmShift d t, tyT_opt'))
    | b                         ->  b

                                        
(* -------------------------------------------------- *) 
(* Substitution *) 
let tySubstOnVar j tyS tyT  = fun    c x n ->   if x=j+c then tyShift c tyS else TyVar(x,n) 
let tySubst      j tyS tyT  = tyWalk(tySubstOnVar j tyS tyT)0 tyT
let tySubstTop     tyS tyT  = tyShift(-1)(tySubst 0(tyShift 1 tyS)tyT)

let tmSubstOnVar j s t      = fun fi c x n ->   if x=j+c then tmShift c s else TmVar(fi, x, n) 
let tmSubst      j s t      = tmWalk (tmSubstOnVar j s t) (fun k x -> x) 0 t
let tmSubstTop     s t      = tmShift (-1) (tmSubst 0 (tmShift 1 s) t) 

let rec tytmSubst j tyS t   = tmWalk (fun fi c x n->TmVar(fi,x,n))(fun j tyT -> tySubst j tyS tyT) j t
let tytmSubstTop    tyS t   = tmShift (-1) (tytmSubst 0 (tyShift 1 tyS) t) 

(* -------------------------------------------------- *) 
(* Extracting file info *)
let tmInfo  = function 
    | TmUnit(fi)            -> fi 
    | TmVar(fi,_,_)         -> fi
    | TmAbs(fi,_,_,_)       -> fi
    | TmApp(fi,_,_)         -> fi 
    | TmLet(fi,_,_,_)       -> fi
    | TmTrue(fi)            -> fi
    | TmFalse(fi)           -> fi
    | TmIf(fi,_,_,_)        -> fi
    | TmZero(fi)            -> fi
    | TmSucc(fi,_)          -> fi
    | TmPred(fi,_)          -> fi
    | TmIsZero(fi,_)        -> fi 
    | TmAscribe(fi,_,_)     -> fi 
    | TmRecord(fi,_)        -> fi
    | TmProj(fi,_,_)        -> fi 
    | TmString(fi,_)        -> fi 
    | TmFloat(fi,_)         -> fi
    | TmTimesfloat(fi,_,_)  -> fi 
    | TmFix(fi,_)           -> fi
    | TmTAbs(fi,_,_)        -> fi
    | TmTApp(fi,_,_)        -> fi
    | TmPack(fi,_,_,_)      -> fi 
    | TmUnpack(fi,_,_,_,_)  -> fi

(* -------------------------------------------------- *) 
(* Bind *) 
let rec getbind fi ctx i        =   try let (_,bind) = List.nth ctx i in bindshift(i+1)bind
                                    with Failure _ -> error fi(getbind_err_msg i(ctxlen ctx))

let getTypeFromContext fi ctx n =   match getbind fi ctx n with 
    | BindTmVar(tyT)                -> tyT
    | BindTmAbb(_,Some(tyT))        -> tyT
    | BindTmAbb(_,None)             -> error fi("No type for variable "^(index2name fi ctx n))
    | _                             -> error fi("getTyFromCtx: Wrong binding"^(index2name fi ctx n))

(* -------------------------------------------------- *) 
(* Value *)
let rec isnum ctx   = function 
    | TmZero(_)                     -> true
    | TmSucc(_,t1)                  -> isnum ctx t1
    | _                             -> false

let rec isval ctx   = function 
    | TmTAbs(_,_,_)                 -> true
    | TmAbs(_,_,_,_)                -> true
    | TmUnit(_)                     -> true
    | TmTrue(_)                     -> true
    | TmFalse(_)                    -> true
    | TmRecord(_,flds)              -> List.for_all (fun(_,t)->isval ctx t) flds 
    | TmString(_,_)                 -> true
    | TmFloat(_,_)                  -> true
    | t when isnum ctx t            -> true
    | _                             -> false


