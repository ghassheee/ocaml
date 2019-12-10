open Format
open Support.Error
open Support.Pervasive

let soi = string_of_int
(* -------------------------------------------------- *) 
(* Datatypes *)

type ty     =
    | TyVar     of int * int 
    | TyVariant of (string * ty) list 
    | TyRecord  of (string * ty) list 
    | TyArr     of ty * ty
    | TyList    of ty 
    | TyFloat 
    | TyString  
    | TyUnit
;;


type term =
    | Universe      of info * int 
    | TmSigma       of info 
    | TmBool        of info 
    | TmNat         of info 
    | TmPi          of info 
    (* Let  *)
    | TmLet         of info * string * term * term
    (* Lambda *) 
    | TmVar         of info * int * int 
    | TmAbs         of info * string * term * term 
    | TmApp         of info * term * term 
    (* Arith *) 
    | TmZero        of info
    | TmSucc        of info * term
    | TmPred        of info * term
    | TmIsZero      of info * term
    (* Bool *) 
    | TmTrue        of info
    | TmFalse       of info
    | TmIf          of info * term * term * term
;;

type bind = 
    | BindName
    | BindTyVar
    | BindTmVar     of term
    | BindTmAbb     of term * (term option) 
    | BindTyAbb     of term 

;;

type context = 
    (string * bind) list 
;;

type command =
    | Eval of info * term
    | Bind of info * string * bind 
;;

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
                                        else ((x,BindName)::ctx), x
let     index2name fi ctx n     =   try let (xn,_) = List.nth ctx n in xn 
                                    with Failure _ -> error fi "Variable Lookup Failure"
let rec name2index fi ctx xn    =   match ctx with 
    | []                                -> error fi ("Identifier "^xn^" is unbound")
    | (x,_) :: rest                     -> if x=xn then 0 else 1+(name2index fi rest xn)


(* -------------------------------------------------- *) 
(* Shifting *)

let rec tyWalk onVar c          = let f = onVar in function 
    | TmVar(fi,x,n)             -> onVar fi c x n
    | TmApp(fi,TmApp(_,TmPi(_),tyT1),tyT2)  -> TmApp(fi,TmApp(fi,TmPi(fi),tyWalk f c tyT1),tyWalk f c tyT2) 
    | tyT                       -> tyT

let rec tmWalk onVar onType c   = let (f,g) = (onVar,onType) in function 
    | TmVar(fi,x,n)             -> onVar fi c x n
    | TmLet(fi,x,t1,t2)         -> TmLet(fi,x,tmWalk f g c t1, tmWalk f g(c+1)t2) 
    | TmAbs(fi,x,tyT,t2)        -> TmAbs(fi,x,g c tyT,tmWalk f g(c+1)t2)
    | TmApp(fi,t1,t2)           -> TmApp(fi,tmWalk f g c t1, tmWalk f g c t2) 
    | TmIf(fi,t1,t2,t3)         -> TmIf(fi,tmWalk f g c t1, tmWalk f g c t2, tmWalk f g c t3) 
    | TmSucc(fi,t)              -> TmSucc(fi,tmWalk f g c t) 
    | TmPred(fi,t)              -> TmPred(fi,tmWalk f g c t) 
    | TmIsZero(fi,t)            -> TmIsZero(fi,tmWalk f g c t)
    | x                         -> x

let tyShiftOnVar d          = fun fi c x n  -> if x>=c then TmVar(fi,x+d,n+d)else TmVar(fi,x,n+d) 
let tyShiftAbove d          = tyWalk (tyShiftOnVar d) 
let tyShift d               = tyShiftAbove d 0    

let tmShiftOnVar d          = fun fi c x n -> if x>=c then TmVar(fi,x+d,n+d)  else TmVar(fi,x,n+d)
let tmShiftAbove d          = tmWalk (tmShiftOnVar d) (tyShiftAbove d) 
let tmShift d               = tmShiftAbove d 0 

let bindshift d             = function 
    | BindTmVar(tyT)            ->  BindTmVar(tyShift d tyT) 
    | BindTmAbb(t,tyT_opt)      ->  (let tyT_opt' = match tyT_opt with 
                                        | None      -> None
                                        | Some(tyT) -> Some(tyShift d tyT) in 
                                    BindTmAbb(tmShift d t, tyT_opt'))
    | b                         ->  b

                                        
(* -------------------------------------------------- *) 
(* Substitution *) 
let tySubstOnVar j tyS tyT  = fun fi c x n ->   if x=j+c then tyShift c tyS else TmVar(fi,x,n) 
let tySubst      j tyS tyT  = tyWalk(tySubstOnVar j tyS tyT)0 tyT

let tmSubstOnVar j s t      = fun fi c x n ->   if x=j+c then tmShift c s else TmVar(fi, x, n) 
let tmSubst      j s t      = tmWalk (tmSubstOnVar j s t) (fun k x -> x) 0 t
let tmSubstTop     s t      = pe"SUBSTITUTE    : [x↦s]t"; tmShift (-1) (tmSubst 0 (tmShift 1 s) t) 


(* -------------------------------------------------- *) 
(* Extracting file info *)
let tmInfo  = function 
    | Universe(fi,_)        -> fi 
    | TmPi(fi)              -> fi 
    | TmSigma(fi)           -> fi
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

(* -------------------------------------------------- *) 
(* Bind *) 
let rec getbind fi ctx i        =   try let (_,bind) = List.nth ctx i in bindshift(i+1)bind
                                    with Failure _ -> error fi(getbind_err_msg i(ctxlen ctx))

let getTypeFromContext fi ctx n =   match getbind fi ctx n with 
    | BindTmVar(tyT)                      -> tyT
    | BindTmAbb(_,Some(tyT))            -> tyT
    | BindTmAbb(_,None)                 -> error fi ("No type recorded for variable "^(index2name fi ctx n))
    | _                                 -> error fi("getTypeFromContext: Wrong binding"^(index2name fi ctx n))

(* -------------------------------------------------- *) 
(* Value *)
let rec isnum ctx   = function 
    | TmZero(_)                     -> true
    | TmSucc(_,t1)                  -> isnum ctx t1
    | _                             -> false

let rec isval ctx   = function 
    | TmAbs(_,_,_,_)                -> true
    | TmTrue(_)                     -> true
    | TmFalse(_)                    -> true
    | t when isnum ctx t            -> true
    | _                             -> false


(* -------------------------------------------------- *) 
(* Printing *)
let obox0()         = open_hvbox 0
let oobox0()        = open_hovbox 0
let obox()          = open_hvbox 2
let cbox()          = close_box()
let br()            = print_break 0 0
let psbr outer      = if outer then ps() else br()
let small           = function
    | TmVar(_,_,_)              -> true
    | _                         -> false 

let rec pr_fldtys prTy outer ctx i  = 
    let pr_fld i (li,tyTi)  = if li<>(soi i)then pr li; pr":";prTy false ctx tyTi in function
    | []                        ->  ()
    | [f]                       ->  pr_fld i f
    | f::rest                   ->  pr_fld i f;pr",";psbr outer;pr_fldtys prTy outer ctx(i+1)rest

let rec pr_flds prTm outer ctx i = 
    let pr_fld i (li,ti)    = if li<>(soi i)then(pr li;pr"=");prTm false ctx ti in function 
    | []                        ->  ()
    | [f]                       ->  pr_fld i f
    | f::rest                   ->  pr_fld i f;pr",";psbr outer;pr_flds prTm outer ctx(i+1)rest 

(* -------------------------------------------------- *) 
(* Type Print *) 
let rec pr_Type outer ctx   = function
    | tyT                       ->  pr_ArrowType outer ctx tyT

and pr_ArrowType outer ctx  = function
    | TmApp(_,TmApp(_,TmPi(_),tyT1),tyT2)          ->  obox0(); 
                                    pr_AType false ctx tyT1;
                                    if outer then pr" ";pr "→";psbr outer;pr_AType outer ctx tyT2; 
                                    cbox()
    | tyT                       ->  pr_AType outer ctx tyT

and pr_AType outer ctx      = function
    | TmBool(fi)                    ->  pr "𝐁" 
    | TmNat(fi)                     ->  pr "𝐍"
    | Universe(fi,i)                ->  pr "𝒰"
    | TmVar(fi,i,n)                 ->  if ctxlen ctx = n then pr(index2name dummyinfo ctx i)else pr"[BadIndex]"  
    | tyT                       ->  pr"("; pr_Type outer ctx tyT; pr ")"
;;

let pr_ty ctx tyT               = pr_Type true ctx tyT

(* -------------------------------------------------- *) 
(* Term Print *)
let rec pr_Term outer ctx  = function 
    | TmLet(fi,x,t1,t2)         ->  obox0();
        pr"let ";pr x;pr" = ";pr_Term false ctx t1;ps();pr"in";ps();pr_Term false(addname ctx x)t2;   cbox()
    | TmAbs(fi,x,tyT1,t2)       ->  let (ctx',x')=pickfreshname ctx x in obox();
        pr"λ";pr x';pr":";pr_Type false ctx tyT1;pr".";psbr((not(small t2))||outer);pr_Term outer ctx' t2;  cbox()
    | TmIf(fi, t1, t2, t3)      ->  obox0();
        pr"if "  ;pr_Term false ctx t1;ps();
        pr"then ";pr_Term false ctx t2;ps();
        pr"else ";pr_Term false ctx t3;      cbox()
    | t                         -> pr_AppTerm outer ctx t

and pr_AppTerm outer ctx   = function 
    | TmApp(_,TmApp(_,TmPi(_),t1),t2) 
                                ->  pr"Π(x:";pr_ty ctx t1;pr")";pr_Term false ctx t2 
    | TmApp(fi, t1, t2)         ->  obox0();  pr_AppTerm false ctx t1;ps();pr_ATerm false ctx t2;  cbox();
    | TmPred(_,t)               ->  pr"pred "  ;pr_ATerm false ctx t
    | TmSucc(_,t)               ->  let rec f n = function 
        | TmZero(_)                 -> pi n 
        | TmSucc(_,s)               -> f (n+1) s
        | _                         -> pr"(succ ";pr_ATerm false ctx t;pr")" in f 1 t
    | TmIsZero(_,t)             ->  pr"iszero ";pr_ATerm false ctx t
    | t                         ->  pr_PathTerm outer ctx t 

and pr_PathTerm outer ctx   = function
    | t                         ->  pr_ATerm outer ctx t

and pr_ATerm outer ctx     = function 
    | TmVar(fi,x,n)             -> let l = ctxlen ctx in 
        if l = n then pr (index2name fi ctx x)
        else (pr"[Context Error: ";pi l;pr"!=";pi n;pr" in { Γ:";pr(List.fold_left(fun s(x,_)->s^" "^x)""ctx);pr" }]")  
    | TmBool(_)                 ->  pr "𝐁"
    | TmNat(_)                  ->  pr "𝐍"
    | Universe(_,_)             ->  pr "𝒰"
    | TmTrue(_)                 ->  pr "true"
    | TmFalse(_)                ->  pr "false"
    | TmZero(fi)                ->  pr "0"
    | t                         ->  pr "("; pr_Term outer ctx t; pr ")"

let pr_tm t = pr_Term true t 


let pr_bind = function 
    | BindName                  -> pr"NEW NAME"
    | BindTmAbb(_,_)            -> pr"ABB TERM" 
    | BindTyAbb(_)              -> pr"ABB TYPE" 
    | BindTmVar(_)              -> pr"NEW TERM" 
    | BindTyVar                 -> pr"NEW TYPE" 

let pr_ctx ctx =
    pn();
    pe "xxxx CONTEXT Γ xxxxxxxxxxxxxx";
    let rec f = function 
        | []                  -> pe" NOTHING MORE "  
        | ((str,bind)::rest)  -> pr" ( ";pr str;pr", ";pr_bind bind;pr" )";pn();f rest in 
    f ctx ; 
    pe "xxxxxxxxxxxxxxxxxxxxxxxxxxxxx"; pn()
