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
    | TyVariant of (string * ty) list 
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
let ctxlen          ctx         =   List.length ctx
let addbind         ctx x bind  =   (x,bind) :: ctx 
let addname         ctx x       =   addbind ctx x BindName 

let rec isbound ctx x           =   match ctx with 
    | []                                -> false
    | (y,_) :: rest                     -> if y=x then true else isbound rest x;;

let rec pickfresh ctx x         =   if isbound ctx x 
                                        then pickfresh ctx (x^"'")
                                        else ((x,BindName)::ctx), x

let rec index2name fi ctx n     =   try let (xn,_) = List.nth ctx n in xn 
                                    with Failure _ -> error fi "Variable Lookup Failure"

let rec name2index fi ctx xn    =   match ctx with 
    | []                                -> error fi ("Identifier "^xn^" is unbound")
    | (x,_) :: rest                     -> if x=xn then 0 else 1+(name2index fi rest xn)


(* -------------------------------------------------- *) 
(* Shifting *)

let rec tyWalk onVar c          = let f = onVar in function 
    | TyVar(x,n)                -> onVar c x n
    | TyVariant(fieldtys)       -> TyVariant(List.map (fun(l,tyT)->(l,tyWalk f c tyT)) fieldtys) 
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

let tmSubstOnVar j s t      = fun fi c x n ->   if x=j+c then tmShift c s else TmVar(fi, x, n) 
let tmSubst      j s t      = tmWalk (tmSubstOnVar j s t) (fun k x -> x) 0 t
let tmSubstTop     s t      = pe"SUBSTITUTE    : [x↦s]t"; tmShift (-1) (tmSubst 0 (tmShift 1 s) t) 


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
    | TmUnit(_)                     -> true
    | TmTrue(_)                     -> true
    | TmFalse(_)                    -> true
    | TmRecord(_,flds)              -> List.for_all (fun(_,t)->isval ctx t) flds 
    | TmString(_,_)                 -> true
    | TmFloat(_,_)                  -> true
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
let rec pr_Ty outer ctx   = function
    | tyT                       ->  pr_ArrowTy outer ctx tyT

and pr_ArrowTy outer ctx  = function
    | TyArr(tyT1,tyT2)          ->  obox0(); 
                                    pr_ATy false ctx tyT1;
                                    if outer then pr" ";pr "→";psbr outer;pr_ATy outer ctx tyT2; 
                                    cbox()
    | tyT                       ->  pr_ATy outer ctx tyT

and pr_ATy outer ctx      = function

    | TyFloat                   ->  pr "𝐅"  
    | TyString                  ->  pr "𝐒" 
    | TyBool                    ->  pr "𝐁" 
    | TyNat                     ->  pr "𝐍"
    | TyUnit                    ->  pr "𝐔"
    | TyId(s)                   ->  pr s
    | TyVar(i,n)                ->  if ctxlen ctx=n then pr(index2name dummy ctx i)else pr"[BadIndex]"  
    | TyVariant(flds)           ->  pr"<"; oobox0(); pr_fldtys pr_Ty outer ctx 1 flds; pr">"; cbox()
    | TyRecord(flds)            ->  pr"{"; oobox0(); pr_fldtys pr_Ty outer ctx 1 flds; pr"}"; cbox()
    | tyT                       ->  pr"("; pr_Ty outer ctx tyT; pr ")"
;;

let pr_ty ctx tyT               = pr_Ty true ctx tyT


let rec pr_cases prTm outer ctx = 
    let pr_case (li,(xi,ti)) = 
    let (ctx',xi') = pickfresh ctx xi in  
    pr"<";pr li;pr"=";pr xi';pr"> ==> ";prTm false ctx' ti in function 
    | []        -> ()
    | [c]       -> pr"| ";pr_case c;ps(); 
    | c::rest   -> pr"| ";pr_case c;ps();pr_cases prTm outer ctx rest

(* -------------------------------------------------- *) 
(* Tm Print *)
let rec pr_Tm outer ctx  = function 
    | TmLet(_,x,t1,t2)          ->  obox0();
        pr"let ";pr x;pr" = ";pr_Tm false ctx t1;ps();pr"in";ps();pr_Tm false(addname ctx x)t2;   cbox()
    | TmAbs(_,x,tyT1,t2)        ->  let (ctx',x')=pickfresh ctx x in obox();
        pr"λ";pr x';pr":";pr_Ty false ctx tyT1;pr".";psbr((not(small t2))||outer);pr_Tm outer ctx' t2;  cbox()
    | TmIf(_, t1, t2, t3)       ->  obox0();
        pr"if "  ;pr_Tm false ctx t1;ps();
        pr"then ";pr_Tm false ctx t2;ps();
        pr"else ";pr_Tm false ctx t3;      cbox()
    | t                         -> pr_AppTm outer ctx t

and pr_AppTm outer ctx   = function 
    | TmApp(_, t1, t2)          ->  obox0();  pr_AppTm false ctx t1;ps();pr_ATm false ctx t2;  cbox();
    | TmPred(_,t)               ->  pr"pred "  ;pr_ATm false ctx t
    | TmSucc(_,t)               ->  let rec f n = function 
        | TmZero(_)                 -> pi n 
        | TmSucc(_,s)               -> f (n+1) s
        | _                         -> pr"(succ ";pr_ATm false ctx t;pr")" in f 1 t
    | TmIsZero(_,t)             ->  pr"iszero ";pr_ATm false ctx t
    | TmAscribe(_,t,tyT)        ->  pr_AppTm outer ctx t
    | TmTimesfloat(_,t1,t2)     ->  pr_AppTm outer ctx t1;pr" *. ";pr_AppTm outer ctx t2
    | TmFix(_,t)                ->  pr"fix "   ;pr_ATm false ctx t
    | t                         ->  pr_PathTm outer ctx t 

and pr_PathTm outer ctx   = function
    | TmProj(_,t,l)             ->  pr_ATm false ctx t; pr"."; pr l
    | t                         ->  pr_ATm outer ctx t

and pr_ATm outer ctx     = function 
    | TmVar(fi,x,n)             -> let l = ctxlen ctx in 
                                    if l=n  then pr (index2name fi ctx x)
                                            else (pr"[NameContext Error: ";pi l;pr"!=";pi n;pr" in { Γ:";
                                                  pr(List.fold_left(fun s(x,_)->s^" "^x)""ctx);pr" }]")  
    | TmString(_,s)             ->  pr "\"";pr s; pr"\""
    | TmFloat(_,f)              ->  print_float f
    | TmUnit(_)                 ->  pr "()" 
    | TmTrue(_)                 ->  pr "true"
    | TmFalse(_)                ->  pr "false"
    | TmZero(fi)                ->  pr "0"
    | TmRecord(fi,flds)         ->  pr"{"; oobox0(); pr_flds pr_Tm outer ctx 1 flds; pr "}"; cbox()
    | t                         ->  pr "("; pr_Tm outer ctx t; pr ")"

let pr_tm t = pr_Tm true t 



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
