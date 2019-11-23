open Format
open Support.Error
open Support.Pervasive

let soi = string_of_int
(* -------------------------------------------------- *) 
(* Datatypes *)

type ty     =
    | TyVar of int * int 
    | TyArr of ty * ty
    | TyBool
    | TyNat 
    | TyUnit
    | TyRecord of (string * ty) list 
;;

type term =
    (* Record *)
    | TmProj    of info * term * string  
    | TmRecord  of info * (string * term) list 
    (* Ascription *) 
    | TmAscribe of info * term * ty
    (* Unit *)
    | TmUnit    of info 
    (* Let  *)
    | TmLet     of info * string * term * term
    (* Lambda *) 
    | TmVar     of info * int * int 
    | TmAbs     of info * string * ty * term 
    | TmApp     of info * term * term 
    (* Arith *) 
    | TmZero    of info
    | TmSucc    of info * term
    | TmPred    of info * term
    | TmIsZero  of info * term
    (* Bool *) 
    | TmTrue    of info
    | TmFalse   of info
    | TmIf      of info * term * term * term
;;

type binding = 
    | NameBind
    | VarBind of ty
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

let emptycontext                =   []
let ctxlen          ctx         =   List.length ctx
let addbinding      ctx x bind  =   (x,bind) :: ctx 
let addname         ctx x       =   addbinding ctx x NameBind 
let rec isnamebound ctx x       =   match ctx with 
    | []                                -> false
    | (y,_) :: rest                     -> if y=x then true else isnamebound rest x;;
let rec pickfreshname ctx x     =   if isnamebound ctx x 
                                        then pickfreshname ctx (x^"'")
                                        else ((x,NameBind)::ctx), x
let     index2name fi ctx x     =   try let(xn,_)=List.nth ctx x in xn 
                                    with Failure _ -> error fi "Variable Lookup Failure"
let rec name2index fi ctx x     =   match ctx with 
    | []                                -> error fi ("Identifier "^x^" is unbound")
    | (y,_) :: rest                     -> if y=x then 0 else 1+(name2index fi rest x)

let rec getbinding fi n         =   function
    | ctx                               ->  try     let (_,bind)    = List.nth ctx n in bind 
                                            with Failure _ -> error fi(getbinding_err_msg n(ctxlen ctx));;

let getTypeFromContext fi ctx n =   match getbinding fi n ctx with 
    | VarBind(tyT)                      -> tyT
    | _                                 -> error fi("getTypeFromContext: Wrong binding"^(index2name fi ctx n))

(* -------------------------------------------------- *) 
(* Shifting *)

let rec walk funOnVar c t = let f = funOnVar in match t with 
    | TmVar(fi,x,n)             -> funOnVar fi c x n
    | TmLet(fi,x,t1,t2)         -> TmLet(fi,x,walk f c t1, walk f(c+1)t2) 
    | TmAbs(fi,x,tyT,t2)        -> TmAbs(fi,x,tyT,walk f(c+1)t2)
    | TmApp(fi,t1,t2)           -> TmApp(fi,walk f c t1, walk f c t2) 
    | TmIf(fi,t1,t2,t3)         -> TmIf(fi,walk f c t1, walk f c t2, walk f c t3) 
    | TmSucc(fi,t)              -> TmSucc(fi,walk f c t) 
    | TmPred(fi,t)              -> TmPred(fi,walk f c t) 
    | TmIsZero(fi,t)            -> TmIsZero(fi,walk f c t)
    | TmAscribe(fi,t,tyT)       -> TmAscribe(fi,walk f c t,tyT) 
    | TmRecord(fi,tl)           -> TmRecord(fi,List.map(fun(l,t)->(l,walk f c t))tl)  
    | TmProj(fi,t,i)            -> TmProj(fi,walk f c t,i) 
    | x                         -> x

let termShiftOnVar d        = fun fi c x n ->   if x>=c then TmVar(fi,x+d,n+d) else TmVar(fi,x,n+d)
let termShiftAbove d        = walk (termShiftOnVar d)
let termShift d             = if d>=0 then pe("SHIFT         : "^(soi d));termShiftAbove d 0 

(* -------------------------------------------------- *) 
(* Substitution *) 
let termSubstOnVar j s t    = fun fi c x n ->   if x=j+c then termShift c s else TmVar(fi, x, n) 
let termSubst j s t         = walk (termSubstOnVar j s t) 0 t
let termSubstTop s t        = pe"SUBSTITUTE    : [x↦s]t"; termShift (-1) (termSubst 0 (termShift 1 s) t) 

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


(* -------------------------------------------------- *) 
(* Value *)
let rec isnum ctx = function 
    | TmZero(_)         -> true
    | TmSucc(_,t1)      -> isnum ctx t1
    | _                 -> false

let rec isval ctx = function 
    | TmAbs(_,_,_,_)                -> true
    | TmUnit(_)                     -> true
    | TmTrue(_)                     -> true
    | TmFalse(_)                    -> true
    | TmRecord(_,flds)              -> List.for_all (fun(_,t)->isval ctx t) flds 
    | t when isnum ctx t     -> true
    | _                             -> false


(* -------------------------------------------------- *) 
(* Printing *)
let obox0() = open_hvbox 0
let oobox0()= open_hovbox 0
let obox()  = open_hvbox 2
let cbox()  = close_box()
let br()    = print_break 0 0

let small   = function
    | TmVar(_,_,_)              -> true
    | _                         -> false 
;;

(* -------------------------------------------------- *) 
(* Type Print *) 
let rec pr_Type outer      = function
    | tyT                       ->  pr_ArrowType outer tyT
and pr_ArrowType outer     = function
    | TyArr(tyT1,tyT2)          ->  obox0(); pr_AType false tyT1;
                                    if outer then pr " "; pr "→"; if outer then ps () else br();
                                    pr_AType outer tyT2; cbox()
    | tyT                       ->  pr_AType outer tyT
and pr_AType outer         = function
    | TyBool                    ->  pr "𝐁" 
    | TyNat                     ->  pr "𝐍"
    | TyUnit                    ->  pr "𝐔"
    | TyRecord(fields)          -> 
            let pf i (li,tyTi)  =   (if (li<>((soi i))) then (pr li;pr":"));pr_Type false tyTi in
            let rec p i         =   function
                | []                ->  ()
                | [f]               ->  pf i f
                | f::rest           ->  pf i f;pr",";if outer then ps()else br();p(i+1)rest in
            pr"{";oobox0();p 1 fields;pr"}";cbox()
    | tyT                       ->  pr"("; pr_Type outer tyT; pr ")"
;;

let pr_ty tyT                 = pr_Type true tyT

(* -------------------------------------------------- *) 
(* Term Print *)
let rec pr_Term outer ctx  = function 
    | TmLet(fi,x,t1,t2)         ->  obox0();
        pr "let "; pr x; pr " = ";
        pr_Term false ctx t1; ps(); pr"in"; ps();
        pr_Term false (addname ctx x) t2;
        cbox()
    | TmAbs(fi,x,tyT1,t2)       ->  let (ctx',x') = pickfreshname ctx x in obox();
        pr "λ"; pr x'; pr ":"; pr_Type false tyT1; pr "."; 
        if (small t2) && not outer then br() else ps();
        pr_Term outer ctx' t2;
        cbox()
    | TmIf(fi, t1, t2, t3)      ->  obox0();
        pr "if "  ; pr_Term false ctx t1; ps();
        pr "then "; pr_Term false ctx t2; ps();
        pr "else "; pr_Term false ctx t3;
        cbox()
    | t                         -> pr_AppTerm outer ctx t

and pr_AppTerm outer ctx   = function 
    | TmApp(fi, t1, t2)         ->  obox0(); pr_AppTerm false ctx t1; ps(); 
                                    pr_ATerm false ctx t2; cbox();
    | TmPred(_,t1)              ->  pr "pred "  ;   pr_ATerm false ctx t1
    | TmIsZero(_,t1)            ->  pr "iszero ";   pr_ATerm false ctx t1
    | TmAscribe(fi,t,tyT)       ->  pr_AppTerm outer ctx t
    | t                         ->  pr_PathTerm outer ctx t 

and pr_PathTerm outer ctx   = function
    | TmProj(_,t,l)             ->  pr_ATerm false ctx t; pr"."; pr l
    | t                         ->  pr_ATerm outer ctx t

and pr_ATerm outer ctx     = function 
    | TmVar(fi,x,n)             -> if ctxlen ctx = n 
        then pr (index2name fi ctx x)
        else pr ("[bad index: " ^ (soi x) ^ "/" ^ (soi n) ^ " in {" 
                ^ (List.fold_left (fun s(x,_)-> s^" "^x) "" ctx) ^ " }]") 
    | TmUnit(_)                 ->  pr "()" 
    | TmTrue(_)                 ->  pr "true"
    | TmFalse(_)                ->  pr "false"
    | TmZero(fi)                ->  pr "0"
    | TmSucc(_,t1)              ->  let rec f n = function 
        | TmZero(_)                 -> pr (soi n)
        | TmSucc(_,s)               -> f (n+1) s
        | _                         -> (pr "(succ "; pr_ATerm false ctx t1; pr ")")
                                    in f 1 t1
    | TmRecord(fi,flds)         -> 
            let pf i (li,ti) = (if li<>(soi i)then(pr li;pr"="));pr_Term false ctx ti in
            let rec p i         = function
                | []                -> ()
                | [f]               -> pf i f
                | f::rest           -> pf i f; pr","; if outer then ps() else br();p(i+1)rest in
            pr"{";oobox0();p 1 flds;pr "}";cbox()
    | t                         ->  pr "("; pr_Term outer ctx t; pr ")"

let pr_tm t = pr_Term true t 


