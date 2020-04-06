open Format
open Support.Error
open Support.Pervasive

let soi = string_of_int
(* -------------------------------------------------- *) 
(* Datatypes *)

type term =
    (* Dependent *)
    | Univ          of info * int 
    | Pi            of info 
    | Sgm           of info 
    | Bool          of info 
    | Nat           of info 
    (* Let  *)
    | Let           of info * string * term * term
    (* Lambda *) 
    | Var           of info * int * int 
    | Lam           of info * string * term * term 
    | Ap            of info * term * term 
    (* Arith *) 
    | Zero          of info
    | Succ          of info * term
    | Pred          of info * term
    | IsZero        of info * term
    (* Bool *) 
    | True          of info
    | False         of info
    | If            of info * term * term * term
;;
type bind = 
    | BindName
    | BindTyVar
    | BindVar       of term
    | BindAbb       of term * (term option) 
    | BindTyAbb     of term 
;;
type context = 
    (string * bind) list 
;;
type command =
    | Eval          of info * term
    | Bind          of info * string * bind 
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
                                    with Failure _ -> err fi "Variable Lookup Failure"
let rec name2index fi ctx xn    =   match ctx with 
    | []                                -> err fi ("Identifier "^xn^" is unbound")
    | (x,_) :: rest                     -> if x=xn then 0 else 1+(name2index fi rest xn)

(* -------------------------------------------------- *) 
(* Shifting *)
let rec tmWalk onVar c      = let f = onVar in function 
    | Var(fi,x,n)             -> onVar fi c x n
    | Lam(fi,x,tyT,t2)        -> Lam(fi,x,tmWalk f c tyT,tmWalk f(c+1)t2)
    | Ap(fi,t1,t2)           -> Ap(fi,tmWalk f c t1, tmWalk f c t2) 
    | If(fi,t1,t2,t3)         -> If(fi,tmWalk f c t1, tmWalk f c t2, tmWalk f c t3) 
    | Succ(fi,t)              -> Succ(fi,tmWalk f c t) 
    | Pred(fi,t)              -> Pred(fi,tmWalk f c t) 
    | IsZero(fi,t)            -> IsZero(fi,tmWalk f c t)
    | x                       -> x

let tmShiftOnVar d          = fun fi c x n -> if x>=c then Var(fi,x+d,n+d) else Var(fi,x,n+d)
let tmShiftAbove d          = tmWalk (tmShiftOnVar d) 
let tmShift d               = tmShiftAbove d 0 

let bindshift d             = function 
    | BindVar(tyT)            ->  BindVar(tmShift d tyT) 
    | BindAbb(t,tyT_opt)      ->  (let tyT_opt' = match tyT_opt with 
                                        | None      -> None
                                        | Some(tyT) -> Some(tmShift d tyT) in 
                                    BindAbb(tmShift d t, tyT_opt'))
    | b                         ->  b
                                        
(* -------------------------------------------------- *) 
(* Substitution *) 
let tmSubstOnVar j s t      = fun fi c x n ->   if x=j+c then tmShift c s else Var(fi, x, n) 
let tmSubst      j s t      = tmWalk (tmSubstOnVar j s t)  0 t
let tmSubstTop     s t      = pe"SUBSTITUTE    : [x↦s]t"; tmShift (-1) (tmSubst 0 (tmShift 1 s) t) 

(* -------------------------------------------------- *) 
(* Extracting file info *)
let tmInfo  = function 
    | Univ(fi,_)        -> fi 
    | Pi(fi)                -> fi 
    | Sgm(fi)             -> fi
    | Var(fi,_,_)           -> fi
    | Lam(fi,_,_,_)         -> fi
    | Ap(fi,_,_)           -> fi 
    | Let(fi,_,_,_)         -> fi
    | True(fi)              -> fi
    | False(fi)             -> fi
    | If(fi,_,_,_)          -> fi
    | Zero(fi)              -> fi
    | Succ(fi,_)            -> fi
    | Pred(fi,_)            -> fi
    | IsZero(fi,_)          -> fi 

(* -------------------------------------------------- *) 
(* Bind *) 
let rec getbind fi ctx i        =   try let (_,bind) = List.nth ctx i in bindshift(i+1)bind
                                    with Failure _ -> err fi(getbind_err_msg i(ctxlen ctx))

let getTypeFromContext fi ctx i =   match getbind fi ctx i with 
    | BindVar(tyT)                    -> tyT
    | BindAbb(_,Some(tyT))            -> tyT
    | BindAbb(_,None)                 -> err fi("No type recorded for var "^(index2name fi ctx i))
    | BindName                        -> err fi " bind name error " 
    | BindTyVar                       -> err fi " bind tyvar error " 
    | BindTyAbb(tyT)                  -> err fi " bind tyabb error " 
    | _                               -> err fi("getTypeFromCtx: Wrong bind "^(index2name fi ctx i))

(* -------------------------------------------------- *) 
(* Value *)
let rec isnum ctx   = function 
    | Zero(_)                       -> true
    | Succ(_,t1)                    -> isnum ctx t1
    | _                             -> false

let rec isval ctx   = function 
    | Lam(_,_,_,_)                  -> true
    | True(_)                       -> true
    | False(_)                      -> true
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
    | Var(_,_,_)              -> true
    | _                         -> false 

(* -------------------------------------------------- *) 
(* Term Print *)
let rec pr_Term outer ctx  = function 
    | Let(fi,x,t1,t2)         ->  obox0();
        pr"let ";pr x;pr" = ";pr_Term false ctx t1;ps();pr"in";ps();pr_Term false(addname ctx x)t2;   cbox()
    | Lam(fi,x,tyT1,t2)       ->  let (ctx',x')=pickfreshname ctx x in obox();
        pr"λ";pr x';pr":";pr_Term false ctx tyT1;pr".";psbr((not(small t2))||outer);pr_Term outer ctx' t2;  cbox()
    | If(fi, t1, t2, t3)      ->  obox0();
        pr"if "  ;pr_Term false ctx t1;ps();
        pr"then ";pr_Term false ctx t2;ps();
        pr"else ";pr_Term false ctx t3;      cbox()
    | t                         -> pr_ApTerm outer ctx t

and pr_ApTerm outer ctx   = function 
    | Ap(_,Ap(_,Pi(_),t1),(Lam(_,x,tyT,t2)as t)) 
                                ->  pr"Π(";pr x;pr":";pr_Term false ctx t1;pr")";pr_ATerm false ctx t 
    | Ap(fi, t1, t2)           ->  obox0();  pr_ApTerm false ctx t1;ps();pr_ATerm false ctx t2;  cbox();
    | Pred(_,t)                 ->  pr"pred "  ;pr_ATerm false ctx t
    | Succ(_,t)                 ->  let rec f n = function 
        | Zero(_)                   -> pi n 
        | Succ(_,s)                 -> f (n+1) s
        | _                         -> pr"(succ ";pr_ATerm false ctx t;pr")" in f 1 t
    | IsZero(_,t)               ->  pr"iszero ";pr_ATerm false ctx t
    | t                         ->  pr_PathTerm outer ctx t 

and pr_PathTerm outer ctx   = function
    | t                         ->  pr_ATerm outer ctx t

and pr_ATerm outer ctx     = function 
    | Univ(_,_)             ->  pr "𝒰"
    | Bool(_)                   ->  pr "𝐁"
    | Nat(_)                    ->  pr "𝐍"
    | Var(fi,x,n)               ->  (* pr (index2name fi ctx x) *)
        let l = ctxlen ctx in 
        if l = n then pr (index2name fi ctx x)
        else (  pr"[Ctx Err: ctxlen ";pi l;pr" != varctxlen ";pi n;
                pr" in { Γ:";pr(List.fold_left(fun s(x,_)->s^" "^x)""ctx);pr" }]")  
    | True(_)                   ->  pr "true"
    | False(_)                  ->  pr "false"
    | Zero(fi)                  ->  pr "0"
    | t                         ->  pr "("; pr_Term outer ctx t; pr ")"

let pr_tm t = pr_Term true t 

let pr_bind ctx = function 
    | BindName                  -> pr"NEW NAME "
    | BindAbb(t,_)              -> pr"ABB TERM ";pr_tm ctx t
    | BindTyAbb(t)              -> pr"ABB TYPE ";pr_tm ctx t
    | BindVar(t)                -> pr"NEW TERM ";pr_tm ctx t
    | BindTyVar                 -> pr"NEW TYPE " 

let pr_ctx ctx =
    pn();
    pe "xxxx CONTEXT Γ xxxxxxxxxxxxxx";
    let rec f = function 
        | []                    -> pe" NOTHING MORE "  
        | ((str,bind)::rest)    -> pr" ( ";pr str;pr", ";pr_bind ctx bind;pr" )";pn();f rest in 
    f ctx ; 
    pe "xxxxxxxxxxxxxxxxxxxxxxxxxxxxx"; pn()
