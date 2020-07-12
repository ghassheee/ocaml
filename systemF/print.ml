open Format
open Support.Error
open Support.Pervasive
open Syntax 

let rec fresh ctx x         =   if isbound ctx x 
                                        then fresh ctx (x^"'")
                                        else ((x,BindName)::ctx), x

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
    | TyAll(tyX,tyT)            ->  let ctx',tyX = fresh ctx tyX in 
                                    obox();pr"∀";pr tyX;pr".";ps();pr_Ty outer ctx' tyT;cbox()
    | TySome(tyX,tyT)           ->  let ctx,tyX = fresh ctx tyX in 
                                    obox();pr"∃";pr tyX;pr".";ps();pr_Ty outer ctx tyT;cbox()
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
    | TyRecord(flds)            ->  pr"{"; oobox0(); pr_fldtys pr_Ty outer ctx 1 flds; pr"}"; cbox()
    | tyT                       ->  pr"("; pr_Ty outer ctx tyT; pr ")"
;;

let pr_ty ctx tyT               = pr_Ty true ctx tyT


let rec pr_cases prTm outer ctx = 
    let pr_case (li,(xi,ti)) = 
    let (ctx',xi') = fresh ctx xi in  
    pr"<";pr li;pr"=";pr xi';pr"> ==> ";prTm false ctx' ti in function 
    | []        -> ()
    | [c]       -> pr"| ";pr_case c;ps(); 
    | c::rest   -> pr"| ";pr_case c;ps();pr_cases prTm outer ctx rest

(* -------------------------------------------------- *) 
(* Tm Print *)
let rec pr_Tm outer ctx  = function 
    | TmLet(_,x,t1,t2)          ->  obox0();pr"let ";pr x;pr" = ";pr_Tm false ctx t1;ps();pr"in";
                                            ps();pr_Tm false(addname ctx x)t2;cbox()
    | TmAbs(_,x,tyT1,t2)        ->  let (ctx',x')=fresh ctx x in obox();
                                    pr"λ";pr x';pr":";pr_Ty false ctx tyT1;pr".";
                                    psbr((not(small t2))||outer);pr_Tm outer ctx' t2;  cbox()
    | TmTAbs(_,tyX,t)           ->  let ctx',tyX' = fresh ctx tyX in obox();
                                    pr"λ";pr tyX';pr".";psbr((not(small t))||outer);
                                    pr_Tm outer ctx' t; cbox()
    | TmUnpack(_,tyX,x,some,t)  ->  let ctx',tyX' = fresh ctx tyX in 
                                    let ctx'', x' = fresh ctx' x in 
                                    obox(); pr"let {";pr tyX';pr",";pr x';pr"} = ";pr_Tm false ctx some; 
                                    pr" in ";pr_Tm outer ctx'' t; cbox()
    | TmIf(_, t1, t2, t3)       ->  obox0();
                                    pr"if "  ;pr_Tm false ctx t1;ps();
                                    pr"then ";pr_Tm false ctx t2;ps();
                                    pr"else ";pr_Tm false ctx t3;      cbox()
    | t                         ->  pr_AppTm outer ctx t

and pr_AppTm outer ctx   = function 
    | TmApp(_, t1, t2)          ->  obox0();  pr_AppTm false ctx t1;ps();pr_ATm false ctx t2;cbox()
    | TmPred(_,t)               ->  pr"pred "  ;pr_ATm false ctx t
    | TmSucc(_,t)               ->  let rec f n = function 
        | TmZero(_)                 ->  pi n 
        | TmSucc(_,s)               ->  f (n+1) s
        | _                         ->  pr"(succ ";pr_ATm false ctx t;pr")" in f 1 t
    | TmIsZero(_,t)             ->  pr"iszero ";pr_ATm false ctx t
    | TmAscribe(_,t,tyT)        ->  pr_AppTm outer ctx t
    | TmTimesfloat(_,t1,t2)     ->  pr_AppTm outer ctx t1;pr" *. ";pr_AppTm outer ctx t2
    | TmFix(_,t)                ->  pr"fix "   ;pr_ATm false ctx t
    | TmTApp(_,t,tyT)           ->  obox0(); pr_AppTm false ctx t;ps();pr_Ty false ctx tyT;cbox() 
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
    | TmZero(_)                 ->  pr "0"
    | TmRecord(_,flds)          ->  pr"{"; oobox0(); pr_flds pr_Tm outer ctx 1 flds; pr "}"; cbox()
    | TmPack(_,tyT,t,tySome)    ->  obox(); pr"{*"; pr_Ty false ctx tyT;pr", ";pr_Tm false ctx t;pr"}";
                                    pr " : ";pr_Ty outer ctx tySome; cbox()
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
