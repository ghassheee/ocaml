open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isnumericval = function 
    TmZero(_)       -> true
  | TmSucc(_,t1)    -> isnumericval t1
  | _               -> false

let rec isval  = function 
    TmTrue(_)               -> true
  | TmFalse(_)              -> true
  | t when isnumericval t   -> true
  | _                       -> false

let rec eval1 = function 
    TmIf(_,TmTrue(_),t2,t3)     ->  t2
  | TmIf(_,TmFalse(_),t2,t3)    ->  t3
  | TmIf(fi,t1,t2,t3)           ->  let t1' = eval1 t1 in TmIf(fi, t1', t2, t3)
  | TmSucc(fi,t1)               ->  let t1' = eval1 t1 in TmSucc(fi, t1')
  | TmPred(_,TmZero(_))         ->  TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval nv1) -> nv1
  | TmPred(fi,t1)               ->  let t1' = eval1 t1 in TmPred(fi, t1')
  | TmIsZero(_,TmZero(_))       ->  TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval nv1) ->    TmFalse(dummyinfo)
  | TmIsZero(fi,t1)             ->  let t1' = eval1 t1 in TmIsZero(fi, t1')
  | _                           ->  raise NoRuleApplies

let rec eval t =
    try let t' = eval1 t in eval t'
    with NoRuleApplies -> t
