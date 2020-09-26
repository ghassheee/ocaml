open Format
open Arg

open Syntax
open Proof
open Compiler
open Support



let fi = dummy;;

(*
let s = pr_lj_tree ""(ljTree ([],PArr(fi,PBot(fi),PAnd(fi,PTop(fi),PTop(fi)))));;
let _ = print_string s ;;
*)

let main () = 
  parseArgs (); 
  let _ = processFile (getFile()) emptyctx in ();;

let _ = set_max_boxes 10000
let _ = set_margin 200
(* let res     = Printexc.catch (fun() -> 
    try main(); 0 
    with | Exit 10 -> flush stdout; main() ; 10
         | Exit x  -> x ) () 
*)
let _ = main()

(* 
let a = PVar(fi,1,3);;
let b = PVar(fi,2,3);;
let c = PVar(fi,3,3);;
let left = PAnd(fi,a,POr(fi,b,c))
let right = POr(fi,PAnd(fi,a,b),c)
let assoc = PArr(fi,left,right);;
let t = pr_lj_tree "" (ljTree ([],assoc));;
let _ = print_string t;;
*) 
