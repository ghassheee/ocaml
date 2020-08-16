open Format
open Arg 

open Support.Pervasive
open Support.Error
open Syntax
open Type
open Eval
open Interpreter


(* #####################
 * ####    REPL     ####
 * ##################### *)

let () = print_string "> "; print_flush()
let process' ()   = 
    let _ (*cmds*)          =   parse'' repl stdin in ()  (* REPL cannot wait returning cmds *) 

let main' ()                =   try process' ();  0
                                with Exit x -> x 

let _                       =   Printexc.catch main' () 


