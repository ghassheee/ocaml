open Format
open Core
open Support.Pervasive
open Support.Error
open Syntax
open Arg 

let rec process_command = function 
  | Eval(fi,t)              ->  let t' = eval t in
                                printtm_ATerm true t'; 
                                force_newline ()

let print_eval cmd      = 
    open_hvbox 0; 
    process_command cmd; 
    print_flush ()




