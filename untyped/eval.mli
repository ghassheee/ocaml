open Format
open Support
open Syntax
open Arg 

val eval            : (string*binding) list -> term -> term 
val process_command : (string*binding) list -> command -> context 
val print_eval      : (string*binding) list -> command -> unit
