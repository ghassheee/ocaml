open Format
open Support
open Syntax

val eval            : context -> term -> term 
val process_cmd     : context -> command -> context 
val process_cmds    : context -> command list -> context 
