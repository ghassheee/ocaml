open Format
open Support.Pervasive
open Support.Error
open Syntax

val eval            : context -> term -> term 
val process_command : context -> command -> context 
val process_commands: context -> command list -> context 
