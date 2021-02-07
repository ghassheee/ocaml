open Format
open Support
open Syntax

val tyeqv           : context -> term -> term -> bool
val typeof          : context -> term -> term
val prbindty        : context -> bind -> unit 
