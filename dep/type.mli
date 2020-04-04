open Format
open Support.Pervasive
open Support.Error
open Syntax

val typeof          : context -> term -> term
val prbindty        : context -> bind -> unit 
