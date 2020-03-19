open Format
open Support.Pervasive
open Support.Error
open Syntax

val tyeqv           : context -> ty -> ty -> bool
val join            : context -> ty -> ty -> ty 
val meet            : context -> ty -> ty -> ty 
val subtype         : context -> ty -> ty -> bool
val simplifyty      : context -> ty -> ty 
