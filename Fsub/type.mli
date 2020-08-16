open Format
open Support.Pervasive
open Support.Error
open Syntax

(* val tyeqv           : context -> ty -> ty -> bool *)
val typeof          : context -> term -> ty
val prbindty        : context -> bind -> unit 
(* val simplifyty      : context -> ty -> ty *)
