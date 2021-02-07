

open Syntax

type goal = Goal of context * ty

type tactic = goal -> list goal 

