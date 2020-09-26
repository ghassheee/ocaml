
  exception Exit of int

  type info
  val dummyinfo : info

  (* Create file position info: filename lineno column *)
  val createInfo : string -> int -> int -> info
  val printInfo : info -> unit

  (* A convenient datatype for a "value with file info."  Used in
     the lexer and parser. *)
  type 'a withinfo = {i: info; v: 'a}

  (* Print an error message and fail.  The printing function is called
     in a context where the formatter is processing an hvbox.  Insert
     calls to Format.print_space to print a space or, if necessary,
     break the line at that point. *)
  val errf : (unit->unit) -> 'a
  val errfAt : info -> (unit->unit) -> 'a

  (* Convenient wrappers for the above, for the common case where the
     action to be performed is just to print a given string. *)
  val err : string -> 'a
  val error : info -> string -> 'a

  (* Variants that print a message but do not fail afterwards *)
  val warning : string -> unit
  val warningAt : info -> string -> unit

val pr : string -> unit 
val ps : unit -> unit 

