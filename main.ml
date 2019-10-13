open Format
open Core
open Support.Pervasive
open Support.Error
open Syntax
open Arg 
open Evaluator

(* #####################
 * #### INTERPRETER ####
 * ##################### *)

type machine = Compiler | Interpreter 

let parse' in_channel   =   (* in_channel -> command list *) 
    (* let lexbuf              =   Lexer.create f in_channel in *)
    let lexbuf              =   Lexing.from_channel in_channel  in
    let result              =   try     Parser.toplevel Lexer.token lexbuf
                                with  | Parsing.Parse_error -> error (Lexer.info lexbuf) "Parse error" 
                                      | e -> raise e 
    in
    Parsing.clear_parser(); close_in in_channel; result


(* ######################
 * ####   COMPILER   ####
 * ###################### *)

let searchpath          = ref  [""]
let addSearchpath f     = searchpath := f :: !searchpath
let argDefs             = [ ("-I", String addSearchpath, "Append a dir to searchpath")]
let fileCase            = ref (None : string option )
let getFile ()          = match !fileCase with
    | None                  -> err "Cannot get File"
    | Some s                -> s;;
let readOneFile str     = match !fileCase with 
    | Some (_)              -> err "Specify Single File."
    | None                  -> fileCase := Some str 
let parseArgs ()        = parse argDefs readOneFile "" ;; 
let searchFile f        =  (* string -> in_channel *)
    let rec trynext         = function 
        | []                    ->  err ("Could not find " ^ f)
        | (d::rest)             ->  let name = if d = "" then f else (d ^ "/" ^ f) in
                                    try open_in name    with Sys_error m -> trynext rest in 
    trynext !searchpath

let parseFile f         =  (* string -> command list *) 
    let in_channel          =   searchFile f in 
    parse' in_channel 

let process_file str    =  (* string -> unit *)  (* print the evals of the list of commands *)  
    let cmds                =   parseFile str in
    List.iter print_eval cmds


let main ()         =   parseArgs (); 
                        process_file (getFile ()) 
let ()              = set_max_boxes 1000
let ()              = set_margin 67
let res             = Printexc.catch (
    fun () -> 
        try main();0 
        with 
            Exit 10 -> flush stdout; main () ; 10 
          | Exit x -> x

    ) ()
let ()              = print_flush()
let ()              = exit res 
