open Arg

open Support
open Syntax
open Proof


let compiler lexbuf = Parser.toplevel Lexer.token lexbuf ;;


let parse machine in_channel = 
    let lexbuf              =   Lexing.from_channel in_channel in 
    let result,ctx          =   try     machine lexbuf emptyctx
                                with  | Parsing.Parse_error -> err(Lexer.info lexbuf)"Parse error"
                                      | e                   -> raise e in 
    Parsing.clear_parser(); close_in in_channel; result

let path                    = ref [""];;
let addpath f               = path := f :: !path ;;
let argDefinitions          = [ ("-I", String addpath, "Append a dir to path") ]
let files                   = ref (None : string option);;
let getFile ()              = match !files with 
    | None                      -> errIO "Cannot get File"
    | Some s                    -> s;;
let pushFile str            = match !files with 
    | Some _                    -> errIO "Specify Single File."
    | None                      -> files := Some str;;
let parseArgs ()            = Arg.parse argDefinitions pushFile "" ;;
let openFile f              = 
    let rec trynext             = function
        | []                        -> errIO ("Could not find " ^ f)
        | d::rest                   -> let name = if d = "" then f else (d^"/"^f) in
                                       try open_in name with Sys_error m -> trynext rest in 
    trynext !path;;

let parseFile f =
    let in_channel          = openFile f in 
    parse compiler in_channel 

let processFile str ctx = 
    let cmds = parseFile str in process_cmds ctx cmds
