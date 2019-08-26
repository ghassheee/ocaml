open Format
open Support.Pervasive
open Support.Error
open Syntax
open Core
open Arg 

let searchpath  = ref [""]
let argDefs     = [
  ("-I", String(fun f -> searchpath := f::!searchpath),"Append a dir to searchpath")
]

(*  open Arg
 *  type spec = | Unit of (unit -> unit) 
 *              | String of (string -> unit) 
 *              | ...
 *  val parse :
 *      (key * spec * doc) list -> anon_fun -> usage_msg -> unit 
 *  type anon_fun = string -> unit 
 *      
 *)

let readOneFile inFile str = match !inFile with 
    | Some (_)              -> err "You cannot load multiple files."
    | None                  -> inFile := Some str 

let parseArgs ()    =   let inFile = ref (None : string option) in
    parse argDefs (fun str -> readOneFile inFile str) ""; 
    match !inFile with
        | None          -> err "You must specify an input file"
        | Some(s)       -> s

let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath

let parseFile inFile =
    let pi      =   openfile inFile in 
    let lexbuf  =   Lexer.create inFile pi in 
    let result  =   try Parser.toplevel Lexer.main lexbuf 
                    with Parsing.Parse_error -> error (Lexer.info lexbuf) "Parse error" in
    Parsing.clear_parser(); close_in pi; result

let alreadyImported = ref ([] : string list)

let rec process_command  cmd = match cmd with
  | Eval(fi,t) -> 
      let t' = eval t in
      printtm_ATerm true t'; 
      force_newline();
      ()
  
let process_file f  =
  alreadyImported := f :: !alreadyImported;
  let cmds = parseFile f in
  let g  c =  
    open_hvbox 0;
    let results = process_command  c in
    print_flush();
    results
  in
    List.iter g  cmds

let main () = 
  let inFile = parseArgs() in
  let _ = process_file inFile  in
  ()

let ()  = set_max_boxes 1000
let ()  = set_margin 67
let res = Printexc.catch (fun () -> 
    try main();0 
    with Exit x -> x) 
  ()
let () = print_flush()
let () = exit res
let parse_error s = print_endline s; flush stdout
let main () =
    try 
        let lexbuf = Lexing.from_channel stdin in
        while true do 
            Calc.input Lexer.token lexbuf
        done
    with 
          End_of_file           -> exit 0
        | Parsing.Parse_error   -> parse_error "Parse error" 


let _ = Printexc.print main ()
