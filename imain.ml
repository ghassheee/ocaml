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

let parse' in_channel   =   (* in_channel -> command list *) 
    (* let lexbuf              =   Lexer.create f in_channel in *)
    let lexbuf              =   Lexing.from_channel in_channel  in
    let result              =   try     Parser.input Lexer.token lexbuf
                                with  | Parsing.Parse_error -> error (Lexer.info lexbuf) "Parse error" 
                                      | e -> raise e 
    in
    Parsing.clear_parser(); close_in in_channel; result

let process' ()         = 
    let cmds                = parse' stdin in 
    List.iter print_eval cmds

(* interpreter *) 
let rec parse_error s = print_endline s; flush stdout ;; 
let main' () =
    while true do 
        try process' (); print_endline "debug loop"
        with    End_of_file -> print_endline "end_of_file"
            |   e           -> raise e
            | _     -> error (Lexer.info (Lexing.from_channel stdin)) "Raised from main process error"  
    done

let _ = Printexc.catch (fun () -> try main' (); 0 with Exit x -> x) () 

