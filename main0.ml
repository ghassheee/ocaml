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
