open Format


let pi  = print_int
let pr  = print_string
let ps  = print_space
let pe  = print_endline
let pn  = print_newline
let cut = print_cut


module Error = struct
    exception Exit of int

    type info               =   FI of string * int * int | UNKNOWN
    type 'a withinfo        =   {i: info; v: 'a}

    let dummy               =   UNKNOWN
    let createInfo f l c    =   FI(f, l, c)
    let errf f              =   print_flush(); 
        open_vbox 0; open_hvbox 0; f(); cut(); 
                     close_box(); pn();raise (Exit 1)
    let prInfo           =   function
           FI(f,l,c)            ->  pr f; pr ":"; pi l; pr "."; pi c; pr ":"
        | UNKNOWN               ->  pr "<Unknown file and line>: "
    let errfAt fi f         =   errf ( fun() -> prInfo fi; ps (); f())
    let err s               =   errf ( fun() -> pr "Error: "; pr s; pn());;
    let error fi s          =   errfAt fi (fun()-> pr s; pn())
    let warning s           =   pr "Warning: "; pr s; pn()
    let warningAt fi s      =   prInfo fi; pr " Warning: "; pr s; pn()
end


module Pervasive = struct

    type info                   = Error.info
    let pr                      = Format.print_string
    let pe                      = print_endline
    let pn                      = print_newline
    let pi                      = print_int
    let ps                      = print_space
    let pb                      = Format.print_break
    let getbind_err_msg         =  
        Printf.sprintf "getbind: Variable lookup failure: offset:%d,ctx size:%d" 

end 


