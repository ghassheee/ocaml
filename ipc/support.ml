open Format 



    let pr = print_string;;
    let pe = print_endline;; 
    let pi = print_int;;
    let ps ()= pr" ";;
    let pf = print_flush();;
    let ovb0 = open_vbox 0;;
    let ohvb0 = open_hvbox 0;;
    let cb = close_box();;
    let pn() = pr"\n";;

    exception Exit of int 

    type info               = FI of string * int * int 
                            | UNKNOWN

    type 'a withinfo        = {i:info; v:'a} 

    let dummy               = UNKNOWN
    let createInfo f l c    = FI(f,l,c)
    let errf f              = pf; ovb0; ohvb0; f(); cb; pn(); raise(Exit 1);;
    let pr_info             = function
        | FI(f,l,c)             -> pr f;pr":";pi l; pr"."; pi c; pr":"
        | UNKNOWN               -> pr "<Unknown file and line>: "
    let errAt fi f          = errf (fun()-> pr_info fi; ps();f())
    let err fi s            = errAt fi (fun() -> pr s; pn());;
    let errIO s             = errf (fun()-> pr "IOERROR: "; pr s; pn();)
    let warning s           = pr "Warning: "; pr s; pn();;
    let warningAt fi s      = pr_info fi; warning s;;

    
