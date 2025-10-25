{ 
    open Support 


    let reservedWords = [
        ("~",   fun i -> Parser.NOT i);
        ("/\\", fun i -> Parser.AND i);
        ("\\/", fun i -> Parser.OR i);
        ("->",  fun i -> Parser.ARROW i);
        (";",   fun i -> Parser.SEMI i);
        ("Top", fun i -> Parser.TOP i);
        ("Bot", fun i -> Parser.BOT i);

        ("(",   fun i -> Parser.LPAREN i);
        (")",   fun i -> Parser.RPAREN i);
    ]

    let ios                 = int_of_string

    type tokenize           =   info -> Parser.token;;
    type tokentbl           =   (string,tokenize) Hashtbl.t;;
    let symboltbl:tokentbl  =   Hashtbl.create 1024;;
    let _                   =   List.iter (fun(str,f)->Hashtbl.add symboltbl str f) reservedWords
    let initCapital str     =   let s=String.get str 0 in s>='A' && s<='Z' 

    let createID i str      =   try     Hashtbl.find symboltbl str i
                                with _  ->  if initCapital str 
                                            then Parser.UCID {i=i;v=str} 
                                            else Parser.LCID {i=i;v=str};;

    let lineno              =   ref 1
    let depth               =   ref 0
    let start               =   ref 0
    let filename            =   ref ""
    let startLex            =   ref dummy 
    let create infile strm  =   if not(Filename.is_implicit infile)
                                    then filename   := infile
                                    else filename   := Filename.concat (Sys.getcwd()) infile;
                                lineno := 1; start := 0; Lexing.from_channel strm;;
    
    let newline lexbuf      =   incr lineno; start := 0; Lexing.lexeme_start lexbuf;;
    let info    lexbuf      =   createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start);;
    let text                =   Lexing.lexeme;;
    
    (* String *)
    let stringBuffer        =   ref (Bytes.create 2048);;
    let stringEnd           =   ref 0;;
    let resetStr ()         =   stringEnd := 0;;
    let addStr chr          =   let x       = !stringEnd in 
                                let buffer  = !stringBuffer in 
                                if x = Bytes.length buffer 
                                    then    let newBuffer   = (Bytes.create (x*2)) in 
                                            Bytes.blit buffer 0 newBuffer 0 x;
                                            Bytes.set newBuffer x chr;
                                            stringBuffer    := newBuffer;
                                            stringEnd       := x+1
                                    else    Bytes.set buffer x chr;
                                            stringEnd       := x+1;;
    let getStr ()           =   Bytes.sub (!stringBuffer) 0 (!stringEnd) ;;
    let extract_lineno yytxt offset     = ios(Bytes.to_string(Bytes.sub yytxt offset(Bytes.length yytxt - offset)));;
    let out_of_char x fi    =   if x>255 then err fi"Illegal Char" else Char.chr x ;;
}

(* lexical analyzer *) 
let digit               =   ['0'-'9']
let init                =   ['a'-'z' 'A'-'Z' '_' ]
let tail                =   ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_' ]
let tabs                =   [' ' '\009' '\012']
let op                  =   ['>' '~' '%' '/' '\\' '+' '-' '&' '|' ':' '`' '$' '^' '*']
let symbol              =   ['*' '#' '/' '!' '?' '^' '(' ')' '{' '}' '[' ']' '<' '>' '.' ';' '_' ',' '=' '\'']
let nl                  =   tabs*("\r")?"\n"
let comment             =   "/*"
let comment_end         =   "*/"

rule token              =   parse
    | tabs+                 { token lexbuf                                                                   } 
    | nl                    { newline lexbuf; token lexbuf                                                   }
    | init tail*            { createID (info lexbuf) (text lexbuf)                                          }
    | symbol                { createID (info lexbuf) (text lexbuf)                                          }
    | op+                   { createID (info lexbuf) (text lexbuf)                                          }
    | "\""                  { resetStr(); startLex := info lexbuf; string lexbuf                            }
    | ";;" nl               { Parser.DSEMI(info lexbuf)                                                     }
    | eof                   { Parser.EOF(info lexbuf)                                                       }
    | comment_end           { err (info lexbuf) "Unmatched end of comment"                                  } 
    | comment               { depth:=1; startLex := info lexbuf; comment lexbuf; token lexbuf               } 

and comment             =   parse
    | comment               { depth := succ !depth; comment lexbuf                                          }
    | comment_end           { depth := pred !depth; if !depth>0 then comment lexbuf                         } 
    | eof                   { err(!startLex) "Comment not terminated"                                       } 
    | [^ '\n']              { comment lexbuf                                                                }
    | "\n"                  { newline lexbuf; comment lexbuf                                                } 

and string              =   parse
    | '"'                   { Parser.STRINGV{i= !startLex; v=Bytes.to_string(getStr())}                     } 
    | eof                   { err(!startLex) "String not terminated"                                        }
    | '\\'                  { addStr(escaped lexbuf)                    ; string lexbuf                     }
    | '\n'                  { addStr('\n') ; newline lexbuf             ; string lexbuf                     }
    | _                     { addStr(Lexing.lexeme_char lexbuf 0)       ; string lexbuf                     }

and escaped             =   parse
    | 'n'                   { '\n'                                                                          }
    | 't'                   { '\t'                                                                          }
    | '\\'                  { '\\'                                                                          }
    | '"'                   { '\034'                                                                        }
    | '\''                  { '\''                                                                          }
    | digit digit digit     { out_of_char (ios(text lexbuf)) (info lexbuf)                                  }
    | [^ '"'  '\\' 't' 'n' '\''] { err (info lexbuf) "Illegal character constant"                           }

                                
                                    
                                     


