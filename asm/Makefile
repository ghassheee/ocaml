OCAMLC=ocamlc -safe-string

all: ml  
	${OCAMLC} -o Asm support.cmo lexer.cmo parser.cmo syntax.cmo asm.cmo 

parser:	
	${OCAMLC} -c support.ml 
	${OCAMLC} -c syntax.ml
	ocamlyacc parser.mly 
	${OCAMLC} -c parser.mli
	
lexer: parser  
	ocamllex lexer.mll

ml: lexer 
	${OCAMLC} -c lexer.ml parser.ml syntax.ml asm.ml 


clean: 
	rm -rf *.mli *.cmo *.cmi Asm lexer.ml parser.ml
