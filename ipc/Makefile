

OBJS = support.cmo syntax.cmo lexer.cmo parser.cmo proof.cmo compiler.cmo


DEPEND += lexer.ml parser.ml 


all: $(DEPEND) $(OBJS) f

include .depend 

# Dependency 
depend: $(DEPEND) 
	ocamldep $(INCLUDE) *.mli *.ml > .depend 

# Lexer
%.ml %.mli: %.mll
	@rm -f $@
	ocamllex $< 
	@chmod -w $@

# Parser
parser.ml parser.mli: parser.mly
	@rm -f parser.ml parser.mli
	ocamlyacc -v parser.mly
	@chmod -w parser.ml parser.mli

# Module Interface 
%.cmi : %.mli
	ocamlc -c $<

# implementation 
%.cmo : %.ml
	ocamlc -c $<

# Build 
f: $(OBJS) main.cmo 
	@echo Linking $@
	ocamlc -o $@ $(OBJS) main.cmo 

test: f test.f 
	./f test.f 

# Clean 
clean::
	rm -rf lexer.ml parser.ml parser.mli *.o *.cmo *.cmi parser.output f TAGS
