OBJS = parser.cmo scanner.cmo printer.cmo sprinter.cmo boilerplate.cmo compile.cmo semantic_analysis.cmo flowc.cmo


# Choose one
YACC = ocamlyacc
# YACC = menhir --explain

TARFILES = Makefile scanner.mll parser.mly flowc.ml

flowc : $(OBJS)
	ocamlc -o flowc $(OBJS)

.PHONY : test
test : flowc testall.sh
	./testall.sh

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	$(YACC) parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

flowc.tar.gz : $(TARFILES)
	cd .. && tar czf flowc/flowc.tar.gz $(TARFILES:%=flowc/%)

.PHONY : clean
clean :
	rm -f flowc parser.ml parser.mli scanner.ml testall.log \
	*.cmo *.cmi *.out *.diff

# Generated by ocamldep *.ml *.mli
ast.cmo:
ast.cmx:
flowc.cmo: scanner.cmo printer.cmo sprinter.cmo parser.cmi ast.cmo boilerplate.cmo compile.cmo semantic_analysis.cmo
flowc.cmx: scanner.cmx printer.cmx sprinter.cmx parser.cmx ast.cmx boilerplate.cmo compile.cmx semantic_analysis.cmx
parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmx parser.cmi
printer.cmo : ast.cmo
printer.cmx : ast.cmx
sprinter.cmo : sast.cmo
sprinter.cmx : sast.cmx
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
parser.cmi: ast.cmo

boilerplate.cmo :
boilerplate.cmx :
compile.cmo : ast.cmo
compile.cmx : ast.cmx
semantic_analysis.cmo: ast.cmo sast.cmo
semantic_analysis.cmx: ast.cmx sast.cmx
