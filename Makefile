OBJS = parser.cmo scanner.cmo printer.cmo compile.cmo flowc.cmo


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
flowc.cmo: scanner.cmo printer.cmo parser.cmi ast.cmo compile.cmo
flowc.cmx: scanner.cmx printer.cmx parser.cmx ast.cmx compile.cmx
parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmx parser.cmi
printer.cmo : ast.cmo
printer.cmx : ast.cmx
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
parser.cmi: ast.cmo

compile.cmo : ast.cmo
compile.cmx : ast.cmx
