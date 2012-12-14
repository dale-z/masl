COMMON_OBJS =\
	scanner.cmo\
	parser.cmo\
	ast.cmo

MASL_OBJS =\
	$(COMMON_OBJS)\
	toplevel.cmo

PRINTAST_OBJS =\
	$(COMMON_OBJS)\
	printast.cmo

CLEAN_OBJS =\
	scanner.ml\
	parser.ml\
	parser.mli\
	parser.output\
	*.cmo\
	*.cmi\
	*.cmx\
	printast\
	masl

.PHONY: all
all: masl printast

masl : $(MASL_OBJS)
	ocamlc -o $@ $(MASL_OBJS)

printast : $(PRINTAST_OBJS)
	ocamlc -o $@ $(PRINTAST_OBJS)

scanner.ml: scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli: parser.mly
	ocamlyacc -v parser.mly

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

ast.cmi ast.cmo: ast.ml
	ocamlc -c ast.ml

printast.cmo printast.cmi: printast.ml
	ocamlc -c printast.ml

toplevel.cmo toplevel.cmi: toplevel.ml
	ocamlc -c toplevel.ml

.PHONY: clean
clean:
	rm -rf $(CLEAN_OBJS)

scanner.cmo: parser.cmi
scanner.cmx: parser.cmx

parser.mli: ast.cmi
parser.cmo: parser.cmi ast.cmi
parser.cmx: parser.cmi ast.cmi