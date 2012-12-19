COMMON_OBJS =\
	scanner.cmo\
	parser.cmo\
	ast.cmo\
	astutils.cmo\
	translate.cmo\
	semantic.cmo

MASL_OBJS =\
	$(COMMON_OBJS)\
	toplevel.cmo

PRINTAST_OBJS =\
	$(COMMON_OBJS)\

CLEAN_OBJS =\
	scanner.ml\
	parser.ml\
	parser.mli\
	parser.output\
	*.cmo\
	*.cmi\
	*.cmx\
	masl

.PHONY: all
all: masl

masl : $(MASL_OBJS)
	ocamlc -o $@ unix.cma $(MASL_OBJS)

scanner.ml: scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli: parser.mly
	ocamlyacc -v parser.mly

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

ast.cmi: ast.ml
	ocamlc -c ast.ml

astutils.cmi: printast.ml
	ocamlc -c printast.ml

semantic.cmi: semantic.ml
	ocamlc -c semantic.ml

translate.cmi: translate.ml
	ocamlc -c translate.ml

toplevel.cmo toplevel.cmi: toplevel.ml
	ocamlc -c toplevel.ml

.PHONY: clean
clean:
	rm -rf $(CLEAN_OBJS)

scanner.cmo: parser.cmi

parser.mli: ast.cmi
parser.cmo: parser.cmi ast.cmi

astutils.cmo: ast.cmi
translate.cmo: ast.cmi
semantic.cmo: ast.cmi