COMMON_OBJS =\
	scanner.cmo\
	parser.cmo\
	ast.cmo\
	astutils.cmo\
	translate.cmo

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
	export OCAMLRUNPARAM=b

masl : $(MASL_OBJS)
	ocamlc -g -o $@ str.cma $(MASL_OBJS)

scanner.ml: scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli: parser.mly
	ocamlyacc -v parser.mly

%.cmo: %.ml
	ocamlc -c -g $<

%.cmi: %.mli
	ocamlc -c -g $<

ast.cmi: ast.ml
	ocamlc -c -g ast.ml

astutils.cmi: printast.ml
	ocamlc -c -g printast.ml

translate.cmi: translate.ml
	ocamlc -c -g translate.ml

toplevel.cmo toplevel.cmi: toplevel.ml
	ocamlc -c -g toplevel.ml

.PHONY: clean
clean:
	rm -rf $(CLEAN_OBJS)

scanner.cmo: parser.cmi

parser.mli: ast.cmi
parser.cmo: parser.cmi ast.cmi

astutils.cmo: ast.cmi
translate.cmo: ast.cmi
