RESULT = build-book
SOURCES = SS.ml SSSCPinfty.ml SSSBSU.ml Run.ml
LIBS = # unix
INCDIRS =
OCAMLMAKEFILE = ./OCamlMakefile

include $(OCAMLMAKEFILE)

run : byte-code
	./$(RESULT) $(shell cat args)
	pdflatex main
