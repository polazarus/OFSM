OCAMLC = ocamlc
OCAMLOPT = ocamlopt

OCAMLCFLAGS = -I ../
OCAMLOPTFLAGS = -I ../

SOURCES = *.ml

all: $(SOURCES:.ml=)

% : %.cmo ../oFSM.cma
	$(OCAMLC) $(OCAMLCFLAGS) ../oFSM.cma $< -o $@

%.cmo:%.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $< 

%.cmx:%.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

%.cmi:%.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $<
